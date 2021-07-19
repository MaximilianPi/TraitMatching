#' Fit TM model
#'
#' @description Fit, tune and cross-validate a TM model
#'
#' @param community object of class classCommunity created by [TraitMatching::createCommunity()]
#' @param settings list of settings for the machine learning model. See details
#' @param method Which ML algorithm to be used. RF, kNN, SVM, DNN, CNN, boost, ngBinDNN, naive, or CNN. See details. "RF" is default
#' @param tune How to tune ML parameters. We support only "random" at the moment.
#' @param metric Which predictive measurement should be used for tuning. "AUC" is default. See details.
#' @param parallel boolean or numeric. See details (not yet supported).
#' @param iters Number of tuning steps
#' @param crossValidation List for CV plan. See details.
#' @param group blocked-CV design, block for X or Y species, or not: I
#' @param balance How to balance classes. Default is oversampling "Over".
#' @param seed set seed
#'
#' @example /inst/examples/example-runTM.R
#' @author Maximilian Pichler
#' @export


runTM = function(community,
                 settings = NULL,
                 method = "RF",
                 tune = c("random","grid"),
                 metric = c("AUC", "R2", "Spear"),
                 parallel = TRUE,
                 iters = 20L,
                 crossValidation = list(
                   outer = list(method = "CV", iters = 10),
                   inner = list(method = "CV", iters = 3)),
                 group = c("X", "Y", "I"),
                 balance = c("no", "oversample", "undersample", "smote"),
                 seed = 42){
  
  set.seed(seed)

  stopifnot(
    any(method %in% c("RF", "SVM", "kNN", "DNN", "BRT")),
    inherits(community, "Community")
  )
  tune = match.arg(tune)
  metric = match.arg(metric)
  if(inherits(community, "regr") && metric == "AUC") metric = "Spear"
  
  balance = match.arg(balance)
  group = match.arg(group)
  if(balance == "no") balance = FALSE

  out = list()

  type = class(community)[2]

  out$type = type
  
  if(parallel) {
    if(is.numeric(parallel)) future::plan("multisession", workers = parallel)
    else future::plan("multisession")
  }
  
  if(type == "regr") balance = FALSE
  
  task =
    if(type == "classif") { mlr3::TaskClassif$new(id = "classif_community", backend = community$data, target = community$target, positive = "positive")
    } else { mlr3::TaskRegr$new(id = "regr_community", backend = community$data, target = community$target) }
  
  if(group != "I") task$col_roles$group = group
  task$col_roles$feature = setdiff(task$col_roles$feature, c("X", "Y"))
  if(group == "I") task$col_roles$stratum = community$target
  
  ## transform task ##
  out$encode = mlr3pipelines::po('encode')
  task = out$encode$train(list(task))[[1]]

  ## extra arguments ##
  extra = list()
  extra$n_features = length(task$feature_names)
  extra$type = type
  extra$balance = balance


  ## resample strategy ##
  crossValidation$block=""
  resampleStrat = getResampleStrategy(crossValidation, community$data[,c(1,2)], task)

  ## Tuner ##
  terminator = mlr3tuning::trm("evals", n_evals = iters)
  tuner =
    if(tune == "random") { mlr3tuning::tnr("random_search")
    } else { mlr3tuning::tnr("grid_search", resolution = 2L) }

  ## Measurement ##
  measures = getMeasure(metric, type)

  ## Build learner pipelines ##
  if(!is.logical(balance)) {
    prefix = paste0(type, ".")
  } else { prefix = ""}

  learners = lapply(method, function(m) getBaseLearners(m, type))
  pars = lapply(method, function(m) getParamSets(m, extra, prefix = prefix))
  if(!is.logical(balance)) {
    balanceLearner = getBalanceMethod(balance)
    pars = lapply(pars, function(p) p$add(balanceLearner$pars))
    learners = lapply(learners, function(l) mlr3pipelines::GraphLearner$new(mlr3pipelines::`%>>%`(balanceLearner$po, l) ))
  }
  learners2 = lapply(1:length(learners),
    function(i) {
      mlr3tuning::AutoTuner$new(learners[[i]],
                                resampling = resampleStrat$inner,
                                search_space = pars[[i]],
                                terminator = terminator,
                                tuner = tuner,
                                measure = measures) })
  
  design = mlr3::benchmark_grid(task, learners2, resamplings = resampleStrat$outer)
  result = mlr3::benchmark(design, store_models = TRUE)
  
  summary = data.table::as.data.table(result,measures = measures, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test")
  res = result$aggregate(measures)
  
  
  ## build full model ensembles ## 
  
  ensembles = lapply(1:length(learners), function(i) getEnsemble(base = learners[[i]]$clone(), 
                                                                 models = result$resample_results$resample_result[[i]]$learners))
  
  names(ensembles) = method
  
  .n = lapply(ensembles, function(e) e$train(task))
  
  out$ensembles = ensembles
  out$task = task
  out$design = design
  out$result = list(result_raw = result, tabular = summary, result = res, learners = learners)
  out$extra = extra
  class(out) = "TraitMatchingResult"
  return(out)
}


#' Predict from a fitted TraitMatchingResult object
#' 
#' @param object a model fitted by \code{\link{runTM}}
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @export
predict.TraitMatchingResult = function(object, newdata = NULL, ...) {
  
  if(object$type == "classif") {
  
    if(is.null(newdata)) {
      pred = (sapply(1:length(object$ensembles), function(i) object$ensembles[[i]]$predict(object$task)$data$prob[,1]))
    } else {
      pred = (sapply(1:length(object$ensembles), function(i) object$ensembles[[i]]$predict_newdata(newdata)$data$prob[,1]))
    }
  
  } else {
    
    if(is.null(newdata)) {
      pred = (sapply(1:length(object$ensembles), function(i) object$ensembles[[i]]$predict(object$task)$data$response))
    } else {
      pred = (sapply(1:length(object$ensembles), function(i) object$ensembles[[i]]$predict_newdata(newdata)$data$response))
    }
  }
  
  pred = data.frame(pred)
  colnames(pred) = names(object$ensembles)
  return(pred)
}


#' Print a fitted TraitMatchingResult model
#' 
#' @param x a model fitted by \code{\link{runTM}}
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @export
print.TraitMatchingResult = function(x, ...) {
  print(x$result$result)
}
