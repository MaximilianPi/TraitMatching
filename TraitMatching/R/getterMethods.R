
getMeasure = function(metric, type) {
  # c("AUC", "R2", "Spear")
  measure =
    switch (metric,
            AUC = mlr3::msr("classif.auc"),
            R2 = mlr3::msr("regr.rsq"),
            Spear = mlr3::msr("regr.srho")
    )
  if(type == "classif" && (measure$task_type != "classif")) {
    maesure = mlr3::msr("classif.auc")
    warning("R2 or Spear is not possible for classification.",call. = FALSE)
  }
  
  if(type == "regr" && (measure$task_type != "regr")) {
    maesure = mlr3::msr("regr.srho")
    warning("AUC is not possible for regression.",call. = FALSE)
  }
  return(measure)
}

getResampleStrategy = function(crossValidation) {
  parseResample = function(method, iters) {
    if(method == "CV") return(mlr3::rsmp("cv", folds = iters))
  }
  outer = do.call(parseResample, crossValidation$outer)
  inner = do.call(parseResample, crossValidation$inner)
  return(list(outer = outer, inner = inner))
}


getBalanceMethod = function(balance = c("oversample", "undersample", "smote")) {
  balance = match.arg(balance)
  
  po = switch(balance,
              oversample =  mlr3pipelines::po("classbalancing", id = balance, adjust = "minor", ratio = 5L),
              undersample = mlr3pipelines::po("classbalancing", id = balance, adjust = "major", ratio = 5L),
              smote =       mlr3pipelines::po("classbalancing", id = balance)
  )
  
  pars = switch (balance,
                 oversample =  list(paradox::ParamDbl$new("oversample.ratio", lower = 1L, upper = 10L)),
                 undersample = list(paradox::ParamDbl$new("undersample.ratio", lower = 1 / 6, upper = 1)),
                 smote =       list(paradox::ParamInt$new("smote.ratio", lower = 1, upper = 10L))
  )
  return(list(po = po, pars = paradox::ParamSet$new(pars)))
}


getBaseLearners = function(method = "RF", type = c("classif", "regr")) {
  type = match.arg(type)
  learner = switch (method,
                    RF  = paste0(type,".ranger"),
                    SVM = paste0(type,".svm"),
                    kNN = paste0(type,".kknn"),
                    DNN = paste0(type,""),
                    BRT = paste0(type,".xgboost"),
  )
  if(type=="classif") return(mlr3::lrn(learner, predict_type = "prob"))
  else return(mlr3::lrn(learner))
}




getParamSets = function(method = "RF", extra = NULL, prefix = "") {
  pars = switch(method,
                RF = getRFparamSet(extra, prefix),
                kNN = getKNNparamSet(extra, prefix),
                SVM = getSVMparamSet(extra, prefix),
                BRT= list(paradox::ParamDbl$new(paste0(prefix, "xgboost.alpha"), lower = 0.0, upper = 3.0),
                          paradox::ParamFct$new(paste0(prefix, "xgboost.booster"), levels = c("gbtree", "gblinear", "dart"), default = "gbtree"),
                          paradox::ParamDbl$new(paste0(prefix, "xgboost.eta"),lower = 0.0, upper = 3.0),
                          paradox::ParamFct$new(paste0(prefix, "xgboost.feature_selector"), levels = c("cyclic", "shuffle", "random", "greedy", "thrifty"), default = "cyclic"),
                          paradox::ParamDbl$new(paste0(prefix, "xgboost.gamma"), lower = 0.0, upper = 3.0),
                          paradox::ParamDbl$new(paste0(prefix, "xgboost.lambda"), lower = 0.0, 5.0),
                          paradox::ParamDbl$new(paste0(prefix, "xgboost.lambda_bias"), lower = 0.0,upper = 5.0),
                          paradox::ParamInt$new(paste0(prefix, "xgboost.max_depth"), lower = 1L, upper = 50L ),
                          paradox::ParamInt$new(paste0(prefix, "xgboost.nrounds"), lower = 1L, upper = 80L))
                
  )
  return(pars)
}


getRFparamSet = function(extra, prefix) {
  
  if(!is.logical(extra$balance)) pf = function(p2) paste0(prefix,"ranger.", p2)
  else pf = function(p2) p2
  
  return(
    paradox::ParamSet$new(
      list(paradox::ParamDbl$new(pf("alpha"), lower = 0.0, upper = 1.0),
           paradox::ParamInt$new(pf("min.node.size"), lower = 1, upper = 30L),
           paradox::ParamInt$new(pf("mtry"), lower = 1, upper = extra$n_features),
           paradox::ParamLgl$new(pf("regularization.usedepth"), default = TRUE))))
}

getKNNparamSet = function(extra, prefix) {
  
  if(!is.logical(extra$balance)) pf = function(p2) paste0(prefix,"kknn.", p2)
  else pf = function(p2) p2
  
  return(
    paradox::ParamSet$new(
      list(paradox::ParamInt$new(pf("k"), lower = 1L, 20L),
           paradox::ParamFct$new(pf("kernel"), levels=c("rectangular","triangular","epanechnikov","biweight","triweight","optimal"),default = "optimal"))))
}

getSVMparamSet = function(extra, prefix) {
  if(!is.logical(extra$balance)) pf = function(p2) paste0(prefix,"svm.", p2)
  else pf = function(p2) p2
  
  SVMtypes =
    if(extra$type == "regr") { c("eps-regression","nu-regression")
    } else { c("C-classification","nu-classification") }
  
  pars =
    paradox::ParamSet$new(
      list(paradox::ParamFct$new(pf("kernel"), levels=c("linear", "polynomial", "radial", "sigmoid"), default = "radial"),
           paradox::ParamDbl$new(pf("gamma"), lower = 0.0, upper = 5),
           paradox::ParamInt$new(pf("degree"), lower = 1, upper = 5),
           paradox::ParamDbl$new(pf("nu"), lower = 0, upper = 3),
           paradox::ParamFct$new(pf("type"), levels = SVMtypes, default = SVMtypes[1])
      ))
  
  if(extra$type == "classif") {
    pars$add(paradox::ParamDbl$new(pf("cost"), lower = 0, upper = 3))
    pars$add_dep(pf("cost"), pf("type"),cond = paradox::CondEqual$new(SVMtypes[1]))
  } else {
    pars$add(paradox::ParamDbl$new(pf("eps"), lower = 0, upper = 1))
    pars$add_dep(pf("eps"), pf("type"),cond = paradox::CondEqual$new(SVMtypes[1]))
  }
  pars$add_dep(pf("nu"), pf("type"),cond = paradox::CondEqual$new(SVMtypes[2]))
  pars$add_dep(pf("degree"), pf("kernel"), cond = paradox::CondEqual$new("polynomial"))
  return(pars)
}