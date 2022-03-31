
getEnsemble = function(base, models) {
  raw = lapply(1:length(models), function(j) {
    tmp = models[[j]]
    base$param_set$values = tmp$learner$param_set$values
    return( mlr3pipelines::PipeOpLearner$new(base$clone(deep = TRUE), id = paste0(tmp$id,"_", j))  )
  })
  avg = mlr3pipelines::po(paste0(base$task_type, "avg"), innum=length(models))
  ensemble = mlr3pipelines::`%>>%`(mlr3pipelines::gunion(raw), avg)
  return(mlr3pipelines::GraphLearner$new(ensemble))
}


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

getResampleStrategy = function(crossValidation, species, task) {
  
  parseResample = function(method, iters) {
    if(method == "CV") return(mlr3::rsmp("cv", folds = iters))
  }
  
  createSampleOne = function(species, iters, w) {
    if(length(w) == 1) {
      sub = species[[w]]
      SubSp = length(unique(sub))/iters
      OuterSp = data.frame(matrix(NA, iters, ceiling(SubSp)))
      
      lenSp = length(unique(sub))
      Sp = matrix(c( unique(sub), rep(NA,  ceiling(SubSp)*iters - lenSp)), ncol = ceiling(SubSp), nrow = iters)
      test_indices = lapply(1:iters, function(i) which(sub %in% levels(sub)[Sp[i,]], arr.ind = TRUE ))
      train_indices = lapply(1:iters, function(i) which(!sub %in% levels(sub)[Sp[i,]], arr.ind = TRUE ))
    } else {
      sub1 = species[[1]]
      sub2 = species[[2]]
      SubSp1 = length(unique(sub1))/iters
      SubSp2 = length(unique(sub2))/iters
      OuterSp1 = data.frame(matrix(NA, iters, ceiling(SubSp1)))
      OuterSp2 = data.frame(matrix(NA, iters, ceiling(SubSp2)))
      
      lenSp1 = length(unique(sub1))
      lenSp2 = length(unique(sub2))
      Sp1 = matrix(c( unique(sub1), rep(NA,  ceiling(SubSp1)*iters - lenSp1)), ncol = ceiling(SubSp1), nrow = iters)
      Sp2 = matrix(c( unique(sub2), rep(NA,  ceiling(SubSp2)*iters - lenSp2)), ncol = ceiling(SubSp2), nrow = iters)
      test_indices = lapply(1:iters, function(i) c(which(sub1 %in% levels(sub1)[Sp1[i,]] | sub2 %in% levels(sub2)[Sp2[i,]], arr.ind = TRUE )))
      train_indices = lapply(1:iters, function(i) c(which(!(sub1 %in% levels(sub1)[Sp1[i,]])  & !(sub2 %in% levels(sub2)[Sp2[i,]]), arr.ind = TRUE )))
    }
    
    outer = mlr3::rsmp("custom")
    outer$instantiate(task, train_indices,  test_indices)
    return(outer)
  }
  
  if(crossValidation$block == "") {
    outer = do.call(parseResample, crossValidation$outer)
  }
  inner = do.call(parseResample, crossValidation$inner)
  
  if(crossValidation$block == "A") {
    outer = createSampleOne(species, crossValidation$outer$iters, w = 1)
  } else if(crossValidation$block == "B") {
    outer = createSampleOne(species, crossValidation$outer$iters, w = 2)
  }
  
  if(crossValidation$block == "AB") {
    outer = createSampleOne(species, crossValidation$outer$iters, w = c(1,2))
  }
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
  if(method != "RF") {
    if(type=="classif") return(mlr3::lrn(learner, predict_type = "prob"))
    else return(mlr3::lrn(learner))
  } else {
    if(type=="classif") return(mlr3::lrn(learner, predict_type = "prob", importance = "impurity"))
    else return(mlr3::lrn(learner, importance = "impurity"))
  }
}




getParamSets = function(method = "RF", extra = NULL, prefix = "") {
  pars = switch(method,
                RF = getRFparamSet(extra, prefix),
                kNN = getKNNparamSet(extra, prefix),
                SVM = getSVMparamSet(extra, prefix),
                BRT= getBRTparamSet(extra, prefix)
                
  )
  return(pars)
}


getRFparamSet = function(extra, prefix) {
  
  if(!is.logical(extra$balance)) pf = function(p2) paste0(prefix,"ranger.", p2)
  else pf = function(p2) p2
  
  return(
    paradox::ParamSet$new(
      list(paradox::ParamInt$new(pf("min.node.size"), lower = 1, upper = 30L),
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
           #paradox::ParamDbl$new(pf("nu"), lower = 0.3, upper = 0.7),
           paradox::ParamDbl$new(pf("coef0"), lower = -3, upper = 3),
           paradox::ParamFct$new(pf("type"), levels = SVMtypes[1], default = SVMtypes[1])
      ))
  
  if(extra$type == "classif") {
    pars$add(paradox::ParamDbl$new(pf("cost"), lower = 0, upper = 3))
    pars$add_dep(pf("cost"), pf("type"),cond = paradox::CondEqual$new(SVMtypes[1]))
  } else {
    pars$add(paradox::ParamDbl$new(pf("epsilon"), lower = 0, upper = 1))
    pars$add_dep(pf("epsilon"), pf("type"),cond = paradox::CondEqual$new(SVMtypes[1]))
  }
  #pars$add_dep(pf("nu"), pf("type"),cond = paradox::CondEqual$new(SVMtypes[2]))
  pars$add_dep(pf("degree"), pf("kernel"), cond = paradox::CondEqual$new("polynomial"))
  pars$add_dep(pf("gamma"), pf("kernel"), cond = paradox::CondAnyOf$new(c("polynomial", "radial", "sigmoid")))
  pars$add_dep(pf("coef0"), pf("kernel"), cond = paradox::CondAnyOf$new(c("polynomial", "sigmoid")))
  return(pars)
}


getBRTparamSet = function(extra, prefix) {
  if(!is.logical(extra$balance)) pf = function(p2) paste0(prefix,"xgboost.", p2)
  else pf = function(p2) p2
  
  pars = 
    paradox::ParamSet$new(
      list(paradox::ParamDbl$new(pf("alpha"), lower = 0.0, upper = 3.0),
           paradox::ParamFct$new(pf("booster"), levels = c("gbtree", "gblinear", "dart"), default = "gbtree"),
           paradox::ParamDbl$new(pf("eta"),lower = 0.0, upper = 1.0),
           paradox::ParamFct$new(pf("feature_selector"), levels = c("cyclic", "shuffle"), default = "cyclic"),
           paradox::ParamDbl$new(pf("gamma"), lower = 0.0, upper = 5.0),
           paradox::ParamDbl$new(pf("lambda"), lower = 0.0, 5.0),
           paradox::ParamDbl$new(pf("skip_drop"), lower = 0.0, 1.0),
           paradox::ParamDbl$new(pf("rate_drop"), lower = 0.0, 1.0),
           paradox::ParamDbl$new(pf("lambda_bias"), lower = 0.0,upper = 5.0),
           paradox::ParamInt$new(pf("max_depth"), lower = 1L, upper = 50L ),
           paradox::ParamInt$new(pf("nrounds"), lower = 1L, upper = 150L))
    )
  pars$add_dep(pf("feature_selector"), pf("booster"), cond = paradox::CondEqual$new("gblinear"))
  pars$add_dep(pf("skip_drop"), pf("booster"), cond = paradox::CondEqual$new("dart"))
  pars$add_dep(pf("rate_drop"), pf("booster"), cond = paradox::CondEqual$new("dart"))
  pars$add_dep(pf("lambda_bias"), pf("booster"), cond = paradox::CondEqual$new("gblinear"))
  
  return(pars)
 # test
}