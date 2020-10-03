library(testthat)
library(TraitMatching)



terminator = mlr3tuning::trm("evals", n_evals = 30L)
tuner = mlr3tuning::tnr("random_search")

X = matrix(rnorm(50*3), 50, 3)
YC = as.factor(rbinom(50, 1, 0.5))
YR = rnorm(50)

d1 = data.frame(Y = YC, X)
d2 = data.frame(Y = YR, X)

classif = mlr3::TaskClassif$new(id = "classif", backend = d1, target = "Y")
regr = mlr3::TaskRegr$new(id = "regr", backend = d2, target = "Y")

test_model = function(method,  task) {
  
  if(task == "classif") {
    data = classif
    metric = TraitMatching:::getMeasure("AUC", "classif")
  } else {
    data = regr
    metric = TraitMatching:::getMeasure("R2", "regr")
  }
  
  ps = switch (method,
    RF = TraitMatching:::getRFparamSet(extra = list(type=task, balance=FALSE, n_features = ncol(X)), prefix = ""),
    SVM = TraitMatching:::getSVMparamSet(extra = list(type=task, balance=FALSE), prefix = ""),
    kNN = TraitMatching:::getKNNparamSet(extra = list(type=task, balance=FALSE), prefix = ""),
    BRT = TraitMatching:::getBRTparamSet(extra = list(type=task, balance=FALSE), prefix = "")
  )
  
  expect_error({
    instance = mlr3tuning::TuningInstanceSingleCrit$new(
      task = data,
      learner = TraitMatching:::getBaseLearners(method, task),
      resampling = mlr3::rsmp("holdout"),
      measure = metric,
      search_space = ps,
      terminator = terminator
    )
    tuner$optimize(instance)
  }, NA,info = paste0("method+", "task"))
  
}

methods = c("RF", "SVM", "kNN", "BRT")
tasks = c("regr", "classif")
tests = expand.grid(methods, tasks)
tests = as.data.frame(lapply(tests, function(cc) as.character(cc)), stringsAsFactors = FALSE)
colnames(tests) = c("method", "task")

testthat::test_that("paramSets", {
  for(i in 1:nrow(tests)) {
    test_model(tests[i,1], tests[i,2])
  }
})