library(testthat)
library(TraitMatching)


testthat::test_that("paramSets", {
  
  # constants
  
  terminator = mlr3tuning::trm("evals", n_evals = 10L)
  tuner = mlr3tuning::tnr("random_search")
  
  X = matrix(rnorm(50*3), 50, 3)
  YC = as.factor(rbinom(50, 1, 0.5))
  YR = rnorm(50)
  
  d1 = data.frame(Y = YC, X)
  d2 = data.frame(Y = YR, X)
  
  classif = mlr3::TaskClassif$new(id = "classif", backend = d1, target = "Y")
  regr = mlr3::TaskRegr$new(id = "regr", backend = d2, target = "Y")
  
  # test models
  
  instance = mlr3tuning::TuningInstanceSingleCrit$new(
    task = regr,
    learner = mlr3::lrn("regr.svm"),
    resampling = mlr3::rsmp("holdout"),
    measure = mlr3::msr("regr.rmse"),
    search_space = TraitMatching:::getSVMparamSet(extra = list(type="regr", balance=FALSE), prefix = ""),
    terminator = terminator
  )
  tuner$optimize(instance)
  
  
})