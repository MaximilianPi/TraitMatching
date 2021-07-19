sim = simulateInteraction(weights = list(main = 0.1, inter = c(0.3,0.3,0.3)))
A = sim$A
B = sim$B
communityC = createCommunity(A, B, sim$binar())
communityR = createCommunity(A, B, sim$poisson())

test_model = function(method, metric, community, tune, balance="smote") {
  testthat::expect_error({model = runTM(community = community, 
                                        method = method, 
                                        metric = metric,
                                        crossValidation = 
                                          list(outer = list(method = "CV", iters = 2), 
                                               inner = list(method = "CV", iters = 2)),
                                        tune = tune,
                                        iters = 2L,
                                        balance = balance
                                        )}, NA, info = paste0(method, metric, tune, balance))
  
  testthat::expect_error(predict(model), NA, info = paste0(method, metric, tune, balance))
  testthat::expect_error(predict(model, newdata = community$data[1:10,]), NA, info = paste0(method, metric, tune, balance))
}



tests = list(
  list("RF", "AUC", communityC, "random", "smote"),
  list(c("RF", "BRT", "BRT"), "AUC", communityC, "random", "smote"),
  list("RF", "R2", communityR, "grid", "no"),
  list(c("RF"), "R2", communityR, "random", "no"),
  list(c("kNN", "RF", "SVM", "BRT"), "Spear", communityR, "grid", "no"),
)

testthat::test_that('runTM', {
  for(i in 1:length(tests)) {
    test_model(tests[[i]][[1]], tests[[i]][[2]], tests[[i]][[3]], tests[[i]][[4]], tests[[i]][[5]])
  }
})