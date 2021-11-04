\dontrun{

## Create Life / simulate community
sim = simulateInteraction(weights = list(main = 0.1, inter = c(0.3,0.3,0.3)))

# First group of species with traits (e.g. Bees)
A = sim$A
# Second group of sepcies with traits (e.g. Plants)
B = sim$B
# Observed interaction matrix for species in A and B
Z = sim$binar()

# First columns in A and B must contain the species names
# The species names have to mach with the 
# rownames and colnames of the interaction matrix
!any(rownames(Z) %in% A$X)
!any(colnames(Z) %in% B$Y)

## The three matrices/data.frames need to be transformed into one data.frame:
community = createCommunity(A, B, Z)
names(community)

## Let's fit Random Forest (default) and BRT:
result = runTM(community = community, iters = 2L, method = c("RF", "BRT"))

# Check average performance on the holdouts of the CV:
print(result)

# We can do now two things with the fitted model(s):
# 1) Predictions
# 2) Estimate trait-matching signals from the fitted model

## Predictions
# As we use nested-CV to tune the ML model, our fitted object actually consists
# of n fitted models. The default of the package is to use all of models and
# average their predictions (which is done under the hood):
predict(result) # predict for the fitted data
predict(result, newdata = community$data) # predict for new data

# However, sometimes we only want to use one model, e.g. because always using
# all models of the ensemble is computationally expensive (especially if we 
# calculate in the following the trait-matching signal)
# There is a variable in the result object to turn on/off the ensemble 
# prediction behavior:
result$pred_ensembles # get status
result$pred_ensembles = FALSE # turn it off
system.time({ predict(result) })
result$pred_ensembles = TRUE # turn it on
system.time({ predict(result)  })

## Find Trait-Matching Signal
result$pred_ensembles = FALSE

# Let's define a search list of interactions we are interested in:
traits = colnames(community$data)[-(1:2)]
traits = traits[-length(traits)]
specificInteractions = list()
for(i in 1:(length(traits)-1)) {
  specificInteractions[[i]] = list(a = traits[1], b = traits[-1])
  traits=traits[-1]
}

# See the help of findInteractions for more information about the options
tm = findInteractions(result, 
                      globalInteractions = FALSE, 
                      gridSize = 10L,
                      specificInteractions = specificInteractions[1:3], 
                      nTimes=1L)
# Keep in mind that calculating the H2 statistic is 
# computationally very expensive
print(tm)

best = 
  tm$RF$pairwise_interactions[order(tm$RF$pairwise_interactions$Interactions, 
                                    decreasing = TRUE),,drop=FALSE]
head(best)
inter = matrix(c("A1", "B1", "A2", "B2", "A3", "B3"), 
               ncol = 2, nrow = 3, byrow = TRUE)
print(inter) # true simulated interactions

## Optional: Variable Importances
# Some of the algorithms (RF and BRT) provide variable importances which 
# we can extract:
# RF:
n_models = length(results$ensembles$RF$model) - 1
importance = sapply(1:n_models, 
        function(i) results$ensembles$RF$model[[i]]$model$variable.importance
        )
imps_mean = apply(importance, 1, mean)

# BRT
n_models = length(results$ensembles$BRT$model) - 1
importance = lapply(1:n_models, function(i) 
  as.data.frame(xgboost::xgb.importance(model=
                                results$ensembles$BRT$model[[i]]$model))[,1:2])

importance = plyr::join_all(importance, by = "Feature", type = "left")


}