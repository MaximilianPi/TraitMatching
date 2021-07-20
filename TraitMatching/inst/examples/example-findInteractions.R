\dontrun{
sim = simulateInteraction(weights = list(main = 0.1, inter = c(0.3,0.3,0.3)))
A = sim$A
B = sim$B
Z = sim$binar()
community = createCommunity(A, B, Z)

result = runTM(community = community, iters = 2L) 
  
Res = findInteractions(result,data = community$data[1:5, -c(1,2)], 
                       gridSize = 3L, depth = 3L, parallel = FALSE)

# Find all interaction strengths
Res = findInteractions(result,data = community$data[1:5, -c(1,2)], 
                       gridSize = 3L) 

# Calculating the global interaction strengths and all possible pair-wise inter-
# actions is computationally expensive, let's calculate the interaction 
# strengths only for a specific set of predictors:
##  Interactions between A1, A2, and B1, B2 (without global interaction strengths):
interList = list(
  list(a = "A1", b = c("B1", "B2")),
  list(a = "A1", b = c("B1", "B2"))
)
Res = findInteractions(result,data = community$data[1:50, -c(1,2)], 
                       gridSize = 10L,
                       globalInteractions = FALSE,
                       specificInteractions = interList,
                       parallel = 2L) 
print(Res)
}