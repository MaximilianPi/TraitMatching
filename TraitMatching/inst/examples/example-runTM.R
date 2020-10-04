\dontrun{
sim = simulateInteraction(weights = list(main = 0.1, inter = c(0.3,0.3,0.3)))
A = sim$A
B = sim$B
Z = sim$binar()
community = createCommunity(A, B, Z)

result = runTM(community = community, iters = 2L)
}