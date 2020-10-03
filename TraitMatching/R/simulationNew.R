
#' createSpecies
#'
#'@description create two groups (plants, pollinators) of species
#'
#'@param NumberA number of A species
#'@param NumberB number of B species
#'@param traitsA vector for traitsA = c(NumberofCategorical, NumberofNumerical), default no categorical traits
#'@param traitsB vector for traitsB = c(NumberofCategorical, NumberofNumerical), default no categorical traits
#'@param rangeDiscrete how many levels to sample for categorical traits
#'@param seed seed
#'@param abundance can be logical, numerical or a function. Numerical will be the rate in the exponential distribution, default = 2
#'@param speciesClass if you want to resample categorical traits, provide createSpecies object
#'@param specialist logical if group B should be specialized or not
#'@param coLin you can set collinear traits (not fully supported yet)
#'@param sampling sampling function for traits, default runif(n,0,1)
#'@param specRanger specialization range, default c(1,2)
#'@export

createSpecies = function(NumberA = 20, NumberB = 40, traitsA = c(0,8), traitsB = c(0,8), rangeDiscrete = 2:8,seed = 1337, abundance = 2,
                         speciesClass = NULL, specialist = T, coLin = NULL, sampling = function(n) stats::runif(n, 0,1), specRange = c(1,2)){
  spec = specialist

  if(!is.null(speciesClass)){
    traitsA = speciesClass$traitsA
    traitsB = speciesClass$traitsB
    ADV = speciesClass$ADV
    BDV = speciesClass$BDV
  } else {
    ADV = list()
    BDV = list()
  }

  out = list()
  A = data.frame(matrix(NA, nrow = NumberA, ncol = sum(traitsA)))
  B = data.frame(matrix(NA, nrow = NumberB, ncol = sum(traitsB)))

  rownames(A) = sapply(1:nrow(A), function(x) return(paste("a",x,sep = "")))
  rownames(B) = sapply(1:nrow(B), function(x) return(paste("b",x,sep = "")))

  colnames(A) = sapply(1:ncol(A), function(x) return(paste("A",x,sep = "")))
  colnames(B) = sapply(1:ncol(B), function(x) return(paste("B",x,sep = "")))

  set.seed(seed)

  # Numerical:

  # if(!traitsA[2] == 0) A[,1:traitsA[2] + traitsA[1]] = sapply(1:traitsA[2], function(x, NumberA) return(rnorm(NumberA, 0, sd = 1)), NumberA)
  # if(!traitsB[2] == 0) B[,1:traitsB[2] + traitsB[1]] = sapply(1:traitsB[2], function(x, NumberB) return(rnorm(NumberB, 0, sd = 1)), NumberB)

  if(class(sampling) != "function"){

  if(!traitsA[2] == 0) A[,1:traitsA[2] + traitsA[1]] = sapply(1:traitsA[2], function(x, NumberA) return(stats::runif(NumberA, 0,1)), NumberA)
  if(!traitsB[2] == 0) B[,1:traitsB[2] + traitsB[1]] = sapply(1:traitsB[2], function(x, NumberB) return(stats::runif(NumberB, 0,1)), NumberB)

  } else {
    if(!traitsA[2] == 0) A[,1:traitsA[2] + traitsA[1]] = sapply(1:traitsA[2], function(x, NumberA) return(sampling(NumberA)), NumberA)
    if(!traitsB[2] == 0) B[,1:traitsB[2] + traitsB[1]] = sapply(1:traitsB[2], function(x, NumberB) return(sampling(NumberB)), NumberB)
  }

  if(!is.null(coLin)){
    for(i in 1:length(coLin)){
      if(names(coLin)[i] %in% colnames(A)) A[,names(coLin)[i]] =  model.matrix(coLin[[i]], data = A)[,2]
      else B[,names(coLin)[i]] =  model.matrix(coLin[[i]], data = B)[,2]
    }
  }


  if(is.null(speciesClass)){

    if(!traitsA[1] == 0) for(i in 1:traitsA[1]){
      ADV[[i]] = createDiscrete(rangeDiscrete)
      A[,i] = ADV[[i]](NumberA)
    }
    if(!traitsB[1] == 0) for(i in 1:traitsB[1]){
      BDV[[i]] = createDiscrete(rangeDiscrete)
      B[,i] = BDV[[i]](NumberB)
    }
  } else {
    if(!traitsA[1] == 0) for(i in 1:traitsA[1]){
      A[,i] = ADV[[i]](NumberA)
    }
    if(!traitsB[1] == 0) for(i in 1:traitsB[1]){
      B[,i] = BDV[[i]](NumberB)
    }
  }

  # if(!traitsA[1] == 0) A[,1:traitsA[1]] = createDiscrete(n = NumberA, nD = traitsA[1], range = rangeDiscrete, seed)
  # if(!traitsB[1] == 0) B[,1:traitsB[1]] = createDiscrete(n = NumberB, nD = traitsB[1], range = rangeDiscrete, seed)



  out$A = A
  out$B = B
  if(is.logical(spec) && spec)  out$spec = stats::runif(NumberB, specRange[1],specRange[2]) #scales::rescale(rexp(NumberB,1), to = c(1,0.001))
  else if(is.logical(spec) && !spec)spec = rep(NumberB,1)
  out$traitsA = traitsA
  out$traitsB = traitsB
  if(!is.function(abundance)){
    if(is.logical(abundance) && !abundance){
      out$Aabund <- rep(1,NumberA)
      out$Babund <- rep(1,NumberB)
    } else {
      out$Aabund <- stats::rpois(NumberA, abundance) + 1
      out$Babund <- stats::rpois(NumberB, abundance) + 1
    }
  } else {
    out$Aabund <- abundance(NumberA, NumberA)
    out$Babund <- abundance(NumberB, NumberB)
  }
  out$ADV = ADV
  out$BDV = BDV
  class(out) = c("SpeciesClass")
  return(out)

}




#' create discrete
#' @param nD number discrete
#' @param range range of discrete
#'

createDiscrete = function(range){

  Nlevels = sample(range, 1)
  prob = sample.int(Nlevels+1,Nlevels)

  create = function(n){
    discreteV = sample(1:Nlevels, size = n, prob = prob, replace = T)
    f = as.factor(discreteV)
    levels(f) = 1:length(levels(f))
    discreteV = as.integer(f)
    return(discreteV)
  }

  #discV = sapply(1:nD, create, n, range)
  return(create)
}



#' Simulate plant-pollinator networks
#'
#' @description Create two groups with weighted trait-matching effects and/or main effects.
#'
#' @param species can be of createSpecies class, optional, default NULL
#' @param main vector of main effects
#' @param inter two column matrix of trait-matching effects
#' @param weights list of two vectors for main and inter (trait-matching) effect weights
#' @param reSim output of simulateInteraction
#' @param ... arguments passed to createSpecies function, see details
#'
#' @details
#' \itemize{
#' \item \code{species}: If you want, you can directly the output of createSpecies, two groups (a and b) with traits. Otherwise, pass arguments to createSpecies with \code{...}
#' \item \code{main}: main effects. Currently not fully supported!
#' \item \code{inter}: matrix of intended trait-matching effects. N rows for n trait-matching effects. Pass trait-matching by trait names (i.e "A1", "B1")
#' \item \code{...} see \code{\link{createSpecies}} for detailed information.
#'
#' }
#'
#' @export

simulateInteraction = function(species = NULL, main = NULL, inter = matrix(c("A1", "B1","A2", "B2","A3", "B3"), ncol = 2, nrow = 3, byrow = T),
                               weights = list(main = 1, inter = c(1,4,2)),
                               reSim = NULL,
                               setSeed = 42,
                               ...){
  set.seed(setSeed)
  if(is.null(reSim)){
    if(is.null(species)) species <- createSpecies(...)
    if(class(species) != "SpeciesClass") warning("species must be of class SpeciesClass")

    if(species$traitsA[1] != 0 && species$traitsB[1] != 0) discrete = c(colnames(species$A)[1:species$traitsA[1]], colnames(species$B)[1:species$traitsB[1]])
    else {
      if(species$traitsA[1] == 0) discrete = "0"
      if(species$traitsB[1] == 0) discrete = "0"
    }

    createCov = function(x){
      cv = stats::runif(1, min = -0.5, 0.5)
      return(matrix(c(1, cv, cv, 1), ncol = 2))
    }


    #weights = lapply(weights, function(x) return(x/sum(x)))
    #weights$main = weights$main*0.5

    # main trait settings
    mainTraits = list()
    counter = 0
    for(i in main){
      counter = counter + 1
      m = i
      if(m %in% discrete){
        mainTraits[[m]]$discrete = TRUE
        if(m %in% colnames(species$A)) mainTraits[[m]]$mean = stats::runif(20, 0,1)
        else mainTraits[[m]]$mean = stats::runif(20, 0,1)
      } else{
        mainTraits[[m]]$discrete = FALSE
        mainTraits[[m]]$mean = 0
      }

      mainTraits[[m]]$weight = weights[["main"]][counter]
    }


    mainFunc = function(x, y, spec = 0.5){
      if(!is.null(main)){
        mT = data.frame(x, y)[main]
        res = 0
        for(k in 1:length(mT)){
          if(mainTraits[[k]]$discrete) res[k] = mainTraits[[k]]$mean[mT[[k]]]*mainTraits[[k]]$weight
          else res[k] = stats::dnorm(mT[[k]], mean = mainTraits[[k]]$mean, spec)*mainTraits[[k]]$weight
        }
        return(prod(res))
      }else{
        return(1)
      }

    }



    counter = 0
    if(!is.null(inter)){
      interTraits = vector("list", nrow(inter))
      for(i in 1:nrow(inter)){
        paar = inter[i,]
        whichD = c(paar %in% discrete)
        k = i

        if(sum(whichD) > 1){
          interTraits[[k]]$both = TRUE
          interTraits[[k]]$interM = matrix(stats::runif(100, 0,1), nrow = 10, ncol = 10) #change 25.3
          interTraits[[k]]$weight = weights[["inter"]][k]
        } else {
          interTraits[[k]]$both = FALSE
          if(any(whichD)){
            if(which(whichD, arr.ind = T) == 1) {
              interTraits[[k]]$which = 1
              interTraits[[k]]$mean = stats::rnorm(length(unique(species$A[,paar[1]])),0,1)
              interTraits[[k]]$weight = weights[["inter"]][k]
            }else{
              interTraits[[k]]$which = 2
              interTraits[[k]]$mean = stats::rnorm(length(unique(species$B[,paar[2]])),0,1)
              interTraits[[k]]$weight = weights[["inter"]][k]
            }
          } else {
            interTraits[[k]]$which = 3
            counter = counter + 1
            # if(is.null(cov)) interTraits[[k]]$cov = createCov(1)
            # else {
            #   if(!is.matrix(cov)) interTraits[[k]]$cov = cov[[counter]]
            #   else interTraits[[k]]$cov = cov
            # }
            # interTraits[[k]]$mean = rep(0,2)
            interTraits[[k]]$weight = weights[["inter"]][k]
          }
        }
      }
    }

    interFunc = function(x, y, spec = 0.5){
      if(!is.null(inter)){
        res = 0
        for(i in 1:length(interTraits)){
          if(interTraits[[i]]$both) res[i] = interTraits[[i]]$interM[x[1,inter[i,1]], y[1,inter[i,2]]] * interTraits[[i]]$weight
          else if(interTraits[[i]]$which != 3){
            if(interTraits[[i]]$which == 1) res[i] = stats::dnorm(y[1,inter[i,2]], mean = interTraits[[i]]$mean[x[1,inter[i,1]]], sd = spec)*interTraits[[i]]$weight
            if(interTraits[[i]]$which == 2) res[i] = stats::dnorm(x[1,inter[i,1]], mean = interTraits[[i]]$mean[y[1,inter[i,2]]], sd = spec)*interTraits[[i]]$weight
          }
          else{
            #res[i] = mvtnorm::dmvnorm(c(x[1,inter[i,1]], y[1,inter[i,2]]), mean = interTraits[[i]]$mean, interTraits[[i]]$cov*spec)*interTraits[[i]]$weight
            res[i] = stats::dnorm(log(x[1,inter[i,1]]/y[1,inter[i,2]]),mean = 0, sd = spec)*interTraits[[i]]$weight #change 25.3
          }

        }
        return(prod(res))
      } else {
        return(1)
      }
    }



    out = list()
    interMatrix = matrix(NA, nrow=nrow(species$A), ncol = nrow(species$B) )

    for(i in 1:nrow(species$A)){
      x = species$A[i,]
      for(j in 1:nrow(species$B)){
        y = species$B[j,]
        spec = species$spec[j]
        interMatrix[i,j] = prod(mainFunc(x,y, spec), interFunc(x,y, spec), species$Aabund[i], species$Babund[j], stats::runif(1, 0.3, 0.7)) #change 24.3
      }

    }

    out$mainFunc = mainFunc
    out$interFunc = interFunc
    rownames(interMatrix) = rownames(species$A)
    colnames(interMatrix) = rownames(species$B)

    if(species$traitsA[1] != 0) species$A[,1:species$traitsA[1]] = data.frame(apply(as.matrix(species$A[,1:species$traitsA[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)
    if(species$traitsB[1] != 0) species$B[,1:species$traitsB[1]] = data.frame(apply(as.matrix(species$B[,1:species$traitsB[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)

    out$A = cbind(rownames(species$A), species$A)
    colnames(out$A)[1] = "X"

    out$B = cbind(rownames(species$B), species$B)
    colnames(out$B)[1] = "Y"

    out$z = data.frame(interMatrix)

    out$poisson = function(x = 1000, seed = 42) {
      data.frame(matrix(as.numeric(stats::rpois(length(interMatrix), interMatrix * x)) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }

    out$binar = function(x = 1000, seed = 42) {
      data.frame(matrix(as.numeric(stats::rpois(length(interMatrix), interMatrix * x) > 0) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }
    out$species = species
    if(!is.null(inter)) out$settings$interT = interTraits
    out$settings$inter = inter
    out$settings$mainT = mainTraits
    out$settings$main = main
    out$interMatrix = interMatrix

    return(out)
  } else {
    species = createSpecies(speciesClass = reSim$species,...)


    out = list()
    interMatrix = matrix(NA, nrow=nrow(species$A), ncol = nrow(species$B) )

    for(i in 1:nrow(species$A)){
      x = species$A[i,]
      for(j in 1:nrow(species$B)){
        y = species$B[j,]
        spec = species$spec[j]
        interMatrix[i,j] = prod(reSim$mainFunc(x,y,spec), reSim$interFunc(x,y,spec), species$Aabund[i], species$Babund[j])
      }
    }
    out$mainFunc = reSim$mainFunc
    out$interFunc = reSim$interFunc
    rownames(interMatrix) = rownames(species$A)
    colnames(interMatrix) = rownames(species$B)

    if(species$traitsA[1] != 0) species$A[,1:species$traitsA[1]] = data.frame(apply(as.matrix(species$A[,1:species$traitsA[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)
    if(species$traitsB[1] != 0) species$B[,1:species$traitsB[1]] = data.frame(apply(as.matrix(species$B[,1:species$traitsB[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)

    out$A = cbind(rownames(species$A), species$A)
    colnames(out$A)[1] = "X"

    out$B = cbind(rownames(species$B), species$B)
    colnames(out$B)[1] = "Y"

    out$z = data.frame(interMatrix)

    out$poisson = function(x = 1000, seed = 42) {
      data.frame(matrix(as.numeric(stats::rpois(length(interMatrix), interMatrix * x)) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }
    out$binar = function(x = 1000, seed = 42) {
       data.frame(matrix(as.numeric(stats::rpois(length(interMatrix), interMatrix * x) > 0) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }
    out$species = species
    out$settings = reSim$settings
    return(out)
  }
}




