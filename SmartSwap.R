maximinSmartSwap<- function(originalInputs, addedInputs){
  #STEP 0
  #Set total number of starting designs
  #numTotalDesigns <- 10 #Should change this later
  #Set current design number to be 0
  #currentDesignNum <- 0
  #Set minimum distance to be 0
  maxMinDistance <- 0

  #STEP 1
  #Iterate current design number
  #currentDesignNum <- currentDesignNum + 1
  #Combine original and addedInputs 
  totalInputs <- originalInputs
  colnames(addedInputs) <- colnames(totalInputs)
  totalInputs <- rbind(totalInputs, addedInputs)
  
  #STEP 2
  #Calculate the minimum interpoint distance
  interpointDistances <- calculateInterpointDistances(totalInputs)
  minimumInterpointDistance <- minimumInterpointDistance(interpointDistances)
  #Record the points giving rise to the min interpoint distance
  minimumInterpointDistanceIndex <- minimumInterpointDistance(interpointDistances)
  
  #STEP 3
  #Is step three needed for our algorithm, why do we need to store the choices. Why not just loop through all choices
  countOfTotalInputs <- nrow(totalInputs)
  ptsCreatingMinDistance <- 2
  
  #STEP 4
  #Every combination of points from the minimum distance, each possible dimension, and possible remaing points
  for(i in 1:ptsCreatingMinDistance)
  {
    for(j in 1:dimensions)
    {
      for(k in 1:(countOfTotalInputs-2))
      {
        #Record original values for swapped elements
        totalInputs[k,j] <- swappedElement_RemainingPt
        totalInputs[i,j] <- swappedElement_MinimumPt
        #Swap the values of the points
        totalInputs[k,j] <- swappedElement_MinimumPt
        totalInputs[i,j] <- swappedElement_RemainingPt
        
        
      }
    }
  }
}

calculateInterpointDistances <- function(inputs)
{
  numTotalInputs <- nrow(inputs)
  distances <- matrix(NA, nrow = numTotalInputs, ncol = numTotalInputs)
  
  for(i in 1:(numTotalInputs-1))
  {
    for(j in (i+1):numTotalInputs)
    {
      newDistance <- dist(rbind(inputs[i,], inputs[j,]), method = "euclidian")
      distances[i,j] <- newDistance
    }
  }
  return(distances)
}

minimumInterpointDistance <- function(distances)
{
  return(distances[which.min(distances)])
}

minimumInterpointDistanceIndex <- function(distances)
{
  minDistanceLocation <- which.min(distances)
  numberOfTotalInputs <- nrow(distances)
  
  row <- minDistanceLocation %% numberOfTotalInputs
  column <- ceiling(minDistanceLocation / numberOfTotalInputs)
  
  minInterpointDistIndex <- c(row, column)
  return(minInterpointDistIndex)
}
