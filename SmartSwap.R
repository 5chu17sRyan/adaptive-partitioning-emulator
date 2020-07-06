maximinSmartSwap<- function(originalInputs, addedInputs){
  
  #STEP 0
  # N/A - Set total number of starting designs
  # N/A - Set current design number to be 0
  
  #Set minimum distance to be 0
  maxMinimumInterpointDistance <- 0

  
  
  #STEP 1
  # N/A - Iterate current design number
  
  #Combine original and addedInputs 
  totalInputs <- originalInputs
  totalInputs <- rbind(totalInputs, addedInputs)
  
  #Plot what the points look like before swapping
  shapes <- c(rep(1, numOriginalInputs), rep(2, numAddedInputs))
  plot(x = totalInputs[,1], 
       y = totalInputs[,2], 
       pch = shapes,
       main = "Before"
  )
  
  #Calculate number of inputs in each subset for later use
  numOriginalInputs <- nrow(originalInputs)
  numAddedInputs <- nrow(addedInputs)
  numTotalInputs <- nrow(totalInputs)
  
  #STEP 2
  #Calculate the minimum interpoint distance for all points
  interpointDistances <- calculateInterpointDistances(totalInputs)
  maxMinimumInterpointDistance <- minimumInterpointDistance(interpointDistances)
  
  #Record the points giving rise to the min interpoint distance
    #Second point in vector will always have a greater index by design
  maxMinimumInterpointDistanceIndex <- minimumInterpointDistanceIndex(interpointDistances)

  #STEP 3
  #Is step three needed for our algorithm, why do we need to store the choices. Why not just loop through all choices
  
  #Calculate the threshold for exiting the next loop
  originalDistances <- calculateInterpointDistances(originalInputs)
  originalMinDistance <- minimumInterpointDistance(originalDistances)
  exitThreshold <- (numOriginalInputs / numTotalInputs) * originalMinDistance
  
  repeat{
    #Determine whether the two points are from different subsets i.e. added and original Inputs
    firstMinPointIndex <- maxMinimumInterpointDistanceIndex[1]
    secondMinPointIndex <- maxMinimumInterpointDistanceIndex[2]
    
    firstPtAddedSecondOriginal <- (firstMinPointIndex > numOriginalInputs) & (secondMinPointIndex <= numOriginalInputs)
    firstPtOriginalSecondAdded <- (firstMinPointIndex <= numOriginalInputs) & (secondMinPointIndex > numOriginalInputs)
    minDistPtsDifferent <- firstPtAddedSecondOriginal | firstPtOriginalSecondAdded

    #Only start step 4 if the points are from different subsets
    if(minDistPtsDifferent){
      #If only one point is in the added points, by construction it must be the second point.
      minDistAddedPtIndex <- secondMinPointIndex - numOriginalInputs
      #STEP 4
      #Every combination of points from the minimum distance, each possible dimension, and possible remaing points
      for(j in 1:dimensions){
        for(k in 1:numAddedInputs){
          #Checking to not swap the point with itself
          minDistAddedPtIndex
          if(k == minDistAddedPtIndex){
            next
          }
          
          #Adjusting the index to retrieve only added elements from total inputs
          swappingPtAdjustedIndex = k + numOriginalInputs
          #Record original values for swapped elements
          swappedElement_RemainingPt <- totalInputs[swappingPtAdjustedIndex,j]
          #Using index for totalInputs
          swappedElement_MinimumPt <- totalInputs[secondMinPointIndex,j]
          
          #Swap the values of the points
          swappedInputs <- totalInputs
          #If we are only swapping points within addedPoints the bounds of added points will remain the same
          swappedInputs[swappingPtAdjustedIndex,j] <- swappedElement_MinimumPt
          swappedInputs[secondMinPointIndex,j] <- swappedElement_RemainingPt
          
          #Plot what the inputs look like after each swap
          plot(x = swappedInputs[,1], 
               y = swappedInputs[,2], 
               pch = shapes
          )
          
          #STEP 5
          #Calculate the interpoint distance of the perturbed design
          recalculatedDistances <- recalculateInterpointDistances(swappedInputs, interpointDistances, secondMinPointIndex, swappingPtAdjustedIndex)
          newMinDistance <- minimumInterpointDistance(recalculatedDistances)
          
          #Calculate the difference between this interpoint distance and the interpoint distance of the current design
          differenceBetweenMinDistances <- newMinDistance - maxMinimumInterpointDistance
          
          #STEP 6
          if(newMinDistance < maxMinimumInterpointDistance){
            #Repeat loop starting at STEP 4
            next
          }
          if(newMinDistance > maxMinimumInterpointDistance){
            #Set perturbed design to be the current design
            totalInputs <- swappedInputs 
            
            #Compute the minimum interpoint distance of the current design (already done)
            maxMinimumInterpointDistance <- newMinDistance
            
            #Record the rows and columns associated with the minimum distance
            maxMinimumInterpointDistanceIndex <- minimumInterpointDistanceIndex
            
            #Repeat loop starting at STEP 3
            break
          }
        }
        if(newMinDistance > maxMinimumInterpointDistance){
          break
        }
      }
    }
    if(differenceBetweenMinDistances < exitThreshold){
      break
    }
  }
  
  bestNewInputs <- totalInputs[(numOriginalInputs+1):numTotalInputs, ]
  plot(x = totalInputs[,1], 
       y = totalInputs[,2], 
       pch = shapes,
       main = "After"
  )
  
  return(bestNewInputs)
}

#Distance Calculation is correct, checked by hand.
calculateInterpointDistances <- function(inputs)
{
  inputs <- totalInputs
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

#This function is also correct
minimumInterpointDistance <- function(distances)
{
  return(distances[which.min(distances)])
}

#This function is also correct
minimumInterpointDistanceIndex <- function(distances)
{
  minDistanceLocation <- which.min(distances)
  numberOfTotalInputs <- nrow(distances)
  
  row <- minDistanceLocation %% numberOfTotalInputs
  column <- ceiling(minDistanceLocation / numberOfTotalInputs)

  minInterpointDistIndex <- c(row, column)
  return(minInterpointDistIndex)
}

recalculateInterpointDistances <- function(inputs, originalDistances, firstPointSwappedIndex, secondPointSwappedIndex)
{
  recalculatedDistances <- originalDistances
  numTotalInputs <- nrow(inputs)
  firstPointSwapped <- inputs[firstPointSwappedIndex, ]
  for(n in 1:numTotalInputs){
    #Don't calculate the distance between the first swapped point and itself
    if(n == firstPointSwappedIndex){
      next
    }
    
    #Calculate the new distance
    newDistance1 <- dist( rbind(firstPointSwapped, inputs[n, ]) )
    
    #Place the new distance in the matrix so only the upper half is filled still
    if(firstPointSwappedIndex < n){
      recalculatedDistances[firstPointSwappedIndex, n] <- newDistance1
    }
    else if(firstPointSwappedIndex > n){
      recalculatedDistances[n, firstPointSwappedIndex] <- newDistance1
    }
  }
  
  #Repeat recalculations for the second swapped point
  secondPointSwapped <- inputs[secondPointSwappedIndex, ]{
    #Don't calculate the distance between the first swapped point and itself
    if(m == secondPointSwappedIndex){
      next
    }
    #Don't recalculate the distance between the first swapped point and the second swapped point
    if(m == firstPointSwappedIndex){
      next
    }
    
    #Calculate the new distance
    newDistance2 <- dist( rbind(secondPointSwapped, inputs[m, ]) )
    #Place the new distance in the matrix so only the upper half is filled still
    if(secondPointSwappedIndex < m){
      recalculatedDistances[secondPointSwappedIndex, m] <- newDistance2
    }
    else if(firstPointSwappedIndex > m){
      recalculatedDistances[m, secondPointSwappedIndex] <- newDistance2
    }
  }

  return(recalculatedDistances)
}
