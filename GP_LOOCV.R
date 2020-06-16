GP_LOOCV <- function(pts){
  #Instantiate CV sum to be 0
  sumCV <- 0
  numPoints <- nrow(pts)
  
  i <- 1
  #Loop for i = 1 to number of pointsR1
  repeat{
    #Remove the ith point from our set of points
    removedPoint <- pts[i,]
    remainingPoints <- pts[-c(i),]
    
    #Create a GP for the new set
    startInputIndex = 3
    endInputIndex = 2 + dimensions #ncol(pts) - 2
    outputIndex = 2
    GPmodel <- GP_fit(remainingPoints[,startInputIndex:endInputIndex], remainingPoints[,outputIndex])
    
    #Predict y_i from x_i using the GP
    prediction <- predict(GPmodel, removedPoint[,startInputIndex:endInputIndex])
    
    #Add squaredError to our CV sum
    squaredError <- (prediction$Y_hat - removedPoint[ , outputIndex])^2
    sumCV <- sumCV + squaredError 
    
    #Add the removed point back into our set
    
    i = i + 1
    if(i > numPoints){
      break
    }
  }
  
  #Divde sum by number of points
  LOOCV <- sumCV / numPoints
  return(LOOCV)
}


GP_k-foldCV <- function(kfoldInput, k){
  numPoints <- nrow(copyOfData)
  
  #randomly shuffle points
  shuffledPtIndecies <- sample(1:numPoints, numPoints, replace = FALSE) 
  
  numFolds <- k
  copyOfData <- kfoldInput
  sumCV <- 0
  for(i in  1:numFolds){
    #Calculate fold
    fold <- copyOfData[(shuffledPtIndecies[]) <= (i*numPoints/numFolds) & shuffledPtIndecies[] > (i-1)*numPoints/numFolds, ]
    
    #Remove from points
    remainingPoints = anti_join(copyOfData, fold)
    
    #Train model on remaining points
    GPmodel <- GP_fit(remainingPoints[,startInputIndex:endInputIndex], remainingPoints[,outputIndex])
    
    #Calculate squared error
    prediction <- predict(GPmodel, removedPoint[,startInputIndex:endInputIndex])
    squaredError <- (prediction$Y_hat - removedPoint[ , outputIndex])^2
    
    #Add squared error to sum
    sumCV <- sumCV + squaredError 
  }
  
  #Divde sum by number of folds
  k-foldCV <- sumCV / numFolds

}