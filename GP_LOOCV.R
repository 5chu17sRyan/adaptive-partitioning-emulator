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


GP_kfoldCV <- function(kfoldInput, k){
  #randomly shuffle points
  numFolds <- 5 # change to k
  copyOfData <- dat #change kfoldInput
  numPointsKFold <- nrow(copyOfData)
  sumCV <- 0
  
  shuffledPtIndecies <- sample(1:numPointsKFold, numPointsKFold, replace = FALSE) 

  for(i in  1:numFolds){
    #Calculate fold
    fold <- copyOfData[(shuffledPtIndecies) <= (i*numPointsKFold/numFolds) & shuffledPtIndecies > (i-1)*numPointsKFold/numFolds, ]
    
    #Remove from points
    #Use conditional statement above as an index
    remainingPoints = anti_join(copyOfData, fold)
    
    startInputIndex = 3
    endInputIndex = 2 + dimensions 
    outputIndex = 2
    #Train model on remaining points
    remainingPoints
    GPmodel <- GP_fit(remainingPoints[,startInputIndex:endInputIndex], remainingPoints[,outputIndex])
    
    #Calculate squared error
    prediction <- predict(GPmodel, fold[,startInputIndex:endInputIndex])
    squaredError <- (prediction$Y_hat - fold[ , outputIndex])^2
    
    #Add squared error to sum
    sumCV <- sumCV + sum(squaredError)
  }
  #sumCV <- sum(sumCV)

  #Divde sum by number of folds
  kfoldCV <- sumCV / numFolds
  return(kfoldCV)

}
