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
