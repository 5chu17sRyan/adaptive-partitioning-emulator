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


GP_5-foldCV <- function(dat){
  #Instantiate CV sum to be 0
  numFolds <- 5
  copyOfData <- dat
  sumCV <- 0
  numPoints <- nrow(dat)
  
  #Randomly divide the set of observations into k folds of approximately the same size.
  
  #randomly shuffle points
  shuffledPtIndecies <- sample(1:numPoints, numPoints, replace = FALSE) 
  shuffledPtIndecies #1,7,6,3,5,2,4,8
  
  fold1 <- NA
  fold2 <- NA
  fold3 <- NA
  fold4 <- NA
  fold5 <- NA
  
  #assign folds 1->k to the points 1,2,...,k,1,2,...,k,...
  i <- 1
  repeat{
    fold1 <- rbind(fold1, copyOfData[shuffledPtIndecies[i],])
    i <- i + 1
    if(i > numPoints)(
      break
    )
    
    fold2 <- rbind(fold2, copyOfData[shuffledPtIndecies[i],])
    i <- i + 1
    if(i > numPoints)(
      break
    )
    
    fold3 <- rbind(fold3, copyOfData[shuffledPtIndecies[i],])
    i <- i + 1
    if(i > numPoints)(
      break
    )
    
    fold4 <- rbind(fold4, copyOfData[shuffledPtIndecies[i],])
    i <- i + 1
    if(i > numPoints)(
      break
    )
    
    fold5 <- rbind(fold5, copyOfData[shuffledPtIndecies[i],])
    i <- i + 1
    if(i > numPoints)(
      break
    )

  }


  i <- 1
  #Loop for i = 1 to number of pointsR1
  repeat{
    #Remove the ith point from our set of points
    removedPoint <- dat[i,]
    remainingPoints <- dat[-c(i),]
    
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