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


GP_5-foldCV <- function(5-foldInput){
  #Instantiate CV sum to be 0
  numFolds <- 5
  copyOfData <- dat
  sumCV <- 0
  numPoints <- nrow(copyOfData)
  
  #Randomly divide the set of observations into k folds of approximately the same size.
  
  #randomly shuffle points
  shuffledPtIndecies <- sample(1:numPoints, numPoints, replace = FALSE) 
  shuffledPtIndecies #1,7,6,3,5,2,4,8
  
  copyOfData
  
  fold1 <- copyOfData[shuffledPtIndecies[] < numPoints/numFolds, ]
  fold2 <- copyOfData[(shuffledPtIndecies[]) < (2*numPoints/numFolds) & shuffledPtIndecies[] >= numPoints/numFolds, ]
  fold3 <- copyOfData[(shuffledPtIndecies[]) < (3*numPoints/numFolds) & shuffledPtIndecies[] >= 2*numPoints/numFolds, ]
  fold4 <- copyOfData[(shuffledPtIndecies[]) < (4*numPoints/numFolds) & shuffledPtIndecies[] >= 3*numPoints/numFolds, ]
  fold5 <- copyOfData[(shuffledPtIndecies[]) <= (5*numPoints/numFolds) & shuffledPtIndecies[] >= 4*numPoints/numFolds, ]
  
  fold1
  fold2
  fold3
  fold4
  fold5
  
  
  
  
  for(i in  1:5){
    grpind
    removedPoints = copyOfData[]
    
  }
  
  
  
  
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