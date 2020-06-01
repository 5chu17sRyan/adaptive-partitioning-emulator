library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")

numInputs <- 20
dimensions <- 2

set.seed(seed = NULL)
x <- maximinLHS(numInputs, dimensions)

initialRegion <- rep(1,numInputs)

points <- data.frame(
  regions = initialRegion,
  outputs = cornerPeak(x),
  inputs = x
)

initialLowerBounds <- rep(0,dimensions)
initialUpperBounds <- rep(1,dimensions)
regions <- data.frame(
  regionID = 1,
  rLowerBounds = matrix(initialLowerBounds, 1, dimensions),
  rUpperBounds = matrix(initialUpperBounds, 1, dimensions)
)

startInputIndex = 3
endInputIndex = 2+dimensions
outputIndex = 2

GPmodel <- GP_fit(points[,startInputIndex:endInputIndex], points[,outputIndex])
print(GPmodel)

#Instantiate CV sum to be 0
sumCV <- 0
numPoints <- nrow(points)

i <- 1

#Loop for i = 1 to number of pointsR1
repeat{
  #Remove the ith point from our set of points
  removedPoint <- points[i,]
  remainingPoints <- points[-c(i),]
  
  #Create a GP for the new set
  startInputIndex = 3
  endInputIndex = 2+dimensions
  outputIndex = 2
  GPmodel <- GP_fit(remainingPoints[,startInputIndex:endInputIndex], remainingPoints[,outputIndex])
  
  #Predict y_i from x_i using the GP
  prediction <- predict(GPmodel, removedPoint[,startInputIndex:endInputIndex])
  
  #Add MSE to our CV sum
  sumCV <- sumCV + prediction$MSE
  
  #Add the removed point back into our set
  
  i = i + 1
  if(i > numPoints){
    break
  }
}
sumCV
#Divde sum by number of points
LOOCV <- sumCV / numPoints

