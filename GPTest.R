library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")

numInputs <- 85
dimensions <- 2

set.seed(seed = NULL)
x <- maximinLHS(numInputs, dimensions)

initialRegion <- NULL
for (i in x) 
{
  initialRegion <- c(initialRegion, 1)
}

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

#Remove point
i <- 1
removedPoint <- points[i,]
remainingPoints <- points[-c(i),]
remainingPoints

#Create a GP for the new set
startInputIndex = 3
endInputIndex = 2+dimensions
outputIndex = 2
GPmodel <- GP_fit(remainingPoints[,startInputIndex:endInputIndex], remainingPoints[,outputIndex])

#Predict y_i from x_i using the GP
prediction <- predict(GPmodel, removedPoint[,startInputIndex:endInputIndex])
prediction$MSE
