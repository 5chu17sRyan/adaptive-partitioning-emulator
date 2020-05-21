library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")

numInputs <- 85
dimensions <- 2

set.seed(seed = NULL)
x <- maximinLHS(numInputs, dimensions)
y <- cornerPeak(inputs)
initialRegion <- NULL
for (i in outputs) 
{
  initialRegion <- c(initialRegion, 1)
}

initialRegion

points <- data.frame(
  regions = initialRegion,
  outputs = y,
  inputs = x
)

print(points)

initialLowerBounds <- NULL
initialUpperBounds <- NULL
for (i in dimensions)
{
  initialLowerBounds <- c(initialLowerBounds, 0)
  initialUpperBounds <- c(initialUpperBounds, 1)
}

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
