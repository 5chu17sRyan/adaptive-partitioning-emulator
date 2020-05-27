library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/GP_LOOCV.R")

maxDesignSize <- 200
numInputs <- 20
dimensions <- 2 # d

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

#Calculate GP for R1
GPmodelR1 <- GP_fit(points[,startInputIndex:endInputIndex], points[,outputIndex])

#Calculate the CV estimate of GP prediction error over R1
errorR1 <- GP_LOOCV(points)
regions['errors'] <- errorR1
regions$errors

#Set overall design size to be number of inputs
designSize <- numInputs # n0

#Set number of regions to be 1
numRegions <- 1

while(designSize < maxDesignSize){
  #Find the region with the largest prediction error
  maxErrorRegionIndex <- which.max(regions$errors) # k*
  maxErrorRegion <- regions[regions$regionID == maxErrorRegionIndex] # Rk*
  maxErrorRegion
  
  #Find the number of points that currently lie in Rk*
  pointsRk <- points[points$regions == maxErrorRegionIndex,]
  numPointsRk <- nrow(pointsRk) # n*
  
  #Generate a new random (2n0 - n*) x d LHD within Rk
  newInputs <- maximinLHS(2*designSize-numPointsRk, dimensions)
  newInputs
  #Scale to be within the lower an upper bounds of Rk
  i <- 1
  while(i < dimensions){
    upperBound = maxErrorRegion[,dimensions+1+i]
    lowerBound = maxErrorRegion[,1+i]
    newInputs[,i] = newInputs[,i]*(upperBound-lowerBound) + lowerBound
    i = i + 1
  }
  
  newPointsRegion <- rep(maxErrorRegionIndex,numPointsRk)
  
  #Evaluate the new responses at this new LHD : y(new)
  newOutputs <- cornerPeak(newInputs)
  
  #Add the new points to the overall design : X <-{X, x(new)}
  newPoints <- data.frame(regions = newPointsRegion, outputs = newOutputs, inputs = newInputs)
  points <- rbind(points, newPoints)
  
}
