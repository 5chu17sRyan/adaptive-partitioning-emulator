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

regions[,4:5]-regions[,2:3]

startInputIndex = 3
endInputIndex = 2+dimensions
outputIndex = 2

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

  #Find the number of points that currently lie in Rk*
  pointsRk <- points[points$regions == maxErrorRegionIndex,]
  numPointsRk <- nrow(pointsRk) # n*

  #Generate a new random (2n0 - n*) x d LHD within Rk
  set.seed(seed = NULL)
  newInputs <- maximinLHS((2*designSize)-numPointsRk, dimensions)
  
  #Scale to be within the lower an upper bounds of Rk
  i <- 1
  while(i <= dimensions){
    upperBound = maxErrorRegion[,dimensions+1+i]
    lowerBound = maxErrorRegion[,1+i]
    newInputs[,i] = newInputs[,i]*(upperBound-lowerBound) + lowerBound
    i = i + 1
  }

  newPointsRegion <- rep(maxErrorRegionIndex,numPointsRk)

  #Evaluate the new responses at this new LHD : y(new)
  newOutputs <- cornerPeak(newInputs)

  #Add the new points to the overall design
  newPoints <- data.frame(regions = newPointsRegion, outputs = newOutputs, inputs = newInputs)
  points <- rbind(points, newPoints)
  
  #Choose dimension for splitting, j*
  #Create vector of within region var to between region var
  varWithinToBetween <- NULL
  j <- 1
  while(j <= dimensions){
    #Get the midpoint of Rk in the jth dimension
    upperBound = maxErrorRegion[,dimensions+1+j]
    lowerBound = maxErrorRegion[,1+j]
    jMidpoint = (upperBound+lowerBound)/2
    
    #Split at the midpoint forming two hypothetical subregions
    hypReg1Pts <- subset(points, points[,j+2] <= jMidpoint)
    hypReg2Pts <- subset(points, points[,j+2] > jMidpoint)
    
    #Find the mean of the responses within sub-region
    hypReg1Mean <- mean(hypReg1Pts[,2])
    hypReg2Mean <- mean(hypReg2Pts[,2])
    means <- c(hypReg1Mean, hypReg2Mean)
    
    #Find the variance of the responses within sub-region
    hypReg1Variance <- var(hypReg1Pts[,2])
    hypReg2Variance <- var(hypReg2Pts[,2])
    variances <- c(hypReg1Variance, hypReg2Variance)
    
    #Calculate the variance of the means
    varBetween <- var(means)
    
    #Calculate the mean of the variances
    varWithin <- mean(variances)

    #Store the ratio of the variance within region to variance between region
    varWithinToBetween <- c(varWithinToBetween, varWithin/varBetween)
    
    j = j + 1
  }
  
  #Find the dimension with the largest ration of varWithin to varBetween 
  splittingDimensionIndex <- which.max(varWithinToBetween) # j*
  splittingDimensionIndex
  
  #Find the midpoint of the dimension j*
  splittingDimension_UB = maxErrorRegion[ , dimensions + 1 + splittingDimensionIndex ]
  splittingDimension_LB = maxErrorRegion[ , 1 + splittingDimensionIndex ]
  splittingMidpoint = ( splittingDimension_UB + splittingDimension_LB ) / 2
  
  #Split the region at this midpoint
  points$regions[points[,splittingDimensionIndex+2] > splittingMidpoint] <- points$regions[points[,splittingDimensionIndex+2] > splittingMidpoint] + 1
  
  #Change the bounds of the original region
  lowerBound <- splittingDimensionIndex + 1
  upperBound <- splittingDimensionIndex + 1 + dimensions
  regions[,upperBound][regions$regionID == maxErrorRegionIndex] <- splittingMidpoint
  
  #Create a new region
  newRegion <- data.frame(
    regionID = maxErrorRegionIndex + 1, 
    maxErrorRegion[1,2:(dimensions+1)], 
    maxErrorRegion[1,(dimensions+2):(2*dimensions+1)],
    errors = 0
    )
  regions <- rbind(regions, newRegion)
  regions[,lowerBound][regions$regionID == (maxErrorRegionIndex+1)] <- splittingMidpoint
  
}


