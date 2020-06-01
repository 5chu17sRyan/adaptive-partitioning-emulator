library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/GP_LOOCV.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/FrankeFunction4D.R")

#Set starting parameters
maxDesignSize <- 200
numInputs <- 20
#dimensions <- 4 # four dimensional Franke
dimensions <- 2 # d # cornerPeak

#Create initial LHD
set.seed(seed = NULL)
x <- maximinLHS(numInputs, dimensions)

#Set the initial region of all inputs to be 1
initialRegion <- rep(1,numInputs)

#Create a representation of all points
points <- data.frame(
  regions = initialRegion,
  #outputs = franke4D(x) # four dimensional Franke Function
  outputs = cornerPeak(x), # two dimensional corner peak function
  inputs = x
)

outputStartIndex <- 2
inputStartIndex <- 3

#Set the initial lower and upper bounds of region 1 to be 0 and 1 respectively for all dimensions
initialLowerBounds <- rep(0,dimensions)
initialUpperBounds <- rep(1,dimensions)
initialRegionID <- 1

#Calculate the CV estimate of GP prediction error over R1
R1_Error <- GP_LOOCV(points)

#Create a representation for all regions, initialized with only region 1
regions <- data.frame(
  regionID = initialRegionID,
  rLowerBounds = matrix(initialLowerBounds, 1, dimensions),
  rUpperBounds = matrix(initialUpperBounds, 1, dimensions),
  errors = R1_Error
)

lowerBoundStartIndex <- 2
lowerBoundEndIndex <- dimensions + 1
upperBoundStartIndex <- dimensions + 2
upperBoundEndIndex <- (2 * dimensions) + 1
errorIndex <- (2 * dimensions) + 2

#Set overall design size to be number of inputs
designSize <- numInputs # n0

#Set number of regions to be 1
numRegions <- 1

while(designSize < maxDesignSize){
  #Find the region with the largest prediction error
  #"maxErrorRegion <- findMaxError(regions)"
  maxErrorRegionIndex <- which.max(regions$errors) # k*
  regions
  maxErrorRegion <- regions[regions$regionID == maxErrorRegionIndex,] # Rk*
  maxErrorRegion
  regions
  
  #Find the number of points that currently lie in Rk*
  #"numPointsRk <- numPointsInRegion(points, regionIndex)"
  pointsRk <- points[points$regions == maxErrorRegionIndex,]
  pointsRk
  numPointsRk <- nrow(pointsRk) # n*
  numPointsRk

  #Generate a new random (2n0 - n*) x d LHD within Rk
  set.seed(seed = NULL)
  designSize
  numPointsRk
  numNewInputs <- (2 * designSize) - numPointsRk
  numNewInputs
  newInputs <- maximinLHS(numNewInputs, dimensions)
  newInputs
  
  #Scale to be within the lower an upper bounds of Rk
  #"scaledNewInputs <- scaleValues(newInputs, upperBounds, lowerBounds)"
  i <- 0
  while(i < dimensions){
    upperBound = maxErrorRegion[ ,upperBoundStartIndex + i]
    lowerBound = maxErrorRegion[ ,lowerBoundStartIndex + i]
    newInputs[ ,i + 1] = newInputs[ ,i + 1]*(upperBound-lowerBound) + lowerBound
    i = i + 1
  }
  newInputs
  
  maxErrorRegion
  maxErrorRegionIndex
  numPointsRk
  regions
  newPointsRegionID <- rep(maxErrorRegionIndex, numNewInputs)
  newPointsRegionID
  points
  #Points is still good
  
  #Evaluate the new responses at this new LHD : y(new)
  #newOutputs <- franke4D(newInputs) # four dimensional Franke function
  newOutputs <- cornerPeak(newInputs) # two dimensional corner peak function

  points
  #Add the new points to the overall design
  newPoints <- data.frame(
    regions = newPointsRegionID, 
    outputs = newOutputs, 
    inputs = newInputs
    )
  newPoints
  points <- rbind(points, newPoints)
  points
  #points is still good
  
  #Choose dimension for splitting, j*
  #Create vector of within region var to between region var
  varWithinToBetween <- NULL
  j <- 0
  while(j < dimensions){
    #Get the midpoint of Rk in the jth dimension
    #"jMidpoint = <- getMidpoint(region, dimension)"
    upperBound = maxErrorRegion[ ,upperBoundStartIndex + j]
    lowerBound = maxErrorRegion[ ,lowerBoundStartIndex + j]
    jMidpoint = (upperBound+lowerBound)/2
    
    #Split at the midpoint forming two hypothetical subregions
    hypReg1Pts <- subset(points, points[ ,inputStartIndex + j] <= jMidpoint)
    hypReg2Pts <- subset(points, points[ ,inputStartIndex + j] > jMidpoint)
    
    #Find the mean of the responses within sub-regions
    hypReg1Responses <- hypReg1Pts[ ,outputStartIndex]
    hypReg2Responses <- hypReg2Pts[ ,outputStartIndex]
    
    hypReg1Mean <- mean(hypReg1Responses)
    hypReg2Mean <- mean(hypReg1Responses)
    means <- c(hypReg1Mean, hypReg2Mean)
    
    #Find the variance of the responses within sub-regions
    hypReg1Variance <- var(hypReg1Responses)
    hypReg2Variance <- var(hypReg2Responses)
    variances <- c(hypReg1Variance, hypReg2Variance)
    
    #Calculate the variance of the means
    varBetween <- var(means)
    
    #Calculate the mean of the variances
    varWithin <- mean(variances)

    #Store the ratio of the variance within region to variance between region
    varWithinToBetween <- c(varWithinToBetween, varWithin/varBetween)
    
    j = j + 1
  }
  
  points
  #points is still good
  maxErrorRegion
  regions
  #Find the dimension with the largest ration of varWithin to varBetween 
  splittingDimensionIndex <- which.max(varWithinToBetween) # j*
  
  #Find the midpoint of the dimension j*
  #"splittingMidpoint = <- getMidpoint(region, dimension)"
  splittingDimension_UB = maxErrorRegion[ , (upperBoundStartIndex - 1) + splittingDimensionIndex ]
  splittingDimension_LB = maxErrorRegion[ , (lowerBoundStartIndex - 1) + splittingDimensionIndex ]
  splittingMidpoint = ( splittingDimension_UB + splittingDimension_LB ) / 2
  
  points #still good
  
  #ERROR HERE this looks at points in all regions not just Rk
  #Look at points where their region is equal to maxErrorRegionIndex
  newRegionIndex <- numRegions + 1
  splittingDimensionInput_Index <- (inputStartIndex - 1) + splittingDimensionIndex
  maxErrorRegionPts <- points[points$regions == maxErrorRegionIndex,]
  maxErrorRegionPts
  pointsGreaterThanMidpoint <- maxErrorRegionPts[ , splittingDimensionInput_Index] > splittingMidpoint
  pointsGreaterThanMidpoint
  #Split the region at this midpoint
  points$regions[pointsGreaterThanMidpoint] <- newRegionIndex
  
  points
  
  #Change the bounds of the original region
  lowerBoundIndex <- (lowerBoundStartIndex - 1) + splittingDimensionIndex
  upperBoundIndex <- (upperBoundStartIndex - 1) + splittingDimensionIndex
  regions[ ,upperBoundIndex][regions$regionID == maxErrorRegionIndex] <- splittingMidpoint
  
  regions
  
  #Create a new region
  newRegion <- data.frame(
    regionID = maxErrorRegionIndex + 1, 
    maxErrorRegion[ 1, lowerBoundStartIndex:lowerBoundEndIndex], 
    maxErrorRegion[ 1, upperBoundStartIndex:upperBoundEndIndex],
    errors = NA
    )
  regions <- rbind(regions, newRegion)
  regions[ ,lowerBoundIndex][regions$regionID == newRegionIndex] <- splittingMidpoint
  
  regions
  
  #Find CV estimate for both new regions 
  rKpoints <- points[points$regions == maxErrorRegionIndex, ]
  errorRK <- GP_LOOCV(rKpoints)
  regions[ ,errorIndex][regions$regionID == maxErrorRegionIndex] <- errorRK
  
  regions
  
  rK1points <- points[points$regions == newRegionIndex, ]
  errorRK1 <- GP_LOOCV(rK1points)
  regions[ ,errorIndex][regions$regionID == newRegionIndex] <- errorRK1
  
  regions

  #Update the total number of regions 
  numRegions <- numRegions + 1

  #Update the overall design size
  designSize <- designSize + (2*designSize - numPointsRk)

}


