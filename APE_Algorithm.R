library(GPfit)
library(lhs)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/GP_LOOCV.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/FrankeFunction4D.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/AdditionalFunctions.R")


#Set starting parameters
maxDesignSize <- 20
numInputs <- 10
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
  outputs = cornerPeak(x), #corner peak function
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
  maxErrorRegionIndex <- which.max(regions$errors) # k*
  maxErrorRegion <- regions[regions$regionID == maxErrorRegionIndex,] # Rk*
  maxErrorRegionPts <- points[points$regions == maxErrorRegionIndex,]
  
  #Find the number of points that currently lie in Rk*
  pointsRk <- points[points$regions == maxErrorRegionIndex,]
  numPointsRk <- nrow(pointsRk) # n*

  #Generate a new random (2n0 - n*) x d LHD within Rk
  set.seed(seed = NULL)
  numNewInputs <- (2 * numInputs) - numPointsRk
  newInputs <- maximinLHS(numNewInputs, dimensions)
  
  #Scale to be within the lower an upper bounds of Rk
  #"scaledNewInputs <- scaleValues(newInputs, upperBounds, lowerBounds)"
  #"scaledNewInputs <- scaleInputsToRegion(newInputs, maxErrorRegion)" 
  i <- 0
  while(i < dimensions){
    upperBound = maxErrorRegion[ ,upperBoundStartIndex + i]
    lowerBound = maxErrorRegion[ ,lowerBoundStartIndex + i]
    newInputs[ ,i + 1] = newInputs[ ,i + 1]*(upperBound-lowerBound) + lowerBound
    i = i + 1
  }
  
  newPointsRegionID <- rep(maxErrorRegionIndex, numNewInputs)
  
  #Evaluate the new responses at this new LHD : y(new)
  #newOutputs <- franke4D(newInputs) # four dimensional Franke function
  newOutputs <- cornerPeak(newInputs) # two dimensional corner peak function

  #Add the new points to the overall design
  newPoints <- data.frame(
    regions = newPointsRegionID, 
    outputs = newOutputs, 
    inputs = newInputs
    )
  points <- rbind(points, newPoints)
  
  #Make all of algorithm 2 into a function
  #Choose dimension for splitting, j*
  #Create vector of within region var to between region var
  #splittingDimensionIndex <- chooseRegionSplit(midpoints, maxErrorRegion, maxErrorRegionPts, inputStartIndex, outputStartIndex)
  #We can redefine maxErrorRegionPts, inputStartIndex, and outputStartIndex within the function 
  midpoints <- NULL
  varWithinToBetween <- NULL
  j <- 0
  while(j < dimensions){
    #Get the midpoint of Rk in the jth dimension
    #Store the hypothetical midpoints in a vector to retieve later
    upperBound = maxErrorRegion[maxErrorRegionIndex ,upperBoundStartIndex + j]
    lowerBound = maxErrorRegion[maxErrorRegionIndex ,lowerBoundStartIndex + j]
    jMidpoint = (upperBound+lowerBound)/2
    midpoints <- c(midpoints, jMidpoint)
    
    #Subset max error region points
    #Split at the midpoint forming two hypothetical subregions
    #Use maxErrorRegionPoints
    hypReg1Pts <- subset(maxErrorRegionPts, maxErrorRegionPts[ ,inputStartIndex + j] <= jMidpoint)
    hypReg2Pts <- subset(maxErrorRegionPts, maxErrorRegionPts[ ,inputStartIndex + j] > jMidpoint)
    
    #Find the mean of the responses within sub-regions
    hypReg1Responses <- hypReg1Pts[ ,outputStartIndex]
    hypReg2Responses <- hypReg2Pts[ ,outputStartIndex]
    
    hypReg1Mean <- mean(hypReg1Responses)
    hypReg2Mean <- mean(hypReg2Responses)
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

  #Find the dimension with the largest ration of varWithin to varBetween 
  splittingDimensionIndex <- which.max(varWithinToBetween) # j*
  
  #Find the midpoint of the dimension j*
  splittingMidpoint = midpoints[splittingDimensionIndex]
  
  #Look at points where their region is equal to maxErrorRegionIndex
  newRegionIndex <- numRegions + 1
  splittingDimensionInput_Index <- (inputStartIndex - 1) + splittingDimensionIndex
  pointsGreaterThanMidpoint <- maxErrorRegionPts[ , splittingDimensionInput_Index] > splittingMidpoint
  #Split the region at this midpoint
  points$regions[pointsGreaterThanMidpoint] <- newRegionIndex
  
  #Change the bounds of the original region
  lowerBoundIndex <- (lowerBoundStartIndex - 1) + splittingDimensionIndex
  upperBoundIndex <- (upperBoundStartIndex - 1) + splittingDimensionIndex
  regions[ ,upperBoundIndex][regions$regionID == maxErrorRegionIndex] <- splittingMidpoint
  
  #Create a new region
  newRegion <- data.frame(
    regionID = newRegionIndex, 
    maxErrorRegion[ 1, lowerBoundStartIndex:lowerBoundEndIndex], 
    maxErrorRegion[ 1, upperBoundStartIndex:upperBoundEndIndex],
    errors = NA
    )
  regions <- rbind(regions, newRegion)
  regions[ ,lowerBoundIndex][regions$regionID == newRegionIndex] <- splittingMidpoint
  
  #Find CV estimate for both new regions 
  rKpoints <- points[points$regions == maxErrorRegionIndex, ]
  scaledPointsRK <- scaleValuesForGP(rKpoints)
  errorRK <- GP_LOOCV(scaledPointsRK)
  regions[ ,errorIndex][regions$regionID == maxErrorRegionIndex] <- errorRK
  
  rK1points <- points[points$regions == newRegionIndex, ]
  scaledPointsRK1 <- scaleValuesForGP(rK1points) 
  errorRK1 <- GP_LOOCV(scaledPointsRK1)
  regions[ ,errorIndex][regions$regionID == newRegionIndex] <- errorRK1

  #Update the total number of regions 
  numRegions <- numRegions + 1

  #Update the overall design size
  designSize <- designSize + (2*numInputs - numPointsRk)
}


########## Create and test model ##########

## Create set of test points

#Create LHD of inputs
numTestInputs <- 1000
set.seed(seed = NULL)
testInputs <- maximinLHS(numTestInputs, dimensions)

#Evaluate the inputs to get outputs
#testOutputs <- franke4D(x) # four dimensional Franke Function
testOutputs <- cornerPeak(testInputs)

testPoints <- data.frame(
  #regions = initialRegion,
  outputs = testOutputs,
  inputs = testInputs
)

#For each region
squaredErrors <- NULL
currentRegionIndex <- 1
while(currentRegionIndex <= numRegions){
  #Scale up points in region to be from 0 to 1
  regionPoints <- points[points$regions == currentRegionIndex, ]
  scaledPoints <- scaleValuesForGP(regionPoints)
  
  #Create GP model
  inputEndIndex <- inputStartIndex + dimensions - 1
  inputsGP <- scaledPoints[ , inputStartIndex:inputEndIndex]
  outputsGP <- scaledPoints[, outputStartIndex]
  GPmodel <- GP_fit(inputsGP, outputsGP)
  
  #Subset test points to be just in this region
  currentRegion <- regions[regions$regionID == currentRegionIndex, ]
  testPointsSubset <- testPoints
  currentDimensionIndex <- 1
  while(currentDimensionIndex < dimensions){
    upperBound <- currentRegion[ ,upperBoundStartIndex + currentDimensionIndex - 1]
    lowerBound <- currentRegion[ ,lowerBoundStartIndex + currentDimensionIndex - 1]
    testPointsSubset <- subset( testPointsSubset, testPointsSubset[ , ( inputStartIndex + currentDimensionIndex - 1 ) ] <= upperBound )
    testPointsSubset <- subset( testPointsSubset, testPointsSubset[ , ( inputStartIndex + currentDimensionIndex - 1 ) ] > lowerBound )
    
    currentDimensionIndex <- currentDimensionIndex + 1
  }

  #Scale up test points to be from 0 to 1
  scaledTestPoints <- scaleValuesForGP(testPointsSubset)
  numScaledTestPoints <- nrow(scaledTestPoints)
  
  #For each test point in region
  currentPointIndex <- 1
  while(currentPointIndex <= numScaledTestPoints){
    #Predict output using GP model
    currentPoint <- scaledTestPoints[currentPointIndex, ]
    currentInputs <- currentPoint[ ,(inputStartIndex-1):(inputEndIndex-1)]
    prediction <- predict(GPmodel, currentInputs)
    
    predictedOutput <- prediction$Y_hat
    actualOutput <- currentPoint[ ,(outputStartIndex-1)]
    
    #Calculate squared error
    squaredError <- (predictedOutput - actualOutput)^2
    
    #Store squared error
    squaredErrors <- c(squaredErrors, squaredError)
    
    currentPointIndex <- currentPointIndex + 1
  }

  currentRegionIndex <- currentRegionIndex + 1
}

#Calculate RMSE
MSE <- sum(squaredErrors) / numTestInputs
RMSE <- MSE^(1/2)

#Calculate MASE
maxSEIndex <- which.max(squaredErrors)
MASE <- squaredErrors[maxSEIndex]