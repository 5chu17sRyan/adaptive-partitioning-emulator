library(GPfit)
library(lhs)
library(dplyr)


#source("/Users/leatherman1/Downloads/adaptive-partitioning-emulator-master/CornerPeakFunction.R")
#source("/Users/leatherman1/Downloads/adaptive-partitioning-emulator-master/GP_LOOCV.R")
#source("/Users/leatherman1/Downloads/adaptive-partitioning-emulator-master/AdditionalFunctions.R")

source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/GP_LOOCV.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/FrankeFunction4D.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/AdditionalFunctions.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/SmartSwap.R")


#Set starting parameters
maxDesignSize <- 64
numInputs <- 4
#dimensions <- 4 # four dimensional Franke
dimensions <- 2 # d # cornerPeak

#Create initial LHD
set.seed(seed = NULL)
x <- maximinLHS(numInputs, dimensions)

#Set the initial region of all inputs to be 1
initialRegion <- rep(1,numInputs)
iteration <- 0
initialIteration <- rep(iteration, numInputs)


#Create a representation of all points
dat <- data.frame(
  regions = initialRegion,
  #outputs = franke4D(x) # four dimensional Franke Function
  outputs = cornerPeak(x), #corner peak function
  inputs = x,
  iterations = initialIteration
)

outputStartIndex <- 2
inputStartIndex <- 3

#Set the initial lower and upper bounds of region 1 to be 0 and 1 respectively for all dimensions
initialLowerBounds <- rep(0,dimensions)
initialUpperBounds <- rep(1,dimensions)
initialRegionID <- 1

#Calculate the CV estimate of GP prediction error over R1
R1_Error <- GP_kfoldCV(dat,5)

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
  iteration <- iteration + 1
  
  #Find the region with the largest prediction error
  maxErrorRegionIndex <- which.max(regions$errors) # k*
  maxErrorRegion <- regions[regions$regionID == maxErrorRegionIndex,] # Rk*
  maxErrorRegionPts <- dat[dat$regions == maxErrorRegionIndex,]
  
  #Find the number of points that currently lie in Rk*
  pointsRk <- dat[dat$regions == maxErrorRegionIndex,]
  numPointsRk <- nrow(pointsRk) # n*
  
  #Generate a new random (2n0 - n*) x d LHD within Rk
  set.seed(seed = NULL)
  numNewInputs <- (2 * numInputs) - numPointsRk
  newInputs <- maximinLHS(numNewInputs, dimensions)
  
  #Scale to be within the lower an upper bounds of Rk
  i <- 0
  while(i < dimensions){
    upperBound = maxErrorRegion[ ,upperBoundStartIndex + i]
    lowerBound = maxErrorRegion[ ,lowerBoundStartIndex + i]
    newInputs[ ,i + 1] = newInputs[ ,i + 1]*(upperBound-lowerBound) + lowerBound
    i = i + 1
  }
  
  inputEndIndex <- inputStartIndex + dimensions - 1
  originalInputs <- dat[ ,inputStartIndex:inputEndIndex]
  colnames(newInputs) <- colnames(originalInputs)
  addedInputs <- newInputs
  newInputs
  newInputs <- maximinSmartSwap(originalInputs, addedInputs)
  newInputs
  
  newPointsRegionID <- rep(maxErrorRegionIndex, numNewInputs)
  
  #Evaluate the new responses at this new LHD : y(new)
  #newOutputs <- franke4D(newInputs) # four dimensional Franke function
  newOutputs <- cornerPeak(newInputs) # two dimensional corner peak function
  
  newIterations <- rep(iteration, numNewInputs)
  
  #Add the new points to the overall design
  newPoints <- data.frame(
    regions = newPointsRegionID, 
    outputs = newOutputs, 
    inputs = newInputs,
    iterations = newIterations
  )
  dat <- rbind(dat, newPoints)
  maxErrorRegionPts = rbind(maxErrorRegionPts, newPoints)
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
    upperBound = maxErrorRegion[maxErrorRegion$regionID == maxErrorRegionIndex ,upperBoundStartIndex + j]
    lowerBound = maxErrorRegion[maxErrorRegion$regionID == maxErrorRegionIndex ,lowerBoundStartIndex + j]
    jMidpoint = (upperBound+lowerBound)/2
    # for some reason it looks like jMidpoints value was NA but when I ran this line again it had no problem its like it was skipped over.
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
  newRegionIndex
  splittingDimensionInput_Index <- (inputStartIndex - 1) + splittingDimensionIndex
  #pointsGreaterThanMidpoint <- maxErrorRegionPts[ , splittingDimensionInput_Index] > splittingMidpoint

  pointsGreaterThanMidpoint <- dat[dat$regions == maxErrorRegionIndex,][ , splittingDimensionInput_Index] > splittingMidpoint
  #Split the region at this midpoint
  dat[dat$regions == maxErrorRegionIndex,][pointsGreaterThanMidpoint,]$regions <- newRegionIndex
  
  
  #Change the bounds of the original region
  lowerBoundIndex <- (lowerBoundStartIndex - 1) + splittingDimensionIndex
  upperBoundIndex <- (upperBoundStartIndex - 1) + splittingDimensionIndex
  regions[regions$regionID == maxErrorRegionIndex,upperBoundIndex] <- splittingMidpoint
  
  #Create a new region
  newRegion <- data.frame(
    regionID = newRegionIndex, 
    maxErrorRegion[ 1, lowerBoundStartIndex:lowerBoundEndIndex], 
    maxErrorRegion[ 1, upperBoundStartIndex:upperBoundEndIndex],
    errors = NA
  )
  regions <- rbind(regions, newRegion)
  regions[regions$regionID == newRegionIndex,lowerBoundIndex] <- splittingMidpoint
  
  #Find CV estimate for both new regions 
  rKpoints <- dat[dat$regions == maxErrorRegionIndex, ]
  scaledPointsRK <- scaleValuesForGP(rKpoints)
  errorRK <- GP_kfoldCV(scaledPointsRK, 5)
  regions[regions$regionID == maxErrorRegionIndex,errorIndex] <- errorRK
  
  rK1points <- dat[dat$regions == newRegionIndex, ]
  scaledPointsRK1 <- scaleValuesForGP(rK1points) 
  errorRK1 <- GP_kfoldCV(scaledPointsRK1, 5)
  regions[regions$regionID == newRegionIndex,errorIndex] <- errorRK1
  
  #Update the total number of regions 
  numRegions <- numRegions + 1
  
  #Update the overall design size
  designSize <- nrow(dat) #also it seems like it didn't run this it ended with 61 points but design size 51
  print(designSize)
}

########## Plot input points ##########

x1 <- dat[ ,inputStartIndex]
x2 <- dat[ ,(inputStartIndex+1)]
y <- dat[ , outputStartIndex]

color.gradient <- function(x, colors=c("light grey","dark blue"), colsteps=256) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

shapes <- as.character(dat$iterations)
shapesRegions <- as.character(dat$regions)

plot(x = x1, 
     y = x2, 
     pch = shapesRegions, 
     col = color.gradient(y, colsteps=256)
)


########## 

#library(ggplot2)
#ggplot(dat,
#       aes(x = inputs.1,
#           y = inputs.2,
#           color = outputs,
#           shape = shapes
#       )
#) + geom_point(size = 2)

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
  regions = NA,
  outputs = testOutputs,
  inputs = testInputs
)

#For each region
squaredErrors <- NULL
currentRegionIndex <- 1
numRegions
while(currentRegionIndex <= numRegions){
  #Scale up points in region to be from 0 to 1
  regionPoints <- dat[dat$regions == currentRegionIndex, ]
  dat$regions
  currentRegionIndex
  dat
  scaledPoints <- scaleValuesForGP(regionPoints)#Only one point in regionPoints, can't scale properly
  regions
  scaledPoints
  regionPoints
  
  #Create GP model
  inputEndIndex <- inputStartIndex + dimensions - 1
  inputsGP <- scaledPoints[ , inputStartIndex:inputEndIndex]
  outputsGP <- scaledPoints[, outputStartIndex]
  GPmodel <- GP_fit(inputsGP, outputsGP) #Error in this line of code
  
  #Subset test points to be just in this region
  currentRegion <- regions[regions$regionID == currentRegionIndex, ]
  testPointsSubset <- testPoints
  currentDimensionIndex <- 1
  while(currentDimensionIndex <= dimensions){
    upperBound <- currentRegion[ ,upperBoundStartIndex + currentDimensionIndex - 1]
    lowerBound <- currentRegion[ ,lowerBoundStartIndex + currentDimensionIndex - 1]
    testPointsSubset <- subset( testPointsSubset, testPointsSubset[ , ( inputStartIndex + currentDimensionIndex) - 1 ] <= upperBound )
    testPointsSubset <- subset( testPointsSubset, testPointsSubset[ , ( inputStartIndex + currentDimensionIndex) - 1 ] > lowerBound )
    
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

traceback()
