library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/GP_LOOCV.R")


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

#Subset points to be from region 1.
regionOne <- regions[regions$regionID == 1,]

#subsettedPoints = points
#R1_UpperBound <- regionOne$rUpperBounds.1
#subsettedPoints = points[points$inputs.1 < R1_UpperBound & points$inputs.2 <regionOne$rUpperBounds.2,]
#subsettedPoints

startInputIndex = 3
endInputIndex = 2+dimensions
outputIndex = 2

#Calculate GP for R1
GPmodelR1 <- GP_fit(points[,startInputIndex:endInputIndex], points[,outputIndex])

#Calculate the CV estimate of GP prediction error over R1
errorR1 <- LOOCV(points)
