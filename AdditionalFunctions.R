#Scales values to 0 to 1 to be used in a GP
scaleValuesForGP <- function(dat){
  minimumValues = sapply(dat[,3:(dimensions+2)], min)
  maximumValues = sapply(dat[,3:(dimensions+2)], max)
  ranges = maximumValues - minimumValues
  
  scaledData = dat
  for (i in 1:dimensions){
    dimensionIndex <- i + 2
    scaledData[ , dimensionIndex] = (scaledData[ , dimensionIndex] - minimumValues[i]) / ranges[i]
  }
  
  return(scaledData)
}

#I RAN BOTH FUNCTIONS AND GOT THE SAME VALUES
# scaleValuesForGP <- function(dat){
#   copyOfPoints <- dat
#   i <- 1
#   while(i <= dimensions){
#     dimensionIndex <- i + 2
#     dimensionPoints <- copyOfPoints[ , dimensionIndex]
#     
#     maxValueIndex <- which.max(dimensionPoints)
#     maxValue <- copyOfPoints[maxValueIndex, dimensionIndex]
#     
#     minValueIndex <- which.min(dimensionPoints)
#     minValue <- copyOfPoints[minValueIndex, dimensionIndex]
#     
#     originalRange <- maxValue - minValue
#     
#     numPoints <- nrow(copyOfPoints)
#     j <- 1
#     while(j <= numPoints){
#       # scaled <- (original - oldMinimum) / oldRange
#       # EXAMPLE: original = .5, oldminimum = .25, oldRange = .25
#       # (0.5 - 0.25) / 0.25 = 1
#       copyOfPoints[j, dimensionIndex] <- (copyOfPoints[j, dimensionIndex] - minValue) / originalRange
#       j = j + 1
#     }
#     
#     i = i + 1
#   }
#   
#   return(copyOfPoints)
# }
# 
