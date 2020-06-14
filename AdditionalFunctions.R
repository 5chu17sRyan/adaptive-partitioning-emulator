#Scales values to 0 to 1 to be used in a GP
scaleValuesForGP <- function(dat){
  m = sapply(dat[,3:(dimensions+2)], min)
  M = sapply(dat[,3:(dimensions+2)], max)
  r = M-m
  
  sc.dat = dat
  for (i in 1:dimensions){
    sc.dat[,2+i] = (sc.dat[,2+i]-m[i])/r[i]
  }
  return(sc.dat)
}

# scaleValuesForGP <- function(points){
#   copyOfPoints <- points
#   dimensions = ncol(copyOfPoints) - 3
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
#     newRange <- 1
#     newMin <- 0
#     
#     numPoints <- nrow(copyOfPoints)
#     j <- 1
#     while(j <= numPoints){
#       copyOfPoints[j, dimensionIndex] <- (copyOfPoints[j, dimensionIndex] - minValue) * (newRange / originalRange)
#       j = j + 1
#     }
#     
#     i = i + 1
#   }
#   
#   return(copyOfPoints)
# }
# 
# createLHD <- function(numInputs, dimensions, upperBounds, lowerBounds){
#   
# }