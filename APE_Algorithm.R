library(GPfit)
library(lhs)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")

numInputs = 85
dimensions = 2

set.seed(seed = NULL)
inputs = maximinLHS(numInputs, dimensions)

outputs <- cornerPeak(inputs[1,])
i <- 2
repeat
{
  y <- cornerPeak(inputs[i,])
  outputs <- c(outputs, y)
  i <- i + 1
  if(i > numInputs)
  {
    break
  }
}
outputs <- t(outputs)