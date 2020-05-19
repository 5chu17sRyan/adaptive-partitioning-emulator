library(GPfit)
library(lhs)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")

numInputs = 85
dimensions = 2

set.seed(seed = NULL)
inputs = maximinLHS(numInputs, dimensions)
inputs
outputs <- cornerPeak(inputs[1,])
outputs
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
inputs
outputs

GPmodel <- GP_fit(inputs, t(outputs))
print(GPmodel)
plot(GPmodel, resolution = 100, line_type = c(6,2), pch = 5)
