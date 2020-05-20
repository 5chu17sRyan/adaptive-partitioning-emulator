library(GPfit)
library(lhs)
library(ramify)

## 1D Example 1
source("C:/Users/ryans/OneDrive/Desktop/KSSS with Leatherman/Adaptive Partitioning Emulator/CornerPeakFunction.R")

numInputs <- 85
dimensions <- 2

set.seed(seed = NULL)
inputs <- maximinLHS(numInputs, dimensions)
inputs
outputs <- cornerPeak(inputs)
outputs

GPmodel <- GP_fit(inputs, outputs)
print(GPmodel)
plot(GPmodel, resolution = 100, line_type = c(6,2), pch = 5)
