#Calculates output of corner peak function for an x in Rd
cornerPeakFunction <- function(x)
{
  dimensions <- length(x)
  #Uniformly sample 'a' parameters
  a = runif(dimensions, 0, 1)
  
  #Rescale parameters so their sum is 1.85
  scalar = 1.85
  a_rescaled <- (a/sum(a))*1.85

  #Sum for the product of all x_j and their related a_j
  sum <- sum(a*x)

  #Calculate output by adding one to the sum and taking it to the -(d+1) power
  y <- (1 + sum)^(-dimensions-1)
  return(y)
}

#Calculates output region of corner peak for the entire input region
cornerPeak <- function(inputs)
{
  
  numInputs <- nrow(inputs)

  x1 <- inputs[1,]
  outputs <- cornerPeakFunction(x1)
  i <- 2
  repeat
  {
    xi <- inputs[i,]
    y <- cornerPeakFunction(xi)
    outputs <- c(outputs, y)
    outputs
    i <- i + 1
    if(i > numInputs)
    {
      break
    }
  }
  transposedOutput <- t(outputs)
  return(transposedOutput)
}
