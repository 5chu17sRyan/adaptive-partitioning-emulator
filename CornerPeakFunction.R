cornerPeak <- function(input)
{
  
  dimensions <- length(input)
  
  #Uniformly sample 'a' parameters
  a = runif(dimensions, 0, 1)
  
  #Rescale parameters so their sum is 1.85
  scalar = 1.85
  a_rescaled <- (a/sum(a))*1.85
  
  #Sum for the product of all x_j and their related a_j
  sum <- sum(a*input)
  
  #Calculate output by adding one to the sum and taking it to the -(d+1) power
  output <- (1 + sum)^(-dimensions-1)
  return(output)
}