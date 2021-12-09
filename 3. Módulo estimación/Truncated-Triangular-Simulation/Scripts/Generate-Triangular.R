## Returns a function that returns a list with each of its elements defined as functions or vectors of one
## element that describe aspects of a triangular distribution
## (pdf, cdf, inverse cdf, mean, median, mode, upper bound, lower bound, variance)
generate.triangular <- function(L, U, M) {
  
  ## Create a function that returns the pdf of the triangular, given some x.
  pdf <- function(x) {
    if (x < L) {
      result <- 0
    } else if (x < M) {
      result <- 2 * (x - L) / ((U - L) * (M - L))
    } else if (x == M) {
      result <- 2 / (U - L)
    } else if (x <= U) {
      result <- 2 * (U - x) / ((U - L) * (U - M))
    } else if (U < x) {
      result <- 0
    }
    return(result)
  }
  
  ## Create a function that returns the cdf of the triangular, given some x.
  cdf <- function(x) {
    if (x <= L) {
      result <- 0
    } else if (x <= M) {
      result <- ((x - L) ^ 2) / ((U - L) * (M - L))
    } else if (x < U) {
      result <- 1 - ((U - x) ^ 2) / ((U - L) * (U - M))
    } else if (U <= x) {
      result <- 1
    }
    return(result)
  }
  
  ## Create a function that returns the inverse cdf of the triangular, given some probability p.
  inverse.cdf <- function(p) {
    if (p < (M - L) / (U - L)) {
      result <- L + sqrt(max(0, (M - L) * (U - L) * p))
    } else if (p >= (M - L) / (U - L)) {
      result <- U - sqrt(max(0, (U - L) * (U - M) * (1 - p)))
    }
    return(result)
  }
  
  ## Create a vector of length 1 that describes the mean of the distribution
  tri.mean <- (L + U + M) / 3
  
  ## Create a vector of length 1 that describes the median of the distribution
  tri.median <- inverse.cdf(0.5)
  
  ## Create a vector of length 1 that describes the mode of the distribution
  tri.mode <- M
  
  ## Create a vector of length 1 that describes the upper bound of the distribution's domain
  tri.upper <- U
  
  ## Create a vector of length 1 that describes the lower bound of the distribution's domain
  tri.lower <- L
  
  ## Create a vector of length 1 that describes the variance of the distribution
  tri.var <-
    ((L ^ 2) + (U ^ 2) + (M ^ 2) - L * U - L * M - U * M) / 18
  
  ## Build the list and return it. This list contains all major properties of the triangular distribution
  return(
    list(
      pdf = pdf,
      cdf = cdf,
      inverse.cdf = inverse.cdf,
      tri.mean = tri.mean,
      tri.median = tri.median,
      tri.mode = tri.mode,
      tri.upper = tri.upper,
      tri.lower = tri.lower,
      tri.var = tri.var
    )
  )
}
