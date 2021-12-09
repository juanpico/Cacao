## Returns a function that returns a list with each of its elements defined as functions or vectors of one
## element that describe aspects of a truncated triangular distribution
## (pdf, cdf, inverse cdf, mean, median, mode, upper bound, lower bound, variance).
generate.truncated.triangular <- function(a, b, orig.tri.dist) {
  
  ## Original parameters of the triangular distribution
  M <- orig.tri.dist$tri.mode
  L <- orig.tri.dist$tri.lower
  U <- orig.tri.dist$tri.upper
  
  ## Throw an error if the new bounds do not fall witin the old bounds of the triangular pdf.
  if (a < L | b > U) {
    stop("The new bounds of the pdf do not fall within the original range")
  }
  
  ## Create a function that returns the pdf of the truncated triangular, given some x.
  pdf <- function(x) {
    if (a < x & x <= b) {
      pdf.g <- orig.tri.dist$pdf
      cumul.F <- orig.tri.dist$cdf
      result <- pdf.g(x) / (cumul.F(b) - cumul.F(a))
      return(result)
    } else {
      result <- 0
      return(result)
    }
  }
  
  ## Create a function that returns the cdf of the truncated triangular, given some x.
  cdf <- function(x) {
    cumul.F <- orig.tri.dist$cdf
    if (x <= a) {
      result <- 0
    } else if (a < x & x <= b) {
      result <- (cumul.F(x) - cumul.F(a)) / (cumul.F(b) - cumul.F(a))
    } else if (b < x) {
      result <- 1
    }
    return(result)
  }
  
  ## Create a function that returns the inverse cdf of the truncated triangular, given some probability p.
  inverse.cdf <- function(p) {
    result <-
      orig.tri.dist$inverse.cdf(p * (orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)) + orig.tri.dist$cdf(a))
    return(result)
  }
  
  ## Create a vector of length 1 that describes the mean of the distribution
  trun.tri.mean <- if (a <= b & b < M) {
    result.numerator <-
      (2 * (-3 * L * ((b ^ 2) / 2 - (a ^ 2) / 2) + b ^ 3 - a ^ 3)) / (3 * (U - L) * (M - L))
    result.denominator <-
      orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)
    result.numerator / result.denominator
  } else if (a < M & b >= M) {
    result.numerator <-
      (
        -M ^ 3 * U - 2 * a ^ 3 * U + 3 * L * a ^ 2 * U + 3 * M * b ^ 2 * U - 3 * L * b ^ 2 * U + L * M ^ 3 + 2 * M * a ^ 3 - 3 * L * M * a ^ 2 - 2 * M * b ^ 3 + 2 * L * b ^ 3
      ) / (3 * (U - L) * (M - L) * (U - M))
    result.denominator <-
      orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)
    result.numerator / result.denominator
  } else if (M <= a & a <= b) {
    result.numerator <-
      (3 * b ^ 2 * U - 3 * a ^ 2 * U - 2 * b ^ 3 + 2 * a ^ 3) / (3 * (U - L) * (U - M))
    result.denominator <-
      orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)
    result.numerator / result.denominator
  }
  
  ## Create a vector of length 1 that describes the median of the distribution
  
  trun.tri.median <- inverse.cdf(0.5)
  
  ## Create a vector of length 1 that describes the mode of the distribution
  trun.tri.mode <- if (a <= b & b <= M) {
    b
  } else if (a <= b & b >= M) {
    M
  } else if (a >= M & b >= a) {
    a
  }
  
  ## Create a vector of length 1 that describes the upper bound of the distribution's domain
  trun.tri.upper <- b
  
  ## Create a vector of length 1 that describes the lower bound of the distribution's domain
  trun.tri.lower <- a
  
  ## Create a vector of length 1 that describes the variance of the distribution
  trun.tri.var <- if (a <= b & b < M) {
    result.numerator <-
      (-4 * L * ((b ^ 3) / 3 - (a ^ 3) / 3) + b ^ 4 - a ^ 4) / (2 * (U - L) * (M - L))
    result.denominator <-
      orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)
    (result.numerator / result.denominator) - trun.tri.mean ^ 2
  } else if (a < M & b >= M) {
    result.numerator <-
      (
        -M ^ 4 * U - 3 * a ^ 4 * U + 4 * L * a ^ 3 * U + 4 * M * b ^ 3 * U - 4 * L * b ^ 3 * U + L * M ^ 4 + 3 * M * a ^ 4 - 4 * L * M * a ^ 3 - 3 * M * b ^ 4 + 3 * L * b ^ 4
      ) / (6 * (U - L) * (M - L) * (U - M))
    result.denominator <-
      orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)
    (result.numerator / result.denominator) - trun.tri.mean ^ 2
  } else if (M <= a & a <= b) {
    result.numerator <-
      (4 * (b ^ 3) * U - 4 * (a ^ 3) * U - 2 * b ^ 4 + 3 * a ^ 4) / (6 * (U - L) * (U - M))
    result.denominator <-
      orig.tri.dist$cdf(b) - orig.tri.dist$cdf(a)
    (result.numerator / result.denominator) - trun.tri.mean ^ 2
  }
  
  ## Build the list and return it. This list contains all major properties of the truncated triangular distribution
  return(
    list(
      pdf = pdf,
      cdf = cdf,
      inverse.cdf = inverse.cdf,
      trun.tri.mean = trun.tri.mean,
      trun.tri.median = trun.tri.median,
      trun.tri.mode = trun.tri.mode,
      trun.tri.upper = trun.tri.upper,
      trun.tri.lower = trun.tri.lower,
      trun.tri.var = trun.tri.var
    )
  )
}
