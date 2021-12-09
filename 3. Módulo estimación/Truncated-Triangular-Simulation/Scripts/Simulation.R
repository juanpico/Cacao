## Import relevant functions to generate triangular and truncated triangular distributions with cached properties (created as a list)
source(file.path("Scripts", "Generate-Triangular.R"))
source(file.path("Scripts", "Generate-Truncated-Triangular.R"))

## Set a seed so results are replicable
set.seed(10)

## Create a list that contains the major properties of a triangular distribution with L = 2, U = 9, and M = 7
my.tri.dist <- generate.triangular(L = 2, U = 9, M = 7)

## Create a list that contains the major properties of a truncated triangular distribution between a = 3 and b = 8, based on the previous triangular
my.trun.tri.dist <-
  generate.truncated.triangular(a = 3,
                                b = 8,
                                orig.tri.dist = my.tri.dist)

## Plot and describe the triangular distribution
plot.function(
  function(x) {
    sapply(x, FUN = my.tri.dist$pdf)
  },
  from = 2,
  to = 9,
  n = 100000,
  ylab = "f(x)",
  xlab = "x",
  main = "Triangular Distribution - PDF"
)
print("Triangular Distribution:")
print(paste("Analytical Mean: ", my.tri.dist$tri.mean))
print(paste("Analytical Median: ", my.tri.dist$tri.median))
print(paste("Analytical Variance: ", my.tri.dist$tri.var))

## Plot and describe the truncated triangular distribution
plot.function(
  function(x) {
    sapply(x, FUN = my.trun.tri.dist$pdf)
  },
  from = 2.99999,
  to = 8.0001,
  n = 100000,
  ylab = "f( x | a < x < b )",
  xlab = "x",
  main = "Truncated Triangular Distribution - PDF"
)
print("Truncated Triangular Distribution:")
print(paste("Analytical Mean: ", my.trun.tri.dist$trun.tri.mean))
print(paste("Analytical Median: ", my.trun.tri.dist$trun.tri.median))
print(paste("Analytical Variance: ", my.trun.tri.dist$trun.tri.var))

## Simulate 10000 instances of a random variable with f(x) = truncated triangular by trial and error.
vec <- rep(0, 10000)
success.count <- 0
while (success.count < 10000) {
  u.1 <-
    runif(
      n = 1,
      min = my.trun.tri.dist$trun.tri.lower,
      max = my.trun.tri.dist$trun.tri.upper
    )
  u.2 <-
    runif(
      n = 1,
      min = 0,
      max = my.trun.tri.dist$pdf(my.trun.tri.dist$trun.tri.mode)
    )
  if (u.2 <= my.trun.tri.dist$pdf(u.1)) {
    success.count <- success.count + 1
    vec[success.count] <- u.1
  }
}

## Simulate 10000 instances of a random variable with f(x) = truncated triangular by inverse cdf method (faster than by trial and error)
rand.unif <- runif(10000, min = 0, max = 1)
vec2 <- sapply(rand.unif, my.trun.tri.dist$inverse.cdf)

## Make the truncated PDF compatible with vectors for plotting
trun.vector.pdf <- function(x) {
  sapply(x, FUN = my.trun.tri.dist$pdf)
}

## Plot and describe the first simulation
hist(vec,
     xlab = "x",
     breaks = (my.trun.tri.dist$trun.tri.upper - my.trun.tri.dist$trun.tri.lower) * 10,
     freq = FALSE)

curve(
  trun.vector.pdf,
  from = 2.99999,
  to = 8.0001,
  n = 100000,
  col = "red",
  add = TRUE
)

#lines(y = trun.tri.dist.range, x = trun.tri.dist.domain, col = "red")
print(paste("Simulated Mean 1: ", mean(vec)))
print(paste("Simulated Median 1: ", median(vec)))
print(paste("Simulated Variance 1: ", var(vec)))

## Plot and describe the second simulation

hist(vec2,
     xlab = "x",
     breaks = (my.trun.tri.dist$trun.tri.upper - my.trun.tri.dist$trun.tri.lower) * 10,
     freq = FALSE)
curve(
  trun.vector.pdf,
  from = 2.99999,
  to = 8.0001,
  n = 100000,
  col = "red",
  add = TRUE
)
#lines(y = trun.tri.dist.range, x = trun.tri.dist.domain, col = "red")
print(paste("Simulated Mean 2: ", mean(vec2)))
print(paste("Simulated Median 2: ", median(vec2)))
print(paste("Simulated Variance 2: ", var(vec2)))
