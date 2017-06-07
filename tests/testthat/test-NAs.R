context("NA, Inf, -Inf handling")
library(gtools) # for function: permutations()

# NOTE: min(c(NA, -Inf)) = NA not -Inf!!



# Following test for double only (for now)
test_that("biganalytics behavior with NA, Inf and -Inf same as default R-functions", {
  
  y <- t(permutations(4, 4, 1:4, repeats = TRUE))
  set.seed(06511)
  y[y == 4] <- round(rnorm(256), 3)
  set.seed(NULL)
  y[y == 3] <- Inf
  y[y == 2] <- -Inf
  y[y == 1] <- NA
  
  # head(t(y)); tail(t(y))
  
  x <- big.matrix(nrow = 4, ncol = 256, init = 0)
  x[,] <- as.vector(y)

  expect_equal(colmin(x), apply(y, 2, min))
  foo <- data.frame(t(y), big.pkg = colmin(x), r.default = apply(y, 2, min))
  foo[1:150, ]
  foo[151:256, ]
  
  # # Error b/c didn't fix colmax
  # expect_equal(colmax(x), apply(y, 2, max)) 
  foo <- data.frame(t(y), big.pkg = colmax(x), r.default = apply(y, 2, max))
  foo[1:150, ]
  foo[151:256, ]
  # NOTE default behavior: max(c(NA, Inf)) = NA not Inf!!

  # # Error b/c didn't fix colmax
  # expect_equal(unname(colrange(x)), t(apply(y, 2, range)))
  expect_equal(unname(colrange(x))[,1], t(apply(y, 2, range))[,1])
  
  expect_equal(colsum(x), apply(y, 2, sum))
  expect_equal(colmean(x), apply(y, 2, mean))
  expect_equal(colprod(x), apply(y, 2, prod))
  expect_equal(colsd(x), apply(y, 2, sd))
})






