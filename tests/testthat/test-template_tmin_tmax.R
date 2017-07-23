library(gtools) # for function: permutations()
library(devtools)
library(testthat)

devtools::load_all()
rm(list = ls())

#===========================================================
# PLEASE look at test-double_tmin_tmax.R file first
#===========================================================


# (a) Generate test data with small integers and NA (No +Inf, -Inf, NaN):
# NOTE: Could have used 2x4 testing matrix. Here used 3x8.
#--------------------------------------------------------------------
y <- t(permutations(2, 3, 1:2, repeats = TRUE)) # from pkg: gtools
y[y == 2] <- 2:13
y[y == 1] <- NA
y

x <- big.matrix(nrow = 3, ncol = 2^3, init = 0, type = "integer")
x[,] <- as.vector(y)
# describe(x)
x[,]

# (b) colmin(), colmax() w/ default settings:
#--------------------------------------------
expect_equal(colmin(x), apply(y, 2, min))
expect_equal(colmax(x), apply(y, 2, max))

# Look at the actual output:
foo <- data.frame(t(y), big.pkg = colmin(x), r.default = apply(y, 2, min))
foo

foo <- data.frame(t(y), big.pkg = colmax(x), r.default = apply(y, 2, max))
foo

# (c) colmin(), colmax() w/ na.rm = TRUE:
#----------------------------------------
expect_equal(colmin(x, na.rm = TRUE), apply(y, 2, min, na.rm = TRUE))
# Look at the actual output:
foo <- data.frame(t(y), big.pkg = colmin(x, na.rm = TRUE),
                  r.default = apply(y, 2, min, na.rm = TRUE))
foo # First row.

# expect_equal(colmax(x, na.rm = TRUE), apply(y, 2, max, na.rm = TRUE))
foo <- data.frame(t(y), big.pkg = colmax(x, na.rm = TRUE),
                  r.default = apply(y, 2, max, na.rm = TRUE))
foo


