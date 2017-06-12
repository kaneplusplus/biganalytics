# # library(gtools) # for function: permutations()
# # library(devtools)
# # library(testthat)
# 
# devtools::load_all()


# A. Generate test data with Number, Inf, -Inf, NA and NaN:
#----------------------------------------------------------
y <- t(permutations(5, 5, 1:5, repeats = TRUE)) # ncol = 5^5, nrow = 5, these dim for good structure of big.matrix:x

# Fill numbers first: No worries about is.na() etc.
# 5^5 = 3125 (5^4: Perm of (NA, Inf, -Inf, NaN) w/ replacement, then 5 ways to place a # b/w them)
# mean = 100, b/c mean = 0 with set.seed(06511) gave two 1s!
set.seed(06511)
y[y == 4] <- round(rnorm(5^5, mean = 100), 3)
set.seed(NULL)
for (i in 1:5) {
  print(sum(y == i))
}
y[y == 1] <- NA
y[y == 2] <- 0/0
y[y == 3] <- Inf
y[y == 5] <- -Inf

# head(t(y)); tail(t(y))
x <- big.matrix(nrow = 5, ncol = 5^5, init = 0)
x[,] <- as.vector(y)


# B. colmin() with default settings:
#-----------------------------------
expect_equal(colmin(x), apply(y, 2, min))

# Look at the actual output:
foo <- data.frame(t(y), big.pkg = colmin(x), r.default = apply(y, 2, min))
foo[1:150, ]
foo[151:256, ]

# C. colmin() with na.rm = TRUE:
#-----------------------------
expect_equal(colmin(x, na.rm = TRUE), apply(y, 2, min, na.rm = TRUE))
warnings()
# Two issues:
# 1. warnings() from min(): When input vector empty, gives warning and +Inf (NOTE: for min o/p +Inf)
# 2. difference in output of colmin() and min()

# C.1. warnings() from min() with na.rm = TRUE:
#---------------------------------------------
# is.na() is TRUE for both NA and NaN
# is.nan() is TRUE only for NaN
min(rep(NA, 3), na.rm = TRUE)
min(c(NA, NA, 24), na.rm = TRUE)
min(c(NA, -Inf, 24), na.rm = TRUE)
min(c(NA, 0/0, 24), na.rm = TRUE)
min(c(NA, Inf, 24), na.rm = TRUE)
min(rnorm(5), Inf)
min(rnorm(5), -Inf)

z.vec <- c(NA, NaN)
z <- t(expand.grid(z.vec, z.vec, z.vec, z.vec, z.vec)) # t() to make dim equivalent to y. dim(z):5x32 (=160)
t(z)
sum(is.na(z)) # 160 = (5x32) all elements of z give TRUE to is.na()

apply(z, 2, min)
apply(z, 2, min, na.rm = TRUE) # All 32 warnings
z2 <- cbind(t(z), narm.false = apply(z, 2, min), narm.true = apply(z, 2, min, na.rm = TRUE))
z2

# C.2. difference in output of colmin() and min():
#--------------------------------------------------
foo <- data.frame(t(y), big.pkg = colmin(x, na.rm = TRUE), r.default = apply(y, 2, min, na.rm = TRUE))
sum(is.na(foo$r.default))
sum(is.na(foo$big.pkg))

check.index <- which(is.na(foo$big.pkg))
foo[check.index, ]


# expect_equal(colmax(x, na.rm = TRUE), apply(y, 2, max, na.rm = TRUE))
foo <- data.frame(t(y), big.pkg = colmax(x), r.default = apply(y, 2, max))
foo <- data.frame(t(y), big.pkg = colmax(x, na.rm = TRUE), r.default = apply(y, 2, max, na.rm = TRUE))

foo[1:150, ]
foo[151:300, ]
foo[301:450, ]
foo[451:600, ]
# foo[1:150, ]
# foo[151:900, ]
# foo[1:150, ]
# foo[151:300, ]


# # Error b/c didn't fix colmax
expect_equal(colmax(x, na.rm = TRUE), apply(y, 2, max, na.rm = TRUE))
big.pkg = colmax(x, na.rm = TRUE)
r.default = apply(y, 2, max, na.rm = TRUE)
foo <- data.frame(t(y), big.pkg, r.default)
foo[1:150, ]

# expect_equal(colmax(x), apply(y, 2, max)) 
foo <- data.frame(t(y), big.pkg = colmax(x), r.default = apply(y, 2, max))
foo[1:150, ]
foo[151:256, ]

# # Error b/c didn't fix colmax
# expect_equal(unname(colrange(x)), t(apply(y, 2, range)))
expect_equal(unname(colrange(x))[,1], t(apply(y, 2, range))[,1])

expect_equal(colsum(x), apply(y, 2, sum))
expect_equal(colmean(x), apply(y, 2, mean))
expect_equal(colprod(x), apply(y, 2, prod))
expect_equal(colsd(x), apply(y, 2, sd))





# # NOTE: behavior of na.rm stuff should be same as for following y:
# y <- t(permutations(4, 4, 1:4, repeats = TRUE))
# set.seed(06511)
# y[y == 4] <- round(rnorm(256), 3)
# set.seed(NULL)
# y[y == 3] <- Inf
# y[y == 2] <- -Inf
# y[y == 1] <- 0/0
# # head(t(y)); tail(t(y))



# # Following test for double only (for now)
# # context("NA, Inf, -Inf handling")
# # test_that("biganalytics behavior with NA, Inf and -Inf same as default R-functions", {
# #   
# #   
# # })






