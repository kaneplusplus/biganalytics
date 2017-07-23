library(gtools) # for function: permutations()
library(devtools)
library(testthat)

devtools::load_all()
rm(list = ls())

# (a) Generate test data with Number, Inf, -Inf, NA and NaN:
# Cases to test: 5^5 = 3125: Perm of (NA, Inf, -Inf, NaN and #) w/ replacement,
#----------------------------------------------------------
y <- t(permutations(5, 5, 1:5, repeats = TRUE)) # from pkg: gtools

# Fill numbers first: No worries about is.na() etc.
# mean = 100, b/c mean = 0 with set.seed(06511) gave two 1s!
set.seed(06511)
y[y == 4] <- round(rnorm(5^5, mean = 100), 3)
set.seed(NULL)
for (i in 1:5) {
  print(sum(y == i))
}
y[y == 1] <- NA
y[y == 2] <- 0/0 # NaN
y[y == 3] <- Inf
y[y == 5] <- -Inf
# head(t(y)); tail(t(y))

# NOTE: following has bad nrow, ncol choices but have to test 5^5 cases. 
#   And the functions work column-wise and so these dimentions.
x <- big.matrix(nrow = 5, ncol = 5^5, init = 0) 
x[,] <- as.vector(y)

# (b) colmin, colmax with default settings:
#-----------------------------------------
expect_equal(colmin(x), apply(y, 2, min))
expect_equal(colmax(x), apply(y, 2, max))

# # Look at the actual output:
# foo <- data.frame(t(y), big.pkg = colmin(x), r.default = apply(y, 2, min))
# foo[1:25, ]
# foo[2320:2350, ]
# foo[3100:3125, ]
# 
# foo <- data.frame(t(y), big.pkg = colmax(x), r.default = apply(y, 2, max))
# foo[1:25, ]
# foo[2320:2350, ]
# foo[3100:3125, ]

# (c) colmin() with na.rm = TRUE:
#-----------------------------
expect_equal(colmin(x, na.rm = TRUE), apply(y, 2, min, na.rm = TRUE))
warnings()
# NOTE 1: Warnings were issued for cases when empty vector was passed to min (expl below).
# NOTE 2: min() gives +Inf while max() gives -Inf.
#   NA is also a valid o/p but for consistency with R-default it was not used.
# Examples of such cases:
foo <- min(); foo
foo <- min(rep(NA, 5)); foo
foo <- min(rep(NA, 5), na.rm = TRUE); foo
foo <- min(c(rep(NA, 5), 0/0), na.rm = TRUE); foo

# Outputs of min() and colmin() are same except that colmin() doesn't issue a warning:
foo <- apply(y, 2, min, na.rm = TRUE)
expect_equal(colmin(x, na.rm = TRUE), foo)

# # Look at the actual output:
# foo <- data.frame(t(y), big.pkg = colmin(x, na.rm = TRUE),
#                   r.default = apply(y, 2, min, na.rm = TRUE))
# foo[1:25, ]
# foo[2320:2350, ]
# foo[3100:3125, ]

# (d) colmax() with na.rm = TRUE:
#-----------------------------
expect_equal(colmax(x, na.rm = TRUE), apply(y, 2, max, na.rm = TRUE))
warnings()

# Outputs of max() and colmax() are same except that colmax() doesn't issue a warning:
foo <- apply(y, 2, max, na.rm = TRUE)
expect_equal(colmax(x, na.rm = TRUE), foo)

# # Look at the actual output:
# foo <- data.frame(t(y), big.pkg = colmax(x, na.rm = TRUE),
#                   r.default = apply(y, 2, max, na.rm = TRUE))
# foo[1:25, ]
# foo[2320:2350, ]
# foo[3100:3125, ]

# Testing other functions:
#--------------------------
# colrange()
expect_equal(unname(colrange(x)), t(apply(y, 2, range)))

foo <- t(apply(y, 2, range, na.rm = TRUE)) # colrange() doesn't issue warning.
expect_equal(unname(colrange(x, na.rm = TRUE)), foo)

# colsum()
#------------
expect_equal(colsum(x), apply(y, 2, sum))
expect_equal(colsum(x, na.rm = TRUE), apply(y, 2, sum, na.rm = TRUE))

# Look into outputs:
foo <- data.frame(t(y), big.pkg = colsum(x, na.rm = TRUE),
                  r.default = apply(y, 2, sum, na.rm = TRUE))
head(foo) # Notice first two rows

check <- foo[(is.na(foo$big.pkg) & !is.nan(foo$big.pkg)), ]
check
check <- foo[is.na(foo$big.pkg), ]
head(check)

# NOTE 1: Warnings were issued for cases when empty vector was passed to sum (expl below).
# NOTE 2: sum() gives 0 (w/o even a warning!) while colsum() gives NA.
#   Just found this discrepancy. Didn't get a chance to discuss this with Jay.
#   In this case, for me, NA is a much reasonable output than 0 (w/o even a warning).
#   Please let me know your thoughts.
# Examples of such cases:
foo <- sum(); foo
foo <- sum(rep(NA, 5)); foo
foo <- sum(rep(NA, 5), na.rm = TRUE); foo
foo <- sum(c(rep(NA, 5), 0/0), na.rm = TRUE); foo

# colmean()
#------------
expect_equal(colmean(x), apply(y, 2, mean))
expect_equal(colmean(x, na.rm = TRUE), apply(y, 2, mean, na.rm = TRUE))

# colprod()
#------------
expect_equal(colprod(x), apply(y, 2, prod))
expect_equal(colprod(x, na.rm = TRUE), apply(y, 2, prod, na.rm = TRUE))

foo <- data.frame(t(y), big.pkg = colprod(x, na.rm = TRUE),
                  r.default = apply(y, 2, prod, na.rm = TRUE))
head(foo) # First two rows

# NOTE 1: Same issue as with sum
# NOTE 2: prod() gives 1 (w/o even a warning!) while colprod() gives NA.
#   Also, just found this discrepancy. Same comments as on sum().

# colsd()
expect_equal(colsd(x), apply(y, 2, sd))
expect_equal(colsd(x, na.rm = TRUE), apply(y, 2, sd, na.rm = TRUE))
