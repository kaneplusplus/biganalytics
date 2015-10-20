\name{biganalytics-package}
\alias{biganalytics-package}
\alias{biganalytics}
\docType{package}
\title{
biganalytics: A library of utilities for big.matrix objects of package bigmemory.
Some work with R matrices, independently of bigmemory.
}
\description{
This package extends the bigmemory package with various analytics.  In addition
to the more obvious summary statistics (see \code{\link{colmean}}, etc...),
\pkg{biganalytics} offers \code{\link{biglm.big.matrix}},
\code{\link{bigglm.big.matrix}}, \code{\link{bigkmeans}},
\code{\link{binit}}, and \code{\link[=apply-method]{apply}} for \code{big.matrix}
objects.  Some of the functions may be used with native \R objects, as well,
providing gains in speed and memory-efficiency.
}
\details{
\tabular{ll}{
Package: \tab biganalytics\cr
Type: \tab Package\cr
Version: \tab 1.1.9\cr
Date: \tab 2015-10-20\cr
License: \tab LGPL-3 | Apache License 2.0\cr
Copyright: \tab (C) 2015 John W. Emerson and Michael J. Kane\cr
URL: \tab http://www.bigmemory.org\cr
LazyLoad: \tab yes\cr
}

The \pkg{bigmemory} package contains the core \code{\link[bigmemory]{big.matrix}}
support; \pkg{biganalytics} contains tools for exploratory data analysis as well as
more advanced analytics on \code{big.matrix} objects.  Sister packages \pkg{synchronicity},
\pkg{bigtabulate}, and \pkg{bigalgebra} provide additional functionality.

}
\author{
John W. Emerson and Michael J. Kane

Maintainer: Michael J. Kane <bigmemoryauthors@gmail.com>
}
\keyword{package}
\references{The Bigmemory Project: \url{http://www.bigmemory.org/}.}
\seealso{For example, see
\code{\link[bigmemory]{big.matrix}}, \code{\link[biglm]{biglm}},
\code{\link{bigkmeans}}, \code{\link{binit}}, \code{\link{colmean}}.
}
\examples{
# Our examples are all trivial in size, rather than burning huge amounts
# of memory simply to demonstrate the package functionality.

library(bigmemory)

x <- big.matrix(5, 2, type="integer", init=0, 
  dimnames=list(NULL, c("alpha", "beta")))
x
x[,]
x[,1] <- 1:5
x[,]
mean(x)
colmean(x)
summary(x)
apply(x, 1, mean)
}
