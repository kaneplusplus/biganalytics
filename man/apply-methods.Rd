\name{apply-methods}
\docType{methods}
\alias{apply-method}
\alias{apply,big.matrix-method}
\title{apply() for big.matrix objects.}
\description{
\code{\link{apply}} for \code{\link[bigmemory]{big.matrix}} objects.  Note that
the performance may be degraded (compared to \code{apply} with regular \R
matrices) because of S4 overhead associated with extracting data from
\code{big.matrix} objects.  This sort of limitation is unavoidable and would
be the case (or even worse) with other "custom" data structures.  Of course,
this would only be partically significant if you are applying over lengthy
rows or columns.
}
\section{Methods}{
\describe{
\item{apply}{\code{signature(x = "big.matrix")}: \code{apply()} where \code{MARGIN} may only be 1 or 2, but otherwise conforming to what you would expect from \code{apply()}.}
}}
\examples{
library(bigmemory)

x <- big.matrix(5, 2, type="integer", init=0, 
                dimnames=list(NULL, c("alpha", "beta")))
x[,] <- round(rnorm(10))
apply(x, 1, mean)
}
\keyword{methods}
