\name{colmean, colmin, min, colrange, colvar,  colsd, colprod, colsum, colna, etc...}
\alias{colmean}
\alias{colmean,big.matrix-method}
\alias{colmin}
\alias{colmin,big.matrix-method}
\alias{min,big.matrix-method}
\alias{colmax}
\alias{colmax,big.matrix-method}
\alias{max,big.matrix-method}
\alias{colrange}
\alias{colrange,big.matrix-method}
\alias{range,big.matrix-method}
\alias{colvar}
\alias{colvar,big.matrix-method}
\alias{colsd}
\alias{colsd,big.matrix-method}
\alias{colprod}
\alias{colprod,big.matrix-method}
\alias{prod,big.matrix-method}
\alias{colsum}
\alias{colsum,big.matrix-method}
\alias{sum,big.matrix-method}
\alias{colna}
\alias{colna,big.matrix-method}
\alias{summary}
\alias{summary,big.matrix-method}
\alias{mean}
\alias{mean,big.matrix-method}
\title{Basic statistics for ``big.matrix'' objects.}
\description{
These functions operate on columns of a 
\code{\link[bigmemory]{big.matrix}} object.
}
\usage{
colmean(x, cols, na.rm)
colmin(x, cols, na.rm)
colmax(x, cols, na.rm)
colrange(x, cols, na.rm)
colvar(x, cols, na.rm)
colsd(x, cols, na.rm)
colsum(x, cols, na.rm)
colprod(x, cols, na.rm)
colna(x, cols)
}
\arguments{
  \item{x}{a \code{\link[bigmemory]{big.matrix}} object.}
  \item{cols}{a scalar or vector of column(s) to be summarized.}
  \item{na.rm}{if \code{TRUE}, remove \code{NA} values before summarizing.}
  \item{\dots}{options associated with the correspoding default \R function}
}
\details{
These functions essentially apply summary functions to each column (or
each specified column) of the 
\code{\link[bigmemory]{big.matrix}} in turn.
}
\value{
For \code{colrange}, a matrix with two columns and \code{length(cols)}
rows; column 1 contains the minimum, and column 2 contains the maximum for
that column.  The other functions return vectors of length \code{length(cols)}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{bigmemory}}
\examples{
x <- as.big.matrix(
  matrix( sample(1:10, 20, replace=TRUE), 5, 4, 
          dimnames=list( NULL, c("a", "b", "c", "d")) ) )
x[,]
mean(x)
colmean(x)
colmin(x)
colmin(x, 1)
colmax(x)
colmax(x, "b")
colsd(x)
colrange(x)
range(x)
colsum(x)
colprod(x)
}
\keyword{methods}
