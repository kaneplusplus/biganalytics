\name{biglm.big.matrix, bigglm.big.matrix}
\alias{biglm.big.matrix}
\alias{bigglm.big.matrix}
\title{Use Thomas Lumley's ``biglm'' package with a ``big.matrix''}
\description{
This is a wrapper to Thomas Lumley's \code{\link[biglm]{biglm}}
package, allowing it to be used with massive data stored in
\code{\link[bigmemory]{big.matrix}} objects.
}
\usage{
biglm.big.matrix( formula, data, chunksize=NULL, ..., fc=NULL,
  getNextChunkFunc=NULL)
bigglm.big.matrix( formula, data, chunksize=NULL, ..., fc=NULL,
  getNextChunkFunc=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a model \code{\link{formula}}.}
  \item{data}{a \code{\link[bigmemory]{big.matrix}}.}
  \item{chunksize}{an integer maximum size of chunks of data to process 
                   iteratively.}
  \item{fc}{either column indices or names of variables that are factors.}
  \item{\dots}{options associated with the \code{\link[biglm]{biglm}}}
    or \code{\link[biglm]{bigglm}} functions
  \item{getNextChunkFunc}{a function which retrieves chunk data}
}
\details{
See \pkg{biglm} package for more information;  
\code{chunksize} defaults to\cr
\code{max(floor(nrow(data)/ncol(data)^2), 10000)}.
}
\value{
an object of class \code{biglm}.
}
\references{Algorithm AS274 Applied Statistics (1992) Vol. 41, No.2

Thomas Lumley (2005). biglm: bounded memory linear and generalized
  linear models. R package version 0.4.
}
%\author{Michael J. Kane}
\seealso{\code{\link[biglm]{biglm}}, \code{\link[bigmemory]{big.matrix}}}
\examples{
# This example is quite silly, using the iris
# data.  But it shows that our wrapper to Lumley's biglm() function
# produces the same answer as the plain old lm() function.

\dontrun{
require(bigmemory)
x <- matrix(unlist(iris), ncol=5)
colnames(x) <- names(iris)
x <- as.big.matrix(x)
head(x)

silly.biglm <- biglm.big.matrix(Sepal.Length ~ Sepal.Width + Species,
                                data=x, fc="Species")
summary(silly.biglm)

y <- data.frame(x[,])
y$Species <- as.factor(y$Species)
head(y)

silly.lm <- lm(Sepal.Length ~ Sepal.Width + Species, data=y)
summary(silly.lm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\keyword{classes}
