binit <- function(x, cols, breaks=10)
  {
    if (!is.matrix(x) && !is.big.matrix(x))
      stop("Error in binit: x must be a matrix or a big.matrix.")
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    if (length(cols)<1 || length(cols)>2) {
      stop("Error in binit: only 1 or 2 columns is supported.")
    }
    if ( !is.list(breaks) && (length(breaks)==1 || length(breaks)==2) ) {
      if (is.numeric(breaks)) {
        usebreaks <- breaks
      } else { stop("Error in binit: breaks must be numeric.\n") }
      if (length(cols)==1) {
        if (!is.big.matrix(x)) {
          breaks <- c(min(x[,cols], na.rm=TRUE),
                      max(x[,cols], na.rm=TRUE), usebreaks[1])
        } else {
          breaks <- c(colmin(x, cols, na.rm=TRUE),
                      colmax(x, cols, na.rm=TRUE), usebreaks[1])
        }
      }
      if (length(cols)==2) {
        if (length(usebreaks)==1) usebreaks <- c(usebreaks, usebreaks)
        if (is.big.matrix(x)) {
          mins <- colmin(x, cols, na.rm=TRUE)
          maxs <- colmax(x, cols, na.rm=TRUE)
        } else {
          mins <- apply(x[,cols], 2, min, na.rm=TRUE)
          maxs <- apply(x[,cols], 2, max, na.rm=TRUE)
        }
        breaks <- list(c(mins[1], maxs[1], usebreaks[1]),
                       c(mins[2], maxs[2], usebreaks[2]))
      }
    }
    if (!is.list(breaks) && length(breaks)!=3)
      stop("Error in binit: incorrect specification of breaks.")
    if (is.list(breaks) & (length(breaks)!=2 ||
                           length(breaks[[1]])!=3 ||
                           length(breaks[[2]])!=3))
      stop("Error in binit: serious breaks problem.")

    if (is.list(breaks)) {
      if (is.big.matrix(x)) {
        ret = .Call("binit2BigMatrix", x@address,
          as.double(cols), as.double(breaks[[1]]), as.double(breaks[[2]]))
      } else {
        if (is.integer(x)) {
          ret = .Call("binit2RIntMatrix", x,
            as.double(cols), as.double(breaks[[1]]), as.double(breaks[[2]]))
        } else {
          ret = .Call("binit2RNumericMatrix", x,
            as.double(cols), as.double(breaks[[1]]), as.double(breaks[[2]]))
        }
      }
      ret <- matrix(ret, breaks[[1]][3], breaks[[2]][3])
      rb <- seq(breaks[[1]][1], breaks[[1]][2], length.out=breaks[[1]][3]+1)
      rnames <- (rb[-length(rb)] + rb[-1]) / 2
      cb <- seq(breaks[[2]][1], breaks[[2]][2], length.out=breaks[[2]][3]+1)
      cnames <- (cb[-length(cb)] + cb[-1]) / 2
      ret <- list(counts=ret, rowcenters=rnames, colcenters=cnames,
                  rowbreaks=rb, colbreaks=cb)
    } else {
      if (is.big.matrix(x)) {
        ret = .Call("binit1BigMatrix", x@address,
          as.double(cols), as.double(breaks))
      } else {
        if (is.integer(x)) {
          ret = .Call("binit1RIntMatrix", x,
            as.double(cols), as.double(breaks))
        } else {
          ret = .Call("binit1RNumericMatrix", x,
            as.double(cols), as.double(breaks))
        }
      }
      b <- seq(breaks[1], breaks[2], length.out=breaks[3]+1)
      rnames <- (b[-length(b)] + b[-1]) / 2
      ret <- list(counts=ret, centers=rnames, breaks=b)
    }

    return(ret)
  }


