
bigkmeans <- function(x, centers, iter.max = 10, nstart = 1) {

  require(foreach)
  if (is.null(getDoParName())) {
    registerDoSEQ() # A little hack to avoid the foreach warning 1st time.
  }

  ################################################################
  # This function is used to construct a list of length nstart
  # of centers so that there are no duplicates.  If this dies,
  # the user probably shouldn't be using k-means.
  getcenters <- function(x, k, nstart) {
    n <- nrow(x)
    centers <- list(x[sample(1:n, k),,drop=FALSE])
    nchecks <- 1000
    if (k<=10) nchecks <- 10 + 2^k
    for (ii in 1:nchecks) {
      if (any(duplicated(centers[[length(centers)]]))) {
        centers[[length(centers)]] <- x[sample(1:n, k),,drop=FALSE]
      } else break;
    }
    if (any(duplicated(centers[[length(centers)]]))) {
      stop("Having trouble finding non-duplicated centers.\n")
    }
    if (nstart>1) {
      for (i in 2:nstart) {
        centers[[length(centers)+1]] <- x[sample(1:n, k),,drop=FALSE]
        for (ii in 1:nchecks) {
          if (any(duplicated(centers[[length(centers)]]))) {
            centers[[length(centers)]] <- x[sample(1:n, k),,drop=FALSE]
          } else break;
        }
        if (any(duplicated(centers[[length(centers)]]))) {
          stop("Having trouble finding non-duplicated centers.\n")
        }
      }
    }
    return(centers)
  }

  ####################################################################
  # A function for aggregating results as foreach is running, to avoid
  # memory overhead.
  choosebest <- function(a, b) {
    if ( sum(a$withinss) < sum(b$withinss) ) {
      return(a)
    } else {
      return(b)
    }
  }
    
  #################################################
  # Check centers for sanity and consider nstart>1:
  if (!is.matrix(centers)) {
    if (is.numeric(centers) && length(centers)==1 && centers>0) {
      k <- centers
      centers <- getcenters(x, k, nstart)
    } else stop("centers must be a matrix of centers or number of clusters > 0")
  } else {
    k <- nrow(centers)
    if (nstart>1) {
      warning(paste("Random starting points will be used",
                    "(not the centers you provided), because nstart>1.\n"))
      centers <- getcenters(x, k, nstart)
    } else {
      if (any(duplicated(centers))) {
        stop(paste("Error: if you provide centers,",
                   "they had better not have duplicates.\n"))
      }
      centers <- list(centers)
    }
  }

  ###############################################################
  # At this point, centers is a list of length nstart of matrices
  # of starting centers without duplicates.
  # I think I allow k=1 cluster, too, but check it later.
  # Note that if number of columns is HUGE, the centers will
  # be memory-intensive.

  if (is.matrix(x)) {
    if (getDoParWorkers()>1) {
      # Generate big.matrix copy so we can work in parallel; we
      # assume in-memory is fine given that x is a matrix.
      if (is.integer(x)) {
        y <- as.big.matrix(x, type="integer")
      } else {
        y <- as.big.matrix(x, type="double")
      }
      xdesc <- describe(y)
    } else { xdesc <- NULL }       # This is the signal of a matrix.
  } else {
    if (is.big.matrix(x)) {
      xdesc <- describe(x)
    } else {
      stop("x must be a matrix or a big.matrix.\n")
    }
  }

  nr <- nrow(x)
  if (typeof(x)=="char") mattype <- 1
  if (typeof(x)=="short") mattype <- 2
  if (typeof(x)=="integer") mattype <- 4
  if (typeof(x)=="double") mattype <- 8

  cen <- NA # Note this is because of foreach's interaction with the
            # R check parser.  There is no real problem, but I need this
            # to avoid a warning.

  # Do the work, possibly in parallel with nstart>1 and a registered
  # parallel backend.
  ans <- foreach(cen=centers, .combine="choosebest") %dopar% {

    # Note that at this point, we're on a worker; I use a local big.matrix
    # object for centers to make the C++ code easier [][] matrix notation.
    require(bigmemory)
    require(biganalytics)
    center <- big.matrix(nrow(cen), ncol(cen), type="double")
    center[,] <- cen
    clust <- big.matrix(nr, 1, type="integer")
    clustsizes <- big.matrix(nrow(cen), 1, type="double")
    wss <- big.matrix(nrow(cen), 1, type="double")
    if (is.null(xdesc)) {
      # .Call with the matrix, which has to be either integer or double.
      if (mattype==4) {
        res <- .Call("kmeansRIntMatrix", x,
                     center@address, clust@address, clustsizes@address,
                     wss@address, as.integer(iter.max))
      } else {
        res <- .Call("kmeansRNumericMatrix", x,
                     center@address, clust@address, clustsizes@address,
                     wss@address, as.integer(iter.max))
      }
    } else {
      # .Call with the big.matrix
      x <- attach.big.matrix(xdesc)
      res <- .Call("kmeansBigMatrix", x@address,
                   center@address, clust@address, clustsizes@address,
                   wss@address, as.integer(iter.max))
    }

    temp <- list(cluster=clust[,],
                 centers=center[,],
                 withinss=wss[,],
                 size=clustsizes[,],
                 iters=res)

    return(temp)

  } # End of the foreach() body.

  if (ans$iters>=iter.max) 
    warning("bigkmeans did not converge in ", iter.max, " iterations.\n")
  ans$iters <- NULL                     # This removes this from the list.
  class(ans) <- "kmeans"

  return(ans)

}


