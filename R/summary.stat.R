
setGeneric('colmin', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colmin'))

setMethod('colmin', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    thistype = bigmemory:::getCType(x)
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    #if (is.null(cols)) cols = 1:ncol(x)
    #if (is.character(cols)) cols <- mmap(cols, colnames(x))
    ret = .Call("CMinColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("min", signature="big.matrix",
  function(x, ..., na.rm=FALSE) {
    return(min(colmin(x, ..., na.rm=na.rm)))
  })

setGeneric('colmax', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colmax'))

# TODO: Can this be optimized to go through a set of rows only once?
setMethod('colmax', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    thistype = bigmemory:::getCType(x)
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    ret = .Call("CMaxColmain", as.integer(thistype), 
      x@address, as.double(cols), na.rm)

    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("max", signature="big.matrix",
  function(x, ..., na.rm=FALSE)
  {
		return(max(colmax(x, ..., na.rm=na.rm)))
  })

setGeneric('colprod', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colprod'))

# TODO: Can this be optimized to go through a set of rows only once?
setMethod('colprod', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    #if (is.null(cols)) cols = 1:ncol(x)
    #if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = bigmemory:::getCType(x)
    ret = .Call("CProdColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("prod", signature="big.matrix",
  function(x, ..., na.rm=FALSE) {
    return(prod(colprod(x, ..., na.rm=na.rm)))
  })

setGeneric('colsum', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colsum'))

setMethod('colsum', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    thistype = bigmemory:::getCType(x)
    ret = .Call("CSumColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("sum", signature="big.matrix",
  function(x, ..., na.rm=FALSE) {
    return(sum(colsum(x, ..., na.rm=na.rm)))
  })


setGeneric('colrange', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colrange'))

setMethod('colrange', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    ret = matrix(c(colmin(x,cols=cols,na.rm=na.rm), 
      colmax(x,cols=cols,na.rm=na.rm)), ncol=2)
    colnames(ret) = c('min', 'max')
    if (!is.null(colnames(x))) rownames(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("range", signature="big.matrix",
  function(x, ..., na.rm=FALSE)
  {
    rangeMat = colrange(x, ..., na.rm=na.rm)
    return(c(min(rangeMat[,1]), max(rangeMat[,2])))
  })

setGeneric('colmean', function(x, cols=NULL, na.rm=FALSE) 
  standardGeneric('colmean'))

setMethod('colmean', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) 
  {
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    #if (is.null(cols)) cols=1:ncol(x)
    #if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = bigmemory:::getCType(x)
    ret = .Call("CMeanColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod('mean', signature(x="big.matrix"),
  function(x, ...)
  {
    return(mean(colmean(x, ...)))
  })

setGeneric('colvar', function(x, cols=NULL, na.rm=FALSE) 
  standardGeneric('colvar'))

setMethod('colvar', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE)
  {
    if (!is.big.matrix(x)) stop("Unknown type.")
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    #if (is.null(cols)) cols = 1:ncol(x)
    #if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = bigmemory:::getCType(x)
    ret = .Call("CVarColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setGeneric('colsd', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colsd'))

setMethod('colsd', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE)
  {
    return(sqrt(colvar(x, cols=cols, na.rm=na.rm)))
  })

setGeneric('colna', function(x, cols=NULL) standardGeneric('colna'))

setMethod('colna', signature(x='big.matrix'),
  function(x, cols=NULL)
  {
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    #if (is.null(cols)) cols = 1:ncol(x)
    #if (is.character(cols)) cols <- mmap(cols, colnames(x))
    cols = as.double(cols)
    if (max(cols) > ncol(x) | min(cols) < 1) stop("Invalid columns")
    ret = c()
    for (col in cols) ret = c(ret, .Call('ColCountNA', x@address, col))
    if (!is.null(colnames(x))) names(ret) = colnames(x)[cols]
    return(ret)
  })

#setMethod('summary',
#  signature(object='big.matrix'),
#  function(object)
#  {
#    rows = 1:ncol(object)
#    cn = c('min', 'max', 'mean', "NAs")
#    s = matrix(NA, ncol = length(cn), nrow = length(rows))
#    colnames(s) = cn
#    rownames(s) = colnames(object)
#    s[,'min'] = colmin(object, rows, na.rm=TRUE)
#    s[,'max'] = colmax(object, rows, na.rm=TRUE)
#    s[,'mean'] = colmean(object, rows, na.rm=TRUE)
#    s[,"NAs"] = colna(object, rows)
#    tab=as.table(s)
#    return(tab)
#  })

setMethod('summary',
  signature(object='big.matrix'),
  function(object)
  {
    rows <- 1:ncol(object)
    cn <- c('min', 'max', 'mean', 'NAs')
    s <- matrix(NA, ncol = length(cn), nrow = length(rows))
    colnames(s) <- cn
    rownames(s) <- colnames(object)
    for (i in rows) {
      s[i,'min'] <- colmin(object, i, na.rm=TRUE)
      s[i,'max'] <- colmax(object, i, na.rm=TRUE)
      s[i,'mean'] <- colmean(object, i, na.rm=TRUE)
      s[i,'NAs'] <- colna(object, i)
    }
    tab <- as.table(s)
    return(tab)
  })



