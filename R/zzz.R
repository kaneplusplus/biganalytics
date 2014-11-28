
.onLoad <- function(libname, pkgname) {
    library.dynam("biganalytics", pkgname, libname)
}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
    library.dynam.unload("biganalytics", libpath)
}
