#' @useDynLib rle

.onUnload <- function(libpath){
  library.dynam.unload("rle",libpath)
}
