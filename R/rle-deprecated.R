#' @name rle-deprecated
#' @title Deprecated functions from `rle`
#' @param ... arguments to deprecated functions.
NULL

#' @rdname rle-deprecated
#' @export
compact.rle <- function(...){
  .Deprecated("compress")
  compress(...)
}
