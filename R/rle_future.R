#  File R/rle_future.R in package rle, currently hosted at https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3 or later.
#  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
#######################################################################
#' @name Extract.rle
#'
#' @title Indexing Methods for `rle` Objects
#'
#' @description These methods are defined and produce an error (except
#'   for [`character`] input) to future-proof code that depends on the
#'   `rle` package by preventing their use.
#'
#' @details At this time, the `rle` package does not support indexing
#'   operations by [`logical`] or [`numeric`] indices, but it is
#'   likely to do so in the future. Therefore, we reserve the syntax
#'   now to prevent users of this package from relying on the default
#'   behaviour of the indexing operators.
#'
#' @param x,i,name,value,... Arguments to indexing operators. See
#'   [Extract] documentation in the `base` package.
#'
#' @return At this time, all functions raise an error except for
#'   [`character`] indices. This behaviour can be overridden by
#'   setting `options(rle.unclass_index=TRUE)`, which effectively
#'   [`unclass`]es the objects before indexing.
#'
#' @examples
#'
#' # Indexing by character or by $ works, including sub-indexing.
#' x <- rle(1:5)
#' x[["values"]] <- 2:6
#' x
#' x$values[2:3] <- 7:8
#' x
#'
#'
#' \dontrun{
#' # Numerical indexing doesn't, unless `options(rle.unclass_index=TRUE)` is set.
#' x[1]
#' x[[1]]
#' }
NULL

IDXERR <- paste("Indexing of",sQuote("rle"),"objects by numeric or logical indexes is not implemented at this time.")

#' @rdname Extract.rle
#' @export
`[.rle` <- function(x, i, ...){
  if(is.character(i) || (!is.null(rle_unclass_index <- getOption("rle.unclass_index")) && rle_unclass_index)) NextMethod()
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`[<-.rle` <- function(x, i, ..., value){
  if(is.character(i) || (!is.null(rle_unclass_index <- getOption("rle.unclass_index")) && rle_unclass_index)) NextMethod()
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`[[.rle` <- function(x, i, ...){
  if(is.character(i) || (!is.null(rle_unclass_index <- getOption("rle.unclass_index")) && rle_unclass_index)) NextMethod()
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`[[<-.rle` <- function(x, i, ..., value){
  if(is.character(i) || (!is.null(rle_unclass_index <- getOption("rle.unclass_index")) && rle_unclass_index)) NextMethod()
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`$.rle` <- function(x, name){
  NextMethod()
}

#' @rdname Extract.rle
#' @export
`$<-.rle` <- function(x, name, value){
  NextMethod()
}
