#  File R/rle-deprecated.R in package rle, currently hosted at https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3 or later.
#  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
#######################################################################
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
