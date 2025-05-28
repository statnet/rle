#  File R/extract.R in package rle, currently hosted at
#  https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3
#  or later. A copy of this license may be found at
#  https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2025 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
################################################################################
#' @name Extract.rle
#'
#' @title Indexing Methods for `rle` Objects
#'
#' @description These methods provide indexing functionality for
#'   [`rle`] objects on the scale of the original scale (the elements
#'   of the vector that was compressed) where possible.
#'
#' @note Some of these methods and inputs produce an error in
#'   order to future-proof code that depends on the `rle` package by
#'   preventing their use.
#'
#' @details At this time, the \pkg{rle} following form of indexing are
#'   supported:
#'
#' \tabular{lll}{
#' \strong{operation} \tab \strong{index} \tab \strong{effect} \cr
#' `[` \tab `numeric >= 0` \tab as vector \cr
#' `[` \tab `numeric < 0` \tab no \cr
#' `[` \tab `logical` \tab no \cr
#' `[` \tab `character` \tab on rle \cr
#' `[<-` \tab `numeric >= 0` \tab no \cr
#' `[<-` \tab `numeric < 0` \tab no \cr
#' `[<-` \tab `logical` \tab no \cr
#' `[<-` \tab `character` \tab on rle \cr
#' `[[` \tab `numeric` \tab as vector \cr
#' `[[<-` \tab `numeric` \tab no \cr
#' `[[` \tab `character` \tab on `rle` \cr
#' `[[<-` \tab `character` \tab on `rle` \cr
#' `$` \tab `character` \tab on `rle` \cr
#' `$<-` \tab `character` \tab on `rle`
#' }
#'
#' Generally, character indexes will access the underlying elements of
#' the [`rle`] object, `$lengths` and `$values`.
#'
#' @param x,i,name,value,... Arguments to indexing operators. See
#'   [Extract] documentation in the \pkg{base} package.
#' @param unclass Logical: whether to process the arguments as if for
#'   an ordinary list; default other than `FALSE` can be set with
#'   `options(rle.unclass_index=...)`.
#'
#' @return For character indices, the corresponding sublists or
#'   elements of the `rle` object; for numeric indices, for `[[` the
#'   element at the specified position and for `[` an `rle` containing the
#'   elements at the specified position(s).
#'
#' @seealso [index_to_run()]
#' @examples
#'
#' # Indexing by character or by $ works, including sub-indexing.
#' x <- rle(1:5)
#' x[["values"]] <- 2:6
#' x
#' x$values[2:3] <- 7:8
#' x
#'
#' # From example(rle):
#' z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
#' rle(z)
#' rle(z)[3:5] # Extract a sub-rle
#' rle(z)[[4]] # Extract an element
#'
#' stopifnot(identical(inverse.rle(rle(z)[3:5]), z[3:5]))
#' # Fractional:
#' stopifnot(identical(inverse.rle(rle(z)[3.5]), z[3.5]))
#' # Zero:
#' stopifnot(identical(inverse.rle(rle(z)[0]), z[0]))
#' # Out of range:
#' stopifnot(identical(inverse.rle(rle(z)[20]), z[20]))
#' # A mix:
#' strange <- c(20, 3:5, 0, NA, 1:2)
#' stopifnot(identical(inverse.rle(rle(z)[strange]), z[strange]))
NULL

IDXERR <- paste("Indexing of", sQuote("rle"), "objects by logical or negative",
                "numeric indexes is not implemented at this time.")

# TODO: This is built into R 4.4, so delete around mid 2026.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @rdname Extract.rle
#' @export
`[.rle` <- function(x, i, ...,
                    unclass = getOption("rle.unclass_index") %||% FALSE) {
  if (is.character(i) || unclass) NextMethod()
  # TODO: This can almost certainly be optimised.
  else if (is.numeric(i) && all(i >= 0L, na.rm = TRUE))
    rle(x$values[index_to_run(x, i)])
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`[<-.rle` <- function(x, i, ...,
                      unclass = getOption("rle.unclass_index") %||% FALSE,
                      value) {
  if (is.character(i) || unclass) NextMethod()
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`[[.rle` <- function(x, i, ...,
                     unclass = getOption("rle.unclass_index") %||% FALSE) {
  if (is.character(i) || unclass) NextMethod()
  else if (is.numeric(i)) x$values[[index_to_run(x, i)]]
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`[[<-.rle` <- function(x, i, ...,
                       unclass = getOption("rle.unclass_index") %||% FALSE,
                       value) {
  if (is.character(i) || unclass) NextMethod()
  else stop(IDXERR)
}

#' @rdname Extract.rle
#' @export
`$.rle` <- function(x, name) {
  NextMethod()
}

#' @rdname Extract.rle
#' @export
`$<-.rle` <- function(x, name, value) {
  NextMethod()
}
