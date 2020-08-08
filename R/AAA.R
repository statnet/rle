#  File R/AAA.R in package rle, currently hosted at https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3 or later.
#  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
#######################################################################
#' @name rle-package
#' @title The `rle` Package
#' @description Common [`base`] and [`stats`]
#'   methods for [`rle`] objects, aiming to make it possible to treat
#'   them transparently as vectors.
#'
#' @section History:
#'
#' This package grew out of the needs of the `ergm` package for a
#' run-length encoded representation of extremely long vectors with a
#' small number of contiguous runs, and these functions were originally
#' implemented in the `statnet.common` package.
#'
#' It has been split out into its own package to enable others to use
#' this functionality without installing any unnecessary dependencies
#' and to facilitate contributions under a simplified license.
#'
#' @section What works and what doesn't:
#'
#' The long-run aim of this package is to make it possible to treat
#' [`rle`] objects transparently as unnamed vectors. As of this
#' writing, the biggest unimplemented feature are:
#'
#' * The indexing (`[` and `[[`) operators. It is not possible to
#'   extract or replace individual elements of the vector represented
#'   by an `rle` object, though it is possible to access its
#'   underlying representation (i.e., `$lenths` and `$values`) by
#'   name using any of the above operators or the `$` operator.
#' * Method [`rep.rle`] currently has limited functionality.
NULL
