#  File R/AAA.R in package rle, currently hosted at https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3 or later.
#  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
#######################################################################
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
#' * It is possible to use the indexing (`[` and `[[`) operators to
#'   extract by positive numeric indices but not by logical or
#'   negative numeric indices, and the implementation is far from
#'   optimal. It is not possible to replace individual elements of the
#'   vector represented by an `rle` object. See [Extract.rle] for more
#'   details.
#'
#' * Method [`rep.rle`] currently has limited functionality.
"_PACKAGE"
