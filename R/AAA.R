#' @name rle-package
#' @title The `rle` Package
#' @description A package implementing common [`base`] and [`stats`]
#'   methods for [`rle`] objects, aiming to make it possible to treat
#'   them transparently as vectors.
#'
#' @section History:
#'
#' This package grew out of the needs of the `ergm` package for a
#' run-length encoded representation of extremely long vectors with a
#' small number of contiguous runs and these functions were originally
#' implemented in the `statnet.common` package.
#'
#' It has been split out into its own package to enable others to use
#' this functionality without installing any unnecessary dependencies
#' and to facilitate contributions under a simplified license.
#'
#' @section What works and what doesn't:
#'
#' The long-run aim of this package is to make it possible to treat
#' [`rle`] objects transparently as nameless vectors. As of this
#' writing, the biggest unimplemented feature is the indexing (`[`)
#' operator. It is not possible to extract or replace elements of an
#' `rle` object.
NULL
