#  File R/zzz.R in package rle, currently hosted at https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3 or later.
#  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
#######################################################################
#' @useDynLib rle

.onUnload <- function(libpath){
  library.dynam.unload("rle",libpath)
}
