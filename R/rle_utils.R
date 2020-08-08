#  File R/rle_utils.R in package rle, currently hosted at https://github.com/statnet/rle .
#
#  This software is distributed under the GNU General Public License Version 3 or later.
#  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
#
#  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
#######################################################################
.check_lengths <- function(rle1, rle2){
  if(sum(as.numeric(rle1$lengths))!=sum(as.numeric(rle2$lengths)))
    stop("At this time, binary rle operators require the vectors represented by the encoding to have equal lengths.")
}

#' Safe multiplication of integer run lengths.
#'
#' Return a vector of run lengths each no larger than maximum
#' representable integer that sum to the product of the arguments. If
#' the product is 0, an empty integer vector is returned.
#'
#' @param e1,e2 arguments to multiply, both `<=.Machine$integer.max`.
#'
#' @noRd
.run_mul <- function(e1, e2){
  o <- as.numeric(e1)*as.numeric(e2)
  if(o > .Machine$integer.max){ # Integer overflow.
    c(as.integer(rep.int(.Machine$integer.max, o %/% .Machine$integer.max)), as.integer(o %% .Machine$integer.max))
  }else if(o==0){
    integer(0)
  }else as.integer(o)
}

#' @name rle-methods
#'
#' @title Miscellaneous Common Methods for [`rle`] Objects
#'
#' @param x,object An [`rle`] object.
#' @param na.rm Whether missing values are to be ignored (`TRUE`) or propagated (`FALSE`).
#' @param ... For `c`, objects to be concatenated. The first object
#'   must be of class [`rle`].
#'
#' @examples
#' x <- rle(as.logical(rbinom(10,1,.7)))
#' y <- rle(as.logical(rbinom(10,1,.3)))
#'
#' stopifnot(isTRUE(all.equal(c(inverse.rle(x),inverse.rle(y)),inverse.rle(c(x,y)))))
#'
#' @export
c.rle <- function(...){
  l <- list(...)
  l <- lapply(l, as.rle)
  structure(list(
    lengths = do.call(c, lapply(l, `[[`, "lengths")),
    values = do.call(c, lapply(l, `[[`, "values"))
  ), class = "rle")
}

#' Unary and Binary Operations for [`rle`] Objects
#'
#' Unary and binary [Arithmetic] and [Logic] operators (with
#' exceptions given below) are implemented between two [`rle`] objects
#' and between an [`rle`] object and a scalar.
#'
#' @param e1,e2 Arguments to unary (`e1`) and binary (`e1` and `e2`)
#'   operators.
#'
#' @details Supported operations include all elements of the `Ops`
#'   group, as well as [`xor`]. Within the [Arithmetic] and [Logic]
#'   operators, this includes (taken from the R help): `+`, `-`, `*`,
#'   `/`, `^`, `<` , `>`, `<=`, `>=`, `!=`, `==`, `%%`, `%/%`, `&`,
#'   `|`, `!`, and `xor`; but excludes non-vector logical functions
#'   and operators such as [`isTRUE`] and [`&&`].
#'
#' @return In every supported case, the operation should result in an
#'   [`rle`] that would have resulted had the operation been applied
#'   to the original (uncompressed) vectors, then compressed using
#'   [`rle`], with the proviso that if the resulting function creates
#'   adjacent runs of the same value, they are *not* merged. This must
#'   be done explicitly with [`compress.rle`]. (At no point in the
#'   calculation are the uncompressed vectors actually constructed, of
#'   course.)
#'
#'   An operation between an `rle` and a zero-length object produces
#'   an empty `rle`.
#'
#' @examples
#'
#' x <- rle(as.logical(rbinom(10,1,.7)))
#' y <- rle(as.logical(rbinom(10,1,.3)))
#'
#' stopifnot(isTRUE(all.equal((!inverse.rle(x)),inverse.rle(!x))))
#'
#' stopifnot(isTRUE(all.equal((inverse.rle(x)|inverse.rle(y)),inverse.rle(x|y))))
#'
#' stopifnot(isTRUE(all.equal((inverse.rle(x)&inverse.rle(y)),inverse.rle(x&y))))
#'
#' x <- rle(sample(c(-1,+1), 10, c(.7,.3), replace=TRUE))
#' y <- rle(sample(c(-1,+1), 10, c(.3,.7), replace=TRUE))
#'
#' stopifnot(isTRUE(all.equal((inverse.rle(x)*inverse.rle(y)),inverse.rle(x*y))))
#' stopifnot(isTRUE(all.equal((2*inverse.rle(y)),inverse.rle(2*y))))
#' stopifnot(isTRUE(all.equal((inverse.rle(x)*2),inverse.rle(x*2))))
#'
#' stopifnot(isTRUE(all.equal((inverse.rle(x)/inverse.rle(y)),inverse.rle(x/y))))
#' stopifnot(isTRUE(all.equal((2/inverse.rle(y)),inverse.rle(2/y))))
#' stopifnot(isTRUE(all.equal((inverse.rle(x)/2),inverse.rle(x/2))))
#'
#' stopifnot(isTRUE(all.equal((-inverse.rle(y)),inverse.rle(-y))))
#' stopifnot(isTRUE(all.equal((inverse.rle(x)-inverse.rle(y)),inverse.rle(x-y))))
#'
#' stopifnot(isTRUE(all.equal((inverse.rle(x)%/%inverse.rle(y)),inverse.rle(x%/%y))))
#'
#' stopifnot(isTRUE(all.equal(inverse.rle(x)==inverse.rle(y),inverse.rle(x==y))))
#'
#' stopifnot(isTRUE(all.equal((inverse.rle(x)>inverse.rle(y)),inverse.rle(x>y))))
#' @export
Ops.rle <- function(e1, e2){
  FUN <- match.fun(.Generic)
  if(missing(e2)){ # Unary operation
    structure(list(lengths = e1$lengths,
                   values = FUN(e1$values)),
              class = "rle")
  }else if(!nzchar(.Method[1L])){ # e1 is not an rle but e2 is
    l <- length(e1)
    if(l == 0L){
      structure(list(lengths = integer(0),
                     values = FUN(e1, e2$values)),
                class = "rle")
    }else if(l == 1L){
      structure(list(lengths = e2$lengths,
                     values = FUN(e1, e2$values)),
                class = "rle")
    }else{
      stop("Binary operations between a non-scalar and an ", sQuote("rle"), " object are not supported at this time.")
    }
  }else if(!nzchar(.Method[2L])){ # e2 is not an rle but e1 is
    l <- length(e2)
    if(l == 0L){
      structure(list(lengths = integer(0),
                     values = FUN(e1$values, e2)),
                class = "rle")
    }else if(l == 1L){
      structure(list(lengths = e1$lengths,
                     values = FUN(e1$values, e2)),
                class = "rle")
    }else{
      stop("Binary operations between an ", sQuote("rle"), " object and a non-scalar are not supported at this time.")
    }
  }else{ # Both are rle.
    .check_lengths(e1, e2)
    syncinfo <- .Call("sync_RLEs", e1$lengths, e2$lengths)
    structure(list(lengths = syncinfo$lengths[seq_len(syncinfo$nruns)],
                   values = FUN(e1$values[syncinfo$val1i[seq_len(syncinfo$nruns)]],
                                e2$values[syncinfo$val2i[seq_len(syncinfo$nruns)]])),
              class = "rle")
  }
}

#' Mathematical functions for [`rle`] Objects
#'
#' Mathematical functions that work independently elementwise on vectors described in [Math] are implemented for [`rle`] objects. See Details for list of exceptions.
#'
#' @param x An [`rle`] object.
#' @param ... Additional arguments.
#'
#' @details Supported functions include all elements of the S3 [Math]
#'   group excluding the "cumulative" ones, which are not supported at
#'   this time and will raise an error. As of this writing, functions
#'   supported include (from R help) `abs`, `sign`, `sqrt`, `floor`,
#'   `ceiling`, `trunc`, `round`, `signif`, `exp`, `log`, `expm1`,
#'   `log1p`, `cos`, `sin`, `tan`, `cospi`, `sinpi`, `tanpi`, `acos`,
#'   `asin`, `atan`, `cosh`, `sinh`, `tanh`, `acosh`, `asinh`,
#'   `atanh`, `lgamma`, `gamma`, `digamma`, and `trigamma`.
#'
#'   Functions `cumsum`, `cumprod`, `cummax`, and `cummin` are not
#'   supported at this time and will raise an error.
#'
#' @return In every supported case, the call should result in an
#'   [`rle`] that would have resulted had the call been applied to the
#'   original (uncompressed) vector, then compressed using
#'   [`rle`]. (At no point in the calculation is the uncompressed
#'   vector actually constructed, of course.)
#'
#'   By default, the functions do not merge adjacent
#'   runs with the same value. This must be done explicitly with
#'   [`compress.rle`].
#'
#' @examples
#'
#' x <- rle(sample(runif(2), 10, c(.7,.3), replace=TRUE))
#'
#' stopifnot(isTRUE(all.equal(sin(inverse.rle(x)),inverse.rle(sin(x)))))
#' stopifnot(inherits(try(cumprod(x)), "try-error"))
#' @export
Math.rle <- function(x, ...){
  if(.Generic %in% c("cumsum", "cumprod", "cummax", "cummin"))
    stop(sQuote(paste0(.Generic,"()")), " method is not yet implemented for ", sQuote("rle"), " objects.")

  FUN <- match.fun(.Generic)
  structure(list(lengths = x$lengths,
                 values = FUN(x$values, ...)),
            class = "rle")
}

#' Summary methods for [`rle`] objects.
#'
#' Summarisation functions for vectors described in [Summary] are implemented for [`rle`] objects.
#'
#' @param ... [`rle`] objects or objects that can be coerced to `rle`.
#' @param na.rm Whether the missing values should be ignored (`TRUE`) or propagated (`FALSE`).
#'
#' @details Supported functions include all elements of the S3
#'   [Summary] group. As of this writing, functions supported include
#'   (from R help) `all`, `any`, `max`, `min`, `prod`, `range`, and
#'   `sum`.
#'
#' @return In every supported case, the call should produce the same
#'   result as what would have resulted had the call been applied to
#'   the original (uncompressed) vector. (At no point in the
#'   calculation is the uncompressed vector actually constructed, of
#'   course.) The exception is that if `values` are of class
#'   `integer`, the result will nonetheless always be upcast to
#'   `numeric` to avert overflows. This behaviour may change in the
#'   future.
#'
#' @examples
#'
#' x <- rle(as.logical(rbinom(20,1,.7)))
#' y <- rle(as.logical(rbinom(20,1,.3)))
#'
#' stopifnot(isTRUE(all.equal(any(x, y),any(inverse.rle(x), inverse.rle(y)))))
#' stopifnot(isTRUE(all.equal(any(y),any(inverse.rle(y)))))
#'
#' stopifnot(isTRUE(all.equal(sum(inverse.rle(x),inverse.rle(y)),sum(x,y))))
#' stopifnot(isTRUE(all.equal(sum(inverse.rle(y)),sum(y))))
#'
#' y$values[2:3] <- NA
#' stopifnot(isTRUE(all.equal(sum(inverse.rle(y), na.rm=TRUE),sum(y, na.rm=TRUE))))
#' stopifnot(isTRUE(all.equal(sum(inverse.rle(y), na.rm=FALSE),sum(y, na.rm=FALSE))))
#'
#' @export
Summary.rle <- function(..., na.rm){
  FUN <- match.fun(.Generic)

  inl <- list(...)

  # If it's just one, strip the length-zero runs and evaluate.
  if(length(inl) == 1L){
    x <- as.rle(inl[[1L]])
    keep <- x$lengths!=0L
    # TODO: Benchmark whether it's better to first check if
    # any(!keep) or, better yet, write a .Call() function that
    # returns a flag indicating that as a part of calculating keep.
    x$values <- x$values[keep]
    x$lengths <- x$lengths[keep]

    switch(.Generic,
           sum = sum(x$values*as.numeric(x$lengths), na.rm = na.rm),
           prod = prod(x$values^as.numeric(x$lengths), na.rm = na.rm),
           FUN(x$values, na.rm=na.rm)) # The rest only test existence.
  }else{ # Otherwise, break up, evaluate individually, and recombine.
    do.call(FUN, c(lapply(inl, FUN, na.rm=na.rm), na.rm=na.rm))
  }
}

#' A generic function for compressing a data structure.
#'
#' @param x the object to be compressed.
#'
#' @param ... additional arguments to methods.
#'
#' @export
compress <- function(x, ...){
  UseMethod("compress")
}

#' Compress the [`rle`] object by merging adjacent runs
#'
#' @param x an [`rle`] object.
#'
#' @param ... additional objects; if given, all arguments are
#'   concatenated.
#'
#' @note Since [`rle`] stores run lengths as integers, [`compress.rle`]
#'   will not merge runs that add up to lengths greater than what can
#'   be represented by a 32-bit signed integer
#'   (\Sexpr{.Machine$integer.max}).
#'
#' @examples
#'
#' x <- rle(as.logical(rbinom(10,1,.7)))
#' y <- rle(as.logical(rbinom(10,1,.3)))
#'
#' stopifnot(identical(rle(inverse.rle(x)&inverse.rle(y)),compress(x&y)))
#'
#' big <- structure(list(lengths=as.integer(rep(.Machine$integer.max/4,6)),
#'                       values=rep(TRUE,6)), class="rle")
#'
#' stopifnot(all(aggregate(as.numeric(lengths)~values,
#'                         data=as.data.frame(unclass(big)),FUN=sum)
#'               ==
#'               aggregate(as.numeric(lengths)~values,
#'                         data=as.data.frame(unclass(compress(big))),
#'                         FUN=sum)))
#' @export
compress.rle <- function(x, ...){
  # First, strip the 0-length runs.
  x$values <- x$values[x$lengths!=0L]
  x$lengths <- x$lengths[x$lengths!=0L]
  # Second, code distinct values as integers if they are not already.
  remap <- ! storage.mode(x$values) %in% c("integer","logical")
  if(remap){
    vf <- as.integer(as.factor(x$values))
    vf[is.na(vf)] <- 0L # NA runs get coded 0.
  }else vf <- x$values
  # Third, call the C code to produce the mapping onto the compressed vector.
  compinfo <- .Call("compress_RLE", x$lengths, vf, remap)
  # Lastly, rebuild the rle with the combined lengths and remapped values.
  structure(list(lengths = compinfo$lengths[seq_len(compinfo$nruns)],
                 values = if(remap) x$values[compinfo$vali[seq_len(compinfo$nruns)]]
                          else compinfo$vali[seq_len(compinfo$nruns)]),
            class = "rle")
}


#' @rdname rle-methods
#'
#' @examples
#'
#' stopifnot(isTRUE(all.equal(mean(inverse.rle(x)),mean(x))))
#' stopifnot(isTRUE(all.equal(mean(inverse.rle(y)),mean(y))))
#'
#' @export
mean.rle <- function(x, na.rm = FALSE, ...){
  if(na.rm) sum(x$values*as.numeric(x$lengths), na.rm = TRUE, ...)/sum(!is.na(x))
  else sum(x$values*as.numeric(x$lengths), na.rm = FALSE, ...)/length(x)
}

#' @rdname rle-methods
#'
#' @note The [`length`] method returns the length of the vector
#'   represented by the object, obtained by summing the lengths of
#'   individual runs. This can be overridden by setting
#'   `options(rle.unclass_index = FALSE)`, which causes it to
#'   return the length of the underlying representation (usually 2) instead.
#'
#' @examples
#'
#' stopifnot(isTRUE(all.equal(length(inverse.rle(x)),length(x))))
#' stopifnot(isTRUE(all.equal(length(inverse.rle(y)),length(y))))
#'
#' @export
length.rle <- function(x){
  if(!is.null(rle_unclass_index <- getOption("rle.unclass_index")) && rle_unclass_index) length(unclass(x))
  else  sum(as.numeric(x$lengths))
}

#' @rdname rle-methods
#'
#' @examples
#' x$values[1] <- NA
#' y$values[1] <- NA
#' stopifnot(isTRUE(all.equal(is.na(inverse.rle(x)),inverse.rle(is.na(x)))))
#' stopifnot(isTRUE(all.equal(is.na(inverse.rle(y)),inverse.rle(is.na(y)))))
#'
#' @export
is.na.rle <- function(x){
  x$values <- is.na(x$values)
  x
}

#' A [`rep`] method for [`rle`] objects
#'
#' @param x an [`rle`] object.
#'
#' @param ... see documentation for [`rep`].
#'
#' @param scale whether to replicate the elements of the
#'   RLE-compressed vector or the runs.
#'
#' @param doNotCompress,doNotCompact whether the method should call
#'   [`compress.rle`] the results before returning. Methods liable to
#'   produce very long output vectors, like [`rep`], have this set
#'   `FALSE` by default. `doNotCompact` is an old name for this argument.
#'
#' @note The [`rep`] method for [`rle`] objects is very limited at
#'   this time. Even though the default setting is to replicate
#'   elements of the vector, only the run-replicating functionality is
#'   implemented at this time except for the simplest case (scalar
#'   `times` argument).
#'
#' @examples
#'
#' x <- rle(sample(c(-1,+1), 10, c(.7,.3), replace=TRUE))
#' y <- rpois(length(x$lengths), 2)
#'
#' stopifnot(isTRUE(all.equal(rep(inverse.rle(x), rep(y, x$lengths)),
#'                                inverse.rle(rep(x, y, scale="run")))))
#'
#' stopifnot(isTRUE(all.equal(rep(inverse.rle(x), max(y)),
#'                                inverse.rle(rep(x, max(y), scale="element")))))
#'
#' @export
rep.rle <- function(x, ..., scale = c("element", "run"), doNotCompact = FALSE, doNotCompress = doNotCompact){
  if(!missing(doNotCompact)) .Deprecated(msg=paste("Argument", sQuote("doNotCompact="), "to", sQuote("rep.rle()"), "is deprecated and has been renamed to", sQuote("doNotCompress="), "."))

  scale <- match.arg(scale)
  ddd <- list(...)

  if(is.null(names(ddd)) && length(ddd)==1) names(ddd) <- "times"

  if(scale=="element" && length(ddd$times)!=1) stop("RLE on element scale is not supported at this time for vector ",sQuote("times")," argument.")

  if(length(x$lengths)==length(ddd$times)){ # This handles the specific scale="run" AND times is vector of appropriate length case.
    tmp <- mapply(function(v, l, times){
      newl <- .run_mul(l, times)
      newv <- rep(v, length(newl))
      list(l = newl, v = newv)
    },
    x$values, x$lengths, ddd$times, SIMPLIFY=FALSE)

    x$values <- as.vector(unlist(sapply(tmp, `[[`, "v")))
    x$lengths <- as.integer(unlist(sapply(tmp, `[[`, "l")))
  }else{  # This handles the scale="run" OR times is scalar case.
    x$values <- rep(x$values, ...)
    x$lengths <- rep(x$lengths, ...)
  }

  if(doNotCompress) x else compress(x)
}

#' Coerce to [`rle`] if not already an [`rle`] object
#'
#' @param x the object to be coerced.
#'
#' @export
as.rle <- function(x){
  UseMethod("as.rle")
}

#' @rdname as.rle
#' @export
as.rle.rle <- function(x) x

#' @rdname as.rle
#' @export
as.rle.default <- function(x){
  #' @importFrom methods is
  if(is(x, "rle")) x else rle(x)
}

#' @rdname rle-methods
#'
#' @examples
#'
#' str(x)
#'
#' @export
str.rle <- function(object, ...){
  # This is needed because `str` needs the length of the underlying
  # list rather than that represented by the RLE.
  op <- options(rle.unclass_index = TRUE)
  on.exit(options(op))
  NextMethod("str")
}
