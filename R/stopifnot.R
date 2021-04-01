#' @title Backport of stopifnot for R versions < 4.0.0.
#'
#' @description
#' See the original description in \code{base::stopifnot}.
#'
#' @keywords internal
#' @rawNamespace if (getRversion() < "4.0.0") export(stopifnot)
#' @examples
#' # get function from namespace instead of possibly getting
#' # implementation shipped with recent R versions:
#' bp_stopifnot = getFromNamespace("stopifnot", "backports")
#'
#' m <- matrix(c(1, 3, 12, 1), 2, 2)
#' \dontrun{bp_stopifnot("m must be symmetric" = m == t(m))}

stopifnot = function(..., exprs, exprObject, local = TRUE) {
  n <- ...length()
  if ((has.e <- !missing(exprs)) || !missing(exprObject)) {
    if (n || (has.e && !missing(exprObject)))
      stop("Only one of 'exprs', 'exprObject' or expressions, not more")
    envir <- if (isTRUE(local)) parent.frame()
    else if (isFALSE(local)) .GlobalEnv
    else if (is.environment(local)) local
    else stop("'local' must be TRUE, FALSE or an environment")
    E1 <- if (has.e && is.call(exprs <- substitute(exprs))) exprs[[1]]
    cl <- if (is.symbol(E1) &&
             E1 == quote(`{`)) {
      exprs[[1]] <- quote(stopifnot) ## --> stopifnot(*, *, ..., *) :
      exprs
    }
    else
      as.call(c(quote(stopifnot),
                if (!has.e) exprObject else as.expression(exprs))) # or fail ..
    names(cl) <- NULL
    return(eval(cl, envir = envir))
  }
  ## else   use '...' (and not 'exprs') :

  Dparse <- function(call, cutoff = 60L) {
    ch <- deparse(call, width.cutoff = cutoff)
    if (length(ch) > 1L) paste(ch[1L], "....") else ch
  }
  head <- function(x, n = 6L) ## basically utils:::head.default()
    x[seq_len(if (n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
  abbrev <- function(ae, n = 3L)
    paste(c(head(ae, n), if (length(ae) > n) "...."), collapse = "\n  ")
  ##
  for (i in seq_len(n)) {
    r <- ...elt(i)
    if (!(is.logical(r) && !anyNA(r) && all(r))) {
      dots <- match.call()[-1L]
      if (is.null(msg <- names(dots)) || !nzchar(msg <- msg[i])) {
        cl.i <- dots[[i]]
        msg <- ## special case for decently written 'all.equal(*)':
          if (is.call(cl.i) && identical(cl.i[[1]], quote(all.equal)) &&
             (is.null(ni <- names(cl.i)) || length(cl.i) == 3L ||
              length(cl.i <- cl.i[!nzchar(ni)]) == 3L))

            sprintf(gettext("%s and %s are not equal:\n  %s"),
                    Dparse(cl.i[[2]]),
                    Dparse(cl.i[[3]]), abbrev(r))
        else
          sprintf(ngettext(length(r),
                           "%s is not TRUE",
                           "%s are not all TRUE"),
                  Dparse(cl.i))
      }
      stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
    }
  }
  invisible()
}
