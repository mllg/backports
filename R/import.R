#' @title Import backported functions into your package
#'
#' @description
#' Imports objects from \pkg{backports} into the namespace of other packages
#' by assigning it during load-time.
#' See examples for a code snippet to copy to your package.
#'
#' @param pkgname [\code{character(1)}]\cr
#'   Name of the package where the backported function should be assigned.
#' @param obj [\code{character}]\cr
#'   Name of objects to assign, as character vector.
#'   If \code{NULL}, all backports which are not provided by R itself are assigned.
#' @param force [\code{logical}]\cr
#'   If \code{obj} is provided and \code{force} is set to \code{FALSE}, only backports
#'   not provided by the base package of the executing R interpreter are imported.
#'   Set to \code{TRUE} to ignore this check and always import the backport into the package's namespace.
#' @export
#' @seealso \code{\link{.onLoad}}
#' @examples
#' \dontrun{
#' # This imports all functions implemented in backports while the package is loaded
#' .onLoad <- function(libname, pkgname) {
#'   backports::import(pkgname)
#' }
#'
#' # This only imports the function "trimws"
#' .onLoad <- function(libname, pkgname) {
#'   backports::import(pkgname, "trimws")
#' }
#'
#' # This imports all backports from base and force-imports "hasName" from utils
#' .onLoad <- function(libname, pkgname) {
#'   backports::import(pkgname)
#'   backports::import(pkgname, "hasName", force = TRUE)
#' }
#' }
import = function(pkgname, obj = NULL, force = FALSE) {
  if (is.null(obj)) {
    obj = get_backports()
  } else if (!isTRUE(force)) {
    obj = intersect(obj, get_backports())
  }

  if (length(obj) > 0L) {
    pkg = getNamespace(pkgname)
    backports = getNamespace("backports")
    for (x in obj) {
      assign(x, get(x, envir = backports), envir = pkg)
    }
  }
  invisible(TRUE)
}

get_backports = function(v = getRversion()) {
  v = package_version(v)
  nn = names(FUNS)
  for (i in seq_along(FUNS)) {
    if (v >= nn[i])
      break
  }
  unlist(head(FUNS, i - 1L), use.names = FALSE)
}

FUNS = list(
  "4.0.0" = c("R_user_dir", "deparse1", "list2DF", "suppressWarnings", "suppressMessages", "stopifnot"),
  "3.6.0" = c("warningCondition", "errorCondition", "vignetteInfo", "dQuote", "sQuote", "removeSource", "asplit"),
  "3.5.0" = c("...length", "...elt", "isFALSE", "isTRUE"),
  "3.4.0" = c("hasName"),
  "3.3.0" = c("startsWith", "endsWith", "strrep", "trimws", "capture.output"),
  "3.2.0" = c("anyNA", "dir.exists", "file.size", "file.mode", "file.mtime", "lengths", "file.info", "URLencode", "isNamespaceLoaded"),
  "3.1.0" = character(),
  "3.0.0" = character(),
  "0.0.0" = character()
)
