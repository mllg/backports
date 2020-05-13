# backports

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/backports)](http://cran.r-project.org/package=backports)
[![Travis-CI Build Status](https://travis-ci.org/r-lib/backports.svg?branch=master)](https://travis-ci.org/r-lib/backports)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/9lup9d3d3olnp2ua/branch/master?svg=true)](https://ci.appveyor.com/project/mllg/backports/branch/master)
[![Download Stats](http://cranlogs.r-pkg.org/badges/backports)](https://cran.r-project.org/package=backports)

This package provides backports of functions which have been introduced in one of the base packages in R version 3.0.1 or later.
The backports are conditionally exported in order to let R resolve the function name to either the implemented backport, or the respective base version, if available.
Package developers can make use of new functions or arguments by selectively importing specific backports to support older installations.

## Usage

### Interactively

Attach the namespace via `library(backports)`.

Note that it is crucial that `backports` is build against the currently running R version.
Do not ignore these kind of warnings for this package.

### In packages
1. Add backports to your DESCRIPTION under `Imports`.
2. Add or modify the function `.onLoad()` (see [here](https://stat.ethz.ch/R-manual/R-devel/library/base/html/ns-hooks.html)) to call the `import()` function of backports:
   ```r
   .onLoad <- function(libname, pkgname) {
     backports::import(pkgname)
   }
   ```
   You can also selectively import functions:
   ```r
   .onLoad <- function(libname, pkgname) {
     backports::import(pkgname, c("get0", "dir.exists"))
   }
   ```
   If you set `force = TRUE`, the provided functions will get imported, regardless of the R version:
   ```r
   .onLoad <- function(libname, pkgname) {
     backports::import(pkgname, "hasName", force = TRUE)
   }
   ```
3. [Optional] Set `Depends` in your DESCRIPTION to require `R (>= 3.0.0)`.

### Importing backports from `utils` or `tools`

Backports for functions which are not in the `base` require additional steps.
Usually, you need to to import those functions in the package NAMESPACE.
However, this is only possible if such a function really exists, and yields a warning during `R CMD check` for older versions.
There are two possibilities to deal with this:

1. Completely import the namespaces instead of selectively importing them, e.g. use `import(utils)` instead of `importFrom(utils, hasName)` in your `NAMESPACPE` file.
2. Force-import the function (see above) so that you always use the backport instead of the implementation shipped with R, even for the most recent version of R.


## Backports for R versions prior to 3.2.0

* `base::anyNA()`
* `base::dir.exists()`
* `base::file.size()`, `base::file.mode()` and `base::file.mtime()`
* `base::lengths()`
* Argument `extra_cols` for `base::file.info()`
* Argument `repeated` for `utils::URLencode`

## Backports for R versions prior to 3.3.0

* `base::startsWith()` and `base::endsWith()`
* `base::strrep()`
* `base::trimws()`
* Argument `type` for `utils::capture.output()`

## Backports for R versions prior to 3.4.0

* `base::.valid.factor()`
* `utils::hasName()`

## Backports for R versions prior to 3.5.0

* `base::...length()`
* `base::...elt()`
* `base::isFALSE()`

## Backports for R versions prior to 3.6.0

* `base::warningCondition()`
* `base::errorCondition()`
* Argument `q` for `base::dQuote()` and `base::sQuote()`

## Backports for R versions prior to 4.0.0

* `base::deparse1()`
* `base::list2DF()`
* `tools::R_user_dir()`
