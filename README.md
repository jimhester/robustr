
<!-- README.md is generated from README.Rmd. Please edit that file -->

# withr - run code ‘with’ modified state <img src="man/figures/logo.png" align="right" />

[![Travis-CI Build
Status](https://travis-ci.org/r-lib/withr.svg?branch=master)](https://travis-ci.org/r-lib/withr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/r-lib/withr?branch=master&svg=true)](https://ci.appveyor.com/project/jimhester/withr)
[![Coverage
status](https://codecov.io/gh/r-lib/withr/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/withr?branch=master)
[![CRAN
Version](http://www.r-pkg.org/badges/version/withr)](http://www.r-pkg.org/pkg/withr)

A set of functions to run code with safely and temporarily modified
global state. There are two sets of functions, those prefixed with
`with_` and those with `local_`. The former reset their state as soon as
the `code` argument has been evaluated. The latter reset when they reach
the end of their scope, usually at the end of a function body.

Many of these functions were originally a part of the
[devtools](https://github.com/hadley/devtools) package, this provides a
simple package with limited dependencies to provide access to these
functions.

  - `set_makevars()`
  - `defer()`
  - `defer_parent()`
  - `with_collate()` / `local_collate()` - collation order
  - `with_connection()` / `local_connection()` - R connections.
  - `with_db_connection()` / `local_db_connection()`
  - `with_dir()` / `local_dir()` - working directory
  - `with_environment()` / `local_environment()`
  - `with_envvar()` / `local_envvar()` - environment variables
  - `with_libpaths()` / `local_libpaths()` - library paths
  - `with_locale()` / `local_locale()` - any locale setting
  - `with_makevars()` / `local_makevars()` - Makevars variables
  - `with_message_sink()` / `local_message_sink()`
  - `with_options()` / `local_options()` - options
  - `with_output_sink()` / `local_output_sink()`
  - `with_par()` / `local_par()` - graphics parameters
  - `with_path()` / `local_path()` - PATH environment variable
  - `with_preserve_seed()`
  - `with_rng_version()` / `local_rng_version()`
  - `with_seed()`  
  - `with_temp_libpaths()` / `local_temp_libpaths()`
  - `with_timezone()` / `local_timezone()`
  - `with_*()` and `local_*()` functions for the built in R devices,
    `bmp`, `cairo_pdf`, `cairo_ps`, `pdf`, `postscript`, `svg`, `tiff`,
    `xfig`, `png`, `jpeg`.
  - `with_package()` / `local_package()`, `with_namespace()` /
    `local_namespace()` and `with_environment()` / `local_environment()`
    - to run code with modified object search paths.
  - `with_tempfile()` / `local_tempfile()` - Create and clean up a temp
    file.
  - `with_file()` / `local_file()` - Create and clean up a normal file.

There are also `with_()` and `local_()` functions to construct new
`with_*` and `local_*` functions if needed.

``` r
Sys.getenv("WITHR")
#> [1] ""
with_envvar(c("WITHR" = 2), Sys.getenv("WITHR"))
#> [1] "2"
Sys.getenv("WITHR")
#> [1] ""

with_envvar(c("A" = 1),
  with_envvar(c("A" = 2), action = "suffix", Sys.getenv("A"))
)
#> [1] "1 2"
```

## Local functions

These functions are variants of the corresponding `with_()` function,
but instead of resetting the value at the end of the function call they
reset when the current context goes out of scope. This is most useful
within a function.

``` r
f <- function(x) {
  local_envvar(c("WITHR" = 2))
  Sys.getenv("WITHR")
}

f()
#> [1] "2"
Sys.getenv("WITHR")
#> [1] ""
```

# See Also

  - [Devtools](https://github.com/hadley/devtools)
