
<!-- README.md is generated from README.Rmd. Please edit that file -->

# withr - run code ‘with’ modified state <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/r-lib/withr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/withr?branch=main)
[![CRAN
Version](https://www.r-pkg.org/badges/version/withr)](https://www.r-pkg.org/pkg/withr)
[![R-CMD-check](https://github.com/r-lib/withr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/withr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

A set of functions to run code with safely and temporarily modified
global state, withr makes working with the global state, i.e. side
effects, less error-prone.

Pure functions, such as the `sum()` function, are easy to understand and
reason about: they always map the same input to the same output and have
no other impact on the workspace. In other words, pure functions have no
*side effects*: they are not affected by, nor do they affect, the global
state in any way apart from the value they return.

The behavior of some functions *is* affected by the global state.
Consider the `read.csv()` function: it takes a filename as an input and
returns the contents as an output. In this case, the output depends on
the contents of the file; i.e. the output is affected by the global
state. Functions like this deal with side effects.

The purpose of the withr package is to help you manage side effects in
your code. You may want to run code with secret information, such as an
API key, that you store as an environment variable. You may also want to
run code with certain options, with a given random-seed, or with a
particular working-directory.

The withr package helps you manage these situations, and more, by
providing functions to modify the global state temporarily, and safely.
These functions modify one of the global settings for duration of a
block of code, then automatically reset it after the block is completed.

## Installation

``` r
#Install the latest version with:
install.packages("withr")
```

Many of these functions were originally a part of the
[devtools](https://github.com/r-lib/devtools) package, this provides a
simple package with limited dependencies to provide access to these
functions.

-   `with_collate()` / `local_collate()` - collation order
-   `with_dir()` / `local_dir()` - working directory
-   `with_envvar()` / `local_envvar()` - environment variables
-   `with_libpaths()` / `local_libpaths()` - library paths
-   `with_locale()` / `local_locale()` - any locale setting
-   `with_makevars()` / `local_makevars()` / `set_makevars()` - makevars
    variables
-   `with_options()` / `local_options()` - options
-   `with_par()` / `local_par()` - graphics parameters
-   `with_path()` / `local_path()` - PATH environment variable
-   `with_*()` and `local_*()` functions for the built in R devices,
    `bmp`, `cairo_pdf`, `cairo_ps`, `pdf`, `postscript`, `svg`, `tiff`,
    `xfig`, `png`, `jpeg`.
-   `with_connection()` / `local_connection()` - R file connections
-   `with_db_connection()` / `local_db_connection()` - DB connections
-   `with_package()` / `local_package()`, `with_namespace()` /
    `local_namespace()` and `with_environment()` /
    `local_environment()` - to run code with modified object search
    paths.
-   `with_tempfile()` / `local_tempfile()` - create and clean up a temp
    file.
-   `with_file()` / `local_file()` - create and clean up a normal file.
-   `with_message_sink()` / `local_message_sink()` - divert message
-   `with_output_sink()` / `local_output_sink()` - divert output
-   `with_preserve_seed()` / `with_seed()`- specify seeds
-   `with_temp_libpaths()` / `local_temp_libpaths()` - library paths
-   `defer()` / `defer_parent()` - defer
-   `with_timezone()` / `local_timezone()` - timezones
-   `with_rng_version()` / `local_rng_version()` - random number
    generation version

## Usage

There are two sets of functions, those prefixed with `with_` and those
with `local_`. The former reset their state as soon as the `code`
argument has been evaluated. The latter reset when they reach the end of
their scope, usually at the end of a function body.

``` r
par("col" = "black")
my_plot <- function(new) {
  with_par(list(col = "red", pch = 19),
    plot(mtcars$hp, mtcars$wt)
  )
  par("col")
}
my_plot()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

    #> [1] "black"
    par("col")
    #> [1] "black"

    f <- function(x) {
      local_envvar(c("WITHR" = 2))
      Sys.getenv("WITHR")
    }

    f()
    #> [1] "2"
    Sys.getenv("WITHR")
    #> [1] ""

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

# See Also

-   [Devtools](https://github.com/r-lib/devtools)
