#' Execute code with a modified search path
#'
#' `with_package()` attaches a package to the search path, executes the code, then
#' removes the package from the search path. The package namespace is _not_
#' unloaded however. `with_namespace()` does the same thing, but attaches the
#' package namespace to the search path, so all objects (even unexported ones) are also
#' available on the search path.
#' @param package \code{[character(1)]}\cr package name to load.
#' @param env \code{[environment()]}\cr Environment to attach.
#' @param .local_envir `[environment]`\cr The environment to use for scoping.
#' @inheritParams defer
#' @inheritParams base::library
#' @template with
#' @examples
#' \dontrun{
#' with_package("ggplot2", {
#'   ggplot(mtcars) + geom_point(aes(wt, hp))
#' })
#' }
#' @export
with_package <- function(package, code, pos = 2, lib.loc = NULL,
  character.only = TRUE, logical.return = FALSE, warn.conflicts = FALSE,
  quietly = TRUE, verbose = getOption("verbose")) {

  local_package(package = package, pos = pos, lib.loc = lib.loc,
                character.only = character.only,
                logical.return = logical.return,
                warn.conflicts = warn.conflicts,
                quietly = quietly, verbose = verbose)
  force(code)
}

#' @rdname with_package
#' @export
local_package <- function(package, pos = 2, lib.loc = NULL,
  character.only = TRUE, logical.return = FALSE, warn.conflicts = FALSE,
  quietly = TRUE, verbose = getOption("verbose"),
  .local_envir = parent.frame()) {

  if (!(package %in% .packages())) {
    suppressPackageStartupMessages(
      (get("library"))(package, pos = pos, lib.loc = lib.loc,
        character.only = character.only, logical.return = logical.return,
        warn.conflicts = warn.conflicts, quietly = quietly, verbose = verbose))

    defer({
      if (package %in% .packages()) {
        detach(paste0("package:", package), character.only = TRUE)
      }
    }, envir = .local_envir)
  }
}

#' @rdname with_package
#' @export
with_namespace <- function(package, code, warn.conflicts = FALSE) {
  ns <- asNamespace(package)
  name <- format(ns)
  (get("attach"))(ns, name = name, warn.conflicts = warn.conflicts)
  on.exit(detach(name, character.only = TRUE))
  force(code)
}

#' @rdname with_package
#' @export
local_namespace <- function(package, .local_envir = parent.frame(), warn.conflicts = FALSE) {
  ns <- asNamespace(package)
  name <- format(ns)
  (get("attach"))(ns, name = name, warn.conflicts = warn.conflicts)
  defer(detach(name, character.only = TRUE), envir = .local_envir)
}

#' @rdname with_package
#' @inheritParams base::attach
#' @export
with_environment <- function(env, code, pos = 2L, name = format(env), warn.conflicts = FALSE) {
  (get("attach"))(env, name = name, pos = pos, warn.conflicts = warn.conflicts)
  on.exit(detach(name, character.only = TRUE))
  force(code)
}

#' @rdname with_package
#' @export
local_environment <- function(env, pos = 2L, name = format(env),
  warn.conflicts = FALSE, .local_envir = parent.frame()) {
  (get("attach"))(env, name = name, pos = pos, warn.conflicts = warn.conflicts)
  defer(detach(name, character.only = TRUE), envir = .local_envir)
}
