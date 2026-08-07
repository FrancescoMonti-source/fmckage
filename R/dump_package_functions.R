# dump_package_functions.R
# Estrae il sorgente delle funzioni di un pacchetto, da namespace installato
# o da albero dei sorgenti, e lo scrive su file / console / valore di ritorno.

#' Dump / print / return package functions as source code
#'
#' Collects top-level function definitions from either an **installed/loaded**
#' package or a **source tree**, then:
#' - writes them to a single, sourceable `.R` file (`dest = "file"`),
#' - prints them to the console (`dest = "console"`), or
#' - returns them as a named character vector (`dest = "return"`).
#'
#' Exactly one of `package` (installed/loaded mode) or `dir` (source-tree mode)
#' must be supplied.
#'
#' @section Modes:
#' **Installed/loaded** (`package=`): Functions are pulled from the package
#' namespace via `utils::dump()`. Formatting is readable. Namespace side-effects
#' (e.g., S3/S4 registration, compiled code, options) are *not* reproduced.
#'
#' **Source tree** (`dir=`): Functions are parsed from files under `R/`
#' and emitted as they appear in source (using `srcref` when available). Best for
#' preserving original formatting and comments. Requires a package source directory.
#'
#' @param out Output path for the generated `.R` file (used only when
#'   `dest = "file"`). Ignored for other `dest` values.
#' @param package Package name to dump from an installed/loaded namespace (installed mode).
#'   Provide *either* `package` or `dir`, not both.
#' @param dir Package root directory containing an `R/` folder (source-tree mode).
#'   Provide *either* `dir` or `package`, not both.
#' @param only_exported Logical. If `TRUE`, include only exported functions
#'   (`getNamespaceExports()` in installed mode; best-effort parse of `NAMESPACE`
#'   in source-tree mode). Default `FALSE`.
#' @param include_internal Logical. If `TRUE`, also include non-exported functions
#'   (ignored when `only_exported = TRUE`). Default `TRUE`.
#' @param pattern Optional regular expression; when provided, only function names matching
#'   `pattern` are included.
#' @param dest One of `"file"`, `"console"`, or `"return"`.
#'   Defaults to `"file"`.
#'
#' @return
#' If `dest = "return"`, a named character vector where names are function names
#' and values are the corresponding source blocks.
#' If `dest = "console"`, prints to `stdout` and returns `NULL` (invisibly).
#' If `dest = "file"`, invisibly returns the normalized output path.
#'
#' @details
#' \itemize{
#'   \item Primitives (e.g., some base functions) cannot be dumped and are skipped.
#'   \item In source-tree mode, only *top-level* function assignments are collected
#'         (e.g., \code{name <- function(...) \{\}} or \code{assign("name", function(...) \{\})}).
#'         Functions defined inside other functions are not included.
#'   \item The generated script may require you to `library()` any dependencies before
#'         `source()`-ing; namespace registrations and compiled code are not replicated.
#' }
#'
#' @examples
#' \dontrun{
#' # 1) Write exported functions from an installed package to a single file
#' dump_package_functions(
#'   out = "stats_dump.R",
#'   package = "stats",
#'   only_exported = TRUE,
#'   dest = "file"
#' )
#'
#' # 2) Print all functions from a source tree to the console
#' dump_package_functions(
#'   dir = "/path/to/pkg",
#'   include_internal = TRUE,
#'   dest = "console"
#' )
#'
#' # 3) Get the code blocks in-memory (named character vector)
#' blocks <- dump_package_functions(
#'   package = "utils",
#'   only_exported = TRUE,
#'   dest = "return"
#' )
#' names(blocks)
#' substr(blocks[[1]], 1, 80)
#' }
#'
#' @seealso [functions_in_dir()], [functions_in_file()]
#' @export

# Collect and write/print/return package functions
# Exactly one of `package` (installed/loaded) or `dir` (source tree) must be set.
# dest = "file"   -> write to `out` (required)
# dest = "console"-> cat to stdout
# dest = "return" -> return a named character vector: name -> code block
dump_package_functions <- function(out = NULL,
                                   package = NULL,
                                   dir = NULL,
                                   only_exported = FALSE,
                                   include_internal = TRUE,
                                   pattern = NULL,
                                   dest = c("file", "console", "return")) {
  dest <- match.arg(dest)

  if (xor(is.null(package), is.null(dir)) == FALSE) {
    stop("Provide exactly one of `package` or `dir`.", call. = FALSE)
  }

  # -- Gather: name -> code block (character) --
  blocks <- list()

  if (!is.null(package)) {
    # installed/loaded: use namespace objects
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package not available: ", package, call. = FALSE)
    }
    ns <- getNamespace(package)
    exports <- tryCatch(getNamespaceExports(package), error = function(e) character(0))
    objs <- as.list(ns, all.names = TRUE)
    funs <- names(objs)[vapply(objs, is.function, logical(1))]
    if (isTRUE(only_exported)) funs <- intersect(funs, exports)
    if (!isTRUE(only_exported) && !isTRUE(include_internal)) funs <- funs[substr(funs, 1L, 1L) != "."]
    if (!is.null(pattern) && nzchar(pattern)) funs <- grep(pattern, funs, value = TRUE)
    # skip primitives (cannot dump)
    funs <- funs[!vapply(mget(funs, envir = ns, inherits = FALSE), is.primitive, logical(1))]
    if (!length(funs)) stop("No functions to dump from namespace: ", package, call. = FALSE)

    for (nm in sort(funs)) {
      tmp <- tempfile(fileext = ".R")
      utils::dump(nm, file = tmp, envir = ns, control = NULL)
      blocks[[nm]] <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
      unlink(tmp)
    }
  } else {
    # source tree: read from R/ files, keep original text (via srcref)
    rdir <- file.path(dir, "R")
    if (!dir.exists(rdir)) stop("Directory does not contain an 'R/' folder: ", dir, call. = FALSE)
    files <- list.files(rdir, pattern = "\\.[Rr]$", full.names = TRUE)
    if (!length(files)) stop("No .R files in ", rdir, call. = FALSE)

    # best-effort exported names
    exports <- character(0)
    if (isTRUE(only_exported)) {
      nsfile <- file.path(dir, "NAMESPACE")
      if (file.exists(nsfile)) {
        lines <- readLines(nsfile, warn = FALSE)
        m <- gregexpr("export\\s*\\(([^\\)]*)\\)", lines, perl = TRUE)
        got <- unlist(regmatches(lines, m))
        names_str <- gsub("^export\\s*\\(|\\)$", "", got)
        names_str <- gsub("\\s", "", names_str)
        parts <- unlist(strsplit(names_str, ","))
        parts <- gsub('^"|"$', "", parts)
        exports <- unique(parts[nzchar(parts)])
      } else {
        warning("NAMESPACE not found; cannot restrict to exported functions.", call. = FALSE)
      }
    }

    is_fun_call <- function(x) is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), "function")

    add_block <- function(nm, e) {
      if (isTRUE(only_exported) && length(exports) && !(nm %in% exports)) {
        return()
      }
      if (!isTRUE(only_exported) && !isTRUE(include_internal) && substr(nm, 1L, 1L) == ".") {
        return()
      }
      if (!is.null(pattern) && nzchar(pattern) && !grepl(pattern, nm)) {
        return()
      }
      sr <- attr(e, "srcref")
      block <- if (!is.null(sr)) paste(getSrcLines(sr), collapse = "\n") else paste(deparse(e, width.cutoff = 500L), collapse = "\n")
      blocks[[nm]] <<- block
    }

    for (path in files) {
      exprs <- tryCatch(parse(file = path, keep.source = TRUE), error = function(e) expression())
      for (e in exprs) {
        if (!is.call(e) || !is.symbol(e[[1L]])) next
        head <- as.character(e[[1L]])
        nm <- NULL
        if (head %in% c("<-", "=")) {
          lhs <- e[[2L]]
          rhs <- e[[3L]]
          if (is_fun_call(rhs)) {
            if (is.symbol(lhs)) {
              nm <- as.character(lhs)
            } else if (is.character(lhs) && length(lhs) == 1L) nm <- lhs
          }
        } else if (identical(head, "assign") && length(e) >= 3L) {
          nm0 <- e[[2L]]
          rhs <- e[[3L]]
          if (is_fun_call(rhs)) {
            if (is.symbol(nm0)) {
              nm <- as.character(nm0)
            } else if (is.character(nm0) && length(nm0) == 1L) nm <- nm0
          }
        }
        if (!is.null(nm) && nzchar(nm)) add_block(nm, e)
      }
    }
    if (!length(blocks)) stop("No top-level functions found under ", rdir, call. = FALSE)
  }

  # -- Deliver: file / console / return --
  header <- c(
    sprintf("# Generated on %s", as.character(Sys.time())),
    if (!is.null(package)) sprintf("# Package (installed): %s", package) else sprintf("# Source: %s", normalizePath(dir, winslash = "/", mustWork = FALSE)),
    ""
  )

  if (dest == "return") {
    # named character vector: name -> code block
    return(stats::setNames(unlist(blocks, use.names = FALSE), names(blocks)))
  }

  if (dest == "console") {
    cat(paste(header, collapse = "\n"))
    for (nm in names(blocks)) {
      cat(sprintf("# --- %s ---\n", nm))
      cat(blocks[[nm]], "\n\n", sep = "")
    }
    return(invisible(NULL))
  }

  # dest == "file"
  if (is.null(out) || !nzchar(out)) stop("`out` must be provided when dest = 'file'.", call. = FALSE)
  con <- file(out, "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(header, con, useBytes = TRUE)
  for (nm in names(blocks)) {
    writeLines(sprintf("# --- %s ---", nm), con, useBytes = TRUE)
    writeLines(blocks[[nm]], con, useBytes = TRUE)
    writeLines("", con, useBytes = TRUE)
  }
  invisible(normalizePath(out, winslash = "/", mustWork = FALSE))
}


#' Alias for `dump_package_functions()`
#' @seealso [dump_package_functions()]
#' @export
export_package_functions <- function(package_name, path, only_exported = TRUE) {
  .Deprecated("dump_package_functions")
  dump_package_functions(
    out = path, package = package_name,
    only_exported = only_exported, dest = "file"
  )
}
