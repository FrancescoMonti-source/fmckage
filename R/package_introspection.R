# package_introspection.R
# Utilities for package code discovery & export
# - functions_in_file / functions_in_dir / functions_table
# - find_unused_functions
# - dump_package_functions / export_package_functions
# - pkg_function_map / expand_by_file
#
# Notes:
# * No code is executed when listing; static parse only.
# * pkg_function_map uses pkgnet only for the call graph; file mapping falls
#   back to parsing your R/ dir, so it works even if pkgnet lacks file paths.


#' List top-level functions defined in a single R file (static; no sourcing)
#'
#' Parses a file and returns the names of functions defined at the top level,
#' i.e. expressions of the form `name <- function(...) {}` or
#' `assign("name", function(...) {})`. No code is executed.
#'
#' @param path Path to a `.R` file.
#' @return A character vector of function names found.
#' @examples
#' \dontrun{
#' functions_in_file("R/gpt.R")
#' }
#' @export
functions_in_file <- function(path, as_table = FALSE) {
    exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) expression())
    out <- character()
    is_fun <- function(x) is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), "function")

    for (e in exprs) {
        if (!is.call(e)) next
        head <- as.character(e[[1L]])
        if (head %in% c("<-","=")) {
            lhs <- e[[2L]]; rhs <- e[[3L]]
            if (is_fun(rhs)) {
                if (is.symbol(lhs)) out <- c(out, as.character(lhs))
                else if (is.character(lhs) && length(lhs) == 1L) out <- c(out, lhs)
            }
        } else if (identical(head, "assign") && length(e) >= 3L) {
            nm <- e[[2L]]; rhs <- e[[3L]]
            if (is_fun(rhs)) {
                if (is.symbol(nm)) out <- c(out, as.character(nm))
                else if (is.character(nm) && length(nm) == 1L) out <- c(out, nm)
            }
        }
    }
    out <- unique(out)
    if (!as_table) return(out)
    data.frame(file = basename(path), function_name = out, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
}

#' List top-level functions for every file in a directory
#'
#' @param dir Directory to scan (defaults to `"R"`).
#' @return A named list: file name -> character vector of function names.
#' @examples
#' \dontrun{
#' functions_in_dir("R")
#' }
#' @export
functions_in_dir <- function(dir = "R", as_table = FALSE) {
    files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE)
    if (!length(files)) return(if (as_table) data.frame(file=character(), function_name=character()) else setNames(list(), character()))
    if (!as_table) {
        res <- lapply(files, functions_in_file)
        names(res) <- basename(files)
        return(res)
    }
    do.call(rbind, lapply(files, function(f) functions_in_file(f, as_table = TRUE)))
}


#' Find functions that are possibly unused inside a package source tree
#'
#' Flags functions defined in `R/` that are never referenced (as a call) in any
#' other `.R` file in that directory. This is a static, approximate check.
#'
#' @param dir Package `R/` directory.
#' @return A data.frame with columns: `file`, `function_name`, `def_file`,
#'   `possibly_unused` (logical). Rows where `possibly_unused` is `TRUE`
#'   are candidates for dead code review.
#' @examples
#' \dontrun{
#' find_unused_functions("R")
#' }
#' @export
find_unused_functions <- function(dir = "R") {
    files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE)
    tbl <- do.call(rbind, lapply(files, function(f) {
        fn <- functions_in_file(f)
        if (!length(fn)) return(NULL)
        data.frame(file = basename(f), function_name = fn, def_file = f,
                   row.names = NULL, check.names = FALSE)
    }))
    if (is.null(tbl) || !nrow(tbl)) {
        return(data.frame(file=character(0), function_name=character(0),
                          def_file=character(0), possibly_unused=logical(0)))
    }
    texts <- setNames(lapply(files, readLines, warn = FALSE), files)

    is_used_elsewhere <- function(name, def_file) {
        if (grepl("%.*%", name)) return(TRUE)                     # operators: ignore
        pat <- paste0("\\b", gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", name), "\\s*\\(")
        for (f in files) {
            if (f == def_file) next
            if (any(grepl(pat, texts[[f]]))) return(TRUE)
        }
        FALSE
    }

    tbl$possibly_unused <- !mapply(is_used_elsewhere, tbl$function_name, tbl$def_file)
    subset(tbl, possibly_unused &
               !grepl("::", function_name, fixed = TRUE) &
               !grepl("%.*%", function_name))
}

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
#' dump_package_functions(out = "stats_dump.R",
#'                        package = "stats",
#'                        only_exported = TRUE,
#'                        dest = "file")
#'
#' # 2) Print all functions from a source tree to the console
#' dump_package_functions(dir = "/path/to/pkg",
#'                        include_internal = TRUE,
#'                        dest = "console")
#'
#' # 3) Get the code blocks in-memory (named character vector)
#' blocks <- dump_package_functions(package = "utils",
#'                                  only_exported = TRUE,
#'                                  dest = "return")
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

    if (xor(is.null(package), is.null(dir)) == FALSE)
        stop("Provide exactly one of `package` or `dir`.", call. = FALSE)

    # -- Gather: name -> code block (character) --
    blocks <- list()

    if (!is.null(package)) {
        # installed/loaded: use namespace objects
        if (!requireNamespace(package, quietly = TRUE))
            stop("Package not available: ", package, call. = FALSE)
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
            if (isTRUE(only_exported) && length(exports) && !(nm %in% exports)) return()
            if (!isTRUE(only_exported) && !isTRUE(include_internal) && substr(nm,1L,1L) == ".") return()
            if (!is.null(pattern) && nzchar(pattern) && !grepl(pattern, nm)) return()
            sr <- attr(e, "srcref")
            block <- if (!is.null(sr)) paste(getSrcLines(sr), collapse = "\n") else paste(deparse(e, width.cutoff = 500L), collapse = "\n")
            blocks[[nm]] <<- block
        }

        for (path in files) {
            exprs <- tryCatch(parse(file = path, keep.source = TRUE), error = function(e) expression())
            for (e in exprs) {
                if (!is.call(e)) next
                head <- as.character(e[[1L]]); nm <- NULL
                if (head %in% c("<-","=")) {
                    lhs <- e[[2L]]; rhs <- e[[3L]]
                    if (is_fun_call(rhs)) {
                        if (is.symbol(lhs)) nm <- as.character(lhs)
                        else if (is.character(lhs) && length(lhs) == 1L) nm <- lhs
                    }
                } else if (identical(head, "assign") && length(e) >= 3L) {
                    nm0 <- e[[2L]]; rhs <- e[[3L]]
                    if (is_fun_call(rhs)) {
                        if (is.symbol(nm0)) nm <- as.character(nm0)
                        else if (is.character(nm0) && length(nm0) == 1L) nm <- nm0
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
    dump_package_functions(out = path, package = package_name,
                           only_exported = only_exported, dest = "file")
}

#' Build per-file and per-function maps from a pkgnet report
#'
#' Produces both views:
#' - `by_function`: one row per function with its source file, outgoing calls
#'   (`calls`) and incoming calls (`called_by`) as list-cols, plus counts.
#' - `by_file`: one row per file with the list of functions and summary counts.
#'
#' Your pkgnet version may not include file paths in nodes; in that case we
#' statically parse `r_dir` to map function -> file.
#'
#' @param pkg_report Result of `pkgnet::CreatePackageReport()`.
#' @param r_dir Fallback directory to scan for `.R` files (default `"R"`).
#' @param internal_only If TRUE, keep only edges where both ends are your package's functions.
#' @return A list with `by_function` (data.frame), `by_file` (data.frame),
#'   and `by_file_list` (nested list: file -> function -> list(calls, called_by)).
#' @examples
#' \dontrun{
#' library(pkgnet)
#' pkg_report <- CreatePackageReport(pkg_name = "yourpkg")
#' map <- pkg_function_map(pkg_report, r_dir = "R")
#' map$by_function; map$by_file
#' }
#' @export
# Long-format function/file map from a pkgnet report (no list-cols)
# Requires: functions_in_dir(r_dir) -> list(file -> character vector of function names)
# Long-format function/file map from a pkgnet report (flat tables only)
# Requires: functions_in_dir(r_dir) -> list(file -> character vector of function names)
# Long-format function/file map from a pkgnet report (flat tables only)
# Requires: functions_in_dir(r_dir)
pkg_function_map <- function(pkg_report,
                             r_dir = NULL,
                             internal_only = TRUE,
                             package = NULL,         # optional: use installed ns to resolve unknowns
                             fill_namespace = TRUE   # set FALSE to skip namespace fallback
) {
    fr <- pkg_report$FunctionReporter

    # -- nodes: functions in your package --
    nodes <- as.data.frame(fr$nodes, stringsAsFactors = FALSE)
    if (!nrow(nodes)) stop("pkgnet FunctionReporter has no nodes; did analysis run?", call. = FALSE)
    name_col <- if ("name" %in% names(nodes)) "name" else if ("node" %in% names(nodes)) "node" else NA_character_
    if (is.na(name_col)) stop("Couldn't find a function-name column in FunctionReporter$nodes.")
    func_names <- as.character(nodes[[name_col]])

    # -- choose r_dir (where your .R files live) --
    if (is.null(r_dir)) {
        pkg_path <- tryCatch(pkg_report$pkg_path, error = function(e) NULL)
        if (!is.null(pkg_path) && dir.exists(file.path(pkg_path, "R"))) {
            r_dir <- file.path(pkg_path, "R")
        } else {
            r_dir <- "R"
        }
    }

    # -- map functions -> files by parsing r_dir --
    lst <- functions_in_dir(r_dir)  # list: file -> character vector of names
    fun2file <- if (length(lst)) {
        setNames(rep(names(lst), lengths(lst)), unlist(lst, use.names = FALSE))
    } else setNames(character(0), character(0))
    func_file <- unname(fun2file[func_names])
    func_file[is.na(func_file) | !nzchar(func_file)] <- "(unknown)"

    # -- optional: fill unknowns using installed namespace srcref --
    if (fill_namespace && !is.null(package) && requireNamespace(package, quietly = TRUE)) {
        ns <- getNamespace(package)
        unknown_idx <- which(func_file == "(unknown)")
        if (length(unknown_idx)) {
            get_srcfile_basename <- function(f) {
                sr <- attr(f, "srcref", exact = TRUE)
                if (!is.null(sr)) {
                    sf <- attr(sr, "srcfile", exact = TRUE)
                    fn <- tryCatch(sf$filename, error = function(e) NULL)
                    if (is.character(fn) && length(fn) == 1L && nzchar(fn)) return(basename(fn))
                }
                NULL
            }
            for (i in unknown_idx) {
                nm <- func_names[i]
                if (exists(nm, envir = ns, inherits = FALSE)) {
                    obj <- get(nm, envir = ns, inherits = FALSE)
                    fn  <- get_srcfile_basename(obj)
                    if (!is.null(fn)) func_file[i] <- fn
                }
            }
        }
    }

    # -- edges (function call graph) --
    edges <- as.data.frame(fr$edges, stringsAsFactors = FALSE)
    if (!nrow(edges)) {
        edges <- data.frame(SOURCE = character(0), TARGET = character(0), stringsAsFactors = FALSE)
    } else if (isTRUE(internal_only)) {
        keep_src <- edges$SOURCE %in% func_names
        keep_tgt <- edges$TARGET %in% func_names
        edges <- edges[keep_src & keep_tgt, , drop = FALSE]
    }

    # annotate edges with files
    if (nrow(edges)) {
        edges$file_src <- unname(fun2file[edges$SOURCE]); edges$file_src[is.na(edges$file_src)] <- "(unknown)"
        edges$file_tgt <- unname(fun2file[edges$TARGET]); edges$file_tgt[is.na(edges$file_tgt)] <- "(unknown)"
    } else {
        edges$file_src <- character(0); edges$file_tgt <- character(0)
    }

    # -- per-function counts (no lists) --
    count_for <- function(tab, keys) { z <- as.integer(tab[keys]); z[is.na(z)] <- 0L; z }
    by_function <- data.frame(
        function_name = func_names,
        file          = func_file,
        calls_out_n   = count_for(table(edges$SOURCE), func_names),
        calls_in_n    = count_for(table(edges$TARGET), func_names),
        stringsAsFactors = FALSE
    )
    by_function <- by_function[order(by_function$file, by_function$function_name), ]
    rownames(by_function) <- NULL

    # --long map: one row per (file, function) --
    split_by_file <- split(by_function, by_function$file)
    file_functions <- do.call(rbind, lapply(split_by_file, function(df) {
        if (!nrow(df)) return(NULL)
        data.frame(file = df$file[1L], function_name = df$function_name, stringsAsFactors = FALSE)
    }))
    if (!is.null(file_functions) && nrow(file_functions)) {
        file_functions <- file_functions[order(file_functions$file, file_functions$function_name), ]
        rownames(file_functions) <- NULL      # drop the ".1" style rownames
    } else {
        file_functions <- data.frame(file = character(0), function_name = character(0), stringsAsFactors = FALSE)
    }

    # -- per-file counts (no lists) --
    files <- names(split_by_file)
    by_file <- data.frame(
        file            = files,
        n_functions     = vapply(split_by_file, nrow, integer(1)),
        total_calls_out = vapply(split_by_file, function(df) sum(df$calls_out_n), integer(1)),
        total_calls_in  = vapply(split_by_file, function(df) sum(df$calls_in_n), integer(1)),
        stringsAsFactors = FALSE
    )
    by_file <- by_file[order(by_file$file), ]
    rownames(by_file) <- NULL

    # -- file-to-file edges with counts (keep names BEFORE coercion) --
    if (nrow(edges)) {
        key_tbl <- table(paste(edges$file_src, edges$file_tgt, sep = " -> "))
        if (length(key_tbl)) {
            keys <- names(key_tbl)
            cnts <- as.integer(key_tbl)
            pr   <- do.call(rbind, strsplit(keys, " -> ", fixed = TRUE))
            file_edges <- data.frame(file_src = pr[,1], file_tgt = pr[,2],
                                     n_edges = cnts, stringsAsFactors = FALSE)
            file_edges <- file_edges[file_edges$file_src != file_edges$file_tgt, , drop = FALSE]
            file_edges <- file_edges[order(file_edges$file_src, file_edges$file_tgt), ]
        } else {
            file_edges <- data.frame(file_src = character(0), file_tgt = character(0),
                                     n_edges = integer(0), stringsAsFactors = FALSE)
        }
    } else {
        file_edges <- data.frame(file_src = character(0), file_tgt = character(0),
                                 n_edges = integer(0), stringsAsFactors = FALSE)
    }
    rownames(file_edges) <- NULL

    list(
        by_function    = by_function,     # one row per function (counts only)
        file_functions = file_functions,  # one row per (file, function)
        edges          = edges,           # SOURCE, TARGET, file_src, file_tgt
        by_file        = by_file,         # one row per file (counts only)
        file_edges     = file_edges       # one row per (file_src, file_tgt) with counts
    )
}


#' Expand the per-file summary to one row per function (internal)
#' @keywords internal
expand_by_file <- function(by_file) {
    do.call(rbind, lapply(seq_len(nrow(by_file)), function(i) {
        fns <- by_file$functions[[i]]
        if (!length(fns)) return(NULL)
        data.frame(file = by_file$file[i],
                   function_name = fns,
                   row.names = NULL, check.names = FALSE,
                   stringsAsFactors = FALSE)
    }))
}
