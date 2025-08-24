#' Grep (and optionally replace) through package sources
#'
#' Recursively search common package directories for files with selected
#' extensions and print lines that match a regular expression. If `replacement`
#' is provided, perform a global substitution on each matching file (entire file
#' text, not only the matched lines), with an optional dry run.
#'
#' For each matching file, lines are read as UTF-8 and tested with
#' [base::grepl()]. Files that cannot be read are silently skipped.
#'
#' ## Modes
#' - **Search-only** (default): behave like the original `find_in_sources()`.
#'   Prints `"path:line | code"` for each hit and **invisibly returns a data
#'   frame** of hits.
#' - **Search-and-replace**: if `replacement` is non-`NULL`, apply
#'   `gsub(pattern, replacement, file_text, perl = perl, ignore.case = ignore_case)`
#'   to the whole file. When `write = FALSE` (default), it's a **dry run** that
#'   reports what would change. When `write = TRUE`, changes are written back.
#'
#' @param pattern Character(1). Regular expression to search for. Passed to
#'   [base::grepl()] and [base::gsub()]. Remember to double-escape backslashes.
#' @param replacement Character(1) or `NULL`. If provided, enables replace mode:
#'   matches will be substituted with this string (supports backreferences).
#' @param write Logical. When `TRUE` and `replacement` is provided, write changes
#'   to disk. Defaults to `FALSE` (dry run).
#' @param dirs Character vector of directories to search recursively.
#'   Defaults to `c("R", "src", "inst")`.
#' @param include_ext Character vector of **regular-expression** suffixes that
#'   define which files to include (e.g. `"\\\\.R$"`, `"\\\\.Rmd$"`). The
#'   entries are combined with `|` into a single pattern for [base::list.files()].
#' @param excludes Character vector of anchored path prefixes to skip
#'   (e.g., `"renv"`, `"packrat"`, `"inst/doc"`). Default `character(0)`.
#' @param perl Logical; whether to use PCRE via `perl = TRUE` in [base::grepl()]
#'   and [base::gsub()]. Defaults to `TRUE`.
#' @param ignore_case Logical; case-insensitive search/replace. Defaults to `FALSE`.
#' @param verbose Logical; print per-line matches (always printed in search-only
#'   mode). In replace mode, lines are printed only when `verbose = TRUE`.
#'
#' @returns
#' - **Search-only:** (when `replacement = NULL`) Invisibly returns a `data.frame`
#'   with columns:
#'   \describe{
#'     \item{file}{File path (character).}
#'     \item{line}{1-based line number (integer).}
#'     \item{code}{Matched line, trimmed (character).}
#'   }
#'   If no matches are found, prints `"No matches."` and returns `NULL` invisibly.
#'
#' - **Replace mode:** (when `replacement` provided) Invisibly returns a `list`
#'   with:
#'   \describe{
#'     \item{ok}{Logical scalar.}
#'     \item{files}{Data frame with per-file stats:
#'       `file`, `changed`, `hits`, `bytes_in`, `bytes_out`.}
#'     \item{total_hits}{Integer total substitutions across all files.}
#'     \item{written}{Logical scalar indicating if writes were requested.}
#'     \item{meta}{List of run settings.}
#'   }
#'
#' @examples
#' \dontrun{
#' # --- Search only ---
#' # Find all TODO markers anywhere under the default dirs
#' find_in_sources("TODO")
#'
#' # Find where function 'foo' is defined/assigned
#' find_in_sources("^\\s*foo\\s*<-")
#'
#' # Restrict search to R and Rmd files
#' find_in_sources("setClass\\(", include_ext = c("\\\\.R$", "\\\\.Rmd$"))
#'
#' # --- Search and replace (dry run first) ---
#' # Would rename ".old_api(" to ".new_api(" across R/ and tests/
#' res <- find_in_sources(
#'   pattern     = "(\\.)old_api\\b",
#'   replacement = "\\1new_api",
#'   dirs        = c("R", "tests"),
#'   write       = FALSE
#' )
#' res$total_hits
#' subset(res$files, changed)
#'
#' # Apply for real:
#' find_in_sources(
#'   pattern     = "(\\.)old_api\\b",
#'   replacement = "\\1new_api",
#'   dirs        = c("R", "tests"),
#'   write       = TRUE
#' )
#' }
#'
#' @seealso [base::grepl()], [base::gsub()], [base::list.files()]
#' @export
find_in_sources <- function(pattern,
                            replacement = NULL,
                            write = FALSE,
                            dirs = c("R", "src", "inst"),
                            include_ext = c("\\.R$", "\\.r$", "\\.Rmd$", "\\.Rnw$"),
                            excludes = character(0),
                            perl = TRUE,
                            ignore_case = FALSE,
                            verbose = interactive()) {
  stopifnot(length(pattern) == 1)
  # discover files
  files <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(d)) {
      return(character())
    }
    list.files(d,
      pattern = paste(include_ext, collapse = "|"),
      recursive = TRUE, full.names = TRUE
    )
  }), use.names = FALSE)

  # exclude prefixes (anchored)
  if (length(excludes)) {
    excl_rx <- paste0("^", gsub("\\.", "\\\\.", excludes))
    keep <- !vapply(
      files, function(p) {
        any(grepl(paste(excl_rx, collapse = "|"), p))
      },
      logical(1)
    )
    files <- files[keep]
  }

  # search-only mode
  if (is.null(replacement)) {
    out <- list()
    for (f in files) {
      txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
        error = function(e) NULL
      )
      if (is.null(txt)) next
      hits <- grepl(pattern, txt, perl = perl, ignore.case = ignore_case)
      if (any(hits)) {
        ln <- which(hits)
        df <- data.frame(
          file = f,
          line = ln,
          code = trimws(txt[ln]),
          row.names = NULL,
          stringsAsFactors = FALSE
        )
        out[[length(out) + 1L]] <- df
        # pretty print lines
        apply(df, 1, function(r) {
          cat(sprintf("%s:%s | %s\n", r["file"], r["line"], r["code"]))
        })
      }
    }
    if (!length(out)) {
      return(invisible(message("No matches.")))
    }
    return(invisible(do.call(rbind, out)))
  }

  # replace mode (whole-file gsub)
  files_rows <- vector("list", length(files))
  total_hits <- 0L
  for (i in seq_along(files)) {
    f <- files[[i]]
    txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
      error = function(e) NULL
    )
    if (is.null(txt)) next
    before <- paste0(txt, collapse = "\n")

    # count matches via gregexpr on full text (more accurate than line-wise sum)
    m <- gregexpr(pattern, before, perl = perl, ignore.case = ignore_case)[[1]]
    n_hits <- if (length(m) == 1L && m[1] == -1L) 0L else length(m)
    after <- if (n_hits > 0L) {
      gsub(pattern, replacement, before, perl = perl, ignore.case = ignore_case)
    } else {
      before
    }

    changed <- !identical(before, after)
    if (changed && isTRUE(write)) {
      # write UTF-8, preserve bytes explicitly
      writeLines(enc2utf8(after), f, useBytes = TRUE)
    }

    if (verbose && n_hits > 0L) {
      cat(sprintf(
        "%s: %d substitution(s)%s\n",
        f, n_hits, if (!write) " (dry run)" else ""
      ))
    }

    files_rows[[i]] <- data.frame(
      file = f,
      changed = changed,
      hits = n_hits,
      bytes_in = nchar(before, type = "bytes"),
      bytes_out = nchar(after, type = "bytes"),
      stringsAsFactors = FALSE
    )
    total_hits <- total_hits + n_hits
  }

  files_df <- do.call(rbind, files_rows)
  # drop rows for unreadable files (NULL txt) which would be NA; keep only non-NA hits
  files_df <- files_df[!is.na(files_df$hits), , drop = FALSE]

  invisible(list(
    ok = TRUE,
    files = files_df[order(files_df$changed, decreasing = TRUE), ],
    total_hits = total_hits,
    written = isTRUE(write),
    meta = list(
      dirs = dirs, include_ext = include_ext, excludes = excludes,
      perl = perl, ignore_case = ignore_case
    )
  ))
}
