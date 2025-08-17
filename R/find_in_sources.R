#' Grep through package sources
#'
#' Recursively search common package directories for files with selected
#' extensions and print lines that match a regular expression. Useful for
#' quickly locating symbols, TODOs, or API usage across `R/`, `src/`, and `inst/`.
#'
#' For each matching file, lines are read as UTF-8 and tested with
#' [base::grepl()]. Files that cannot be read are silently skipped. Each hit is
#' printed as `"path:line | code"` and also collected into a data frame that is
#' returned invisibly.
#'
#' @param pattern Character scalar. Regular expression to search for. Passed to
#'   [base::grepl()] as `pattern`. Remember to double-escape backslashes in R
#'   strings.
#' @param dirs Character vector of directories to search recursively.
#'   Defaults to `c("R", "src", "inst")`.
#' @param include_ext Character vector of **regular-expression** suffixes that
#'   define which files to include (e.g. `"\\\\.R$"`, `"\\\\.Rmd$"`). The
#'   entries are combined with `|` into a single pattern for [base::list.files()].
#' @param perl Logical; whether to use PCRE via `perl = TRUE` in [base::grepl()].
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns a `data.frame` with columns:
#' \describe{
#'   \item{file}{File path (character).}
#'   \item{line}{1-based line number (integer).}
#'   \item{code}{Matched line, trimmed (character).}
#' }
#' If no matches are found, a message `"No matches."` is emitted and `NULL` is
#' returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Find all TODO markers anywhere under the default dirs
#' find_in_sources("TODO")
#'
#' # Find where function 'foo' is defined/assigned
#' find_in_sources("^\\s*foo\\s*<-")
#'
#' # Look for .Call usages in R/ and src/
#' find_in_sources("\\.Call\\(", dirs = c("R", "src"))
#'
#' # Restrict search to R and Rmd files
#' find_in_sources("setClass\\(", include_ext = c("\\\\.R$", "\\\\.Rmd$"))
#' }
#'
#' @seealso [base::grepl()], [base::list.files()]
#' @export

find_in_sources <- function(pattern,
                            dirs = c("R", "src", "inst"),
                            include_ext = c("\\.R$", "\\.r$", "\\.Rmd$", "\\.Rnw$"),
                            perl = TRUE) {
    stopifnot(length(pattern) == 1)
    files <- unlist(lapply(dirs, function(d)
        list.files(d, pattern = paste(include_ext, collapse = "|"),
                   recursive = TRUE, full.names = TRUE)),
        use.names = FALSE)
    out <- list()
    for (f in files) {
        txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
        if (is.null(txt)) next
        hits <- grepl(pattern, txt, perl = perl)
        if (any(hits)) {
            ln <- which(hits)
            out[[f]] <- data.frame(
                file = f,
                line = ln,
                code = trimws(txt[ln]),
                row.names = NULL,
                stringsAsFactors = FALSE
            )
        }
    }
    if (!length(out)) return(invisible(message("No matches.")))
    res <- do.call(rbind, out)
    # pretty print
    apply(res, 1, function(r) cat(sprintf("%s:%s | %s\n", r["file"], r["line"], r["code"])))
    invisible(res)
}
