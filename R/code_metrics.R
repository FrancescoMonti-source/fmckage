# code_metrics.R
# Motore di misura condiviso da audit_repo() e audit_library().
#
# Due raccoglitori (sorgenti / namespace) alimentano una sola funzione di
# misura, cosi' un repo e un pacchetto installato producono numeri confrontabili.
#
# Notes:
# * Nessun codice del repo viene eseguito: si valutano solo le espressioni
#   `function(...)`, che costruiscono la chiusura senza toccarne il corpo.
# * Il grafo delle chiamate e' statico e approssimato: non vede il dispatch
#   S3/S4, do.call(), get(). Serve a produrre una lista di posti dove guardare.

#' Pattern contati sull'AST
#'
#' Simboli il cui uso viene conteggiato da [audit_repo()] e [audit_library()],
#' normalizzato ogni 1000 nodi di AST. Conta il ricorso a controlli e guardie,
#' non righe di codice, cosi' e' confrontabile fra basi di codice di dimensioni
#' diverse.
#'
#' @export
code_patterns <- c(
  "is.null", "tryCatch", "stop", "stopifnot", "warning", "message",
  "%||%", "suppressWarnings", "inherits", "missing", "match.arg"
)

#' Estrai gli oggetti funzione definiti in una directory di sorgenti
#'
#' Controparte a livello di oggetti di [functions_in_dir()]: invece dei soli
#' nomi restituisce le chiusure, necessarie per misurare complessita' e AST.
#' Nessun codice viene eseguito (si veda la nota nel file).
#'
#' @param dir Directory da leggere. Se contiene `R/`, viene usata quella.
#' @param recursive Se `TRUE`, scende nelle sottocartelle.
#' @param exclude Nomi di cartelle da saltare.
#' @return Lista con nome -> funzione.
#' @examples
#' \dontrun{
#' fns <- fn_objects_in_dir("C:/Users/franc/Documents/Git/redsan")
#' length(fns)
#' }
#' @export
fn_objects_in_dir <- function(dir, recursive = FALSE,
                              exclude = c("renv", "packrat", ".Rproj.user", "revdep")) {
  if (dir.exists(file.path(dir, "R"))) dir <- file.path(dir, "R")
  .fn_objects(dir, recursive = recursive, exclude = exclude)
}

# Come fn_objects_in_dir() ma senza la deviazione automatica su R/: audit_repo()
# passa cartelle esplicite e non deve vederle reinterpretate.
.fn_objects <- function(dir, recursive = FALSE, exclude = character()) {
  files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  if (length(exclude) && length(files)) {
    rel <- gsub("\\\\", "/", substring(files, nchar(dir) + 2L))
    parts <- strsplit(rel, "/")
    files <- files[!vapply(parts, function(p) any(p %in% exclude), logical(1))]
  }

  env <- new.env(parent = globalenv())
  is_fun_call <- function(x) {
    is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), "function")
  }
  out <- list()
  for (path in files) {
    exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) expression())
    for (e in exprs) {
      # e[[1]] puo' essere a sua volta una call (es. `(function(x) x)(1)`), e
      # allora as.character() restituisce piu' di un elemento.
      if (!is.call(e) || !is.symbol(e[[1L]])) next
      head <- as.character(e[[1L]])
      nm <- NULL
      rhs <- NULL
      if (head %in% c("<-", "=", "<<-") && length(e) == 3L) {
        lhs <- e[[2L]]
        rhs <- e[[3L]]
        if (is.symbol(lhs)) {
          nm <- as.character(lhs)
        } else if (is.character(lhs) && length(lhs) == 1L) nm <- lhs
      } else if (identical(head, "assign") && length(e) >= 3L) {
        nm0 <- e[[2L]]
        rhs <- e[[3L]]
        if (is.symbol(nm0)) {
          nm <- as.character(nm0)
        } else if (is.character(nm0) && length(nm0) == 1L) nm <- nm0
      }
      if (is.null(nm) || !nzchar(nm) || !is_fun_call(rhs)) next
      # eval ristretta: is_fun_call() garantisce che rhs sia una call a
      # `function`. Valutarla costruisce la chiusura e basta -- corpo e default
      # degli argomenti non vengono eseguiti.
      out[[nm]] <- eval(rhs, envir = env)
      attr(out[[nm]], "src_file") <- basename(path)
    }
  }
  out
}

#' Estrai gli oggetti funzione dal namespace di un pacchetto installato
#'
#' I pacchetti installati non contengono i sorgenti (R li compila in un
#' database lazy-load), quindi qui si legge il namespace. `body()` restituisce
#' l'AST anche di una funzione byte-compilata, percio' le misure restano le
#' stesse di [fn_objects_in_dir()].
#'
#' @param package Nome del pacchetto.
#' @return Lista con nome -> funzione. Primitive e funzioni senza corpo escluse.
#' @export
fn_objects_in_namespace <- function(package) {
  ns <- asNamespace(package)
  out <- list()
  for (n in ls(ns, all.names = TRUE)) {
    o <- tryCatch(get(n, envir = ns), error = function(e) NULL)
    if (is.function(o) && !is.primitive(o) && !is.null(body(o))) out[[n]] <- o
  }
  out
}

# Misura un insieme di funzioni. Cuore condiviso: qualunque cosa arrivi qui
# (sorgenti o namespace) viene misurata allo stesso modo.
.analyse_functions <- function(fns, exported = character()) {
  nms <- names(fns)
  cc <- vapply(fns, function(f) {
    tryCatch(cyclocomp::cyclocomp(f), error = function(e) NA_integer_)
  }, integer(1))

  syms <- lapply(fns, function(f) tryCatch(all.names(body(f)), error = function(e) character()))
  refs <- Map(function(s, self) setdiff(intersect(s, nms), self), syms, nms)
  callers <- vapply(nms, function(n) sum(vapply(refs, function(r) n %in% r, TRUE)), 0L)

  files <- vapply(fns, function(f) {
    s <- attr(f, "src_file")
    if (is.null(s)) NA_character_ else s
  }, character(1))

  d <- data.frame(
    fn = nms,
    file = unname(files),
    cc = unname(cc),
    exported = nms %in% exported,
    callers = as.integer(unname(callers)),
    calls = vapply(refs, length, 0L),
    row.names = NULL, stringsAsFactors = FALSE
  )

  all_syms <- unlist(syms, use.names = FALSE)
  nodes <- length(all_syms)
  pat <- vapply(code_patterns, function(k) 1000 * sum(all_syms == k) / nodes, numeric(1))

  list(functions = d[order(-d$cc, d$fn), ], ast_nodes = nodes, patterns = pat)
}

# Riga di sintesi: un pacchetto o un repo -> una riga di metriche confrontabili.
.summarise <- function(a, label) {
  d <- a$functions
  int <- !d$exported
  if (!any(int)) int <- rep(TRUE, nrow(d))
  cc <- d$cc
  out <- data.frame(
    label = label,
    n_fn = nrow(d),
    n_exported = sum(d$exported),
    cc_median = stats::median(cc, na.rm = TRUE),
    cc_p90 = unname(stats::quantile(cc, .9, na.rm = TRUE, names = FALSE)),
    cc_max = suppressWarnings(max(cc, na.rm = TRUE)),
    top8_share = if (sum(cc, na.rm = TRUE) > 0) {
      sum(utils::head(sort(cc, decreasing = TRUE), 8)) / sum(cc, na.rm = TRUE)
    } else NA_real_,
    pct_1caller = mean(d$callers[int] == 1L),
    pct_0callers = mean(d$callers[int] == 0L),
    ast_nodes = a$ast_nodes,
    row.names = NULL, stringsAsFactors = FALSE
  )
  for (k in code_patterns) out[[.pat_col(k)]] <- unname(a$patterns[[k]])
  out
}

# Nome di colonna per un pattern. make.names() renderebbe "%||%" in "X....".
.pat_col <- function(k) paste0("p_", ifelse(k == "%||%", "null_default", k))

.exports_of <- function(path) {
  ns <- file.path(path, "NAMESPACE")
  if (!file.exists(ns)) return(character())
  txt <- readLines(ns, warn = FALSE)
  ex <- regmatches(txt, regexpr("(?<=^export\\()[^)]+", txt, perl = TRUE))
  s3 <- regmatches(txt, regexpr("(?<=^S3method\\()[^)]+", txt, perl = TRUE))
  s3 <- vapply(strsplit(s3, ","), function(p) paste(trimws(p), collapse = "."), "")
  unique(trimws(c(ex, s3)))
}

# --- estrazione dei soli nomi (piu' economica di fn_objects_in_dir) --------

#' List top-level functions defined in a single R file (static; no sourcing)
#'
#' Parses a file and returns the names of functions defined at the top level,
#' i.e. expressions of the form `name <- function(...) {}` or
#' `assign("name", function(...) {})`. No code is executed.
#'
#' @param path Path to a `.R` file.
#' @param as_table If `TRUE`, return a `file`/`function_name` data frame instead
#'   of a character vector.
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
    if (!is.call(e) || !is.symbol(e[[1L]])) next
    head <- as.character(e[[1L]])
    if (head %in% c("<-", "=")) {
      lhs <- e[[2L]]
      rhs <- e[[3L]]
      if (is_fun(rhs)) {
        if (is.symbol(lhs)) {
          out <- c(out, as.character(lhs))
        } else if (is.character(lhs) && length(lhs) == 1L) out <- c(out, lhs)
      }
    } else if (identical(head, "assign") && length(e) >= 3L) {
      nm <- e[[2L]]
      rhs <- e[[3L]]
      if (is_fun(rhs)) {
        if (is.symbol(nm)) {
          out <- c(out, as.character(nm))
        } else if (is.character(nm) && length(nm) == 1L) out <- c(out, nm)
      }
    }
  }
  out <- unique(out)
  if (!as_table) {
    return(out)
  }
  data.frame(file = basename(path), function_name = out, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
}

#' List top-level functions for every file in a directory
#'
#' @param dir Directory to scan (defaults to `"R"`).
#' @param as_table If `TRUE`, return a single `file`/`function_name` data frame
#'   instead of a named list.
#' @return A named list: file name -> character vector of function names.
#' @examples
#' \dontrun{
#' functions_in_dir("R")
#' }
#' @export
functions_in_dir <- function(dir = "R", as_table = FALSE) {
  files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE)
  if (!length(files)) {
    return(if (as_table) data.frame(file = character(), function_name = character()) else stats::setNames(list(), character()))
  }
  if (!as_table) {
    res <- lapply(files, functions_in_file)
    names(res) <- basename(files)
    return(res)
  }
  do.call(rbind, lapply(files, function(f) functions_in_file(f, as_table = TRUE)))
}
