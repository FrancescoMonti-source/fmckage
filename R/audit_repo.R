# audit_repo.R
# Fotografia strutturale di un repo R: dove si concentra la complessita', quali
# astrazioni non sono mai riusate, cosa esiste solo per i test.
#
# Funziona sia sui pacchetti (DESCRIPTION + NAMESPACE) sia sui repo di soli
# script. Non emette giudizi: produce una lista di posti dove guardare.

#' Audit strutturale di un repo R
#'
#' Misura ogni funzione definita nel repo e restituisce le tabelle su cui
#' impostare una revisione: complessita' ciclomatica, numero di chiamanti,
#' uso nei test, densita' di guardie e controlli.
#'
#' Le metriche sono le stesse di [audit_library()], cosi' il risultato si puo'
#' collocare in una distribuzione di riferimento con [compare_to_library()].
#'
#' @section Cosa non vede:
#' Il grafo delle chiamate e' statico: dispatch S3/S4, `do.call()` e `get()`
#' non sono rilevati, quindi `callers == 0` va letto come "da verificare", non
#' come "morto". Le funzioni annidate dentro altre funzioni non sono contate.
#'
#' @param path Radice del repo.
#' @param dirs Cartelle da misurare, relative a `path`. Default `NULL`: usa
#'   `R/` se esiste, altrimenti la radice. `scripts/` non e' incluso di
#'   default -- gli script d'ingresso hanno una forma diversa dal codice di
#'   libreria e mescolarli falsa il confronto con [audit_library()]. Per
#'   guardarli si passi esplicitamente `dirs = c("R", "scripts")`.
#' @param exclude Cartelle da escludere dal conteggio (default: `"tools"`, di
#'   solito script usa-e-getta che sporcano le funzioni senza chiamanti).
#' @param recursive Se `TRUE`, scende nelle sottocartelle di `dirs`.
#' @return Oggetto di classe `repo_audit`: lista con `functions` (una riga per
#'   funzione), `by_file`, `summary` (una riga di metriche confrontabili),
#'   `patterns` e i metadati del repo. Stampalo per il report leggibile.
#' @examples
#' \dontrun{
#' a <- audit_repo("C:/Users/franc/Documents/Git/redsan")
#' a
#' subset(a$functions, callers == 1 & cc > 10)
#'
#' # includendo anche gli script d'ingresso
#' audit_repo("C:/Users/franc/Documents/Git/orchidee", dirs = c("R", "scripts"))
#' }
#' @seealso [audit_library()], [compare_to_library()]
#' @export
audit_repo <- function(path, dirs = NULL, exclude = c("tools"), recursive = FALSE) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  is_pkg <- file.exists(file.path(path, "DESCRIPTION"))

  has_r <- dir.exists(file.path(path, "R"))
  if (is.null(dirs)) dirs <- if (has_r) "R" else "."
  src_dirs <- file.path(path, dirs)
  absent <- !dir.exists(src_dirs)
  if (any(absent)) {
    stop("cartelle inesistenti sotto ", path, ": ",
         paste(dirs[absent], collapse = ", "), call. = FALSE)
  }

  skip <- c(exclude, "renv", "packrat", ".Rproj.user", "tests", "testthat")
  fns <- list()
  for (d in src_dirs) {
    fns <- c(fns, .fn_objects(d, recursive = recursive || identical(dirs, "."), exclude = skip))
  }
  if (!length(fns)) {
    stop("nessuna definizione di funzione trovata sotto ", paste(src_dirs, collapse = ", "),
         call. = FALSE)
  }
  # stesso nome definito in piu' cartelle: si tiene il primo, ma lo si dichiara
  dup <- unique(names(fns)[duplicated(names(fns))])
  fns <- fns[!duplicated(names(fns))]

  a <- .analyse_functions(fns, exported = .exports_of(path))
  d <- a$functions

  # simboli citati nei test: distingue "nessuno la chiama" da "la chiamano solo i test"
  test_dir <- file.path(path, "tests")
  test_syms <- character()
  if (dir.exists(test_dir)) {
    for (f in list.files(test_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
      for (e in tryCatch(parse(f), error = function(err) expression())) {
        test_syms <- c(test_syms, all.names(e))
      }
    }
  }
  d$in_tests <- d$fn %in% unique(test_syms)

  by_file <- do.call(rbind, lapply(split(d, d$file), function(g) {
    data.frame(
      file = g$file[1L], n_fn = nrow(g),
      cc_tot = sum(g$cc, na.rm = TRUE), cc_max = max(g$cc, na.rm = TRUE),
      n_1caller = sum(g$callers == 1L), n_0callers = sum(g$callers == 0L),
      row.names = NULL, stringsAsFactors = FALSE
    )
  }))
  if (!is.null(by_file)) by_file <- by_file[order(-by_file$cc_tot), ]

  structure(
    list(
      path = path, is_package = is_pkg, dirs = dirs,
      functions = d, by_file = by_file,
      summary = .summarise(a, basename(path)),
      patterns = a$patterns, ast_nodes = a$ast_nodes,
      duplicated_names = dup
    ),
    class = "repo_audit"
  )
}

#' @param x Oggetto `repo_audit`.
#' @param n Righe da mostrare per sezione.
#' @param ... Ignorato.
#' @rdname audit_repo
#' @export
print.repo_audit <- function(x, n = 20, ...) {
  d <- x$functions
  s <- x$summary
  rule <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")
  show <- function(df, cols) {
    if (!nrow(df)) {
      cat("  nessuna\n")
      return(invisible())
    }
    print(utils::head(df[, cols, drop = FALSE], n), row.names = FALSE)
    if (nrow(df) > n) cat(sprintf("  ... e altre %d\n", nrow(df) - n))
  }

  rule("QUADRO")
  cat(sprintf("repo            : %s\n", x$path))
  cat(sprintf("tipo            : %s\n", if (x$is_package) "pacchetto" else "repo di script"))
  cat(sprintf("cartelle lette  : %s\n", paste(x$dirs, collapse = ", ")))
  cat(sprintf("funzioni        : %d (%d esportate)\n", s$n_fn, s$n_exported))
  cat(sprintf("CC mediana / p90: %s / %s   (max %s)\n", s$cc_median, s$cc_p90, s$cc_max))
  if (length(x$duplicated_names)) {
    cat(sprintf("nomi definiti piu' volte, tenuto il primo: %s\n",
                paste(x$duplicated_names, collapse = ", ")))
  }

  rule("DOVE SI CONCENTRA LA COMPLESSITA")
  cat(sprintf("le prime 8 funzioni valgono il %.0f%% della complessita totale\n\n", 100 * s$top8_share))
  show(utils::head(d, 8), c("fn", "cc", "callers", "calls", "file"))

  rule("UN SOLO CHIAMANTE  (astrazione estratta e mai riusata)")
  cat(sprintf("%.0f%% delle funzioni interne\n\n", 100 * s$pct_1caller))
  show(d[d$callers == 1L & !d$exported, ], c("fn", "cc", "calls", "file"))

  rule("ZERO CHIAMANTI  (da verificare: il dispatch S3 non e' rilevato)")
  show(d[d$callers == 0L & !d$exported, ], c("fn", "cc", "in_tests", "file"))

  rule("ESISTONO SOLO PER I TEST")
  show(d[d$callers == 0L & !d$exported & d$in_tests, ], c("fn", "cc", "file"))

  rule("PATTERN, OGNI 1000 NODI DI AST")
  for (k in code_patterns) cat(sprintf("  %-18s %6.2f\n", k, x$patterns[[k]]))

  invisible(x)
}
