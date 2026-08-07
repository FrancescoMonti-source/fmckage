# audit_library.R
# Misura tutti i pacchetti installati per costruire una base di confronto, e ci
# colloca dentro i propri repo.
#
# Serve a rendere interpretabili i numeri di audit_repo(): "il 46% delle
# funzioni ha un solo chiamante" non dice niente finche' non si sa quanto fa
# quel numero altrove.

#' Misura tutti i pacchetti installati
#'
#' Costruisce la distribuzione di riferimento contro cui leggere l'audit di un
#' repo. I pacchetti installati non contengono i sorgenti, quindi la misura
#' passa dal namespace (si veda [fn_objects_in_namespace()]).
#'
#' @section Effetti collaterali:
#' Misurare un pacchetto richiede `loadNamespace()`, che esegue il suo
#' `.onLoad()` -- lo stesso codice che gira a ogni `library()`. Sono pacchetti
#' gia' installati sulla macchina. I pacchetti che non si caricano vengono
#' saltati e riportati in `attr(x, "failed")`.
#'
#' @param packages Nomi da misurare. Default: tutti gli installati.
#' @param min_functions Scarta i pacchetti sotto questa soglia: sotto le ~10
#'   funzioni le percentuali sono rumore.
#' @param quiet Se `FALSE`, stampa l'avanzamento (l'operazione dura minuti).
#' @return Data frame, una riga per pacchetto, con le stesse colonne di
#'   `audit_repo(...)$summary` piu' `version` e `priority`.
#' @examples
#' \dontrun{
#' base <- audit_library()
#' saveRDS(base, "~/baseline_cran.rds")
#' }
#' @seealso [audit_repo()], [compare_to_library()]
#' @export
audit_library <- function(packages = NULL, min_functions = 10, quiet = FALSE) {
  ip <- utils::installed.packages()
  if (is.null(packages)) packages <- rownames(ip)
  rows <- list()
  failed <- character()

  for (i in seq_along(packages)) {
    p <- packages[[i]]
    r <- tryCatch(
      {
        suppressMessages(suppressWarnings(loadNamespace(p)))
        fns <- fn_objects_in_namespace(p)
        if (length(fns) < min_functions) NULL else {
          s <- .summarise(.analyse_functions(fns, getNamespaceExports(p)), p)
          s$version <- as.character(utils::packageVersion(p))
          s$priority <- if (p %in% rownames(ip) && !is.na(ip[p, "Priority"])) ip[p, "Priority"] else ""
          s
        }
      },
      error = function(e) NA
    )
    if (identical(r, NA)) {
      failed <- c(failed, p)
      next
    }
    if (is.null(r)) next
    rows[[length(rows) + 1L]] <- r
    if (!quiet) cat(sprintf("[%3d/%3d] %-28s %4d fn\n", i, length(packages), p, r$n_fn))
  }

  out <- do.call(rbind, rows)
  attr(out, "failed") <- failed
  out
}

#' Dove vive la baseline su questa macchina
#'
#' La baseline descrive la libreria R *di questa macchina*, non un progetto:
#' non ha senso versionarla in un repo. Sta in `tools::R_user_dir()`, la
#' cartella cache dell'utente, che sopravvive alla reinstallazione del
#' pacchetto e all'aggiornamento di R -- cosa che `.libPaths()` non fa.
#'
#' @return Percorso del file `.rds`, esista o no.
#' @export
baseline_path <- function() {
  file.path(tools::R_user_dir("fmckage", "cache"), "library_baseline.rds")
}

#' Baseline dei pacchetti installati, con cache su disco
#'
#' Restituisce la baseline salvata, calcolandola alla prima chiamata. Serve a
#' non rifare ogni volta un'operazione che dura minuti.
#'
#' La baseline invecchia: riflette i pacchetti installati e la versione di R al
#' momento in cui e' stata costruita, entrambi registrati fra gli attributi.
#' Dopo un aggiornamento importante della libreria, `refresh = TRUE`.
#'
#' @param refresh Se `TRUE`, ricalcola e sovrascrive la copia salvata.
#' @param ... Passati a [audit_library()].
#' @return Data frame come [audit_library()], con attributi `built_at`,
#'   `r_version` e `lib_paths`.
#' @examples
#' \dontrun{
#' b <- library_baseline()          # la prima volta calcola, poi legge
#' attr(b, "built_at")
#' compare_to_library(audit_repo("~/Git/redsan"), b)
#' }
#' @seealso [baseline_path()], [audit_library()]
#' @export
library_baseline <- function(refresh = FALSE, ...) {
  p <- baseline_path()
  if (!refresh && file.exists(p)) return(readRDS(p))

  b <- audit_library(...)
  attr(b, "built_at") <- Sys.time()
  attr(b, "r_version") <- as.character(getRversion())
  attr(b, "lib_paths") <- .libPaths()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  saveRDS(b, p)
  message("baseline salvata in ", p)
  b
}

#' Colloca un repo nella distribuzione dei pacchetti installati
#'
#' Per ogni metrica restituisce il valore del repo, il percentile che occupa
#' nella base di confronto e la mediana di riferimento.
#'
#' @section Come leggerlo:
#' I pacchetti su CRAN *sono* librerie per un pubblico ampio: sulla superficie
#' pubblica e sulla difensivita' sono il termine di paragone sbagliato per del
#' codice interno, e uno scarto verso il basso li' non e' un difetto. Sulla
#' complessita' per funzione e sull'organizzazione interna il confronto regge.
#'
#' @param x Oggetto `repo_audit` da [audit_repo()], o il suo `$summary`.
#' @param baseline Data frame da [audit_library()]. Default: la copia salvata
#'   su questa macchina, costruita alla prima chiamata.
#' @param contributed_only Se `TRUE`, esclude i pacchetti base e recommended,
#'   scritti in un'epoca e con vincoli diversi.
#' @param metrics Metriche da confrontare. Default: tutte quelle comuni.
#' @return Data frame con `metric`, `value`, `percentile`, `baseline_median`.
#' @examples
#' \dontrun{
#' base <- audit_library()
#' compare_to_library(audit_repo("~/Git/redsan"), base)
#' }
#' @export
compare_to_library <- function(x, baseline = library_baseline(),
                               contributed_only = TRUE, metrics = NULL) {
  s <- if (inherits(x, "repo_audit")) x$summary else x
  b <- if (contributed_only && "priority" %in% names(baseline)) {
    baseline[is.na(baseline$priority) | baseline$priority == "", ]
  } else {
    baseline
  }

  if (is.null(metrics)) {
    metrics <- c(
      "cc_median", "cc_p90", "top8_share", "pct_1caller", "pct_0callers",
      .pat_col(code_patterns)
    )
    metrics <- intersect(metrics, intersect(names(s), names(b)))
  }

  out <- data.frame(
    metric = metrics,
    value = vapply(metrics, function(m) as.numeric(s[[m]]), numeric(1)),
    percentile = vapply(metrics, function(m) {
      100 * mean(b[[m]] <= as.numeric(s[[m]]), na.rm = TRUE)
    }, numeric(1)),
    baseline_median = vapply(metrics, function(m) {
      stats::median(b[[m]], na.rm = TRUE)
    }, numeric(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
  attr(out, "n_baseline") <- nrow(b)
  out
}
