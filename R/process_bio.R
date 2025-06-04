#' Process Bio Data (Fast Version with Package Check)
#'
#' Efficiently flattens a nested list of bio lab data where each element is named by biol_ID
#' and contains patient metadata and a nested RESULTATS dataframe. Uses data.table for performance.
#'
#' @param data A named list where each element is a list containing metadata and a RESULTATS dataframe.
#'
#' @return A long-format data.frame with one row per lab result, including patient/test metadata.
#'
#' @export
process_bio <- function(data) {
    # Ensure data.table is available
    if (!requireNamespace("data.table", quietly = TRUE)) {
        message("The 'data.table' package is required but not installed.")
        answer <- readline("Would you like to install it now? [y/n]: ")
        if (tolower(answer) == "y") {
            install.packages("data.table")
        } else {
            stop("Cannot continue without 'data.table'. Please install it and try again.")
        }
    }

    # Load data.table (if not already)
    if (!"data.table" %in% .packages()) {
        suppressPackageStartupMessages(library(data.table))
    }

    all_results <- vector("list", length(data))

    for (i in seq_along(data)) {
        biol_id <- names(data)[i]
        entry <- data[[i]]

        if (!"RESULTATS" %in% names(entry) || is.null(entry$RESULTATS)) next

        results <- entry$RESULTATS
        n <- nrow(results)
        if (n == 0) next

        meta <- data.frame(
            PATID     = entry$PATID,
            EVTID     = entry$EVTID,
            ELTID     = entry$ELTID,
            biol_ID   = biol_id,
            DATEXAM   = entry$DATEXAM,
            SEJUM     = entry$SEJUM,
            SEJUF     = entry$SEJUF,
            PATBD     = entry$PATBD,
            PATAGE    = entry$PATAGE,
            PATSEX    = entry$PATSEX,
            CSTE_LABO = entry$CSTE_LABO,
            stringsAsFactors = FALSE
        )

        all_results[[i]] <- cbind(meta[rep(1, n), ], results)
    }

    data.table::rbindlist(all_results, fill = TRUE)
}
