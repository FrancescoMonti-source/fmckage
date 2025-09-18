# -*- coding: UTF-8 -*-
#' Match multiple queries against EDSaN 'doceds' using d2imr and return binary indicators
#'
#' This function queries the EDSaN document database (module 'doceds') via the `d2imr` API,
#' applying multiple keyword searches defined in `query_list`. It returns a wide-format data frame
#' with one row per unique (PATID, EVTID) combination and one binary column per query, indicating
#' whether a match was found.
#'
#' @param query_list A named list of query strings. Each name will become a binary column in the output.
#' @param DALL Optional filter for the service who realised the CCAM act. Defaults to "".
#' @param PATID Optional filter for patient ID. Defaults to "".
#' @param EVTID Optional filter for event ID. Defaults to "".
#' @param PATBD Optional filter for birth date (e.g., ">2000"). Defaults to "".
#' @param DATENT Optional filter for day of hospitalisation. The scope is a single RUM not the entire hospital stay. Defaults to "".
#' @param DATSORT Optional filter for descharge day. The scope is a single RUM not the entire hospital stay. Defaults to "".
#' @param PATAGE Optional filter for patient age. Defaults to "".
#' @param PATSEX Optional filter for patient sex. Defaults to "".
#' @param SEJUM Optional filter for medical unit. Defaults to "".
#' @param SEJUF Optional filter for medical service. Defaults to "".
#' @param CODEACTE Optional filter CCAM acts. Defaults to "".
#' @param DATEACTE Optional filter CCAM acts date. Defaults to "".
#' @param UFPRO Optional filter for the service who realised the CCAM act. Defaults to "".
#' @param UFDEM Optional filter for the service who prescribed the CCAM act to be realised. Defaults to "".
#' @param NOMENCLATURE Optional filter for the nomenclature the specified act belongs to. Defaults to "".
#'
#' @return A data frame with (PATID, EVTID) and binary columns for each query. Absence of a match is left encoded as NA.
#'
#' @examples
#' query_list <- list(
#'   rachitisme = "E55*",
#'   osteoporose = "M80* OR M81*"
#' )
#' query_match_binary_df_doceds(query_list, PATID = "123456", PATBD = ">2000", RECDATE = "<2025")
#'
#' @export
query_match_binary_df_pmsi <- function(query_list = NULL,
                                       PATID = "",
                                       EVTID = "",
                                       ELTID = "",
                                       PATBD = "",
                                       PATAGE = "",
                                       PATSEX = "",
                                       SEJUM = "",
                                       SEJUF = "",
                                       DATENT = "",
                                       DATSORT = "",
                                       SEJDUR = "",
                                       MODEENT = "",
                                       MODESORT = "",
                                       PMSISTATUT = "",
                                       CODEACTE = "",
                                       DATEACTE = "",
                                       NOMENCLATURE = "",
                                       UFPRO = "",
                                       UFDEM = "") {

    results <- purrr::map_dfr(names(query_list), function(query_name) {
        query <- query_list[[query_name]]

        filters <- list(
            DALL = query,
            PATBD = PATBD,
            PATID = PATID,
            EVTID = EVTID,
            ELTID = ELTID,
            DATENT = DATENT,
            DATSORT = DATSORT,
            MODEENT = MODEENT,
            MODESORT = MODESORT,
            PMSISTATUT = PMSISTATUT,
            PATAGE = PATAGE,
            PATSEX = PATSEX,
            SEJUM = SEJUM,
            SEJUF = SEJUF,
            DATEACTE = DATEACTE,
            CODEACTE = CODEACTE,
            NOMENCLATURE = NOMENCLATURE,
            UFPRO = UFPRO,
            UFDEM = UFDEM
        )

        # Remove elements with empty string values
        filters <- Filter(function(x) x != "", filters)

        json_query <- jsonlite::toJSON(filters, auto_unbox = TRUE)

        #print(json_query)

        res <- d2imr::d2im_wsc.get_edsan_idtriplets_as_dataframe("pmsi", json_query, "edsan")

        if (is.null(res) || length(res) == 0) {
            return(NULL)
        }

        res <- tryCatch({
            res %>%
                apply(2, unlist) %>%
                as.data.frame(stringsAsFactors = FALSE) %>%
                `colnames<-`(toupper(names(.))) %>%
                dplyr::distinct(PATID, EVTID) %>%
                dplyr::mutate(CONDITION = query_name)
        }, error = function(e) {
            message(paste("Skipping", query_name, "due to error:", e$message))
            return(NULL)
        })

        res %>%
            dplyr::mutate(value = 1) %>%
            tidyr::pivot_wider(names_from = CONDITION, values_from = value, values_fill = 0)
    })

    return(results)
}
