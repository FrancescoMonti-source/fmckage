#' Match multiple queries against EDSaN 'doceds' using d2imr and return binary indicators
#'
#' This function queries the EDSaN document database (module 'doceds') via the `d2imr` API,
#' applying multiple keyword searches defined in `query_list`. It returns a wide-format data frame
#' with one row per unique (PATID, EVTID) combination and one binary column per query, indicating
#' whether a match was found.
#'
#' @param query_list A named list of query strings. Each name will become a binary column in the output.
#' @param PATID Optional filter for patient ID. Defaults to "".
#' @param EVTID Optional filter for event ID. Defaults to "".
#' @param PATBD Optional filter for birth date (e.g., ">2000"). Defaults to "".
#' @param RECDATE Optional filter for record date (e.g., "<2025"). Defaults to "".
#' @param PATAGE Optional filter for patient age. Defaults to "".
#' @param PATSEX Optional filter for patient sex. Defaults to "".
#' @param SEJUM Optional filter for medical unit. Defaults to "".
#' @param SEJUF Optional filter for medical service. Defaults to "".
#' @param RECTYPE Optional filter for document type. Defaults to "".
#'
#' @return A data frame with (PATID, EVTID) and binary columns for each query. Absence of a match is left encoded as NA.
#'
#' @examples
#' query_list <- list(
#'   rachitisme = "rachitisme",
#'   osteoporose = "osteoporo* OR demineralis*"
#' )
#' query_match_binary_df_doceds(query_list, PATID = "123456", PATBD = ">2000", RECDATE = "<2025")
#'
#' @export
query_match_binary_df_doceds <- function(query_list = NULL, PATID = "", EVTID = "", PATBD = "", RECDATE = "", PATAGE = "", PATSEX = "", SEJUM = "", SEJUF = "", RECTYPE = "") {
    # Check for required packages
    required_packages <- c("d2imr","dplyr", "tidyr", "purrr")
    missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
    if (length(missing_packages) > 0) {
        stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "), ". Please install them."))
    }


    results <- purrr::map_dfr(names(query_list), function(query_name) {
        query <- query_list[[query_name]]

        res <- d2imr::d2im_wsc.get_edsan_idtriplets_as_dataframe(
            "doceds",
            paste0(
                '{"RECTXT":"', query, '",',
                '"PATBD":"', PATBD, '",',
                '"PATID":"', PATID, '",',
                '"EVTID":"', EVTID, '",',
                '"RECDATE":"', RECDATE, '",',
                '"PATAGE":"', PATAGE, '",',
                '"PATSEX":"', PATSEX, '",',
                '"SEJUM":"', SEJUM, '",',
                '"SEJUF":"', SEJUF, '",',
                '"RECTYPE":"', RECTYPE, '"}'
            ),
            "edsan"
        )

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
