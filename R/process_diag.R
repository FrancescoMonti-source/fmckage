#' Process PMSI Diagnosis Data
#'
#' This function processes diagnosis (diag) data by selecting relevant columns, unnesting, and separating diagnosis codes.
#'
#' @param data A dataframe containing PMSI data.
#' @return A transformed dataframe with diagnosis type and diagnosis codes.
#' @export
#' @examples
#' data <- data.frame() # Example data
#' diag_data <- process_diag(data)
process_diag <- function(data) {
    data %>%
        select(ends_with("ID"), DATENT, DATSORT, DALL) %>%
        separate_rows(DALL, sep = " ") %>%
        mutate(
            DATENT  = lubridate::as_datetime(DATENT),
            DATSORT = lubridate::as_datetime(DATSORT),
            HEURE_DATENT   = lubridate::hour(DATENT),
            MINUTE_DATENT  = lubridate::minute(DATENT),
            SECOND_DATENT  = lubridate::second(DATENT),
            HEURE_DATSORT  = lubridate::hour(DATSORT),
            MINUTE_DATSORT = lubridate::minute(DATSORT),
            SECOND_DATSORT = lubridate::second(DATSORT)
        ) %>%
        dplyr::filter(stringr::str_detect(DALL, "\\d")) %>%
        mutate(
            HEURE_DATENT  = hms::hms(hours = HEURE_DATENT,  minutes = MINUTE_DATENT,  seconds = SECOND_DATENT),
            HEURE_DATSORT = hms::hms(hours = HEURE_DATSORT, minutes = MINUTE_DATSORT, seconds = SECOND_DATSORT),
            diag      = stringr::str_extract(DALL, ":(.+)", group = 1),
            type_diag = stringr::str_sub(DALL, 1, 2)
        ) %>%
        select(-DALL, -contains("MINUTE"), -contains("SECOND"))
}
