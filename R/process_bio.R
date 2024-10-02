#' Process Bio Data
#'
#' This function processes bio data by first cleaning the dataset by unlisting and removing specified columns.
#' It then reshapes the data by selecting relevant columns, including bio-specific variables such as "PATID",
#' "EVTID", "ELTID", and results-related information. The function uses "pivot_longer" to convert wide-format
#' columns into long-format based on the presence of specific bio-related terms and returns a distinct set of results.
#'
#' @param data A list or data frame containing bio data to be processed. Each entry is expected to be a named list.
#' @param drop_cols A character vector of column names to be removed during the cleaning process. Default is c("CSTE_LABO").
#'
#' @return A data frame where bio-related data is reshaped into long format, with distinct rows of results.
#' The columns selected include "PATID", "EVTID", "ELTID", "PATBD", "PATAGE", "PATSEX", "DATEXAM", "SEJUM",
#' "SEJUF", and those containing the string "RESULT". Additionally, columns with the strings "TYPEANA",
#' "NUMRES", "STRRES", "LOINC", "CR", and "CMT" are reshaped into long format.
#'
#' @details
#' The function performs the following steps:
#' 1. Unlists all elements in the data and removes columns specified in `drop_cols`.
#' 2. Selects specific columns for bio data analysis.
#' 3. Reshapes certain columns (based on "TYPEANA", "NUMRES", "STRRES", "LOINC", "CR", "CMT") into a long format.
#' 4. Returns distinct rows to remove duplicates.
#'
#' @examples
#' # Example usage:
#' bio_data <- list(
#'     list(PATID = 1, EVTID = 101, ELTID = 1001, PATAGE = 45, PATSEX = "M", DATEXAM = "2024-01-01", RESULT1 = "Normal"),
#'     list(PATID = 2, EVTID = 102, ELTID = 1002, PATAGE = 50, PATSEX = "F", DATEXAM = "2024-01-02", RESULT1 = "High")
#' )
#' result <- process_bio(bio_data)
#' print(result)
#'
#' @import dplyr tidyr
#' @export

process_bio <- function(data, drop_cols = c("CSTE_LABO")) {
    # Core processing: clean and unlist the data, removing filter columns
    clean_data <- lapply(data, unlist) %>%
        lapply(function(x) x[!names(x) %in% drop_cols]) %>%
        bind_rows()

    # Continue with processing specific to bio data
    clean_data %>%
        select(PATID,EVTID,ELTID, PATBD, PATAGE, PATSEX, DATEXAM, SEJUM, SEJUF, contains("RESULT")) %>%
        pivot_longer(
            cols = c(contains("TYPEANA"), contains("NUMRES"), contains("STRRES"), contains("LOINC"), contains("CR"), contains("CMT")),
            names_to = c(".value"),
            names_pattern = "(TYPEANA|NUMRES|STRRES|LOINC|CR|CMT).+",
            values_drop_na = TRUE
        ) %>%
        distinct()
}
