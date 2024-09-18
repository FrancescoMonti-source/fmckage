#' Wrapper Function to Process PMSI Data
#'
#' This function serves as a wrapper to process PMSI data by calling the `process_actes`, `process_diag`, and `process_main` functions.
#'
#' @param data A dataframe containing PMSI data.
#' @return Three dataframes: main PMSI data, actes data, and diagnosis data. The dfs are named "x", "x_doag" and "x_actes".
#' @export
#' @examples
#' data <- data.frame()  # Example data
#' process_pmsi(data)
process_pmsi <- function(data) {
    data_name <- deparse(substitute(data))

    cleaned_data <- process_pmsi_core(data)

    pmsi_actes <- process_actes(cleaned_data)
    pmsi_diag <- process_diag(cleaned_data)
    pmsi_main <- process_main(cleaned_data)

    assign(data_name, pmsi_main, envir = .GlobalEnv)
    assign(paste0(data_name, "_actes"), pmsi_actes, envir = .GlobalEnv)
    assign(paste0(data_name, "_diag"), pmsi_diag, envir = .GlobalEnv)
}
