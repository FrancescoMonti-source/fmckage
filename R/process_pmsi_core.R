#' Core PMSI Data Processing Function
#'
#' This function applies core data cleaning and processing logic to PMSI data.
#'
#' @param data A list or dataframe containing PMSI data.
#' @param filter_cols A character vector of columns to exclude during processing. Defaults to c("GHM", "SRC", "SEVERITE").
#' @return A cleaned dataframe after removing filtered columns and unlisting the data.
#' @export
#' @examples
#' data <- list()  # Example data
#' clean_data <- process_pmsi_core(data)
process_pmsi_core <- function(data, filter_cols = c("GHM", "SRC", "SEVERITE")) {
    clean_data <- lapply(data, unlist) %>%
        lapply(function(x) x[!names(x) %in% filter_cols]) %>%
        bind_rows()

    clean_data
}
