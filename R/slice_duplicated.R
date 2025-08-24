#' Slice Duplicated Rows Based on Specific Columns
#'
#' This function allows you to select and inspect rows in a data frame that have duplicates based on specific columns.
#' It groups the data by the specified columns, filters out unique rows, and returns only the duplicated rows.
#'
#' @param data A data frame or tibble that you want to inspect for duplicate rows.
#' @param ... One or more unquoted column names that you want to check for duplicates. You can pass multiple columns to check for duplicates based on a combination of those columns.
#'
#' @return A data frame or tibble containing only the duplicated rows based on the specified columns.
#'
#' @examples
#' # Example dataset
#' data <- tibble::tibble(
#'   id = c(1, 2, 2, 3, 4, 4, 4),
#'   value = c("A", "B", "B", "C", "D", "D", "D"),
#'   date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-04", "2023-01-04"))
#' )
#'
#' # Use the function to select duplicated rows based on the 'id' column
#' slice_duplicated(data, id)
#'
#' # Use the function to select duplicated rows based on both 'id' and 'date' columns
#' slice_duplicated(data, id, date)
#'
#' @export
slice_duplicated <- function(data, ...) {
  data %>%
    group_by(across(c(...))) %>% # Group by the specified columns
    filter(n() > 1) %>% # Keep only groups with more than one row (i.e., duplicates)
    ungroup() # Ungroup the data
}
