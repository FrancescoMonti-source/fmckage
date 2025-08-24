#' Get Names of Columns with Only NA Values
#'
#' This function returns the names of the columns in a data frame or tibble that contain only `NA` values.
#'
#' @param df A data frame or tibble.
#'
#' @return A character vector containing the names of the columns where all values are `NA`.
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 3),
#'   b = c(NA, NA, NA),
#'   c = c(4, NA, 6)
#' )
#' na_only_cols(df)
#' # Returns: "b"
#'
#' @export
na_only_cols <- function(df) {
  names(df)[colSums(!is.na(df)) == 0]
}
