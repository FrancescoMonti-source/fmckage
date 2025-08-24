#' Perform a fuzzy join between two dataframes and fill NA values
#'
#' This function performs a fuzzy left join between two dataframes based on specified matching columns and functions.
#' It then fills NA values in the specified columns of the first dataframe with the corresponding values from the second dataframe.
#' Beware that if a column in the first dataframe is specified in fill_cols or replace_cols arguments but does not exist in the second dataframe,
#' the column in the resulting dataframe will be filled with NA values.
#'
#' @param df1 A dataframe. The dataframe to fill NA values in.
#' @param df2 A dataframe. The dataframe to fill NA values from.
#' @param match_cols A character vector. The names of the columns to match on.
#' @param match_fun_list A list of functions. The matching functions to use for each column specified in match_cols.
#' @param fill_cols A character vector. The names of the columns in df1 to fill NA values in.
#' @param replace_cols A character vector, optional. The names of the columns in df1 to replace values in. Default is NULL.
#' @param keep_cols A character vector, optional. The names of the columns in df2 to keep in the result. Default is NULL.
#' @return A dataframe. The first dataframe with NA values filled.
#' @examples
#' \dontrun{
#' # Create some synthetic data
#' df1 <- data.frame(
#'   name = c("John", "Alice", "Bob", "Charlie"),
#'   birthdate = as.Date(c("2000-01-01", "2000-02-02", "2000-03-03", "2000-04-04")),
#'   city = c("New York", "Los Angeles", "Chicago", "San Francisco"),
#'   id = c(NA, "123", "456", NA),
#'   stringsAsFactors = FALSE
#' )
#' df2 <- data.frame(
#'   name = c("John", "Alice", "Bob", "Charlie", "Charlie"),
#'   birthdate = as.Date(c("2000-01-05", "2000-02-02", "2000-03-03", "2000-04-01", "2000-04-07")),
#'   city = c("New York", "Los Angeles", "Chicago", "San Francisco", "San Francisco"),
#'   id = c("789", "123", "456", "789", "012"),
#'   stringsAsFactors = FALSE
#' )
#' # Define the matching columns and functions
#' match_cols <- c("name", "birthdate", "city")
#' match_fun_list <- list(`stringdist::stringdist`, `%within%`, `stringdist::stringdist`)
#' # Define the columns to fill
#' fill_cols <- c("id")
#' # Perform the fuzzy join and fill NA values
#' df_filled <- fuzzy_fill_na(df1, df2, match_cols, match_fun_list, fill_cols)
#' }
#' @export



fuzzy_fill_na <- function(df1, df2, match_cols, match_fun_list, fill_cols, replace_cols = NULL, keep_cols = NULL) {
  # Perform the fuzzy join
  join_result <- df1 %>%
    fuzzyjoin::fuzzy_left_join(df2,
      by = match_cols,
      match_fun = match_fun_list
    )

  # Update fill_cols to reflect the column names after the join
  fill_cols <- paste0(fill_cols, ".x")

  # Fill in missing values in df1 with values from df2
  for (col in fill_cols) {
    join_result <- join_result %>%
      dplyr::mutate({{ col }} := coalesce(.data[[col]], .data[[gsub(".x", ".y", col)]]))
  }

  # Replace specified columns in df1 with their counterparts in df2
  if (!is.null(replace_cols)) {
    replace_cols <- paste0(replace_cols, ".x")
    for (col in replace_cols) {
      join_result <- join_result %>%
        dplyr::mutate({{ col }} := .data[[gsub(".x", ".y", col)]])
    }
  }

  # Keep specified columns from df2
  if (!is.null(keep_cols)) {
    keep_cols <- paste0(keep_cols, ".y")
  } else {
    keep_cols <- character(0)
  }

  # Keep only the original columns from df1 and specified columns from df2
  original_cols <- names(df1)
  df_filled <- join_result %>%
    dplyr::select(c(any_of(original_cols), dplyr::ends_with(".x"), keep_cols)) %>%
    dplyr::rename_with(~ gsub("\\.x$", "", .x), -all_of(keep_cols)) %>%
    dplyr::rename_with(~ paste0(gsub("\\.y$", "", .x), "_df2"), all_of(keep_cols))

  return(df_filled)
}
