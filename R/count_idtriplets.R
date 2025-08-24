#' Count Unique ID Triplets
#'
#' This function counts distinct values for specific columns in a dataset, which typically represent patient IDs, event IDs, or other identifier columns.
#' The function is case-insensitive and allows you to specify the columns to count.
#'
#' @param data A dataframe containing the data to analyze.
#' @param cols A character vector of column names to count distinct values from. Default is `c("patid", "evtid", "eltid", "doc_id", "pmsi_id")`.
#' If not provided, it attempts to find these columns in the data (case-insensitive).
#' @return This function prints the distinct counts for the specified columns in the dataset.
#' @export
#' @examples
#' # Example usage:
#' data <- data.frame(
#'   patid = c(1, 1, 2, 3),
#'   evtid = c(101, 102, 101, 103),
#'   eltid = c(201, 202, 201, 203),
#'   doc_id = c(301, 302, 301, 303),
#'   pmsi_id = c(401, 402, 401, 403)
#' )
#' count_idtriplets(data)
count_idtriplets <- function(data, cols = c("patid", "evtid", "eltid", "doc_id", "pmsi_id")) {
  # Normalize both the columns in the data and the provided column names to lowercase
  data_colnames <- tolower(names(data))

  # Automatically detect columns if `cols` is not explicitly passed
  if (missing(cols)) {
    # Check for default column names (case-insensitive)
    cols <- intersect(cols, data_colnames)
  } else {
    # Normalize user-supplied `cols` to lowercase
    cols <- tolower(cols)
  }

  # Loop through each column and find distinct values
  for (col in cols) {
    # Find the correct column name in the data (case-insensitive match)
    col_index <- match(col, data_colnames)
    if (!is.na(col_index)) {
      # Print the distinct count for the matched column
      col_name <- names(data)[col_index]
      cat(paste(col_name, ": n =", n_distinct(data[[col_name]]), "\n"))
    } else {
      # Skip missing columns and continue with the others
      cat(paste("Column", col, "not found in the data, skipping.\n"))
    }
  }
}
