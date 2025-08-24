#' Align Multiple Vectors by Matching Values
#'
#' `align_vectors` takes multiple vectors as input and aligns them by unique values,
#' creating a data frame with matched rows across all vectors and filling unmatched positions with `NA`.
#' This function is useful for comparing multiple sets of elements and aligning them based on common values.
#'
#' @param ... Vectors to be aligned. Any number of vectors can be provided, and they do not need to be of the same length.
#'            The names of the vectors in the argument list will be used as column names in the output data frame.
#'
#' @return A data frame where each row represents a unique element from the union of all input vectors. Columns are named
#'         after the input vectors. Each column aligns its values to this unique element list, and positions with no match
#'         are filled with `NA`. Rows with all `NA` values are removed.
#'
#' @examples
#' # Example vectors
#' x <- c("a", "b", "c", "d")
#' y <- c("a", NA, NA, "d", "e")
#' z <- c("f", "b", "d", NA, "a")
#'
#' # Align vectors x, y, and z
#' align_vectors(x, y, z)
#' # Expected Output:
#' #   x    y    z
#' # 1 a    a    a
#' # 2 b   NA    b
#' # 3 c   NA   NA
#' # 4 d    d    d
#' # 5 NA   e   NA
#' # 6 NA   NA   f
#'
#' @export
align_vectors <- function(...) {
  # Capture the vectors and their names
  vectors <- list(...)
  vector_names <- sapply(substitute(list(...))[-1], deparse)

  # Create a unique ordered list of all elements across all vectors
  all_elements <- unique(unlist(vectors))

  # Align each vector to `all_elements`, filling with NA where necessary
  aligned_data <- lapply(vectors, function(vec) {
    ifelse(all_elements %in% vec, all_elements, NA)
  })

  # Combine the aligned vectors into a data frame
  result <- as.data.frame(aligned_data)

  # Set column names using the captured names
  colnames(result) <- vector_names

  # Remove rows where all elements are NA
  result <- result[rowSums(is.na(result)) < ncol(result), ]

  return(result)
}
