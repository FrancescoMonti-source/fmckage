#' Fix column names of a dataframe
#'
#' This function take a *df* as argument and return a *df* with modified *colnames* to make it easier to work with
#' variable names. Here's what it does: trims whitespace, replaces punctuation with underscores, transforms to lowercase,
#' and removes diacritic characters from the column names of the given dataframe.
#'
#' @param df A dataframe whose column names need to be "fixed".
#'
#' @return A dataframe with fixed column names.
#'
#' @examples
#' df <- data.frame("A .column " = 1:5, "b.Column" = 6:10, check.names = FALSE)
#' fix_colnames(df)
#'
#' @export
#'

fix_colnames <- function(df) {
  # Trim whitespace, replace dots with underscores, lowercase everything, and remove accents
  names(df) <- sapply(names(df), function(x) {
    x <- stringr::str_trim(x) %>%
      stringr::str_replace_all("[:punct:]", "_") %>%
      stringr::str_replace_all("\\s", "_") %>%
      tolower() %>%
      rm_accent()
  })

  return(df)
}
