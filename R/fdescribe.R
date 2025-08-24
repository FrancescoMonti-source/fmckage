#' fdescribe: Function to compute descriptive statistics
#'
#' This function computes descriptive statistics (mean, standard deviation, min, max, range, and standard error) for specified variables in a data frame, optionally grouped by one or more grouping variables.
#'
#' @param data A data frame containing the data.
#' @param vars A character vector of variable names for which to compute summary statistics.
#' @param group_vars A character vector of variable names to group by (optional).
#' @param names_sep A character string according to which split the variables' names
#'
#'
#' @return A data frame with descriptive statistics. If `group_vars` is provided, the output is grouped by these variables. The resulting data frame has statistics as columns and the specified variables as rows.
#'
#' @examples
#' # Sample data
#' data <- tibble(
#'   PATSEX = rep(c("F", "M"), each = 10),
#'   PATAGE = rnorm(20, mean = 50, sd = 10),
#'   taille = rnorm(20, mean = 170, sd = 5)
#' )
#'
#' # With grouping variable
#' result_with_group <- fdescribe(data, vars = c("PATAGE", "taille"), group_vars = "PATSEX")
#' print(result_with_group)
#'
#' # Without grouping variable
#' result_without_group <- fdescribe(data, vars = c("PATAGE", "taille"))
#' print(result_without_group)
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
fdescribe <- function(data, vars, group_vars = NULL, names_sep = "_") {
  summary_data <- if (!is.null(group_vars)) {
    data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        n = n(),
        across(
          all_of(vars),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            min = ~ min(.x, na.rm = TRUE),
            max = ~ max(.x, na.rm = TRUE),
            range = ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
            se = ~ sd(.x, na.rm = TRUE) / sqrt(n())
          )
        )
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  } else {
    data %>%
      summarise(
        n = n(),
        across(
          all_of(vars),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            min = ~ min(.x, na.rm = TRUE),
            max = ~ max(.x, na.rm = TRUE),
            range = ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
            se = ~ sd(.x, na.rm = TRUE) / sqrt(n())
          )
        )
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  }

  df_long <- summary_data %>% pivot_longer(
    cols = matches(paste(vars, collapse = "|")),
    names_to = c("Variable", "Statistic"),
    names_sep = names_sep,
    values_to = "Value"
  )

  if (!is.null(group_vars)) {
    df_wide <- df_long %>%
      pivot_wider(
        names_from = "Statistic",
        values_from = "Value"
      ) %>%
      arrange(Variable, across(all_of(group_vars))) %>%
      relocate(Variable, .before = all_of(group_vars)) %>%
      mutate(Variable = str_to_title(Variable))
    names(df_wide) <- str_to_title(names(df_wide))
  } else {
    df_wide <- df_long %>%
      pivot_wider(
        names_from = "Statistic",
        values_from = "Value"
      ) %>%
      arrange(Variable) %>%
      relocate(Variable) %>%
      mutate(Variable = str_to_title(Variable))
    names(df_wide) <- str_to_title(names(df_wide))
  }

  return(df_wide)
}
