#' ffreq: Function to compute frequency counts and percentages
#'
#' This function calculates the frequency counts and percentages for specified columns in a data frame, optionally grouped by one or more grouping variables.
#'
#' @param data A data frame containing the data.
#' @param vars A character vector of variable names for which to compute frequency counts and percentages.
#' @param group_vars A character vector of variable names to group by (optional). Default is NULL.
#' @param na.rm A logical value indicating whether to remove NA values in the grouping variable(s). Default is TRUE.
#'
#' @return A data frame with frequency counts and percentages. If `group_vars` is provided, the output is grouped by these variables. The resulting data frame has columns for each variable, their counts, frequencies by group, and absolute frequencies.
#'
#' @examples
#' # Sample data
#' data <- tibble(
#'   PATSEX = rep(c("F", "M"), each = 10),
#'   migration_lithiasique = sample(c("Yes", "No"), 20, replace = TRUE),
#'   anticoagulants = sample(c("Yes", "No"), 20, replace = TRUE)
#' )
#'
#' # vars of interest
#' vars <- c("migration_lithiasique", "anticoagulants")
#'
#' # With grouping variable
#' result_with_group <- ffreq(data, vars, group_vars = "PATSEX")
#' print(result_with_group)
#'
#' # Without grouping variable
#' result_without_group <- ffreq(data, vars)
#' print(result_without_group)
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @export

ffreq <- function(data, vars, group_vars = NULL, na.rm = F) {

    calculate_frequencies_for_column <- function(data, column, group_vars, na.rm) {
        if (!is.null(group_vars)) {
            if (na.rm) {
                data <- data %>% filter(across(all_of(group_vars), ~!is.na(.)))
            }
            data %>%
                group_by(across(all_of(group_vars))) %>%
                count(!!sym(column)) %>%
                mutate(freq_by_group = round(n * 100 / sum(n), 2)) %>%
                ungroup() %>%
                mutate(freq_abs = round(n * 100 / sum(n), 2)) %>%
                rename(value = !!sym(column))
        } else {
            data %>%
                count(!!sym(column)) %>%
                mutate(freq_abs = round(n * 100 / sum(n), 2)) %>%
                rename(value = !!sym(column))
        }
    }

    result <- lapply(vars, function(col) {
        freqs <- calculate_frequencies_for_column(data, col, group_vars, na.rm)
        freqs %>% mutate(Variable = col) %>% select(Variable, value, everything())
    })

    combined_result <- bind_rows(result)
    return(combined_result)
}


