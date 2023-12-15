#' Calculate Total Stay Duration
#'
#' Computes the total duration of stays (nights spent) in a hospital or similar settings,
#' accounting for overlapping and non-overlapping intervals.
#'
#' @param data A dataframe containing the stay data.
#' @param id_col The name of the column in `data` that identifies each stay event or patient.
#' @param start_col The name of the column in `data` representing the start date of each stay.
#' @param end_col The name of the column in `data` representing the end date of each stay.
#' @return A dataframe with each unique identifier from `id_col` and the corresponding
#'         total duration of stays.
#' @export
#' @examples
#' # Assuming 'pmsi_general' is your dataframe and it has columns 'evtid' (event ID),
#' # 'datent' (admission date), and 'datsort' (discharge date):
#' # total_durations <- stay_duration(pmsi_general, 'evtid', 'datent', 'datsort')
#'
#' @import dplyr
#' @import purrr
#' @import lubridate
    stay_duration <- function(data, id_col, start_col, end_col, new_col_name = "stay_duration") {
        data %>%
            mutate(
                start = as.numeric(as.Date(!!sym(start_col))),
                end = as.numeric(as.Date(!!sym(end_col)))
            ) %>%
            filter(!is.na(start), !is.na(end)) %>%
            group_by(!!sym(id_col)) %>%
            summarise(sequences = list(map2(start, end, seq)), .groups = 'keep') %>%
            mutate(unioned = map(sequences, ~ reduce(.x, union))) %>%
            summarise(new_col_name = length(unique(unlist(unioned[1])))-1)
    }

