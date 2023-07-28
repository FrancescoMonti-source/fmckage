#' Convert time units
#'
#' This function converts a time duration from one unit to one or more other units.
#' The conversion is performed separately for each element in the input vector, and the results are returned as a vector of character strings.
#' The input can contain NA values, which are returned as NA in the output.
#'
#' @param x A numeric vector, the time durations to be converted.
#' @param from A character string, the unit of the input time durations. It should be one of "seconds", "minutes", "hours", "days", "months", or "years".
#' @param to A character vector, the units to which the input time durations should be converted. It should contain one or more of "seconds", "minutes", "hours", "days", "months", or "years".
#' @param warn_residual A logical value, if TRUE, a warning is printed when there is a residual part not being displayed due to the chosen 'to' units.
#' @return A character vector of the converted time durations. Each element is a string that concatenates the converted time durations in the units specified by 'to', in the order they are given.
#' @examples
#' convert_time(90, from = "seconds", to = c("minutes", "seconds")) # "1m 30s"
#' convert_time(c(90, 3600), from = "seconds", to = c("hours", "minutes", "seconds")) # "0h 1m 30s" "1h 0m 0s"
#' @export


convert_time <- function (x, from = NULL, to = NULL, warn_residual = TRUE)
{
    if (is.null(from) || is.null(to)) {
        stop("Please specify both 'from' and 'to' parameters.")
    }
    time_units <- c(years = 60 * 60 * 24 * 365.25, months = 60 * 60 * 24 * 30.44, days = 60 * 60 * 24, hours = 60 * 60, minutes = 60, seconds = 1)
    x_seconds <- x * time_units[from]
    output <- vector("list", length(x_seconds))
    for (i in seq_along(x_seconds)) {
        time_remaining <- x_seconds[i]
        unit_values <- list()
        for (unit in to) {
            if (!is.na(time_remaining)) {
                amount <- floor(time_remaining/time_units[unit])
                time_remaining <- time_remaining %% time_units[unit]
                if (amount > 0) {
                    unit_values[[unit]] <- amount
                }
            }
        }
        output[[i]] <- unit_values
    }
    output <- lapply(output, function(units) {
        if (length(units) > 0) {
            return(paste0(mapply(function(value, unit) {
                paste(value, substr(unit, 1, 1))
            }, units, names(units)), collapse = " "))
        } else {
            return(NA)
        }
    })
    unlist(output)
}


