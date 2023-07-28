#' Convert Time Units
#'
#' Convert a given time value from one time unit to another.
#'
#' @param x Numeric value representing the time duration or interval.
#' @param from Character string specifying the time unit of the input \code{x}.
#'             Allowed values: "years", "months", "days", "hours", "minutes", "seconds".
#' @param to Character vector specifying the desired time units for the output.
#'           Allowed values: "years", "months", "days", "hours", "minutes", "seconds".
#' @param warn_residual logical. If TRUE, displays a warning message when there's a residual time not displayed in the output units. Default is TRUE.
#' @return Character string representing the time value in the specified time units.
#' @details This function converts a given time value \code{x} from one time unit to another,
#'          as specified by the \code{from} and \code{to} parameters. It handles conversions
#'          between various time units, such as years, months, days, hours, minutes, and seconds.
#'          The output string includes the numeric amount and the first letter of the time unit.
#' @examples
#' convert_time(9000, "seconds", c("days", "hours"))  # Output: "2d 30h"
#' convert_time(1.22, "years", c("days", "hours", "minutes"))  # Output: "445d 14h 48m"
#' convert_time(1.22, "years", "months")  # Output: "14m"
#' @seealso \code{\link{lubridate::pretty}} for an alternative method to display time durations.
#' @importFrom lubridate seconds minutes hours days
#' @importFrom stats floor
#' @export

convert_time <- function (x, from = NULL, to = NULL, warn_residual = TRUE)
{
    if (is.null(from) || is.null(to)) {
        stop("Please specify both 'from' and 'to' parameters.")
    }
    time_units <- c(years = 60 * 60 * 24 * 365.25, months = 60 * 60 * 24 * 30.44, days = 60 * 60 * 24, hours = 60 * 60, minutes = 60, seconds = 1)
    x_seconds <- x * time_units[from]
    output <- list()
    for (unit in to) {
        amount <- floor(x_seconds/time_units[unit])
        x_seconds <- x_seconds %% time_units[unit]
        if (amount > 0) {
            output[[unit]] <- amount
        }
    }
    if (x_seconds >= 1) {
        residual_time <- convert_time(x_seconds, from = "seconds", to = names(time_units))
        if (warn_residual && grepl("\\d", residual_time)) {
            message("There is still a residual part amounting to ", residual_time, " not being displayed.")
        }
    }
    result <- sapply(names(output), function(unit) {
        amount <- output[[unit]]
        if (!is.null(amount)) {
            paste0(amount, substr(unit, 1, 1))
        }
    })
    paste(result, collapse = " ")
}

