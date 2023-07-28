#' Convert Time Units
#'
#' Convert a given time value from one time unit to another.
#'
#' @param x Numeric value representing the time duration or interval.
#' @param from Character string specifying the time unit of the input \code{x}.
#'             Allowed values: "years", "months", "days", "hours", "minutes", "seconds".
#' @param to Character vector specifying the desired time units for the output.
#'           Allowed values: "years", "months", "days", "hours", "minutes", "seconds".
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

convert_time <- function(x, from = NULL, to = NULL) {
    if (is.null(from) || is.null(to)) {
        stop("Please specify both 'from' and 'to' parameters.")
    }

    time_units <- c("years" = 60 * 60 * 24 * 365.25,
                    "months" = 60 * 60 * 24 * 30.44,
                    "days" = 60 * 60 * 24,
                    "hours" = 60 * 60,
                    "minutes" = 60,
                    "seconds" = 1)

    # Convert the input to seconds (smallest unit)
    x_seconds <- x * time_units[from]

    # Initialize an empty list to store the output
    output <- list()

    # Calculate the amount for each unit in the desired units
    for (unit in to) {
        # Calculate the amount for this unit
        amount <- floor(x_seconds / time_units[unit])

        # Update the remaining seconds for the next iteration
        x_seconds <- x_seconds %% time_units[unit]

        # Store the amount and unit in the output list if it is greater than 0
        if (amount > 0) {
            output[[unit]] <- amount
        }
    }

    # Handle the residual part by calling convert_time recursively on the remaining seconds
    if (x_seconds > 0) {
        residual_time <- convert_time(x_seconds, from = "seconds", to = names(time_units))
        if (grepl("\\d", residual_time)) {
            message("There is still a residual part amounting to ", residual_time, " not being displayed.")
        }
    }

    # Combine the amounts and units into a single string
    result <- sapply(names(output), function(unit) {
        amount <- output[[unit]]
        if (!is.null(amount)) {
            paste0(amount, substr(unit, 1, 1))
        }
    })

    paste(result, collapse = " ")
}

