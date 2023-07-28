#' Convert Time Units
#'
#' Converts a given time value from one unit to another.
#'
#' @param x Numeric value representing the time duration.
#' @param from Character string indicating the starting time unit.
#'             Possible values: "years", "months", "days", "hours", "minutes", "seconds".
#' @param to Character vector indicating the desired time units for conversion.
#'           Possible values: "years", "months", "days", "hours", "minutes", "seconds".
#' @return Character string representing the converted time duration in the specified units.
#' @details This function converts a given time value 'x' from one time unit to another.
#'          The 'from' parameter specifies the starting time unit of 'x', and the 'to'
#'          parameter specifies the desired time units for the conversion. The function
#'          returns a character string representing the converted time duration in the
#'          specified units.
#' @examples
#' convert_time(9000, from = "seconds", to = c("hours", "minutes"))
#' # Returns: "2h 30m"
#'
#' convert_time(1.22, from = "years", to = c("days", "hours", "minutes"))
#' # Returns: "445d 14h 48m"
#'
#' @note The 'from' and 'to' parameters must be specified to avoid any possible mistakes.
#'       If any of them are missing, the function will throw an error message.
#'
#' @export
convert_time <- function(x, from = NULL, to = NULL) {
    # Function implementation (as provided earlier)
}


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

    # Combine the amounts and units into a single string
    result <- sapply(names(output), function(unit) {
        amount <- output[[unit]]
        if (!is.null(amount)) {
            paste0(amount, substr(unit, 1, 1))
        }
    })

    paste(result, collapse = " ")
}
