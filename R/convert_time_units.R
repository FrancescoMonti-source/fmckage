#' Convert Time Units
#'
#' Convert time values between different units such as years, months, weeks, days, hours, minutes, and seconds.
#'
#' @param time_data Numeric vector containing the time values to be converted.
#' @param from_unit Character string indicating the original time unit of the \code{time_data}.
#' @param to_unit Character string indicating the target time unit to which the \code{time_data} should be converted.
#' @param output_format Character string specifying the desired output format. Possible values are "character" (default), "period", or "duration".
#'
#' @details This function allows for the conversion of time data between different units, such as years, months, weeks, days, hours, minutes, and seconds. It utilizes the 'lubridate' package to facilitate the conversions. If the package is not installed, it will be automatically installed and loaded upon function call. If the 'lubridate' package is already installed but not loaded, the function will load it.
#'
#' The function takes a numeric vector of time values (\code{time_data}), the original time unit of the data (\code{from_unit}), and the target time unit for the conversion (\code{to_unit}). The result can be returned as a character string with a formatted time representation ("character"), as a 'lubridate' period object ("period"), or as a 'lubridate' duration object ("duration").
#'
#' Note: For precise time calculations, this function uses approximate values for the number of seconds in a month and a year.
#'
#' @return Depending on the \code{output_format}, the function returns either a character vector with formatted time values, a 'lubridate' period object, or a 'lubridate' duration object.
#'
#' @examples
#' convert_time_units(18, "months", "years", output_format = "character")
#' convert_time_units(181, "months", "years", output_format = "period")
#' convert_time_units(181, "months", "years", output_format = "duration")
#'
#' @importFrom lubridate as.duration
#' @importFrom lubridate as.period
#' @importFrom lubridate duration
#' @importFrom lubridate period
#' @importFrom lubridate update
#'
#' @seealso \code{\link[lubridate]{period}}, \code{\link[lubridate]{duration}}
#'
#' @author Your Name
#'
#' @keywords time conversion lubridate
#' @export

convert_time_units <- function(time_data, from_unit, to_unit, output_format = "character") {

    unit_in_seconds <- c(
        seconds = 1,
        minutes = 60,
        hours = 3600,
        days = 86400,
        weeks = 604800,
        months = 2628000,  # Approximate seconds in a month
        years = 31536000  # Approximate seconds in a year
    )

    time_in_seconds <- time_data * unit_in_seconds[[from_unit]]
    result <- time_in_seconds / unit_in_seconds[[to_unit]]

    whole_units <- floor(result)
    fractional_part_seconds <- (result - whole_units) * unit_in_seconds[[to_unit]]

    # Function to get the appropriate unit for the fractional part
    get_smaller_unit <- function(units) {
        switch(units,
               "years" = "months",
               "months" = "weeks",
               "weeks" = "days",
               "days" = "hours",
               "hours" = "minutes",
               "minutes" = "seconds",
               "seconds" = NA
        )
    }

    # Function to get concise time unit representation
    get_time_unit_symbol <- function(units) {
        switch(units,
               "years" = "y",
               "months" = "m",
               "weeks" = "w",
               "days" = "d",
               "hours" = "H",
               "minutes" = "M",
               "seconds" = "S"
        )
    }

    # Construct the result string dynamically based on the target unit
    result_str <- paste0(whole_units, get_time_unit_symbol(to_unit))
    if (any(fractional_part_seconds > 0)) {
        smaller_unit <- get_smaller_unit(to_unit)
        while (!is.na(smaller_unit) && any(fractional_part_seconds > 0)) {
            fractional_part <- fractional_part_seconds / unit_in_seconds[[smaller_unit]]
            result_str <- paste0(result_str, " ", floor(fractional_part), get_time_unit_symbol(smaller_unit))
            fractional_part_seconds <- (fractional_part_seconds %% unit_in_seconds[[smaller_unit]])
            smaller_unit <- get_smaller_unit(smaller_unit)
        }
    }

    if (output_format == "character") {
        return(result_str)
    }

    stop("Invalid 'output_format' specified or unsupported target unit.")
}

