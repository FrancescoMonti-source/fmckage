#' Convert Time Units
#'
#' This function converts time units between different scales such as seconds, minutes, hours, days, weeks, months, and years.
#'
#' @param time_data A numeric vector representing the time values to be converted.
#' @param from_unit A character string specifying the unit of the input \code{time_data}. Allowed values: "seconds", "minutes", "hours", "days", "weeks", "months", "years".
#' @param to_unit A character string specifying the desired output unit. Allowed values: "seconds", "minutes", "hours", "days", "weeks", "months", "years".
#' @param output_format A character string specifying the desired output format. Allowed values: "character" (default), "period", or "duration".
#'
#' @return A character string, lubridate period, or duration representing the converted time units, based on the \code{output_format}.
#'
#' @details The function provides three different output formats:
#' \itemize{
#'   \item "character" (default): Returns the result as a character string in the format "x unit y months", where "x" is the whole number of units and "y" is the fractional part in months.
#'   \item "period": Returns the result as a lubridate period object representing the converted time units.
#'   \item "duration": Returns the result as a lubridate duration object representing the converted time units.
#' }
#'
#' @examples
#' # Convert 182 months to years and months in character format
#' convert_time_units(182, "months", "years")
#'
#' # Convert 182 months to years and months as a lubridate period object
#' convert_time_units(182, "months", "years", output_format = "period")
#'
#' # Convert 182 months to years and months as a lubridate duration object
#' convert_time_units(182, "months", "years", output_format = "duration")
#'
#' @import lubridate
#' @importFrom utils readline
#'
#' @keywords time units conversion lubridate period duration
#'
#' @export

convert_time_units <- function (time_data, from_unit, to_unit, output_format = "character")
{
    if (!requireNamespace("lubridate", quietly = TRUE)) {
        cat("The 'lubridate' package is not installed.\n")
        install_choice <- readline(prompt = "Do you want to install and load it? (y/n): ")
        if (tolower(install_choice) == "y") {
            install.packages("lubridate")
            if (!requireNamespace("lubridate", quietly = TRUE)) {
                stop("Failed to install 'lubridate'. Please install it manually and try again.")
            }
            else {
                library(lubridate)
                cat("The 'lubridate' package has been installed and loaded successfully.\n")
            }
        }
        else {
            stop("The 'lubridate' package is required for this function. Please install it manually and try again.")
        }
    }
    else {
        if (!requireNamespace("lubridate", quietly = TRUE)) {
            library(lubridate)
            cat("The 'lubridate' package has been loaded successfully.\n")
        }
    }
    unit_in_seconds <- c(seconds = 1, minutes = 60, hours = 3600,
                         days = 86400, weeks = 604800, months = 2628000, years = 31536000)
    time_in_seconds <- time_data * unit_in_seconds[[from_unit]]
    result <- time_in_seconds/unit_in_seconds[[to_unit]]
    whole_units <- floor(result)
    fractional_part <- result - whole_units

    # output == character
    if (output_format == "character") {
        result_str <- paste(whole_units, to_unit)
        if (fractional_part > 0) {
            fractional_months <- round(fractional_part * 12)
            result_str <- paste(result_str, fractional_months,
                                "months")
        }
        result_str[is.na(time_data)] = NA
        return(result_str)
    }
    # output == period
    if (output_format == "period") {
        period <- as.period(result_str)
        period[is.na(time_data)] = NA
        return(period)
    }
    # output == duration
    if (output_format == "duration") {
        duration <- as.duration(result_str)
        duration[is.na(time_data)] = NA
        return(duration)
    }
    stop("Invalid 'output_format' specified. Use 'character', 'period', or 'duration'.")
}
