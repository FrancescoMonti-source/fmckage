#' Calculate the Difference in Days to the Previous Specified Weekday
#'
#' This function calculates the number of days between a given date and the nearest previous occurrence of a specified weekday.
#'
#' @param target_date A date object or a character string representing the starting date.
#' @param weekday_name A character string representing the name of the desired previous weekday (e.g., "samedi" for Saturday in French).
#'
#' @return An integer representing the number of days difference between the target_date and the nearest previous occurrence of the specified weekday.
#'
#' @examples
#' difftime_previous_weekday("2019-07-03", "samedi")
#' difftime_previous_weekday(as.Date("2021-12-15"), "mardi")
#'
#' @import lubridate
#' @export
difftime_previous_weekday <- function(target_date, weekday_name) {
  # Convert target_date to a Date object if it's not already
  target_date <- as.Date(target_date)

  # Loop to find the difference in days
  for (X in 0:365) {
    new_date <- target_date - days(X)
    if (weekdays(new_date, abbreviate = FALSE) == weekday_name) {
      return(X)
    }
  }
}
