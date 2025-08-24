#' Get Season from Date
#'
#' This function determines the meteorological season (Winter, Spring, Summer, or Fall)
#' from a given date.
#'
#' @param input.date A Date object or a character string representing a date.
#'
#' @return A character string representing the season of the input date: "Winter", "Spring", "Summer", or "Fall".
#'
#' @examples
#' getSeason(as.Date("2023-06-15"))
#' getSeason(as.Date("2023-12-15"))
#'
#' @note
#' The function uses the following date ranges to determine seasons:
#' - Winter: Dec 21 - Mar 19
#' - Spring: Mar 20 - Jun 20
#' - Summer: Jun 21 - Sep 21
#' - Fall: Sep 22 - Dec 20
#' Dates are treated as being in the northern hemisphere.
#'
#' @export
getSeason <- function(input.date) {
  numeric.date <- 100 * month(input.date) + day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0, 319, 0620, 0921, 1220, 1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  return(cuts)
}
