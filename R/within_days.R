#' Check if two dates are within a certain number of days of each other
#'
#' @param x A Date. The first date to compare.
#' @param y A Date. The second date to compare.
#' @param days An integer. The maximum number of days apart the dates can be.
#' @return A logical. TRUE if the dates are within the specified number of days of each other, FALSE otherwise.
#' @export
within_days <- function(x, y, days = 2) {
    abs(as.numeric(difftime(x, y, units = "days"))) <= days
}
