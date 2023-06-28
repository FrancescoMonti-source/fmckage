#' Find the mode(s) of a vector
#'
#' This function calculates the mode (most frequently occurring value) in a numeric or character vector.
#' In case of multiple modes, all modes are returned.
#'
#' @param x A numeric or character vector.
#'
#' @return A vector containing the mode(s) of the input vector. If the input vector has multiple modes, all will be returned.
#'
#' @examples
#' find_mode(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
#' find_mode(c("a", "b", "b", "c", "c", "c", "d", "d", "d", "d"))
#'
#' @export
find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
}
