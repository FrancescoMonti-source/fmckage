#' Plot Age Pyramid
#'
#' This function creates an age pyramid plot using ggplot2 for a given data frame
#' containing age and sex variables.
#'
#' @param data A data frame containing the age and sex variables.
#' @param age_var The name of the column in the \code{data} data frame representing age.
#' @param sex_var The name of the column in the \code{data} data frame representing sex.
#' @param breaks The breaks for the \code{cut()} function used to categorize ages into groups.
#' @param ... Additional arguments passed to ggplot for customization of the plot.
#'            For example, you can use \code{labs()}, \code{theme()}, etc.
#' @param install_ggplot2 Logical value indicating whether to install and load ggplot2
#'                         if it is not already available (default is \code{TRUE}).
#'
#' @return A ggplot2 plot representing the age pyramid.
#'
#' @examples
#' \dontrun{
#' # Sample data
#' etude <- data.frame(
#'   age = c(24, 45, 67, 38, 55, 28, 78, 63, 42, 55),
#'   sex = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
#' )
#'
#' # Plot age pyramid with customized breaks
#' plot_age_pyramid(etude, age_var = "age", sex_var = "sex", breaks = seq(0, 100, 10),
#'                  fill = "lightblue", color = "black")
#' }
#'
#' @import ggplot2
#' @importFrom dplyr count
#' @importFrom utils menu
#'
#' @keywords plot visualization age pyramid ggplot2
#' @export
plot_age_pyramid <- function(data, age_var, sex_var, breaks = NULL, ..., install_ggplot2 = TRUE) {
    # Check if ggplot2 is already loaded
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        if (install_ggplot2) {
            cat("The 'ggplot2' package is not loaded. Do you want to install and load it? (y/n): ")
            install_choice <- readline()
            if (tolower(install_choice) == "y") {
                install.packages("ggplot2")
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    stop("Failed to install 'ggplot2'. Please install it manually and try again.")
                } else {
                    library(ggplot2)
                    cat("The 'ggplot2' package has been installed and loaded successfully.\n")
                }
            } else {
                stop("The 'ggplot2' package is required for this function. Please install it manually and try again.")
            }
        } else {
            stop("The 'ggplot2' package is required for this function. Please install it manually and try again.")
        }
    } else {
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            library(ggplot2)
            cat("The 'ggplot2' package has been loaded successfully.\n")
        }
    }

    # Convert age_var to factor with customized breaks
    data <- data %>%
        mutate(!!age_var := cut(!!sym(age_var), breaks = breaks, ordered_result = TRUE))

    # Create the age pyramid plot
    plot <- ggplot(data, aes(x = !!sym(age_var), fill = !!sym(sex_var))) +
        geom_bar(position = "identity") +
        coord_flip() +
        ...

    return(plot)
}
