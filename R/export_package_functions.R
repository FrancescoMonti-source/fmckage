#' Export Functions from a Package
#'
#' This function exports all the functions from a specified R package into a single script file.
#' Each function is extracted and written to the file with its name, making it easy to source in another environment.
#'
#' @param package_name A string specifying the name of the package from which to export functions.
#' @param path A string specifying the path and name of the output file where the functions will be written.
#'
#' @return Invisible. The function writes to a file and does not return a value.
#'
#' @examples
#' export_package_functions("dplyr", "dplyr_functions.R")
#'
#' @note This function only exports R functions and does not handle dependencies or compiled code.
#' Ensure that the package's license allows for this kind of use.
#'
#' @export
#'

export_package_functions <- function(package_name, path) {
    # Check if the package is available
    if (!require(package_name, character.only = TRUE)) {
        stop("Package not found: ", package_name)
    }

    # List all objects in the package
    pkg_objects <- ls(paste0("package:", package_name), all.names = TRUE)

    # Filter out the function names
    function_names <- Filter(function(x) is.function(get(x, envir = asNamespace(package_name))), pkg_objects)

    # Open the output file with UTF-8 encoding
    output_file <- file(path, "w", encoding = "UTF-8")

    # Write each function's source code to the file
    for (fn in function_names) {
        fn_obj <- get(fn, envir = asNamespace(package_name))
        cat("\n\n# Function: ", fn, "\n", file = output_file)
        cat(fn, " <- ", file = output_file)
        dput(fn_obj, file = output_file)
    }

    # Close the file
    close(output_file)

    cat("Functions exported to: ", path, "\n")
}

