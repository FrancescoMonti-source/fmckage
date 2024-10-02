#' Wrapper Function to Process PMSI Data
#'
#' This function serves as a wrapper to process PMSI data by calling the `process_actes`, `process_diag`, and `process_main` functions.
#'
#' @param data A dataframe containing PMSI data.
#' @return Three dataframes: main PMSI data, actes data, and diagnosis data. The dfs are named "x", "x_doag" and "x_actes".
#' @export
#' @examples
#' data <- data.frame()  # Example data
#' process_pmsi(data)

# Wrapper function to call the other three processes and return with specific names
process_pmsi <- function(data) {
    # Get the name of the passed data object
    data_name <- deparse(substitute(data))

    # Handle edge cases where data_name might return an empty string or incorrect value
    if (data_name == ".") {
        stop("The name of the data object couldn't be determined. Please do NOT use a pipeline but the
             form process_pmsi(x).")
    }

    # Call the core function once and store the result
    cleaned_data <- process_pmsi_core(data)

    # Process each subset of the data
    pmsi_actes <- process_actes(cleaned_data)
    pmsi_diag <- process_diag(cleaned_data)
    pmsi_main <- process_main(cleaned_data)

    # Dynamically assign the resulting dataframes to variables with appropriate names
    assign(data_name, pmsi_main, envir = .GlobalEnv)
    assign(paste0(data_name, "_actes"), pmsi_actes, envir = .GlobalEnv)
    assign(paste0(data_name, "_diag"), pmsi_diag, envir = .GlobalEnv)

    # Optional: Print a message to confirm creation
    print(paste("Created objects:", data_name, paste0(data_name, "_actes"), paste0(data_name, "_diag")))
}
