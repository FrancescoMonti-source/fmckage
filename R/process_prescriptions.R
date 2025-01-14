process_prescriptions = function(data){
    # Get the name of the passed data object
    data_name <- deparse(substitute(data))

    # Handle edge cases where data_name might return an empty string or incorrect value
    if (data_name == ".") {
        stop("The name of the data object couldn't be determined. Please do NOT use a pipeline but the
             form process_pmsi(x).")
    }

    data = data %>%
        lapply(unlist) %>%
        bind_rows %>%
        select(ends_with("ID"), PATBD, PATAGE, PATSEX, DATENT, DATSORT, SEJUM, SEJUF, contains("PRES")) %>%
        pivot_longer(cols = contains("PRES"),
                     names_to = ".value",
                     names_pattern = "PRES\\.?(.+?)\\d*$")

    # Optional: Print a message to confirm creation
    print(paste("Created objects:", data_name, ". Prescriptions processed."))

    return(data)
}
