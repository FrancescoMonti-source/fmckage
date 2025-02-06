#' Process PMSI Actes Data
#'
#' This function processes PMSI "actes" data by pivoting and transforming relevant columns.
#'
#' @param data A dataframe containing PMSI data.
#' @return A transformed dataframe containing relevant "actes" data.
#' @export
#' @examples
#' data <- data.frame()  # Example data
#' actes_data <- process_actes(data)
process_actes <- function(data) {
    data %>%
        select(ends_with("(?i)id"), PATBD, PATAGE, PATSEX, DATENT, DATSORT, SEJDUR, SEJUM, SEJUF, contains("ACTE")) %>%
        pivot_longer(
            cols = c(contains("CODEACTE"), contains("DATEACTE"), contains("UFPRO"), contains("UFDEM"), contains("NOMENCLATURE")),
            names_to = c(".value"),
            names_pattern = "(CODEACTE|DATEACTE|UFPRO|UFDEM|NOMENCLATURE).*",
            values_drop_na = TRUE
        ) %>%
        distinct()
}

