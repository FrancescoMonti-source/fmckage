#' Process PMSI Diagnosis Data
#'
#' This function processes diagnosis (diag) data by selecting relevant columns, unnesting, and separating diagnosis codes.
#'
#' @param data A dataframe containing PMSI data.
#' @return A transformed dataframe with diagnosis type and diagnosis codes.
#' @export
#' @examples
#' data <- data.frame()  # Example data
#' diag_data <- process_diag(data)
process_diag <- function(data) {
    data %>%
        select(ends_with("(?i)id"), PATAGE, PATSEX, DALL) %>%
        distinct() %>%
        unnest(DALL) %>%
        separate_rows(DALL, sep = " ") %>%
        filter(DALL != "") %>%
        mutate(
            diag = str_extract(DALL, ":(.+)", group = 1),
            type_diag = str_sub(DALL, 1, 2)
        ) %>%
        select(-DALL)
}
