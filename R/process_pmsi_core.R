#' Process PMSI Raw Data into a Flat Data Frame
#'
#' This function takes a list of nested patient data objects (e.g., parsed from PMSI data),
#' flattens each patient record into a single-row data frame, and combines all patients
#' into a single tibble. It ensures consistent column types and prevents data loss due
#' to inconsistent structures by coercing all fields to character.
#'
#' @param data A list where each element represents a patient record (as a list of variables).
#'             The elements may be nested, and may contain atomic vectors or short lists.
#'
#' @return A tibble where each row corresponds to a patient and each column represents
#'         a flattened field. All values are character vectors; fields containing multiple
#'         values are collapsed into a `;`-separated string.
#'
#' @examples
#' raw_data <- list(
#'   list(PATID = "123", GHM = "04M07", DIAG = c("A123", "B456")),
#'   list(PATID = "456", GHM = "05K04", SEVERITE = 2)
#' )
#' processed <- process_pmsi_core(raw_data)
#' print(processed)
#'
#' @export
process_pmsi_core <- function(data) {
    clean_data <- lapply(data, function(x) {
        flat <- unlist(x, recursive = TRUE)

        # Ensure all values are character, collapse vectors
        flat <- lapply(flat, function(v) {
            if (length(v) > 1) paste(v, collapse = ";") else as.character(v)
        })

        as.data.frame(flat, stringsAsFactors = FALSE)
    }) %>%
        bind_rows()

    clean_data
}
