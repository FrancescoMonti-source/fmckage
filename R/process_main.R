#' Process PMSI Main Data
#'
#' This function processes the main PMSI data, including admission and discharge dates, patient age and sex, and stay duration.
#'
#' @param data A dataframe containing PMSI data.
#' @return A transformed dataframe containing main PMSI data fields.
#' @export
#' @examples
#' data <- data.frame()  # Example data
#' main_data <- process_main(data)
process_main <- function(data) {
    data %>%
        select(PATID,EVTID,ELTID, DATENT, DATSORT, PATBD, PATAGE, PATSEX, SEJDUR, MODEENT, MODESORT, PMSISTATUT, SEJUM, SEJUF) %>%
        distinct()
}
