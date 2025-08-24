#' Match multiple keyword queries on a tCorpus and return binary indicators by group
#'
#' This function applies a set of keyword queries to a tokenized corpus (tCorpus) and returns a data frame
#' indicating whether each query matched for each unique combination of metadata variables (e.g., PATID, EVTID).
#'
#' @param tcorpus A `tCorpus` object created with `create_tcorpus()`.
#' @param query_list A named list of query strings. Each name will become a column in the output.
#' @param meta_vars Character vector of metadata variables used to group match results. Defaults to `c("PATID", "EVTID")`.
#' You can also include the following optional metadata fields:
#' \
#' - `RECDATE`: Date of the clinical record (e.g., note timestamp)
#' - `RECTYPE`: Type of clinical record (e.g., discharge summary, radiology report)
#' - `SEJUM`: Medical unit of the hospital stay
#' - `SEJUF`: Medical service of the hospital stay (each UM is composed by multiple UFs)
#' - `PATBD`: Patient birth date
#' - `PATAGE`: Patient age at time of encounter
#' - `PATSEX`: Patient sex (e.g., M/F)
#'
#' @return A wide-format data frame where each row is a unique combination of `meta_vars`, and each column corresponds to a query in `query_list`, with binary values (1 = match, 0 = no match).
#'
#' @examples
#' df <- data.frame(EVTID = 1:3, text = c("rachitisme", "osteoporose", "healthy"), PATID = c("P1", "P2", "P3"))
#' tc <- create_tcorpus(df, doc_col = "EVTID", text_col = "text", meta = c("PATID", "EVTID"))
#' qlist <- list(rachitisme = "rachitisme", osteoporose = "osteoporo*")
#' query_match_binary_df_tcorpus(tc, qlist)
#'
#' @export
query_match_binary_df_tcorpus <- function(tcorpus, query_list, meta_vars = c("PATID", "EVTID")) {
  # Check for required packages
  required_packages <- c("corpustools", "dplyr", "tidyr", "purrr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "), ". Please install them."))
  }

  # Check if input is a tCorpus
  if (!inherits(tcorpus, "tCorpus")) {
    stop("The input object must be a tCorpus. Use create_tcorpus() to create one.")
  }

  # Check if required metadata columns exist
  missing_meta <- setdiff(meta_vars, colnames(tcorpus$meta))
  if (length(missing_meta) > 0) {
    stop(paste("Missing metadata columns in tCorpus:", paste(missing_meta, collapse = ", ")))
  }

  all_hits <- purrr::map_dfr(names(query_list), function(condition) {
    query <- query_list[[condition]]
    subcorpus <- subset_query(tcorpus, query = query)

    if (nrow(subcorpus$meta) == 0) {
      return(NULL)
    }

    subcorpus$meta %>%
      dplyr::distinct(across(all_of(meta_vars))) %>%
      dplyr::mutate(CONDITION = condition)
  })

  if (nrow(all_hits) == 0) {
    return(data.frame())
  }

  all_hits %>%
    dplyr::mutate(value = 1) %>%
    tidyr::pivot_wider(names_from = CONDITION, values_from = value, values_fill = 0)
}
