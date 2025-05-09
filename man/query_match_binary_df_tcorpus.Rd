% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_match_binary_df_tcorpus.R
\name{query_match_binary_df_tcorpus}
\alias{query_match_binary_df_tcorpus}
\title{Match multiple keyword queries on a tCorpus and return binary indicators by group}
\usage{
query_match_binary_df_tcorpus(
  tcorpus,
  query_list,
  meta_vars = c("PATID", "EVTID")
)
}
\arguments{
\item{tcorpus}{A `tCorpus` object created with `create_tcorpus()`.}

\item{query_list}{A named list of query strings. Each name will become a column in the output.}

\item{meta_vars}{Character vector of metadata variables used to group match results. Defaults to `c("PATID", "EVTID")`.
You can also include the following optional metadata fields:
\
- `RECDATE`: Date of the clinical record (e.g., note timestamp)
- `RECTYPE`: Type of clinical record (e.g., discharge summary, radiology report)
- `SEJUM`: Medical unit of the hospital stay
- `SEJUF`: Medical service of the hospital stay (each UM is composed by multiple UFs)
- `PATBD`: Patient birth date
- `PATAGE`: Patient age at time of encounter
- `PATSEX`: Patient sex (e.g., M/F)}
}
\value{
A wide-format data frame where each row is a unique combination of `meta_vars`, and each column corresponds to a query in `query_list`, with binary values (1 = match, 0 = no match).
}
\description{
This function applies a set of keyword queries to a tokenized corpus (tCorpus) and returns a data frame
indicating whether each query matched for each unique combination of metadata variables (e.g., PATID, EVTID).
}
\examples{
df <- data.frame(EVTID = 1:3, text = c("rachitisme", "osteoporose", "healthy"), PATID = c("P1", "P2", "P3"))
tc <- create_tcorpus(df, doc_col = "EVTID", text_col = "text", meta = c("PATID", "EVTID"))
qlist <- list(rachitisme = "rachitisme", osteoporose = "osteoporo*")
query_match_binary_df_tcorpus(tc, qlist)

}
