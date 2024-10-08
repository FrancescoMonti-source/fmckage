% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_actes.R
\name{process_actes}
\alias{process_actes}
\title{Process PMSI Actes Data}
\usage{
process_actes(data)
}
\arguments{
\item{data}{A dataframe containing PMSI data.}
}
\value{
A transformed dataframe containing relevant "actes" data.
}
\description{
This function processes PMSI "actes" data by pivoting and transforming relevant columns.
}
\examples{
data <- data.frame()  # Example data
actes_data <- process_actes(data)
}
