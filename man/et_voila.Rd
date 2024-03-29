% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/et_voila.R
\name{et_voila}
\alias{et_voila}
\title{Generate a Sample with a Specific Aggregate Statistic}
\usage{
et_voila(n, min_val, max_val, target_value, agg_func = mean)
}
\arguments{
\item{n}{Integer. The size of the sample to generate.}

\item{min_val}{Numeric. The minimum value that can appear in the sample.}

\item{max_val}{Numeric. The maximum value that can appear in the sample.}

\item{target_value}{Numeric. The target value for the aggregate statistic.}

\item{agg_func}{Function. The aggregate function to use (e.g., `mean`, `median`). Default is `mean`.}
}
\value{
A numeric vector of length `n` that approximates the target aggregate statistic.
}
\description{
This function generates a sample of `n` numbers within a given range `[min_val, max_val]`
such that a specified aggregate statistic (e.g., mean, median) is approximately equal to a target value.
}
\examples{
\dontrun{
# Generate a sample with a target mean of 6
sample_data_mean <- et_voila(300, 1, 30, 6, mean)

# Generate a sample with a target median of 6
sample_data_median <- et_voila(300, 1, 30, 6, median)
}

}
