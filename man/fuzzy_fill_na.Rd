\name{Fuzzy_fill_NA}
\alias{Fuzzy_fill_NA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fuzzy fill NA
%%  ~~function to do ... ~~
}
\description{
%%  This function performs a fuzzy join between two dataframes based on specified columns and matching functions.
It then fills NA values in the first dataframe with values from the second dataframe.
Optionally, it can replace specified columns in the first dataframe with their counterparts in the second dataframe, and keep specified columns from the second dataframe in the result.
}
\usage{
Fuzzy_fill_NA(df1, df2, match_cols, match_fun_list, fill_cols, replace_cols = NULL, keep_cols = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1}{A dataframe. The dataframe to fill NA values in.}
  \item{df2}{A dataframe. The dataframe to fill NA values from.}
  \item{match_cols}{A character vector. The names of the columns to perform the fuzzy join on.}
  \item{match_fun_list}{A list of functions. The matching functions to use for each column in match_cols.}
  \item{fill_cols}{A character vector. The names of the columns to fill NA values in.}
  \item{replace_cols}{A character vector, optional. The names of the columns to replace in df1 with their counterparts in df2.}
  \item{keep_cols}{A character vector, optional. The names of the columns from df2 to keep in the result.}
  \item{stringdist_threshold}{A numeric value, optional. The maximum string distance for a match. Default is 2.}
  \item{stringdist_method}{A character string, optional. The method to use for string distance calculation.
  Can be one of "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", or "soundex".
  Default is "lv" (Levenstein). For more information about these methods, see \code{\link[stringdist]{stringdist}}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe. The first dataframe with NA values filled and specified columns replaced and kept.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

fuzzy_fill_na <- function(df1, df2, match_cols, match_fun_list, fill_cols, replace_cols = NULL, keep_cols = NULL) {
    # Perform the fuzzy join
    join_result <- df1 \%>\%
        fuzzyjoin::fuzzy_left_join(df2,
                                   by = match_cols,
                                   match_fun = match_fun_list)

    # Update fill_cols to reflect the column names after the join
    fill_cols <- paste0(fill_cols, ".x")

    # Fill in missing values in df1 with values from df2
    for (col in fill_cols) {
        join_result <- join_result \%>\%
            dplyr::mutate({{col}} := coalesce(.data[[col]], .data[[gsub(".x",".y", col)]]))
    }

    # Replace specified columns in df1 with their counterparts in df2
    if (!is.null(replace_cols)) {
        replace_cols <- paste0(replace_cols, ".x")
        for (col in replace_cols) {
            join_result <- join_result \%>\%
                dplyr::mutate({{col}} := .data[[gsub(".x",".y", col)]])
        }
    }

    # Keep specified columns from df2
    if (!is.null(keep_cols)) {
        keep_cols <- paste0(keep_cols, ".y")
    } else {
        keep_cols <- character(0)
    }

    # Keep only the original columns from df1 and specified columns from df2
    df_filled <- join_result \%>\%
        dplyr::select(c(dplyr::ends_with(".x"), keep_cols)) \%>\%
        dplyr::rename_with(~ gsub("\\.x$", "", .
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
