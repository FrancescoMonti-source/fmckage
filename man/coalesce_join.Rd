\name{coalesce_join}
\alias{coalesce_join}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coalesce_join(x, y, by = NULL, suffix = c(".x", ".y"), join = dplyr::full_join, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{suffix}{
%%     ~~Describe \code{suffix} here~~
}
  \item{join}{
%%     ~~Describe \code{join} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
https://alistaire.rbind.io/blog/coalescing-joins/
}
\author{
%%  ~~who you are~~
Edward Visel: Odds, ends, and R code

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
function (x, y, by = NULL, suffix = c(".x", ".y"), join = dplyr::full_join,
    ...)
{
    joined <- join(x, y, by = by, suffix = suffix, ...)
    cols <- union(names(x), names(y))
    to_coalesce <- names(joined)[!names(joined) \%in\% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]),
        1, 2)]
    to_coalesce <- unique(substr(to_coalesce, 1, nchar(to_coalesce) -
        nchar(suffix_used)))
    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(joined[[paste0(.x,
        suffix[1])]], joined[[paste0(.x, suffix[2])]]))
    names(coalesced) <- to_coalesce
    dplyr::bind_cols(joined, coalesced)[cols]
  }
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
