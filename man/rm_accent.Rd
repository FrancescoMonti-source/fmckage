\name{rm_accent}
\alias{rm_accent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Remove diacritic characters
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Remove all diaritic characters from the argument. Takea a vector/matrix/dataframe as input, outputs a vector.
}
\usage{
rm_accent(str, pattern = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{a vector or a dataframe
%%     ~~Describe \code{str} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
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
df = cbind(acute = "áéíóúÁÉÍÓÚýÝ",
      grave = "àèìòùÀÈÌÒÙ",
      circunflex = "âêîôûÂÊÎÔÛ",
      tilde = "ãõÃÕñÑ",
      umlaut = "äëïöüÄËÏÖÜÿ",
      cedil = "çÇ")

rm_accent(df)


## The function is currently defined as
function(str,pattern="all") {
    if(!is.character(str))
        str <- as.character(str)

    pattern <- unique(pattern)

    if(any(pattern=="Ç"))
        pattern[pattern=="Ç"] <- "ç"

    symbols <- c(
        acute = "áéíóúÁÉÍÓÚýÝ",
        grave = "àèìòùÀÈÌÒÙ",
        circunflex = "âêîôûÂÊÎÔÛ",
        tilde = "ãõÃÕñÑ",
        umlaut = "äëïöüÄËÏÖÜÿ",
        cedil = "çÇ"
    )

    nudeSymbols <- c(
        acute = "aeiouAEIOUyY",
        grave = "aeiouAEIOU",
        circunflex = "aeiouAEIOU",
        tilde = "aoAOnN",
        umlaut = "aeiouAEIOUy",
        cedil = "cC"
    )

    accentTypes <- c("´","`","^","~","¨","ç")

    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern))
        return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

    for(i in which(accentTypes%in%pattern))
        str <- chartr(symbols[i],nudeSymbols[i], str)

    return(str)
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
