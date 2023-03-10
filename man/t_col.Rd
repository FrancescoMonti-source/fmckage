\name{t_col}
\alias{t_col}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transparent colors
%%  ~~function to do ... ~~
}
\description{Sometimes you may be forced to specify a colour like in an if-else statement and the function you are using doesn't accept the "alpha" argument. This was my case using kableExtra and wanting to do conditional formatting of cell_spec. This function allows you to easily make transparent colors the rgb() command. These colors can be useful for charts and graphics with overlapping elements.

The rgb() command is the key: you define a new color using numerical values (0 to 255) for red, green and blue. In addition, you set an alpha value (also 0 to 255), which sets the transparency (0 being fully transparent and 255 being "solid").

You also need to set the maximum color value, so that the command can relate your alpha value to a level of transparency. In practice setting max = 255 works well (since RGB colors are usually defined in the range 0 to 255).


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
t_col(color, percent = 50, name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{color name}
  \item{percent}{\% of transparency}
  \item{name}{an optional name for the color
%%     ~~Describe \code{x} here~~
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
Mark Gardener 2015 @ www.dataanalytics.org.uk
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
The following example takes the standard blue and makes it transparent (~50\%):

mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")

mycol


## The function is currently defined as
function (x)
{
t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color

    ## Get RGB values for named color
    rgb.val <- col2rgb(color)

    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)

    ## Save the color
    invisible(t.col)
}
## END
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
