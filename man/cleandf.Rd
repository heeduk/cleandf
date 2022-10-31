\name{cleandf}
\alias{abbr.para1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Abbreviate parameters in FQ set
%%  ~~function to do ... ~~
}
\description{
Return abbreviated parameters.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cleandf::abbr.para1(x, colname=TRUE, unit=FALSE, chem=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Dataframe to be abbreviated
%%     ~~Describe \code{x} here~~
}
  \item{colname}{
  Are the parameters column names? TRUE or FALSE
%%     ~~Describe \code{x} here~~
}
  \item{unit}{
  Do you want to include units? TRUE or FALSE
%%     ~~Describe \code{x} here~~
}
}
\details{
AIF, area internal firmness; AST, area skin strength; Dia, fruit diameter; INF, initial firmness; ICL, initial curve length; GRN, graininess; ISL, initial slope; MIF, mean internal firmness; Mold, mold development; SEL, skin elasticity; SI, storage index; Ssd, stem scar diameter; Sst, stem scar tearing; SST, skin strength; Ssw, wetness of stem scar; T0, week 0; T6, week 6; TAR, total area; TCL, total curve length; YM10, Young’s modulus 10; YM20, Young’s modulus 20; YM30, Young’s modulus 30; Wg, fruit weight; Wrk, rate of wrinkle.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Abbreviated parameter names will be returned.
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
Heeduk Oh
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
df <- cleandf::abbr.para1(df, colname=TRUE, unit = FALSE, chem=FALSE)
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
