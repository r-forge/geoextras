\name{gcd.longlat}
\alias{gcd.longlat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Great circle distance
}
\description{
 Function that determines great circle distance between two positions given in lat and long. Fairly good agreement with function 'arcdist' in package 'geo'.
}
\usage{
gcd.longlat(tlat, tlon, rlat, rlon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tlat}{
%%     ~~Describe \code{tlat} here~~
}
  \item{tlon}{
%%     ~~Describe \code{tlon} here~~
}
  \item{rlat}{
%%     ~~Describe \code{rlat} here~~
}
  \item{rlon}{
%%     ~~Describe \code{rlon} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tlat, tlon, rlat, rlon) 
{
    track <- matrix(c(tlon, rlon, tlat, rlat), ncol = 2, dimnames = list(NULL, 
        c("x", "y")))
    LineLength(track, longlat = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
