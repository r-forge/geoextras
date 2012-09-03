\name{crossesLand}
\alias{crossesLand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Crosses land
}
\description{
 Function that determines whether or not the great circle connecting tag and return positions crosses land as given in 'land.raster'
}
\usage{
crossesLand(tx, ty, rx, ry, land.raster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tx}{
%%     ~~Describe \code{tx} here~~
}
  \item{ty}{
%%     ~~Describe \code{ty} here~~
}
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
}
  \item{land.raster}{
%%     ~~Describe \code{land.raster} here~~
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
function (tx, ty, rx, ry, land.raster) 
{
    trLine <- Lines(list(Line(matrix(c(tx, ty, rx, ry), ncol = 2, 
        byrow = TRUE))), ID = "ln")
    trLine <- SpatialLines(list(trLine), proj4string = CRS("+proj=merc +datum=WGS84"))
    any(!is.na(unlist(extract(land.raster, trLine))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
