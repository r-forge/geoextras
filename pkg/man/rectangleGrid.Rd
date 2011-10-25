\name{rectangleGrid}
\alias{rectangleGrid}
\alias{mrgrid}
\alias{drgrid}
\alias{rgrid}
\alias{srgrid}
\alias{hrgrid}

\title{Produce a grid of rectangles on a plot, filled with colors if desired}
\description{

  Functions that produce a line grid or filled with colors for a
  rectangle codes in systems of various resolutions, calling the
  perimeter utility functions (see \code{\link{rectanglePerimeters}}).
  
}
\usage{
mrgrid(mr, dlat = 5, dlon = 10, fill = F, col = "black", ...)
drgrid(mr, dlat = 1, dlon = 2, fill = F, col = "black", ...)
rgrid(mr, fill = F, col = "black", ...)
srrgrid(mr, fill = F, col = "black", ...)
hrgrid(mr, fill = F, col = "black", ...)
}


\arguments{

  \item{mr, dr, r, sr, hr}{ Rectangle codes in the different
  resolutions, \code{mr} dimensions based on minutes, \code{dr}
  dimensions based on degrees, \code{r}, \code{sr} and \code{hr} with
  their own system (see \sQoute{degrees2rectangles}).  }

\item{dlat, dlon}{Dimensions of latitude and longitude given in minutes
  and degrees for \code{mrgrid} and \code{drgrid}, respectively}

  \item{fill}{Should the grid be filled with colors?}

  \item{col}{Fill color, either only one color or a vector of length
  equal to the number of rectangles}

  \item{\dots}{Other arguments passed to \code{\link{geolines}} or \code{\link{geopolygon}}}
  
}
\details{
Maybe later
}

\value{
Returns \sQuote{invisible}, and a grid as a side-effect on the plotting device
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Sigurdur Thor Jonsson
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
function (mr, dlat = 5, dlon = 10, fill = F, col = "black", ...) 
{
  mr <- lapply(mr, mrPeri, dlat = dlat, dlon = dlon)
  if(fill) {
    if(length(col) == 1) {
      invisible(lapply(mr, geopolygon, col = col, ...))
    }
    else {
      if(length(mr) != length(col)) {
        stop("More than one color but not as many as the rectangles")
      }
      else {
        invisible(lapply(1:length(mr), function(i, mr, col)
          geopolygon(mr[[i]], col = col[i], ...), mr = mr, col = col, ...))
      }
    }
  }
  else {
    if(length(col) == 1) {
      invisible(lapply(mr, geolines,  col = col, ...))
    }
    else {
      if(length(mr) != length(col)) {
        stop("More than one color but not as many as the rectangles")
      }
      else {
        invisible(lapply(1:length(mr), function(i, mr, col, ...)
          geolines(mr[[i]], col = col[i], ...), mr = mr, col = col, ...))
      }
    }
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
