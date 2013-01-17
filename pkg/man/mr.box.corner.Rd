\name{mr.box.corner}
\alias{mr.box.corner}

\title{
Minute rectangle corner.
}

\description{
From a list of positions giving the corner of a rectangles in dimensions
of minutes plot a grid or fill with color.
}

\usage{
mr.box.corner(pos, dlat = 5, dlon = 10, corner = "NW", fill = F, ...)
}

\arguments{

  \item{pos}{
position to be 'put in a minute rectangle'. 
}
  \item{dlat, dlon}{
\code{lat}, \code{lon} dimensions of rectangles.
}
  \item{corner}{
default \code{NW}, other corners given by \code{NE}, \code{SW} and \code{SE}.
}
  \item{fill}{
Logical, default FALSE, whether or not to fill the rects.
}
  \item{\dots}{
further args to pass to \code{\link[geo]{geolines}} or \code{\link[geo]{geopolygon}}.
}

}

\value{
Adds to a current \code{geoplot}.
}

\note{
Esoteric, experimental.
}

\seealso{
  \code{\link[geo:deg2rect]{d2mr}}, 
  \code{\link[geo:rectGrid]{mrgrid}},
    \code{\link[geo]{geolines}}, \code{\link[geo]{geopolygon}}.
}
  
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
"mr.box.corner"
}

\keyword{aplot}
