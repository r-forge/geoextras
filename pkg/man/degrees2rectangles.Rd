\encoding{latin1}

\name{degrees2rectangles}
\alias{degrees2rectangles}
\alias{d2dr}
\alias{d2mr}
\alias{d2hr}
\alias{d2sr}
\alias{d2r}


\title{Given position return statistical rectangle}

\description{

  Functions that convert positions in decimal degrees (\sQuote{ddd}) latitude and longitude to rectangle codes in systems of various resolutions.

}
\usage{
d2dr(lat, lon = NULL, dlat = 1, dlon = 2)
d2mr(lat, lon = NULL, dlat = 5, dlon = 10)
d2hr(lat, lon = NULL)
d2sr(lat, lon = NULL)
d2r(lat, lon = NULL)
}

\arguments{

  \item{lat, lon}{position(s) as decimal degrees latitude and longitude. If \code{lat} is \code{list} its components \code{lat$lat} and \code{lat$lon} are used for \code{lat} and \code{lon}.}

  \item{dlat, dlon}{rectangle height and width in degrees and minutes
  latitude and longitude for \code{d2dr} and \code{d2mr} respectively}

}

\details{

 \itemize{

  \item \code{dr2d} for rectangles with resolution given in \code{dlat} by
  \code{dlon} degrees lat and lon,

  \item \code{mr2d} with resolution given in \code{dlat} by
  \code{dlon} minutes lat and lon,

  \item \code{hr2d} with a resolution of 10 min latitude by 1 deg
  longitude ("h" refering to hex),

  \item \code{sr2d} with a resolution of 15 min latitude by 30 min longitude in the
  Icelandic numbering system for statistical rectangles which starts
  counting at 60 deg N latitude, with sub-rectangles of 30 min lat by 1 deg lon
  coded 1, 2, 3 and 4 for the NW, NA, SW and SA quadrants respectively,

  \item \code{r2d} with a resolution of 30 min latitue and 1 deg longitude (the Icelandic numbering system).
}

  A small number (1e-06) is added to latitude and subtracted from
  longitude to ensure rectangle membership of positions on border are
  \dQuote{logical} on the nw-hempisphere. In line with whats implemented in
  'd2r' and 'd2sr' in package 'geo'.

}
\value{vector of rectangle codes in a variety of coding systems}

\note{\code{d2dr} returns negative rectangle codes in the western hemisphere. \code{d2mr} hasn't been made hemisphere-aware, and no attention has been given to making the functions work in the southern hemisphere.}

\author{

  \enc{Sigurður}{Sigurdur} \enc{Þór}{Thor} \enc{Jónsson}{Jonsson},
  except \code{d2sr} and \code{d2r} borrowed from the 'geo' package,
  author \enc{Höskuldur}{Hoskuldur} \enc{Björnsson}{Bjornsson}

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  \code{rectangles2degrees}
%% \link{\code{rectangles2degrees}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{

## aggregating from 30' lat by 1 deg lon to 1 deg lat by 2 deg lon

data(zbyrec)
zbyrec$dr <- d2dr(zbyrec)
regions <- names(zbyrec)[4:14]
zbydr <- aggregate(zbyrec[regions], 
  list(dr = zbyrec$dr), sum, na.rm = TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

