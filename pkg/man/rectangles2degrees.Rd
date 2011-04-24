\name{rectangles2degrees}

\alias{rectangles2degrees}
\alias{dr2d}
\alias{mr2d}
\alias{hr2d}
\alias{sr2d}
\alias{r2d}

\title{Given statistical rectangle return its center position.}

\description{

  Functions that convert statistical rectangle codes under a number of
  coding systems (results of functions \code{d2dr}, \code{d2mr},
  \code{d2hr}, \code{d2sr} and \code{d2r}) to decimal representation of
  the rectangles center positions in degreees latitude and longitude.

}

\usage{
dr2d(dr, dlat = 1, dlon = 2)
mr2d(mr, dlat = 5, dlon = 10)
hr2d(hr)
sr2d(sr)
r2d(r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dr}{rectangle code \code{dr} from \code{d2dr}}
  \item{mr}{rectangle code \code{mr} from \code{d2mr}}
  \item{hr}{rectangle code \code{hr} from \code{d2hr}}
  \item{sr}{rectangle code \code{sr} from \code{d2sr}}
  \item{r}{rectangle code \code{r} from \code{d2r}}      

  \item{dlat}{rectangle height in degrees and minutes
  latitude for \code{dr2d} and \code{mr2d} respectively}
  \item{dlon}{As \code{dlat} except now in longitude}
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
}

\value{dataframe of center positions (latitude \code{lat} and longitude
  \code{lon}) of rectangles in one of the coding systems  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\note{Mostly used for plotting.}

\author{

  Sigurdur Thor Jonsson, except \code{sr2d} and \code{r2d} borrowed from
  the 'geo' package, author Hoskuldur Bjornsson

}

\seealso{\code{\link{degrees2rectangles}}}

\examples{
  dr2d(d2dr(data.frame(lat = 65, lon = -19), dlat = 1, dlon = 2),
    dlat = 1, dlon = 2)
  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

