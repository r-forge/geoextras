\name{rectangleArea}
\alias{rectangleArea}
\alias{drArea}
\alias{mrArea}
\alias{hrArea}
\alias{srArea}
\alias{rArea}

\title{Given rectangle code return area in square kilometers or nautical miles
}
\description{Rectangle area is returned through a call to
\code{\link{rectanglePerimeter}}-fuyntions and \code{\link[geo]{geoarea}}.
}
\usage{drPeri(dr, dlat = 1, dlon = 2)
}

\arguments{\item{r, sr, mr, dr}{rectangle code}
  \item{dlat, dlon}{either minutes or degrees latitude determining rectangle size}
}

\keyword{manip}
\keyword{arith}
