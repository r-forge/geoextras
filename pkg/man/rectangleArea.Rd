\name{rectangleArea}
\alias{rectangleArea}
\alias{rA}
\alias{srA}
\alias{drA}
\alias{mrA}
\alias{hrA}

\title{Given rectangle code return area in square kilometers or nautical miles
}

\description{
  Rectangle area is returned through a call to 
  \code{\link{rectanglePerimeter}}-functions and \code{\link[geo]{geoarea}}.
}
\usage{
rA(r, scale = "nmi")
srA(sr, scale = "nmi")
drA(dr, dlat = 1, dlon = 2, scale = "km")
mrA(mr, dlat = 5, dlon = 10, scale = "nmi")
hrA(hr, scale = "nmi")
}

\arguments{
  \item{r, sr, dr, mr, hr}{rectangle code}
  \item{dlat, dlon}{either minutes or degrees latitude 
    determining rectangle size}
  \item{scale}{unit, either \code{nmi} or \code{km}, with historical defaults}
}

\keyword{manip}
\keyword{arith}
