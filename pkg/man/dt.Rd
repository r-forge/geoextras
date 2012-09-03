\name{dt}
\alias{dt}
\docType{data}
\title{
A set of unique taggging and recapture localities
}
\description{
Pairs of taggging and recapture positions used in developing a method to
find the shortest distance 
}
\usage{data(dt)}
\format{
  A data frame with 5045 observations on the following 4 variables.
  \describe{
    \item{\code{tlat}}{a numeric vector}
    \item{\code{tlon}}{a numeric vector}
    \item{\code{rlat}}{a numeric vector}
    \item{\code{rlon}}{a numeric vector}
  }
}
\details{
\itemize{
 \item 'tlat', 'tlon': tagging position in lat./long.
 \item 'rlat', 'rlon': recapture position in lat./long.
}
}
\source{
%%  ~~ reference to a publication or URL from which the data were
%%  obtained ~~
%%  ~~ reference to Eydna i Homrum et al. in review???
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dt)
## maybe str(dt) ; plot(dt) ...
}
\keyword{datasets}
