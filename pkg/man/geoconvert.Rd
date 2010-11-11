\encoding{latin1}

\name{geoconvert}
\alias{geoconvert}
\alias{geoconvert.1}
\alias{geoconvert.2}

\title{Convert between degrees, decimal minutes ('dmm') and decimal degrees ('ddd')}

\description{
Conversion of positions between \sQuote{dmm} (degrees, minutes and decimal minutes) to \sQuote{ddd} (decimal degrees).
}

\usage{
geoconvert(data, inverse = FALSE, col.names = c("lat", "lon"))
geoconvert.1(x)
geoconvert.2(lat)
}

\arguments{
  \item{data}{
Positions in either a dataframe or a single variable.
}
  \item{inverse}{
logical, if \code{TRUE}, convert back to the more conventional \sQuote{dmm} format.
}
  \item{col.names}{
Names of columns of positions to convert if \code{data} is a data.frame.
}
  \item{x}{
Latitue or longitude or other data in \sQuote{dmm} (degrees, minutes and decimal minutes) format.
}
  \item{lat}{
Latitude or longitude or other data in \sQuote{ddd} (decimal degree) format.
}
}

\details{

}

\value{
  Positions in the desired format.
}

\author{
\enc{Höskuldur}{Hoskuldur} \enc{Björnsson}{Bjornsson}, borrowed from the \sQuote{geo} package.
}
\note{
Back conversion to \sQuote{dmm} only to 1/100th of a minute.
}

\examples{
  rvk <- geoconvert(data.frame(lat = 640900, lon = -215700))
  print(rvk)
  geoconvert(rvk, inverse = TRUE)
}

\keyword{manip}
\keyword{arith}
