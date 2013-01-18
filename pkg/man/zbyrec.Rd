\encoding{latin1}

\name{zbyrec}

\alias{zbyrec}

\docType{data}

\title{Zonal(by EEZs) proportions of ICES rectangles in N-Atlantic.
}

\description{

  A dataframe giving percentages of statistical rectangles (0.5°
latitude by 1° longitude) within EEZs (Exclusive Economic Zones) in the
North Atlantic as agreed by NEAFC (North East Atlantic Fisheries
Comission) in negotiations on blue whiting zonal attachment using an
ICES (International Council for the Exploration of the Sea) rectangle
coding system.

}

\usage{data(zbyrec)}

\format{
  A data frame with 4510 observations on the following 16 variables.
  \describe{
    \item{\code{rect}}{a character vector}
    \item{\code{area}}{a numeric vector}
    \item{\code{ices.div}}{a character vector}
    \item{\code{eu}}{a numeric vector}
    \item{\code{norway}}{a numeric vector}
    \item{\code{iceland}}{a numeric vector}
    \item{\code{russia}}{a numeric vector}
    \item{\code{faroe}}{a numeric vector}
    \item{\code{svalbard}}{a numeric vector}
    \item{\code{janmayen}}{a numeric vector}
    \item{\code{int.north}}{a numeric vector}
    \item{\code{int.west}}{a numeric vector}
    \item{\code{greenland}}{a numeric vector}
    \item{\code{grayzone}}{a numeric vector}
    \item{\code{lat}}{a numeric vector}
    \item{\code{lon}}{a numeric vector}
  }
}

\details{
\itemize{
 \item Column 'rect' is a ICES-code for a rectangle, 
 \item 'area' code in an unknown area coding system, 
 \item 'ices.div' is ICES division, 
 \item columns 'eu' through 'grayzone' give zonal attachment in percentages, agreed on by a working group. Note that these percentages are only approximate, and
could be reworked using functions in this package and 'geo', 
 \item columns 'lat' and 'lon' are center positions in the rectangles.
}
}

\source{

  Report of the NEAFC workshop on mackerel and blue whiting ---
\enc{Tórshavn}{Torshavn}, February 1999, Table 3.?? zonal proportions of ices
rectangles.

}
\seealso{Function \code{d2ir} in the \code{geo}-package.
%\code{\link[geo]{d2ir}}
}

\examples{
\dontrun{
data(zbyrec)
tmp <- d2ir(zbyrec, useI = TRUE)
all.equal(tmp, zbyrec$rect)
}
}
\keyword{datasets}
