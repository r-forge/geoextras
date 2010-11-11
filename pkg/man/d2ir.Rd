\encoding{latin1}

\name{d2ir}
\alias{d2ir}

\title{Find ICES rectangle code for a position}

\description{Based on an ancient system the North Atlantic was divided into so--called statistical rectangles, this function allocates positions to these recatngles}

\usage{
d2ir(lat, lon = NULL, useI = FALSE)
}

\arguments{
  \item{lat}{latitude of positions or if \code{lat} is a list its components \code{lat$lat} and \code{lat$lon} should give latitude and longitude.}

  \item{lon}{longitude of positions, \emph{optional} if \code{lat} contains both lat and lon.}

  \item{useI}{logical, if \code{TRUE}, go against the prescription in ICES CM77/G:3 and use letter 'I', as has been done on occasion.}

}

\details{
The principle of the statistical rectangle coding system is as
follows:

The latitudinal rows, each of which is 30' high, are numbered (two
digits) from 01 at the southern boundary of the ICES statistical area
(36°00'N latitude (see ICES No.2 chart)) northwards to 99. The
northern limit of the statistical recangle system is thus 85°30'N
latitude. The longitudinal columns, each of which is 1° wide, are
coded according to an alphanumeric system which starts at the western
boundary of the ICES statistical area (44°00'W longitude (see ICES
No.1 chart)) with A0, continuing A1, A2, A3 to 40°W longitude. East of
40°W the system continues B0, B1, B2, to B9, C0, C1, C2, ..., C9, etc
using a different letter for each 10° block and covering the entire
west-east extent of the ICES statistical area, thus:

\itemize{
  \item A: 44°W - 40°W
  \item B: 40°W - 30°W
  \item C: 30°W - 20°W
  \item D: 20°W - 10°W
  \item E: 10°W - 0°
  \item F: 0°   - 10°E
  \item G: 10°E - 20°E
  \item H: 20°E - 30°E
  \item J: 30°E - 40°E
  \item K: 40°E - 50°E
  \item L: 50°E - 60°E
  \item M: 60°E - 68°30'E (M8)
}

Note that letter 'I' is omitted.

When designating a statistical rectangle, the northing co-ordinate is
to be stated first. Thus, the recangle of which the south-west corner
is 54°00'N, 3°00'E is designated 37 F3.

}
\value{
ICES statitstical rectangle code of position(s).
}

\references{
ICES C.M. 1977/Gen:3
}
\author{
\enc{Sigurður}{Sigurdur} \enc{Þór}{Thor} \enc{Jónsson}{Jonsson} with contributions from \enc{Ásta}{Asta} \enc{Guðmundsdóttir}{Gudmundsdottir}
}
\note{
The ices rectangles are the same size as those in the Icelandic system which is used albeit for a different part of the N-Atlantic in \code{d2r} and \code{r2d}.
}

\seealso{
  \code{\link{rectangles2degrees}}
}

\examples{
d2ir(lat = 54.25, lon = 3.5)
}

\keyword{manip}
\keyword{arith}

