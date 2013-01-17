\name{lat2rA}
\alias{lat2rA}

\title{
Rectangle area at latitude
}
\description{
Statistical rectangle area at a given latitude
}
\usage{
lat2rA(lat, scale = "km")
}

\arguments{
  \item{lat}{
Latitude
}
  \item{scale}{\code{km} is the default returns area in square
    kilometers, any other value returs area in square nmi.
}
}

\value{
Approximate area of standard statistical rectangle at latitute
}

\author{
STJ
}

\note{Care needs to be taken to give rectangle center latitudes}

\seealso{
\code{\link[geo]{rA}} which gets the rectangle area from the rectangle code.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
"lat2rA"
}

\keyword{arith}

