\name{r.bar.plot}
\alias{r.bar.plot}

\title{
Rectangle bar plots
}
\description{
For a matrix of rectangle distributions do a bar plot in each.
}

\usage{
r.bar.plot(mat)
}

\arguments{
  \item{mat}{
Matrix of \code{distributions}, with rectangle distributions in rows,
with \code{dimnames} of rectangle codes.
}
}

\value{
A barplot (originally the age distribution) in each rectangle given in \code{mat}.
}

\note{
Experimental, rudimentary, needs work
}

\seealso{
\code{\link[geo]{geosubplot}}, \code{\link{r.corners}}, \code{\link[graphics]{barplot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
"r.bar.plot"
}

\keyword{aplot}

