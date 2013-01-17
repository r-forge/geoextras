\name{geoinsideList}
\alias{geoinsideList}

\title{
Area of positions
}
\description{
Finds the index of an area a positions belongs to
}
\usage{
geoinsideList(data, arealist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
(A dataframe of) positions.
}
  \item{arealist}{
A list of areas given as lat/lon dataframes/lists.
}
}


\value{
Index of area in \code{arealist} that records in \code{data} belong to.
}
\seealso{
\code{\link[geo]{inside.reg.bc}}, \code{\link[geo]{inside.reg.lump}}
tailored to the bormicon and lumpsucker regulation area divisions, respectively.
}
\author{
AM
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
"geoinsideList"
}

