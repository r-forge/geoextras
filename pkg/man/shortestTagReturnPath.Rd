\name{shortestTagReturnPath}
\alias{shortestTagReturnPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shortest tag return path
}
\description{
Find shortest path betweeen tagging and recapture position not over
land. Uses Dijkstra's algorithm as implemented in package 'spatgraphs'
}
\usage{
shortestTagReturnPath(tlat, tlon, rlat, rlon, res, land.raster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tlat}{
%%     ~~Describe \code{tlat} here~~
}
  \item{tlon}{
%%     ~~Describe \code{tlon} here~~
}
  \item{rlat}{
%%     ~~Describe \code{rlat} here~~
}
  \item{rlon}{
%%     ~~Describe \code{rlon} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{land.raster}{
%%     ~~Describe \code{land.raster} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{cross }{Logical, does the great circle connecting tag and return
  positions cross land?}
  \item{owDist }{Estimated distance given 'res' and rest.}
  \item{track }{Track through grid/graph in lat/long.}

}
\references{
  R-packages:

    Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial
  data in R. R News 5 (2), http://cran.r-project.org/doc/Rnews/.

  Roger S. Bivand, Edzer J. Pebesma, Virgilio Gomez-Rubio, 2008.
  Applied spatial data analysis with R. Springer, NY.
  http://www.asdar-book.org/

  Nicholas J. Lewin-Koh, Roger Bivand, contributions by Edzer J.
  Pebesma, Eric Archer, Adrian Baddeley, Hans-Jörg Bibiko, Jonathan
  Callahan, Stéphane Dray, David Forrest, Michael Friendly, Patrick
  Giraudoux, Duncan Golicher, Virgilio Gómez Rubio, Patrick Hausmann,
  Karl Ove Hufthammer, Thomas Jagger, Sebastian P. Luque, Don MacQueen,
  Andrew Niccolai, Tom Short, Greg Snow, Ben Stabler and Rolf Turner
  (2012). maptools: Tools for reading and handling spatial objects. R
  package version 0.8-14. http://CRAN.R-project.org/package=maptools

  Timothy H. Keitt, Roger Bivand, Edzer Pebesma and Barry Rowlingson
  (2012). rgdal: Bindings for the Geospatial Data Abstraction Library.
  R package version 0.7-11. http://CRAN.R-project.org/package=rgdal

  Robert J. Hijmans & Jacob van Etten (2012). raster: Geographic
  analysis and modeling with raster data. R package version 2.0-04.
  http://CRAN.R-project.org/package=raster

  A. Baddeley and R. Turner (2005). Spatstat: an R package for
  analyzing spatial point patterns. Journal of Statistical Software 12
  (6), 1-42. ISSN: 1548-7660. URL: www.jstatsoft.org

  Tuomas Rajala (2012). spatgraphs: Graphs for spatial point patterns.
  R package version 2.60. http://CRAN.R-project.org/package=spatgraphs

  Simon Urbanek (2011). multicore: Parallel processing of R code on
  machines with multiple cores or CPUs. R package version 0.1-7.
  http://CRAN.R-project.org/package=multicore

  Henrik Bengtsson (2012). R.utils: Various programming utilities. R
  package version 1.12.1. http://CRAN.R-project.org/package=R.utils


  Wikipedia on Dijkstra's algorithm:
  http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
%% ~put references to the literature/web site here ~
}
\author{
Julian Mariano Burgos and Sigurdur Thor Jonsson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tlat, tlon, rlat, rlon, res, land.raster) 
{
    raster_filename <- tempfile("tmprast")
    on.exit(unlink(raster_filename))
    dll <- data.frame(x = tlon, y = tlat)
    coordinates(dll) <- ~x + y
    proj4string(dll) <- CRS("+proj=longlat +datum=WGS84")
    dxy <- as.data.frame(coordinates(spTransform(dll, CRS("+proj=merc +datum=WGS84"))))
    tx <- dxy$x
    ty <- dxy$y
    dll <- data.frame(x = rlon, y = rlat)
    coordinates(dll) <- ~x + y
    proj4string(dll) <- CRS("+proj=longlat +datum=WGS84")
    dxy <- as.data.frame(coordinates(spTransform(dll, CRS("+proj=merc +datum=WGS84"))))
    rx <- dxy$x
    ry <- dxy$y
    x.range <- range(c(tx, rx))
    y.range <- range(c(ty, ry))
    xmin <- min(x.range) - 2 * res
    xmax <- max(x.range) + 2 * res
    ymin <- min(y.range) - 2 * res
    ymax <- max(y.range) + 2 * res
    test <- TRUE
    test2 <- FALSE
    while (test) {
        this.raster <- intersect(land.raster, extent(c(xmin, 
            xmax, ymin, ymax)))
        this.raster <- writeRaster(this.raster, filename = raster_filename, 
            overwrite = TRUE)
        cross <- crossesLand(tx, ty, rx, ry, this.raster)
        grid <- sp:::genHexGrid(dx = res, ll = c(xmin, ymin), 
            ur = c(xmax, ymax))
        coordinates(grid) <- ~x + y
        proj4string(grid) <- CRS("+proj=merc +datum=WGS84")
        overlay <- extract(this.raster, grid)
        grid <- grid[is.na(overlay), ]
        grid <- coordinates(grid)
        ns <- which.min(sqrt((grid[, 1] - tx)^2 + (grid[, 2] - 
            ty)^2))
        ne <- which.min(sqrt((grid[, 1] - rx)^2 + (grid[, 2] - 
            ry)^2))
        if (ns == ne) 
            break
        grid.ppp <- ppp(grid[, 1], grid[, 2], window = owin(xrange = range(grid[, 
            1]), yrange = range(grid[, 2])))
        graph <- spatgraph(grid.ppp, type = "geometric", res * 
            1.1)
        clusters <- spatcluster(graph)
        test <- !any(sapply(clusters$clusters, function(x) all(c(ns, 
            ne) \%in\% x)))
        test2 <- extent(this.raster) == union(extent(land.raster), 
            extent(this.raster))
        if (test2) 
            break
        xmin <- xmin - 2 * diff(x.range)
        xmax <- xmax + 2 * diff(x.range)
        ymin <- ymin - 2 * diff(y.range)
        ymax <- ymax + 2 * diff(y.range)
        rm(this.raster)
    }
    if (test2) {
        owDist <- NA
        track <- NA
    }
    if (ns == ne) {
        owDist <- 0
        track <- NA
    }
    if (ns != ne && !test2) {
        path <- shortestPath(ns, ne, g = graph)
        track <- as.data.frame(grid[path$path, ])
        track <- track[!is.na(track$x), ]
        coordinates(track) <- ~x + y
        proj4string(track) <- CRS("+proj=merc +datum=WGS84")
        track <- spTransform(track, CRS("+proj=longlat +datum=WGS84"))
        track <- coordinates(track)
        owDist <- LineLength(track, longlat = TRUE)
    }
    rm(this.raster)
    list(cross = cross, owDist = owDist, track = track)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
