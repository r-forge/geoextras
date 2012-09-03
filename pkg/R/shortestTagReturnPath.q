shortestTagReturnPath <- function(tlat, tlon, rlat, rlon, res, land.raster)
{
  raster_filename <- tempfile("tmprast")
  on.exit(unlink(raster_filename))

  dll <- data.frame(x = tlon, y = tlat)
  coordinates(dll) <- ~ x + y
  proj4string(dll) <- CRS("+proj=longlat +datum=WGS84")
  dxy <- as.data.frame(coordinates(spTransform(dll,
    CRS("+proj=merc +datum=WGS84"))))
  tx <- dxy$x
  ty <- dxy$y

  dll <- data.frame(x = rlon, y = rlat)
  coordinates(dll) <- ~ x + y
  proj4string(dll) <- CRS("+proj=longlat +datum=WGS84")
  dxy <- as.data.frame(coordinates(spTransform(dll,
    CRS("+proj=merc +datum=WGS84"))))
  rx <- dxy$x
  ry <- dxy$y

  x.range <- range(c(tx, rx)); y.range <- range(c(ty, ry))

  xmin <- min(x.range) - 2*res; xmax <- max(x.range) + 2*res
  ymin <- min(y.range) - 2*res; ymax <- max(y.range) + 2*res

  test <- TRUE
  test2 <- FALSE

  while(test)
  {
    this.raster <- intersect(land.raster, 
      extent(c(xmin, xmax, ymin, ymax)))
    this.raster <- writeRaster(this.raster, filename = raster_filename,
      overwrite = TRUE)

    cross <- crossesLand(tx, ty, rx, ry, this.raster)

    grid <- sp:::genHexGrid(dx = res, 
      ll = c(xmin, ymin), ur = c(xmax, ymax))
    coordinates(grid) <- ~ x + y
    proj4string(grid) <-CRS("+proj=merc +datum=WGS84")  

    overlay <- extract(this.raster, grid)
    grid<- grid[is.na(overlay),]
    grid <- coordinates(grid)

    ns <- which.min(sqrt((grid[ , 1] - tx)^2 + (grid[ , 2] - ty)^2))
    ne <- which.min(sqrt((grid[ , 1] - rx)^2 + (grid[ , 2] - ry)^2))

    if(ns == ne) break
  
    grid.ppp <- ppp(grid[ , 1], grid[ , 2],
      window = owin(xrange = range(grid[ , 1]), 
      yrange = range(grid[ , 2])))
    graph <- spatgraph(grid.ppp, type = "geometric", res*1.1)
    clusters <- spatcluster(graph)

    test <- !any(sapply(clusters$clusters, 
      function(x) all(c(ns, ne) %in% x)))

    test2 <- extent(this.raster) == union(extent(land.raster), 
      extent(this.raster))

    if(test2) break

    xmin <- xmin - 2*diff(x.range); xmax <- xmax + 2*diff(x.range)
    ymin <- ymin - 2*diff(y.range); ymax <- ymax + 2*diff(y.range)

    rm(this.raster)
  }

  if(test2) {owDist <- NA; track <- NA}
  if(ns == ne) {owDist <- 0; track <- NA}
  if(ns != ne && !test2) {
    path <- shortestPath(ns, ne, g = graph)
    track <-as.data.frame(grid[path$path, ])
    track <- track[!is.na(track$x), ]
    coordinates(track) <- ~ x + y
    proj4string(track) <- CRS("+proj=merc +datum=WGS84")
    track <- spTransform(track, CRS("+proj=longlat +datum=WGS84"))
    track <- coordinates(track)
    owDist <- LineLength(track, longlat = TRUE)
  } 
  rm(this.raster)
  list(cross = cross, owDist = owDist, track = track)
}
