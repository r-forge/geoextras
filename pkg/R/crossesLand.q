crossesLand <- function(tx, ty, rx, ry, land.raster) 
{
  trLine <- Lines(list(Line(matrix(c(tx, ty, rx, ry), 
    ncol = 2, byrow = TRUE))), ID = "ln")
  trLine <- SpatialLines(list(trLine),
    proj4string = CRS("+proj=merc +datum=WGS84"))
  any(!is.na(unlist(extract(land.raster,trLine))))
}

