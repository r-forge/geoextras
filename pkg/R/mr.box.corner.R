mr.box.corner <-
function(pos, dlat = 5, dlon = 10, corner="NW",fill = F, ...)
{
	n <- length(pos)
	lat <- c(rep(pos$lat, 5), rep(NA, n))
	lon <- c(rep(pos$lon, 5), rep(NA, n))
	lat<-switch(corner,
		NW=lat-dlat/120,
		NE=lat-dlat/120,
		SW=lat+dlat/120,
		SE=lat+dlat/120)
	lon<-switch(corner,
		NW=lon+dlon/120,
		NE=lon-dlon/120,
		SW=lon+dlon/120,
		SE=lon-dlon/120)
	lat <- as.vector(matrix(matrix(lat, nrow = 6, byrow = T), ncol = 1))
	lon <- as.vector(matrix(matrix(lon, nrow = 6, byrow = T), ncol = 1))
	lat <- lat + c(dlat/120, dlat/120,  - dlat/120,  - dlat/120, dlat/
		120, NA)
	lon <- lon + c(dlon/120,  - dlon/120,  - dlon/120, dlon/120, dlon/
		120, NA)
	lat <- data.frame(lat = lat, lon = lon)
	if(fill)
		geopolygon(lat, ...)
	else geolines(lat, ...)
}

