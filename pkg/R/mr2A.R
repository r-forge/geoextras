mr2A <-
function(minsq, dlat = 5, dlon = 10, scale = "Miles")
{
	lat <- minsq2d(minsq, dlat, dlon)$lat
	dlat * arcdist(lat,  - dlon/120, lat, dlon/120, scale = scale)
}

