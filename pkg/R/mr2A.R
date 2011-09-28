mr2A <-
function(mr, dlat = 5, dlon = 10, scale = "Miles")
{
	lat <- mr2d(mr, dlat, dlon)$lat
	dlat * arcdist(lat,  - dlon/120, lat, dlon/120, scale = scale)
}

