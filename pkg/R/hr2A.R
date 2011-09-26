hr2A <-
function(hexsq, scale = "Miles")
{
	lat <- hexsq2d(hexsq)$lat
	10. * arcdist(lat, -0.5, lat, 0.5, scale = scale)
}

