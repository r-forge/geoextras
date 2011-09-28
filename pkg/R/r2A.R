r2A <-
function(r, scale = "Miles")
{
	lat <- r2d(r)$lat
	A <- 30. * arcdist(lat, -0.5, lat, 0.5, scale = scale)
	return(A)
}

