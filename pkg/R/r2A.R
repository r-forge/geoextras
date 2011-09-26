r2A <-
function(sq, scale = "Miles")
{
	lat <- r2d(sq)$lat
	A <- 30. * arcdist(lat, -0.5, lat, 0.5, scale = scale)
	return(A)
}

