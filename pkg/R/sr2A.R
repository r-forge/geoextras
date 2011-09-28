sr2A <-
function(sr, scale = "Miles")
{
	lat <- sr2d(sr)$lat
	A <- 15. * arcdist(lat, -0.25, lat, 0.25, scale = scale)
	return(A)
}

