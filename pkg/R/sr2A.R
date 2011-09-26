sr2A <-
function(ssq, scale = "Miles")
{
	lat <- sr2d(ssq)$lat
	A <- 15. * arcdist(lat, -0.25, lat, 0.25, scale = scale)
	return(A)
}

