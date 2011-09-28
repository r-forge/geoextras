hr2A <-
function(hr, scale = "Miles")
{
	lat <- hr(hr)$lat
	10. * arcdist(lat, -0.5, lat, 0.5, scale = scale)
}

