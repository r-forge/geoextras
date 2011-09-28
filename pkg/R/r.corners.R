r.corners <-
function(square)
{
	lat <- r2d(square)
	lat$lat <- c(lat$lat + 0.25, lat$lat - 0.25)
	lat$lon <- c(lat$lon - 0.5, lat$lon + 0.5)
	lat
}

