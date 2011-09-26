dr2A <-
function(dr, dlat = 1, dlon = 2, scale = "km")
{
  lat <- dr2d(dr, dlat, dlon)$lat
  if (scale == "Miles")
    60*dlat*arcdist(lat, -dlon/2, lat, dlon/2, scale = scale)
  else
    1.852*60*dlat*arcdist(lat, -dlon/2, lat, dlon/2, scale = scale)
}

