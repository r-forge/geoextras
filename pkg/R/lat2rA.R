lat2rA <-
function(lat, scale = "km")
{
  if (scale == "Miles")
    30. * arcdist(lat, -0.5, lat, 0.5, scale = scale)
  else
    1.852*30*arcdist(lat, -0.5, lat, 0.5, scale = scale)
}

