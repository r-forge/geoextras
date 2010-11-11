mrPeri <-
function(mr, dlat = 1, dlon = 2)
{
  lat <- mr2d(mr, dlat = dlat, dlon = dlon)$lat
  lon <- mr2d(mr, dlat = dlat, dlon = dlon)$lon
  lat <- lat + c(dlat/120, dlat/120,  - dlat/120,  - dlat/120, dlat/120)
  lon <- lon + c(dlon/120,  - dlon/120,  - dlon/120, dlon/120, dlon/120)
  data.frame(lat = lat, lon = lon)
}

