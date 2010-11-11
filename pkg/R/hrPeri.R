hrPeri <-
function(hr)
{
  lat <- hr2d(hr)$lat
  lon <- hr2d(hr)$lon
  lat <- lat + c(1/12, 1/12,  - 1/12,  - 1/12, 1/12)
  lon <- lon + c(0.5,  - 0.5,  - 0.5, 0.5, 0.5)
  data.frame(lat = lat, lon = lon)
}

