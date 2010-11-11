hr2d <-
function(hr)
{
  lon <-  - (hr %/% 10 %% 100) - 0.5
  lat <- 60 + hr %/% 1000 + (hr %% 10)/6 - 1/12
  data.frame(lat = lat, lon = lon)
}

