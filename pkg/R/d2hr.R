d2hr <-
function(lat, lon = NULL)
{
  if(is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  r <- d2r(lat, lon)
  r <- ifelse(r %% 100 > 50, r - 50, r)
  lat <- lat + 1e-06
  h <- lat - floor(lat)
  h <- floor(6 * h) + 1
  return(10 * r + h)
}

