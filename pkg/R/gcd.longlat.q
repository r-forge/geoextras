gcd.longlat <- function(tlat, tlon, rlat, rlon)
{
  track <- matrix(c(tlon, rlon, tlat, rlat), 
    ncol = 2, dimnames=list(NULL, c("x", "y")))
  LineLength(track, longlat = TRUE)
}
