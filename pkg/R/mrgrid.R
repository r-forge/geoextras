mrgrid <-
function (mr, dlat = 5, dlon = 10, fill = FALSE, col = "black", ...) 
{
  mr <- lapply(mr, mrPeri, dlat = dlat, dlon = dlon)
  if(fill) {
    if(length(col) == 1) {
      invisible(lapply(mr, geopolygon, col = col, ...))
    }
    else {
      if(length(mr) != length(col)) {
        stop("More than one color but not as many as the rectangles")
      }
      else {
        invisible(lapply(1:length(mr), function(i, mr, col)
          geopolygon(mr[[i]], col = col[i], ...), mr = mr, col = col, ...))
      }
    }
  }
  else {
    if(length(col) == 1) {
      invisible(lapply(mr, geolines,  col = col, ...))
    }
    else {
      if(length(mr) != length(col)) {
        stop("More than one color but not as many as the rectangles")
      }
      else {
        invisible(lapply(1:length(mr), function(i, mr, col, ...)
          geolines(mr[[i]], col = col[i], ...), mr = mr, col = col, ...))
      }
    }
  }
}
