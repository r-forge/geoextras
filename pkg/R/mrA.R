mrA <-
function (mr, dlat = 5, dlon = 10, scale = "Miles") 
{
  if(!(scale == "Miles" | scale == "km")) 
    stop("Unit square (nautical) miles or kilometers only")
  A <- sapply(mr, function(x, dlat, dlon) 
    geoarea(mrPeri(x, dlat = dlat, dlon = dlon)), 
      dlat = dlat, dlon = dlon)
  if(scale == "Miles") A/1.852^2
    else A
}

