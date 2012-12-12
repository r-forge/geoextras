hrA <-
function (hr, scale = "nmi") 
{
  if(!(scale == "nmi" | scale == "km"))
    stop("Unit square nautical miles or kilometers only")
  A <- sapply(hr, function(x) geoarea(hrPeri(x)))
  if(scale == "nmi") A/1.852^2
    else A
}

