rA <-
function (r, scale = "nmi") 
{
  if(!(scale == "Miles" | scale == "km")) 
    stop("Unit square (nautical) miles or kilometers only")
  A <- sapply(r, function(x) geoarea(rPeri(x)))
  if(scale == "nmi") A/1.852^2
    else A
}

