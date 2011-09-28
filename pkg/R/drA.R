drA <-
function (dr, dlat = 1, dlon = 2, scale = "km") 
{
  sapply(dr, function(x) geoarea(drPeri(x)))
}

