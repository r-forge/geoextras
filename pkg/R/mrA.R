mrA <-
function (mr, dlat = 5, dlon = 10, scale = "Miles") 
{
  sapply(mr, function(x) geoarea(mrPeri(x)))
}

