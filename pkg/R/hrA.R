hrA <-
function (hr, scale = "Miles") 
{
  sapply(hr, function(x) geoarea(hrPeri(x)))
}

