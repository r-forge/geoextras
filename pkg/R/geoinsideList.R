geoinsideList <-
function(data, arealist)
{
	n <- length(arealist)
	if(nrow(data) > 1)
		tmpdata <- data[, c("lat", "lon")]
	else tmpdata <- as.data.frame(data[, c("lat", "lon")])
	tmpdata$area <- rep(0, nrow(tmpdata))
	i <- 1
	ind <- geoinside(tmpdata, reg = arealist[[i]], option = 0, robust = F)
	if(length(ind) > 0)
		tmpdata[ind, "area"] <- i
	for(i in 2:n) {
		j <- tmpdata$area == 0
		j1 <- c(1:length(j))
		j1 <- j1[j == T]
		if(length(j1) > 0) {
			ind <- geoinside(tmpdata[j1,  ], reg = arealist[[i]],
				option = 0, robust = F)
			if(length(ind) > 0)
				tmpdata[j1[ind], "area"] <- i
		}
	}
	tmpdata$area
}

