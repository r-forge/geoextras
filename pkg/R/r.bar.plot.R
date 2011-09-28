r.bar.plot <-
function(mat)
{
	maxy <- max(mat)
	r <- as.numeric(dimnames(mat)[[1]])
	for(i in seq(along = r))
		geosubplot(barplot(mat[i,  ], ylim = c(0, maxy), axes = F),
			pos = r.corners(r[i]))
}

