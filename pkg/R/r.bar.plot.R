r.bar.plot <-
function(mat)
{
	maxy <- max(mat)
	sq <- as.numeric(dimnames(mat)[[1]])
	for(i in seq(along = sq))
		geosubplot(barplot(mat[i,  ], ylim = c(0, maxy), axes = F),
			pos = sq.corners(sq[i]))
}

