
simplify_shapefile <- function(shpf, tol = 0.01) {
	for (i in 1:length(shpf@polygons)) {
		for (j in 1:length(shpf@polygons[[i]]@Polygons)) {
			temp <- as.data.frame(shpf@polygons[[i]]@Polygons[[j]]@coords)
			names(temp) <- c("x", "y")
			temp2 <- dp(temp, tol)
			shpf@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, 
				temp2$y))
		}
	}
	return(shpf)
}
