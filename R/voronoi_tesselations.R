voronoi_cells <- function(points, bounds){
	require(deldir)
	z <- deldir(points$x, points$y, rw=bounds)
	w <- tile.list(z)
	polys <- vector(mode='list', length=length(w))
	require(sp)
	for (i in seq(along=polys)) {
		pcrds <- cbind(w[[i]]$x, w[[i]]$y)
		pcrds <- rbind(pcrds, pcrds[1,])
		polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
	}
	SP <- SpatialPolygons(polys)
	SpatialPolygonsDataFrame(SP, 
		data=data.frame(x=points$x, y=points$y, id=points$id,
			row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))))
}

voronoi_buff <- function(points, bounds, radius, proj, quadsegs=30) {
	# print ("be sure that points really are in proj...")
	vor <- voronoi_cells(points, bounds)
	proj4string(vor) <- proj
	sp <- SpatialPoints(points[c('x','y')], proj4string=CRS(utm14N))
	buff <- gBuffer(sp, width=radius, byid=TRUE, quadsegs=quadsegs) 
	polys <- list()
	for (i in 1:length(buff)) {
		clipd <- gIntersection(vor[i,], buff[i,], byid=TRUE) 
		subpoly <- list(clipd@polygons[[1]]@Polygons[[1]]) # the actual polygon is nested quite deep
		polys <- c(polys, Polygons(subpoly, ID=row.names(buff)[i])) 
	}
	sp <- SpatialPolygons(polys, proj4string=CRS(proj))
	SpatialPolygonsDataFrame(sp, data=vor@data)
}