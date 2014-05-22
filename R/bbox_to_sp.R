
bbox_to_sp <- function(bbx) {
	readWKT(paste("POLYGON((", 
		bbx[1,1], " ", bbx[2,1], ",", 
		bbx[1,1], " ", bbx[2,2], ",", 
		bbx[1,2], " ", bbx[2,2], ",",
		bbx[1,2], " ", bbx[2,1], ",",
		bbx[1,1], " ", bbx[2,1], "))", sep=''))
}

bbox_sp <- function(sp) {
	bbx <- bbox(sp)
	bbx_sp <- bbox_to_sp(bbx)
	proj4string(bbx_sp) <- proj4string(sp)
	return(bbx_sp)
}