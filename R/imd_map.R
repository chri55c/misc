map = ggplot(lsoa_stations, aes(x, y, group=group)) + geom_polygon(aes(fill=imd), size=0)

map + scale_colour_gradient(
	name=NULL, low="#FBEFEF", high="#FF0000", 
	space="rgb", breaks=NULL, labels=NULL, 
	limits=NULL, trans="identity")

ggplot(lsoa_stations, aes(x, y, group=group)) +
 	geom_polygon(
		aes(fill = imd_bin), 
		colour = alpha("white", 1/2), 
		size = 0.2) +
	geom_polygon(
		data=lsoa, 
		colour='white', 
		fill=NA) +
	scale_fill_brewer(palette='PuRd')