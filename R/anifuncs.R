source("~/R/curve_points.R")

# return an list of length 'n' where each item is a data.frame
# to be plotted in the order it appears in the list
moving_trail <- function(a, b, n, trail_length=10, resolution=5) {
	# TODO: a curve line
	path <- points_on_line(a, b, n-1)
	frames <- list()
	for (i in 1:n) {
		frame <- add_trail(path[i,], a, i, len=trail_length, n=n, res=resolution)
		frames[[i]] <- frame
	}
	for (i in 1:(trail_length-1)) {
		frame <- add_trail(path[n,], a, -1*i, len=trail_length, n=n, res=resolution)
		frames[[n+i]] <- frame
	}
	return (frames)
}

points_on_line <- function(a, b, n) {
	# proj4str <- sprintf("+proj=utm +zone=%s +datum=WGS84 utm_zone)
	# a <- project(as.matrix(a), proj=proj4str)
	# b <- project(as.matrix(b), proj=proj4str)
	dist <- b - a
	# d <- sqrt(Reduce('+', xydist^2))
	# npoints <- floor(d / scale)

	delta <- dist / n
	xs <- seq(a$long, b$long, delta$long)
	ys <- seq(a$lat, b$lat, delta$lat)
	# points <- data.frame(project(cbind(xs,ys), inv=TRUE, proj4str))
	# names(points) = c('long','lat')
	points <- data.frame(long=xs, lat=ys)
	return (points)
}

add_trail <- function(b, a, i, len, n, res) {
	delta <- (b-a) / (i*res)
	if (i < 0) {
		# already reached target
		delta <- (b-a) / (n*res)
		len <- len + i -1
		trail_pos <- seq(0, 1, 1/(len*res))
		j <- n - len 
	} else if (i >= len) {
		trail_pos <- seq(0, 1, 1/(len*res))
		j <- i-len
	} else {
		trail_pos <- seq(0, 1 , 1/(i*res))
		j <- 0
	}
	x <- seq(a$long + j*delta$long*res, b$long, delta$long)
	y <- seq(a$lat + j*delta$lat*res, b$lat, delta$lat)

	# horrible hack needed as rounding error sometime results in 
	# different length x and y
	if (length(x) != length(y)) {
		xd <- delta$long
		yd <- delta$lat
		trys <- 0
		while (length(x) < length(y)) {
			xd <- xd - xd*.00001*sign(xd)
			x <- seq(a$long + j*xd*res, b$long, xd)
			trys <- trys + 1
			if (trys > 20) break
		}
		trys <- 0
		while (length(y) < length(x)) {
			yd <- yd - yd*.00001*sign(yd)
			y <- seq(a$lat + j*yd*res, b$lat, yd)
			trys <- trys + 1
			if (trys > 20) break
		}
	}
	return (data.frame(long=x, lat=y, trail_pos=trail_pos))
}
# add_trail <- function(point, origin, i, len, res) {
# 	xd <- (point[[1]]-origin[[1]]) / (i*res)
# 	yd <- (point[[2]]-origin[[2]]) / (i*res)
# 	if (i >= len) {
# 		trail_pos <- 0:(len*res)
# 		j <- i-len
# 	} else {
# 		trail_pos <- 0:(i*res)
# 		j <- 0
# 	}
# 	x <- seq(origin[[1]] + j*xd*res, point[[1]], xd)
# 	y <- seq(origin[[2]] + j*yd*res, point[[2]], yd)
# 	# horrible hack needed as rounding error sometime results in 
# 	# different length x and y
# 	trys <- 0
# 	while (length(x) < length(y)) {
# 		xd <- xd - xd*.00001*sign(xd)
# 		x <- seq(origin[[1]] + j*xd*res, point[[1]], xd)
# 		trys <- trys + 1
# 		if (trys > 20) break
# 	}
# 	trys <- 0
# 	while (length(y) < length(x)) {
# 		yd <- yd - yd*.00001*sign(yd)
# 		y <- seq(origin[[2]] + j*yd*res, point[[2]], yd)
# 		trys <- trys + 1
# 		if (trys > 20) break
# 	}

# 	return (data.frame(long=x, lat=y, trail_pos=trail_pos))
# }

project_df <- function(df, proj, inv=FALSE, xy_cols=c(1,2)) {
	df[xy_cols] <- project(cbind(df[,xy_cols[1]], df[,xy_cols[2]]), inv=inv, proj=proj)
	return (df)
}
