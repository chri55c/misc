normalise <- function(x) {
	x <- as.matrix(x)
	nc <- ncol(x)
	for (i in 1:nc) {
		v = x[,i]
		v.min = min(v)
		v.max = max(v)
		x[,i] = (v - v.min) / (v.max - v.min)
	}
	x
}
