


shannon_diversity1 <- function(p, d) {
	a = p/d^2
	return(sum(a^2)/(sum(a))^2)
}

shannon_diversity2 <- function(p, d, k=NULL) {
	if (is.null(k)) k = length(p)
	a = p/d^2
	a.sum = sum(a)
	x = -(sum((a/a.sum) * log(a/a.sum)))
	return(x/log(k))
}

# when p is constant
shannon_diversity3 <- function(d) {
	n = length(d)
	a = 0
	d.sqd = d^2
	b = numeric(length=n)

	for (i in 1:n) 
		b[i] = sum(d.sqd[i]/d.sqd)
	
	return(sum(log(b)/b) / log(n))
}

# d is already squared
shannon_diversity3 <- function(d) {
	n = length(d)
	a = 0
	b = numeric(length=n)

	for (i in 1:n) 
		b[i] = sum(d[i]/d)
	
	return(sum(log(b)/b) / log(n))
}


shannon_diversity4 <- function(d) {
	n = length(d)
	x = 1/d^2n
	x.sum = sum(x)
	b = x / x.sum
	-1 * sum(b * log(b)) / log(n)
}

gravity <- function(p1, p2, d, g=1) {
	return(g*p1*p2/d^2)
}

eucl_distance <- function(x, y) {
	sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2)
}

