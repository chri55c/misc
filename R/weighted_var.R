
weighted.var <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    sum.w2 <- sum(w^2)
    mean.w <- sum(x * w) / sum(w)
    (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
na.rm)
}


weighted.var2 <- function(x, w, na.rm = FALSE) {
	if (na.rm) {
		w <- w[i <- !is.na(x)]
		x <- x[i]
	}
	sum.w <- sum(w)
	(sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2))
}

weighted.var3 <- function(x, w, na.rm = FALSE) {
	if (na.rm) {
		w <- w[i <- !is.na(x)]
		x <- x[i]
	}
	w.sum <- sum(w)
	wx.mean <- sum(x*w) / w.sum
	sum(w*(x-wx.mean)^2) / w.sum	
}