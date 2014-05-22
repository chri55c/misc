

# angle in radians
curve_points <- function(p1, p2, theta, n=10) {
	d = sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
	if (d==0)
		return(0)
	r = d*.05

	if (p1[2] > p2[2]) {
		p3 = p2
		p2 = p1
		p1 = p3
	}
	# positive slope or horizontal
	phi_a = asin(abs(p1[2]-p2[2])/d)
	phi_b = pi + phi_a

	# negative slope or vertical
	if (p1[1]==p2[1] | (p1[1]<p2[1])==(p1[2]>p2[2])) {
		phi_a = pi - phi_a
		phi_b = pi + phi_a
	}
	# might change +/- assignment to random or something clever
	a = c(r * cos(phi_a + theta) + p1[1], r * sin(phi_a + theta) + p1[2])
	b = c(r * cos(phi_b - theta) + p2[1], r * sin(phi_b - theta) + p2[2])
	if (n > 0)
		return(rbind(p1, a, curve_points(a, b, theta*.9, n-1), b, p2))
	return(rbind(a, b))
}
