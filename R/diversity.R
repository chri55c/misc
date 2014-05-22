
# make a distance matrix
data = london
n = nrow(data)
D = matrix(nrow=n, ncol=n)
for (i in 1:n) {
	D[i,] = eucl_distance(c(data[i,]$x, data[i,]$y), data[2:3])
}

distances = D
diversity = numeric()

for (i in 1:nrow(data)) {
	diversity[i] = shannon_diversity3(data$population[-i], distances[i,-i])
}



