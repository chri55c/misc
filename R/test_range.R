
test_range_svm <- function(x, y, fn, gamma=1, cost=1000, k) {
	df = data.frame(matrix(ncol=4, nrow=0))
	names(df) = c('test_size','rmse','mae','accuracy')
	test_sizes = seq(.1,.9,.1)
	
	for(i in 1:length(test_sizes)) {
		scores = crossval.svm(x, y, k=k, test.size=test_sizes[i], tune=F, cost=cost, gamma=gamma)
		write_scores(scores, fn, "svm", i)
		df.row = cbind(test_sizes[i], scores[k+1,2:4])
		df = rbind(df, df.row)
	}
	
	return (df)
}


test_range_lm <- function(x, y, fn, k) {
	df = data.frame(matrix(ncol=5, nrow=0))
	names(df) = c('test_size','rmse','mae','accuracy', 'r.squared')
	test_sizes = seq(.1,.9,.1)
	
	for(i in 1:length(test_sizes)) {
		scores = crossval.lm(x, y, k=k, test.size=test_sizes[i])
		write_scores(scores, fn, "lm", i)
		df.row = cbind(test_sizes[i], scores[k+1,2:5])
		df = rbind(df, df.row)
	}
	
	return (df)
}

test_range_lm2 <- function(x, y, fn, k) {
	df = data.frame(matrix(ncol=5, nrow=0))
	names(df) = c('test_size','rmse','mae','accuracy', 'r.squared')
	test_sizes = seq(.1,.9,.1)
	
	for(i in 1:length(test_sizes)) {
		scores = crossval.lm2(x, y, k=k, test.size=test_sizes[i])
		write_scores(scores, fn, "lm2", i)
		df.row = cbind(test_sizes[i], scores[k+1,2:5])
		df = rbind(df, df.row)
	}
	
	return (df)
}

test_range_lm3 <- function(x, y, fn, k) {
	df = data.frame(matrix(ncol=5, nrow=0))
	names(df) = c('test_size','rmse','mae','accuracy','r.squared')
	test_sizes = seq(.1,.9,.1)
	
	for(i in 1:length(test_sizes)) {
		scores = crossval.lm3(x, y, k=k, test.size=test_sizes[i])
		write_scores(scores, fn, "lm3", i)
		df.row = cbind(test_sizes[i], scores[k+1,2:5])
		df = rbind(df, df.row)
	}
	
	return (df)
}

write_scores = function(data, name, type, round) {
	write.csv(data, paste(name, "/scores/", type, "/", round, ".csv", sep=""), row.names=FALSE)
}

