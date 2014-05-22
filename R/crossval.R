library(e1071)
source('~/R/evaluate.R')

# 
systematic_sample_index <- function(n, sample.size, column=NULL) {
	index = 1:n
	start = sample(index, 1)
	if (start > 1)
		index = c(index[start:n], index[1:start-1])
		
	return(index[seq(1, n, 1/sample.size)])
}

# returns the average coefficients of k iterations
# x is a data.frame and y is a vector
tune.lm.classify <- function(x, y, k=10) {
	n <- length(y)
	folds = split(sample(n), rep(1:k, length=n))
	coefs = matrix(rep(0, ncol(x)+1))
	
	for (i in 1:k) {
		test.index = folds[[i]]
		test.x = x[test.index,]
		test.y = y[test.index]		
		train.x = x[-test.index,]
		train.y = y[-test.index]
		
		model.coefs = matrix(coef(lm(y ~ ., data=x)))
		coefs = coefs + model.coefs
	}
	return(coefs/k)
}

crossval.lm <- function(x, y, k=10, test.size=.1, sample='random') {
	cols = 5
	scores = data.frame(matrix(ncol=cols, nrow=0))
	n = length(y)
	
	for (i in 1:k) {
		if (sample == 'random')
			test_index = sample(n, round(n*test.size))
		else if (sample == 'systematic')
			test_index = systematic_sample_index(n, test.size)
		else 
			stop('sample must be either "random" or "systematic"')
			
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		model = lm(ytrain ~ ., data=as.data.frame(xtrain))
		prediction = predict(model, xtest)
		scores.row = c(i, rmse(prediction, ytest), mae(prediction, ytest), accuracy(prediction, ytest, c(-Inf,.333,.667,Inf)), summary(model)$r.squared)
		scores = rbind(scores, scores.row)
	}
	means =  c(0, sapply(scores[2:cols], mean))
	sds = c(0, sapply(scores[2:cols], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'rmse', 'mae', 'accuracy', 'r.squared')
	
	return (scores)
}

crossval.lm2 <- function(x, y, k=10, test.size=.1, sample='random') {
	cols = 5
	scores = data.frame(matrix(ncol=4, nrow=0))
	n = nrow(x)
	
	for (i in 1:k) {
		if (sample == 'random')
			test_index = sample(n, round(n*test.size))
		else if (sample == 'systematic')
			test_index = systematic_sample_index(n, test.size)
		else 
			stop('sample must be either "random" or "systematic"')
			
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		model = lm(ytrain ~ .^2, data=as.data.frame(xtrain))
		prediction = predict(model, xtest)
		scores.row = c(i, rmse(prediction, ytest), mae(prediction, ytest), accuracy(prediction, ytest, c(-Inf,.333,.667,Inf)), summary(model)$r.squared)

		scores = rbind(scores, scores.row)
	}
	means =  c(0, sapply(scores[2:cols], mean))
	sds = c(0, sapply(scores[2:cols], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'rmse', 'mae', 'accuracy', 'r.squared')
	
	return (scores)
}

# ** only for names(x) = c('x','y') **
crossval.lm3 <- function(x, y, k=10, test.size=.1, sample='random') {
	cols = 5
	scores = data.frame(matrix(ncol=4, nrow=0))
	n = nrow(x)
	
	for (i in 1:k) {
		if (sample == 'random')
			test_index = sample(n, round(n*test.size))
		else if (sample == 'systematic')
			test_index = systematic_sample_index(n, test.size)
		else 
			stop('sample must be either "random" or "systematic"')
			
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		model = lm(ytrain ~ .^2 + I(x^2) + I(y^2), data=as.data.frame(xtrain))
		prediction = predict(model, xtest)
		scores.row = c(i, rmse(prediction, ytest), mae(prediction, ytest), accuracy(prediction, ytest, c(-Inf,.333,.667,Inf)), summary(model)$r.squared)

		scores = rbind(scores, scores.row)
	}
	means =  c(0, sapply(scores[2:cols], mean))
	sds = c(0, sapply(scores[2:cols], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'rmse', 'mae', 'accuracy', 'r.squared')
	
	return (scores)
}

crossval.svm <- function(x, y, k=10, test.size=.1, sample='random', tune=T, cost=NULL, gamma=NULL) {
	cols = 4
	scores = data.frame(matrix(ncol=cols, nrow=0))
	n = nrow(x)
	
	for (i in 1:k) {
		if (sample == 'random')
			test_index = sample(n, round(n*test.size))
		else if (sample == 'systematic')
			test_index = systematic_sample_index(n, test.size)
		else 
			stop('sample must be either "random" or "systematic"')
			
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		if (tune) {
			tuned = tune.svm(xtrain, ytrain, kernel='radial', gamma=10^(-3:0), cost=10^(0:3), type='eps')
			model = tuned$best.model
		}
		else {
			model = svm(xtrain, ytrain, kernel='radial', gamma=gamma, cost=cost, type='eps')
		}
		prediction = predict(model, xtest)
		scores.row = c(i, rmse(prediction, ytest), mae(prediction, ytest), accuracy(prediction, ytest, c(-Inf,.333,.667,Inf)))
		scores = rbind(scores, scores.row)
	}
	means =  c(0, sapply(scores[2:cols], mean))
	sds = c(0, sapply(scores[2:cols], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'rmse', 'mae', 'accuracy')
	
	return (scores)
}

# makes a train.size/1-train.size systematic split k times and
# returns the performance measures for each round
# x is a data.frame and y is a vector
crossval.lm.classify <- function(x, y, k=10, test.size=0.1, sample='systematic') {
	scores = data.frame(matrix(ncol=8, nrow=0))
	
	n = nrow(x)
	for (i in 1:k) {
		if (sample == 'random')
			test_index = sample(n, round(n*(1-test.size)))
		else if (sample == 'systematic')
			test_index = systematic_sample_index(n, test.size)
		else 
			stop('sample must be either "random" or "systematic"')
			
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		model = lm(ytrain ~ ., data=as.data.frame(xtrain))
		
		scores.row = evaluate_bin_class(model, xtest, ytest)
		scores = rbind(scores, scores.row)
		scores[i,1] = i
		
	}		
	means =  c(0, sapply(scores[2:8], mean))
	sds = c(0, sapply(scores[2:8], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'accuracy', 'precision', 'sensitivity', 'specificity', 'mcc', 'f_measure','f2_measure')

	return(scores)
}

# includes all pairwise interactions
crossval.lm2.classify <- function(x, y, k=10, test.size=0.1) {
	scores = data.frame(matrix(ncol=8, nrow=0))
	
	n = nrow(x)
	for (i in 1:k) {
		test_index = systematic_sample_index(n, test.size)
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		model = lm(ytrain ~ .^2, data=as.data.frame(xtrain))
		
		scores.row = evaluate_bin_class(model, xtest, ytest)
		scores = rbind(scores, scores.row)
		scores[i,1] = i
		
	}		
	means =  c(0, sapply(scores[2:8], mean))
	sds = c(0, sapply(scores[2:8], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'accuracy', 'precision', 'sensitivity', 'specificity', 'mcc', 'f_measure','f2_measure')

	return(scores)
}

# makes a train.size/1-train.size systematic split k times and
# returns the performance measures for each round
# x is a data.frame and y is a vector
crossval.svm.classify <- function(x, y, k=10, test.size=0.1, tune=TRUE) {
	scores = data.frame(matrix(ncol=8, nrow=0))
	
	n = nrow(x)
	for (i in 1:k) {
		test_index = systematic_sample_index(n, test.size)
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		if (tune) {
			tuned = tune.svm(xtrain, as.factor(ytrain), kernel='radial', gamma=10^(-5:-1), cost=10^(0:2), type='C')
			model = tuned$best.model
		}
		else 
			model = svm(xtrain, as.factor(ytrain), kernel='radial', gamma=0.1, cost=10, type='C', cachesize=256)
		
		scores.row = evaluate_bin_class(model, xtest, ytest)
		scores = rbind(scores, scores.row)
		scores[i,1] = i
		
	}		
	means =  c(0, sapply(scores[2:8], mean))
	sds = c(0, sapply(scores[2:8], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'accuracy', 'precision', 'sensitivity', 'specificity', 'mcc', 'f_measure','f2_measure')

	return(scores)
}

