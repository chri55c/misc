# k-fold cross validation of ksvm
# returns a list of predicted decision scores
crossval.svm.classify <- function(x, y, k=10, test.size=0.1, tune=TRUE) {
	scores = data.frame(matrix(ncol=9, nrow=0))
	
	n = length(x)
	for (i in 1:k) {
		# test_index = systematic_sample_index(n, test.size)
		test_index = sample(n, round(n*test.size))
		xtrain = x[-test_index,]
		ytrain = y[-test_index]
		xtest = x[test_index,]
		ytest = y[test_index]
		
		if (tune) {
			tuned = tune.svm(xtrain, as.factor(ytrain), kernel='radial', gamma=10^(-5:-1), cost=10^(0:2), type='C', cachesize=256)
			model = tuned$best.model
		}
		else 
			model = svm(xtrain, as.factor(ytrain), kernel='radial', gamma=0.1, cost=10, type='C', cachesize=256)
		
		scores.row = evaluate_bin_class(model, xtest, ytest)
		scores = rbind(scores, scores.row)
		scores[i,1] = i
		
	}		
	means =  c(0, sapply(scores[2:9], mean))
	sds = c(0, sapply(scores[2:9], sd))
	scores = rbind(scores, means)
	scores = rbind(scores, sds)
	
	scores[k+1,1] = 'mean'
	scores[k+2,1] = 'sd'
	names(scores) = c('fold', 'accuracy', 'precision', 'sensitivity', 'specificity', 'mcc', 'f_measure','f2_measure','phi')

	return(scores)
}



# can't remember if this works...
# kfold.tables = function(predictions, y) {
	# k = length(predictions)
	# tables = list()
	# length(tables) = k
	
	# for (i in 1:k) {
		# ytest = y[as.integer(names(predictions[[i]]))]
		# tables[[i]] = table(pred=predictions[[i]], true=ytest)
	# }
	# return(tables)
# }

