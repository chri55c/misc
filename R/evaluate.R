
evaluate_bin_class <- function(model, xtest, ytest) {
	prediction = predict(model, xtest)
	if (!is.factor(prediction))
		prediction = as.factor(ifelse(prediction>0.5, 1, 0))
	performance_bin_class(prediction, ytest)
}

performance_bin_class <- function(prediction, truth) {
	conftab = table(pred=prediction, true=truth)
	if (nrow(conftab)==1) {
		if (row.names(conftab)[1]=="0")
			conftab = rbind(conftab, c(0,0))
		else
			conftab = rbind(c(0,0), conftab)
	}

	tp = conftab[2,2]
	tn = conftab[1,1]
	fp = conftab[2,1]
	fn = conftab[1,2]	
		
	accuracy = (tp + tn) / (tp + tn + fp + fn)		
	precision = tp / (tp + fp)
	sensitivity = tp / (tp + fn)
	specificity = tn / (fp + tn)
		
	den = (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn)
	den = ifelse(den==0, 1, sqrt(den))
	mcc = (tp * tn - fp * fn) / den 
		
	f_measure = (2*precision*sensitivity) / (2*precision + sensitivity)
	f2_measure = (5*precision*sensitivity) / (4*precision + sensitivity)
		
	scores = data.frame(matrix(nrow=1, ncol=8))
	names(scores) = c('','accuracy', 'precision', 'sensitivity', 'specificity', 'mcc', 'f_measure','f2_measure')	
		
	scores[1,1] = 'scores'
	scores[1,2] = accuracy
	scores[1,3] = precision
	scores[1,4] = sensitivity
	scores[1,5] = specificity
	scores[1,6] = mcc
	scores[1,7] = f_measure
	scores[1,8] = f2_measure

	scores
}

rmse <- function(pred, obs) {
	return( sqrt( mean( (pred-obs)^2, na.rm=T) ))
}

nrmse <- function(pred, obs) {
	err = rmse(pred, obs) 
	return(err / (max(obs) - min(obs)) )
}

mae <- function(pred, obs) {
	return( mean(abs(pred - obs)))
}

accuracy <- function(pred, obs, breaks=seq(0,1,1/3)) {
	conftab = table(cut(pred, breaks), cut(obs, breaks))
	tp = 0
	for (i in 1:(length(breaks)-1)) {
		tp = tp + conftab[i,i]
	}
	return (tp / length(pred))
}