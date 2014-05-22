crossval.lm = function(x, y, k=10, train.size=0.9) {
	scores = data.frame(matrix(ncol=5, nrow=k+2))
	names(scores) = c('fold','accuracy','precision','sensitivity','specificity')

	for (i in 1:k) {
		n = nrow(x)
		train = sample(n, round(n*train.size))
		xtrain = x[train,]
		ytrain = y[train]
		xtest = x[-train,]
		ytest = y[-train]
		
		model = lm(ytrain ~ ., data=xtrain)

		prediction = predict(model, xtest)
		prediction = as.factor(ifelse(prediction>0.5, 1, 0))
		conftab = table(pred=prediction, true=ytest)

		tp = conftab[2,2]
		tn = conftab[1,1]
		pos = (conftab[1,2] + tp)
		neg = (tn + conftab[2,1])
		truepos = conftab[2,2]
		tpandfp = conftab[2,1] + tp
		
		if (pos==0 | neg==0 | tpandfp==0) {
			accu = NA
			sens = NA
			spec = NA
			prec = NA
		} else {
			accu = (tp + tn) / (pos + neg)
			sens = tp / pos
			spec = tn / neg
			prec = tp / tpandfp
		}
				
		scores[i,1] = i
		scores[i,2] = accu
		scores[i,3] = prec
		scores[i,4] = sens
		scores[i,5] = spec
		
	}
	scores[k+1,1] = 'mean'
	scores[k+1,2] = mean(scores[1:k,2], na.rm=T)
	scores[k+1,3] = mean(scores[1:k,3], na.rm=T)
	scores[k+1,4] = mean(scores[1:k,4], na.rm=T)
	scores[k+1,5] = mean(scores[1:k,5], na.rm=T)
	
	scores[k+2,1] = 'sd'
	scores[k+2,2] = sd(scores[1:k,2], na.rm=T)
	scores[k+2,3] = sd(scores[1:k,3], na.rm=T)
	scores[k+2,4] = sd(scores[1:k,4], na.rm=T)
	scores[k+2,5] = sd(scores[1:k,5], na.rm=T)
	
	return(scores)
}

crossval.lm2 = function(x, y, k=10, train.size=0.9) {
	scores = data.frame(matrix(ncol=5, nrow=k+2))
	names(scores) = c('fold','accuracy','precision','sensitivity','specificity')

	for (i in 1:k) {
		n = nrow(x)
		train = sample(n, round(n*train.size))
		xtrain = x[train,]
		ytrain = y[train]
		xtest = x[-train,]
		ytest = y[-train]
		
		model = lm(ytrain ~ .^2, data=xtrain)

		prediction = predict(model, xtest)
		prediction = as.factor(ifelse(prediction>0.5, 1, 0))
		conftab = table(pred=prediction, true=ytest)

		tp = conftab[2,2]
		tn = conftab[1,1]
		pos = (conftab[1,2] + tp)
		neg = (tn + conftab[2,1])
		truepos = conftab[2,2]
		tpandfp = conftab[2,1] + tp
		
		if (pos==0 | neg==0 | tpandfp==0) {
			accu = NA
			sens = NA
			spec = NA
			prec = NA
		} else {
			accu = (tp + tn) / (pos + neg)
			sens = tp / pos
			spec = tn / neg
			prec = tp / tpandfp
		}
				
		scores[i,1] = i
		scores[i,2] = accu
		scores[i,3] = prec
		scores[i,4] = sens
		scores[i,5] = spec		
	}
	scores[k+1,1] = 'mean'
	scores[k+1,2] = mean(scores[1:k,2], na.rm=T)
	scores[k+1,3] = mean(scores[1:k,3], na.rm=T)
	scores[k+1,4] = mean(scores[1:k,4], na.rm=T)
	scores[k+1,5] = mean(scores[1:k,5], na.rm=T)
	
	scores[k+2,1] = 'sd'
	scores[k+2,2] = sd(scores[1:k,2], na.rm=T)
	scores[k+2,3] = sd(scores[1:k,3], na.rm=T)
	scores[k+2,4] = sd(scores[1:k,4], na.rm=T)
	scores[k+2,5] = sd(scores[1:k,5], na.rm=T)
	
	return(scores)
}

crossval.svm <- function(x, y, k=10, train.size=0.9, gamma, cost) {
	scores = data.frame(matrix(ncol=5, nrow=k+2))
	names(scores) = c('fold','accuracy','precision','sensitivity','specificity')
	
	for (i in 1:k) {
		n = nrow(x)
		train = sample(n, round(n*train.size))
		xtrain = x[train,]
		ytrain = y[train]
		xtest = x[-train,]
		ytest = y[-train]
		
		model = svm(xtrain, as.factor(ytrain), kernel='radial', gamma=gamma, cost=cost, type='C')
		prediction = predict(model, xtest)
	
		conftab = table(pred=prediction, true=ytest)

		tp = conftab[2,2]
		tn = conftab[1,1]
		pos = (conftab[1,2] + tp)
		neg = (tn + conftab[2,1])
		truepos = conftab[2,2]
		tpandfp = conftab[2,1] + tp
		
		if (pos==0 | neg==0 | tpandfp==0) {
			accu = NA
			sens = NA
			spec = NA
			prec = NA
		} else {
			accu = (tp + tn) / (pos + neg)
			sens = tp / pos
			spec = tn / neg
			prec = tp / tpandfp
		}
				
		scores[i,1] = i
		scores[i,2] = accu
		scores[i,3] = prec
		scores[i,4] = sens
		scores[i,5] = spec
	}
	scores[k+1,1] = 'mean'
	scores[k+1,2] = mean(scores[1:k,2], na.rm=T)
	scores[k+1,3] = mean(scores[1:k,3], na.rm=T)
	scores[k+1,4] = mean(scores[1:k,4], na.rm=T)
	scores[k+1,5] = mean(scores[1:k,5], na.rm=T)
	
	scores[k+2,1] = 'sd'
	scores[k+2,2] = sd(scores[1:k,2], na.rm=T)
	scores[k+2,3] = sd(scores[1:k,3], na.rm=T)
	scores[k+2,4] = sd(scores[1:k,4], na.rm=T)
	scores[k+2,5] = sd(scores[1:k,5], na.rm=T)
	
	return(scores)
}