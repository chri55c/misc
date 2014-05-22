source("R/kfold_svm.r")

s = read.csv('stations_svmtable.csv')
# check column index
names(s)

m = 8
imd.is = 20:27
x = s[,3:19]
n = nrow(x)

# classify stations according to if they are in top quartile (1) or not (-1)
ys = data.frame(matrix(nrow=n,ncol=m))
for (i in 1:m) {
	ys[,i] = ifelse(s[,imd.is[i]]>quantile(s[,imd.is[i]])[4], 1, -1)
	ys[,i] = as.factor(ys[,i])
}
names(ys) = names(s)[imd.is]


# find best parameters for each y
train80 = sample(n, round(0.8*n))
tuned = list()
length(tuned) = m
for (i in 1:m) { 
	tuned[[i]] = tune.svm(x[train80,], ys[train80,i], gamma=10^(-3:-1), cost=10^(0:2))
}

# get svm predictiond for each y
predictions = list()
length(predictions) = m

for (i in 1:m) {
	predictions[[i]] = predict(tuned[[i]]$best.model, x[-train80,]) 
}

# calculate performance scores for each y
conftabs = list()
length(conftabs) = m

for (i in 1:m) { 
	conftabs[[i]] = table(pred=predictions[[i]], true=ys.c[-train80,i])
}

scores = data.frame(matrix(ncol=3, nrow=m))
names(scores) = c('sensitivity','specificity','precision')
row.names(scores) = colnames(ys)

for (i in 1:m) { 
	conftab = conftabs[[i]]
	pos = (conftab[1,2]+conftab[2,2])
	neg = (conftab[1,1]+conftab[2,1])
	truepos = conftab[2,2]
	tpandfp = conftab[2,1] + truepos
		
	if (pos==0 | neg==0 | tpandfp==0) {
		sens = NA
		spec = NA
		prec = NA
	} else {
		sens = truepos / pos
		spec = conftab[1,1] / neg
		prec = truepos / tpandfp
	}
	scores[i,1] = sens
	scores[i,2] = spec
	scores[i,3] = prec
}
scores

errors = data.frame(matrix(ncol=1, nrow=m))
names(errors) = 'rmse'
row.names(errors) = colnames(ys)
for (i in 1:m) { 
	errors[i,1] = rmse(predictions[[i]], ys[-train80,i])
	}
errors


