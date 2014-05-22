


model = lm(ytrain ~ ., data=xtrain)

prediction = predict(model, xtest)
prediction = as.factor(ifelse(prediction>0.5, 1, 0))
conftab = table(pred=prediction, true=ytest)
conftab

sens = conftab[2,2]/(conftab[1,2]+conftab[2,2])
spec = conftab[1,1]/(conftab[1,1]+conftab[2,1])
prec = conftab[2,2]/(conftab[2,1]+conftab[2,2])
sens; spec; prec



rmse(prediction, ytest)
mae(prediction, ytest)
mape(prediction, ytest)
summary(model)


mape <- function(pred, obs) {
	return( mean( ifelse(obs==0, NA, abs((pred-obs) / obs)), na.rm=T))
}


tuned = tune.svm(xtrain, as.factor(ytrain), kernel='radial', gamma=10^(-5:-1), cost=10^(0:2), type='C')
model = tuned$best.model
prediction = predict(tuned$best.model, xtest)



for (i in 10:17) s[i] = imd.norm(s[i])

top.bot <- function(v) {
	return( v = ifelse(v < mean(v, na.rm=T), 0, 1))
}



[1] "station_id"   "station_name" "p.res"        "diversity"    "p.x"         
 [6] "f.sum.x"      "degree.x"     "res.abs.mean" "res.sd.y"     "imd.x"       
[11] "income.x"     "empl.x"       "health.x"     "housing.x"    "crime.x"     
[16] "livenv.x"     "edu.x"  

qs1and4 <- function(data, i) {
	x <- data[,i]
	qs <- quantile(x)
	return(data[x<=qs[2] | x>qs[4],])
}

qs1and4.index <- function(data, i) {
	x <- data[,i]
	qs <- quantile(x)
	row.names(data[x<=qs[2] | x>qs[4],])
}

matching.xs <- function(data, ys, match.by='station_id', is=3:9) {
	xs = merge(data, ys, by=match.by)
	if (!is.null(is))
		xs = xs[is]
	return(xs)	
}

n = nrow(x)
train90 = sample(n, round(n*.9))
xtrain = x[train90,]
ytrain = y[train90]
xtest = x[-train90,]
ytest = y[-train90]

add.pairs <- function(data) {
 	a = combn(1:7, 2)
	xx = data.frame(matrix(ncol=ncol(a),nrow=nrow(data)))
	for (i in 1:21) { 
		xx[,i] = data[a[1,i]]*data[a[2,i]] 
	}
	return(cbind(data, xx))
}

# linear regression summary
for (i in 10:17) {
	print(summary(lm(stations2[,i] ~ ., data=stations2[3:9])))
}

# linear regression summaries for q 1 and 4
for (i in 10:17) {
	data = qs1and4(stations2, i)[c(3:9,i)]
	print(summary(lm(data[,8]~ ., data=data[1:7])))
}