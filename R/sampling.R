
# flow parameters
y.column = ?
xy = stations[with(stations, order(stations[,y.column])),]
x = xy[3:9]
x$mass = log(x$mass)
x$flow = log(x$flow)
x$res_mean = log(x$res_mean)
x$res_sd = log(res_sd)
x = as.data.frame(normalise(x))

# distance parameters
xy = s[with(s, order(s[,y.column])),]
x = as.data.frame(normalise(xy[2:11]))


y = xy[y.column]
q = quantile(y[,1])
y = ifelse(y[,1] > q[4], 1, 0)



q = quantile(stations[,y.column])
stations0 = stations[stations[y.column]<q[4],]
stations1 = stations[stations[y.column]>=q[4],]

n1 = nrow(stations1) # sample size relative to number of positive examples
n0 = nrow(stations0)
test1 = sample(n1, round(n1*.1))
test0 = sample(n0, round(n1*.1)) # same number of negative cases

xtest = rbind(stations0[test0,3:9], stations1[test1,3:9])
xtrain = rbind(stations0[-test0,3:9], stations1[-test1,3:9])
ytest = c(stations0[test0,y.column], stations1[test1,y.column])
ytrain = c(stations0[-test0,y.column], stations1[-test1,y.column])

df = data.frame(matrix(ncol=2, nrow=50))
names(df) = c('test_ratio','train_ratio')

for (i in 1:50) {
	n = nrow(x)
	test_index = systematic_sample_index(n, test.size)
	ytrain = summary(as.factor(y[-test_index]))
	ytest = summary(as.factor(y[test_index]))
	
	df[i,1] = ytest[[1]]/ytest[[2]]
	df[i,2] = ytrain[[1]]/ytrain[[2]]
}
df