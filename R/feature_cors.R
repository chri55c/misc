cors = data.frame(matrix(nrow=8,ncol=14))
names(cors) = c('mass', 'p-value', 'flow_sum', 'p-value', 'res_mean', 'p-value', 'res_sd', 'p-value', 'diversity', 'p-value', 'degree', 'p-value', 'p_bias', 'p-value')
row.names(cors) = c('imd','income','empl','health','housing','crime','livenv','edu')

is = 10:17
js = seq(1,13,2)

for (i in 1:8) {
	for (j in 1:7) {
		corel = cor.test(s[,is[i]], s[,j+2])
		cors[i,js[j]] = corel$estimate
		cors[i,js[j]+1] = corel$p.value
	}
}