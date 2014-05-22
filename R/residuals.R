
# edges
for (k in seq(0,20,2)) {
	file = paste("residuals/residual_imd_gt", k, "se.csv", sep="")
	res.gtse = read.csv(file)
	print('-------------------')
	print(k)
    print(length(res.gtse$r_ij))
    for (j in seq(9,23,2)) { 
    	res.cor = cor.test(res.gtse$r_ij,abs(res.gtse[,j]-res.gtse[,j+1]))
    	print(paste("correlation:", res.cor$estimate))
    	print(paste("p-value:", res.cor$p.value))
    }
}

# stations
station.res = read.csv("station_residuals.csv")
station.res.names = names(station.res)
for (i in c(2:7,9)) {
	print("-------------")
	for (j in 11:18) {
		print(paste(station.res.names[i],"~",station.res.names[j]))
		res.cor = cor.test(station.res[,i],station.res[,j])
		print(paste("correlation:", res.cor$estimate))
    	print(paste("p-value:", res.cor$p.value))
	}
}

edges = read.csv("peak_user_1_1_edges_g.csv")
edges = data.frame(edges$s1,edges$s2,edges$p1,edges$p2,edges$distance,edges$weight)
names(edges) = c("s_i","s_j","p_i","p_j","d_ij","f_ij")

lm.est = exp(lm.coefs[1]) * ((p_i^lm.coefs[2] * p_j^lm.coefs[3]  * empl_ij^lm.coefs[6] * health_ij^lm.coefs[7] * crime_ij^lm.coefs[8] * livenv_ij^lm.coefs[9] * edu_ij^lm.coefs[10]) / (d_ij^lm.coefs[11] * imd_ij^-lm.coefs[4] * income_ij^-lm.coefs[5]))


edges = read.csv("residuals/residual_imd_gt0se.csv");
attach(edges)
g_ij = p_i*p_j / d_ij^2

res = g_ij - f_ij 
res.abs = abs(res)
res.norm = res / f_ij
res.norm.abs = abs(res.norm)
res.abs.log = log(res.abs)
res.norm.abs.log = log(res.norm.abs)
res.abs.log.z = scale(res.abs.log)
res.norm.abs.log.z = scale(res.norm.abs.log)

imd = abs(imd_i - imd_j)
income = abs(income_i - income_j)
empl = abs(empl_i - empl_j)
health = abs(health_i - health_j)
housing = abs(housing_i - housing_j)
crime = abs(crime_i -crime_j)
livenv = abs(livenv_i - livenv_j)
edu = abs(edu_i - edu_j)

imd.fns = c("imd","income","empl","health","housing","crime","livenv","edu")
imd.names = c("IMD","Income","Employment","Health","Housing","Crime","Living Environment","Education")
res.fns = c("reslog","reslogz","nreslog","nreslogz")
res.names = c("Logarithm of Absolute Residual","Z-score of Logarithm of Absolute Residual","Logarithm of Absolute Normalised Residual","Z-score of Logarithm of Normalised Absolute Residual")
imd.df = data.frame(imd,income,empl,health,crime,housing,livenv,edu)
res.df = data.frame(res.abs.log,res.abs.log.z,res.norm.abs.log,res.norm.abs.log.z)

for (i in 1:8) {
	for (j in 1:4) {
		res.cor = cor.test(res.df[,j],imd.df[,i])
		print(paste(res.fns[j],"~",imd.fns[i]))
		print(paste("r =",res.cor$estimate,", p-value =",res.cor$p.value))
		print("-------------------")
	}
}

# correlation plots (don't use this, use binned below)
oldpar = par(mfrow=c(3,3))
for (i in 1:4) {
	pdf(paste("resplots/imd_vs_",res.fns[i],".pdf",sep=""))
	for (j in 1:8) {
		plot(res.df[,i], imd.df[,j], main="", xlab="", ylab=imd.names[j], pch=".")
	}	
	dev.off()
}
par(oldpar)
#################

# correlation plots binned
bin.files = c("imd","income","empl","health","housing","crime","livenv","edu")

pdf.files = c("all_undir_reslog_dif.pdf","all_undir_nreslog_dif.pdf")

ys = c(5,7,9,11)
xlabs = c(expression(IMD['i,j']),expression(Income['i,j']),expression(Employment['i,j']),expression(Health['i,j']),expression(Housing['i,j']),expression(Crime['i,j']),expression(LivingEnv['i,j']),expression(Education['i,j']))


edges = read.csv("offpk_residuals_imdprod.csv")
edges = edges[2:21] # get rid of row number column
res.xs = c(13:20)
res.ys = c(10,12)

for (f in 1:2) {
	pdf(pdf.files[f])
	oldpar = par(mfrow=c(3,3))
	for (i in 1:8) {
		res.cor = cor.test(edges[,res.xs[i]],edges[,res.ys[f]])
		title = sprintf("pcc = %1.3f\np-value = %1.2E",res.cor$estimate,res.cor$p.value)
		bins = read.csv(paste(bin.files[i],"_all_undir_imddif_bins.csv",sep=""))
		bins = bins[bins$bin.size>0,]

		plot(bins$bin.mean,bins[,ys[f]],main=title,xlab=xlabs[i],ylab="",pch="")
		error.bars(bins$bin.mean,bins[,ys[f]],bins[,ys[f]+1],col=256,length=0.01)
		points(bins$bin.mean,bins[,ys[f]],pch=4,cex=0.5,lwd=0.5)
	}
	par(oldpar)
	dev.off()
}
##############

# pop dens corelations
pdf("all_pdprod_reslog.pdf")
res.cor = cor.test(edges$res.log,edges$pd_ij)
 		title = sprintf("pcc = %1.3f\np-value = %1.2E",res.cor$estimate,res.cor$p.value)
 		bins = bins[bins$bin.size>0,]
 		plot(bins$bin.mean,bins$res.log.mean,main=title,pch="")
 		error.bars(bins$bin.mean,bins$res.log.mean,bins$res.log.sd,col=256)
 		points(bins$bin.mean,bins$res.log.mean,pch=4)
 dev.off()


# linear regression models

residuals = read.csv("all_residuals_imdprod.csv")
attach(residuals)
res.fit = lm(res.log~ imd_ij + income_ij + empl_ij + health_ij + housing_ij + crime_ij + livenv_ij + edu_ij + f_ij)
summary(res.fit)
res.fit = lm(res.norm.log~ imd_ij + income_ij + empl_ij + health_ij + housing_ij + crime_ij + livenv_ij + edu_ij + f_ij)
summary(res.fit)

## 1 factor + flow
fit = lm(res.log~ imd_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ imd_ij + f_ij)
summary(fit)

fit = lm(res.log~ income_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ income_ij + f_ij)
summary(fit)

fit = lm(res.log~ empl_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ empl_ij + f_ij)
summary(fit)

fit = lm(res.log~ health_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ health_ij + f_ij)
summary(fit)

fit = lm(res.log~ housing_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ housing_ij + f_ij)
summary(fit)

fit = lm(res.log~ crime_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ crime_ij + f_ij)
summary(fit)

fit = lm(res.log~ livenv_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ livenv_ij + f_ij)
summary(fit)

fit = lm(res.log~ edu_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ edu_ij + f_ij)
summary(fit)

# 2 factors + flow
fit = lm(res.log~ imd_ij + income_ij + f_ij) # empl, health
summary(fit)
fit = lm(res.norm.log~ imd_ij + income_ij + f_ij)
summary(fit)

fit = lm(res.log~ imd_ij + empl_ij + f_ij) # empl, health
summary(fit)
fit = lm(res.norm.log~ imd_ij + empl_ij + f_ij)
summary(fit)

fit = lm(res.log~ imd_ij + health_ij + f_ij) # empl, health
summary(fit)
fit = lm(res.norm.log~ imd_ij + health_ij + f_ij)
summary(fit)

fit = lm(res.log~ income_ij + health_ij + f_ij) # health
summary(fit)
fit = lm(res.norm.log~ income_ij + health_ij + f_ij) # health
summary(fit)

fit = lm(res.log~ income_ij + empl_ij + f_ij) # health
summary(fit)
fit = lm(res.norm.log~ income_ij + empl_ij + f_ij) # health
summary(fit)

fit = lm(res.log~ empl_ij + health_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ empl_ij + health_ij + f_ij)
summary(fit)

# 3 factors + flow
fit = lm(res.log~ income_ij + empl_ij + health_ij + f_ij)
summary(fit)
fit = lm(res.norm.log~ income_ij + empl_ij + health_ij + f_ij)
summary(fit)


# factor correlations
edges.names = names(edges)
print("x ~ y,pcc,p-value")
for (i in 13:19) { 
	for (j in (i+1):20) {
		f.cor = cor.test(edges[,i], edges[,j])
		print(paste(edges.names[i],"~",edges.names[j],",",f.cor$estimate,",",f.cor$p.value))
		
	}	
}

title = ""
plot(bins$bin.mean, bins$r.norm.log_ij.mean, main=title, xlab="d_ij", ylab="log(residual_ij/flow_ij)", pch="")
error.bars(bins$bin.mean, bins$r.norm.log_ij.mean, bins$r.norm.log_ij.sd,col=256)
points(bins$bin.mean, bins$r.norm.log_ij.mean,pch=4)