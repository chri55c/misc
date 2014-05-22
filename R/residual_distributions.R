edgesedges = read.csv(untransformed.file)
# might need to take off row numbers
if (names(edges)[1]=="X") edges = edges[2:ncol(edges)]

# find transformations by trial and error by ploting hists
oldpar = par(mfrow=c(3,3))
bars = 40
hist((edges$imd_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$income_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$empl_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$health_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$housing_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$crime_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$livenv_ij),n=bars,freq=FALSE,main="",ylab="")
hist((edges$edu_ij),n=bars,freq=FALSE,main="",ylab="")
par(oldpar)


# then put transformations in data.frame
res.trans = edges[1:13]
# add columns like below

# then plot hists
pdf(pdf.filename)
oldpar = par(mfrow=c(3,3))
bars = 40
hist(res.trans$imd_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$income_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$empl_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$health_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$housing_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$crime_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$livenv_ij,n=bars,freq=FALSE,main="",ylab="")
hist(res.trans$edu_ij,n=bars,freq=FALSE,main="",ylab="")
par(oldpar)
dev.off()

# writer transformations ready to be binned
write.csv(res.trans,transformed.file)

#------------------------------------------------#

# distributions transformed so they are close to normal

# products >> f_ij = f_i * f_j
# if min(f) <= 0, f_i = f_i - min(f) + 1



# all and weekends
res.trans$imd_ij = sqrt(edges$imd_ij)
res.trans$income_ij = log(0.01 + edges$income_ij)
res.trans$empl_ij = log(0.01 + edges$empl_ij)
res.trans$health_ij = edges$health_ij
res.trans$housing_ij = sqrt(edges$housing_ij)
res.trans$crime_ij = edges$crime_ij
res.trans$livenv_ij = sqrt(edges$livenv_ij)
res.trans$edu_ij = log(5 + edges$edu_ij)

pdf.filename = "all_imdprod_dists.pdf"
pdf.filename = "wknd_imdprod_dists.pdf"

#------------------------------------------------#

# distributions of residuals
pdf("all_res_dist.pdf")
oldpar = par(mfrow = c(1,2))
hist(edges$res.log,n=50,main='')
hist(edges$res.norm.log,n=50,main='')
par(oldpar)
dev.off()