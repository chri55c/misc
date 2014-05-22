
ds = c("eucl","hops","intch","time","rankeucl","ranktime")

for (d in ds) {
	edges = read.csv(paste(data,d,"_g.csv",sep=""))	
	flow = log(edges$flow + 0.1)
	gforce = log(edges$gforce + 0.1)
	print(d)
 	ct = cor.test(flow,gforce)
 	print(ct$estimate)
 	print(ct$p.value)
 }
 	pdf(paste(data,d,"_real_vs_est.pdf",sep="")) 
	plot(flow,gforce,pch=".",main="Estimated vs Observed Passenger Flows",xlab="Observed",ylab="Estimated")
	dev.off()
}



# real
tld.real = read.csv(paste(data,"_tld.csv",sep=""))
pdf(paste(data,"_tld.pdf",sep=""))
plot(x=tld.real$length[1:180],y=tld.real$freq[1:180]/max(tld.real$freq),type="s",main="Observed Trip Length Frequency Distribution",xlab="Length (mins)",ylab="Normalised Frequency")
dev.off()

weighted.mean(tld.real$length,tld.real$freq)
sqrt(weighted.var3(tld.real$length,tld.real$freq))

# estimated
tld.est = read.csv(paste(data,"_g_tld.csv",sep=""))
pdf(paste(data,"_gtld.pdf",sep=""))
plot(x=tld.est$length[1:180],y=tld.est$freq[1:180]/max(tld.est$freq),type="s",main="Estimated Trip Length Frequency Distribution",xlab="Length (mins)",ylab="Normalised Frequency")
dev.off()

weighted.mean(tld.est$length,tld.est$freq)
sqrt(weighted.var3(tld.est$length,tld.est$freq))

pdf(paste(data,"_tld_real_vs_est.pdf",sep=""))
plot(tl.real$freq,tl.est$freq,xlab="Observed",ylab="Estimated",main="Correlation of Trip Length Frequency Distributions")
dev.off()