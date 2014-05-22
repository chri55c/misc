bins.all = bins.all[bins.all$bin.size>0,]
bins.am = bins.am[bins.am$bin.size>0,]
bins.wknd = bins.wknd[bins.wknd$bin.size>0,]

xmin = min(min(bins.all$bin.mean),min(bins.am$bin.mean),min(bins.wknd$bin.mean))
xmax = max(max(bins.all$bin.mean),max(bins.am$bin.mean),max(bins.wknd$bin.mean)) +0.1

ymin = min(min(bins.all$flow.mean-bins.all$flow.sd), min(bins.am$flow.mean-bins.am$flow.sd), min(bins.wknd$flow.mean-bins.wknd$flow.sd)) - 0.1
ymax = max(max(bins.all$flow.mean+bins.all$flow.sd), max(bins.am$flow.mean+bins.am$flow.sd), max(bins.wknd$flow.mean+bins.wknd$flow.sd)) +0.1

plot(bins$bin.mean, bins$flow.mean, main="Estimated vs Observed Flows", xlab="Estimated Flow", ylab="Observed Flow", xlim=c(xmin,xmax), ylim=c(ymin,ymax), pch="")

error.bars(bins.am$bin.mean, bins.am$flow.mean, bins.am$flow.sd, col="#529DFF", length=0.05)
error.bars(bins.wknd$bin.mean, bins.wknd$flow.mean, bins.wknd$flow.sd, col="#62E362",length=0.05)
error.bars(bins.all$bin.mean,bins.all$flow.mean, bins.all$flow.sd,col="#FF5252", length=0.05)


points(bins.wknd$bin.mean, bins.wknd$flow.mean, type='l', cex=2,col="#00C700")
points(bins.am$bin.mean,bins.am$flow.mean,type='l',cex=2,col="#0051FF")
points(bins.all$bin.mean, bins.all$flow.mean, type='l', cex=2, col="#FF0000")