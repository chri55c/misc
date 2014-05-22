nodes = read.csv("am-peak-norm [Nodes].csv")

y = nodes$Out.Degree
max = max(y)
pdf("outdegree.pdf")
hist(y,seq(0,max+10-max%%10,10),main="Out Degree Distribution",xlab="Out Degree",ylab="Frequency")
dev.off()