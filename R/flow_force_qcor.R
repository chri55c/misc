
flow.force.qcor <- function(name,q) {
	df <- data.frame(r=rep(NA,q), p.value=rep(NA, q),size=rep(NA,q),stringsAsFactors=FALSE)

    for (i in 1:q) {
        nodes <- read.csv(paste(name,"_q",
        			i,".csv",sep=""))
        nodes.cor = cor.test(nodes$flow,
        				nodes$gforce)
        df[i,] <- list(nodes.cor$estimate,
        			nodes.cor$p.value,
        			nrow(nodes))
    }
    df
}
