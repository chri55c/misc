
# e.g. data = "all_data_g"
# e.g. type = "_income_q"

df <- data.frame(r=rep(NA,4),size=rep(NA,4),stringsAsFactors=FALSE,q=c("1 to 1","1 to 3","3 to 3","3 to 1"))
edges = read.csv(paste(data,"_q/",data,type,"1-1.csv",sep=""))
 ec <- cor.test(edges$flow,edges$gforce)
 df[1,1:2] = list(ec$estimate,nrow(edges))
edges = read.csv(paste(data,"_q/",data,type,"1-3.csv",sep=""))
 ec <- cor.test(edges$flow,edges$gforce)
 df[2,1:2] = list(ec$estimate,nrow(edges))
edges = read.csv(paste(data,"_q/",data,type,"3-3.csv",sep=""))
  ec <- cor.test(edges$flow,edges$gforce)
  df[3,1:2] = list(ec$estimate,nrow(edges))
edges = read.csv(paste(data,"_q/",data,type,"3-1.csv",sep=""))
ec <- cor.test(edges$flow,edges$gforce)
  df[4,1:2] = list(ec$estimate,nrow(edges))
  df
  write.csv(df,file=paste(data,type,".csv",sep=""))
