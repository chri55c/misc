
for (i in 1:nrow(pois)) 
{
	counts = ratings.count[ratings.count$id_poi==pois[i,1],]
	pois[i,5:9] = ifelse(nrow(counts)>0, counts$Freq, 0)
}