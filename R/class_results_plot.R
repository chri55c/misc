
repeat.8times <- function(x) {
	return(rep(x, 8))
}

domain.names = row.names(LR1)
domains = rep(domain.names, 3)

score.names = names(LR1)[1:3]
score.types = c(apply(as.array(score.names), 1, repeat.8times))

d = LR1
df = data.frame(domain=factor(domains, levels=domain.names), 				score.type=factor(score.types, levels=score.names),
				score=c(d$Precision, d$Sensitivity, d$Specificity))
	
			
ggplot(data=df, aes(x=domain, y=score, group=score.type, shape=score.type)) + 
    geom_line(aes(linetype=score.type), size=1) +     
    geom_point(size=5, fill="white") +        
    scale_colour_hue(name="", l=30)  + 
    scale_shape_manual(name="", values=c(0,1,2)) +  
    scale_linetype_discrete(name="") +
    ylim(.4,.95) +
    xlab("IMD Domain") + ylab("Score") + 
    theme_bw() +
    opts(	legend.position=c(.3, .8), 	
    		legend.background=theme_rect(size=0), 
    		legend.key.size=unit(0.08,'npc'), 
    		legend.text=theme_text(size=18),
    		axis.text.x=theme_text(size=16, angle=45),
    		axis.text.y=theme_text(size=16),
    		axis.title.x=theme_text(size=20),
    		axis.title.y=theme_text(size=20, angle=90)
    )

ggplot(stations, aes(x=res_sd)) + 
	geom_histogram(colour='black', fill='white') +
	xlab('Station gravity residual std. dev.') +
	ylab('Frequency') +
	 opts(	axis.text.x=theme_text(size=16,),
    		axis.text.y=theme_text(size=16),
    		axis.title.x=theme_text(size=20),
    		axis.title.y=theme_text(size=20, angle=90)
    )
    
rescut = cut(s$res_mean, seq(-110,2,1), right=FALSE)
rescf = cumsum(table(rescut))
rescf = as.data.frame(cbind(-109:2,rescf)    
ggplot(data=rescf, aes(x=cf, y=g, group=1)) + 
	geom_line(size=1.5) + 
	xlab('Station gravity residual') +
	ylab('Cumulative frequency') +
	 opts(	axis.text.x=theme_text(size=16,),
    		axis.text.y=theme_text(size=16),
    		axis.title.x=theme_text(size=20),
    		axis.title.y=theme_text(size=20, angle=90)
    )


lsoa = readShapePoly('~/data/boundary-data/glondon_lsoa_2001/glondon_low_soa_2001.shp')
london_imd = read.csv('~/data/demographic/london_imd.csv')
imd = london_imd$IMD.SCORE
q = quantile(imd)
for (i in 1:length(imd)) {
	if (imd[i] < q[2]) imd[i] = 1
	else if (imd[i] < q[3]) imd[i] = 2
	else if (imd[i] < q[4]) imd[i] = 3
	else imd[i] = 4
}
london_imd$IMD.SCORE = imd

gpclibPermit()
lsoa.geom = fortify(lsoa, region='zonecode')
lsoa.imd = merge(lsoa.geom, london_imd[c(1,8)], by.x='id', by.y='LSOA.CODE')
head(lsoa.imd)

map<- ggplot(lsoa.imd, aes(x=long, y=lat)) +
		geom_polygon(aes(fill=IMD.SCORE, group=id)) +
		#scale_fill_identity(colour=c('white','grey30','grey60','black'),
		#					labels=c('Q1','Q2','Q3','Q4'),
		#					name='IMD quartile') +
		scale_colour_grey(name='IMD quartile', start=0, end=1, limits=4) +
		theme_map(18)
   

	
> map
> ggsave('imd_map.pdf')


