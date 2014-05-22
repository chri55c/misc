
df = <data>
dodge= position_dodge(width=.9)
err.limits = aes(ymax=mean + sd, ymin=mean - sd)

ggplot(df, aes(x=X.ref_points, y=mean, fill=ref_points_type)) 	+ geom_bar(stat='identity', position=dodge) 
	+ geom_errorbar(err.limits, position=dodge, width=0.5) 
	+ xlab('# ref points') 
	+ ylab('sensitivity')

ggsave('<data>_sensitivity.pdf')