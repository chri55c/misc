
# correlation matrix
ggplot(melt(cor(building)), aes(x=X1, y=X2, fill=value, label=round(value, 2))) + scale_fill_gradient2(low='red') + geom_tile() + geom_text(size=1.5) +theme(axis.title=element_blank()) + scale_y_discrete(labels=1:33) + scale_x_discrete(labels=1:33)