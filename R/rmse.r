rmse <- function(pred, obs) {
	return( sqrt( mean( (pred-obs)^2, na.rm=T) ))
}

nrmse <- function(pred, obs) {
	err = rmse(pred, obs) 
	return(err / (max(obs) - min(obs)) )
}