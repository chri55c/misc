
station.means <- function(s, source = TRUE) {
	station <- s
	if (source) {
		station = s[s$relation=="source",]
	} else {
		station = s[s$relation=="target",]
	}
	flow = station$weight*station$population_per_km
	flowsum = sum(flow)
	c(sum(flow*station$imd_amt) / flowsum,
		sum(flow*station$income_amt) / flowsum,
		sum(flow*station$employment_amt) / flowsum,
		sum(flow*station$health_amt) / flowsum,
		sum(flow*station$housing_amt) / flowsum,
		sum(flow*station$crime_amt) / flowsum,
		sum(flow*station$living_environment_amt) / flowsum,
		sum(flow*station$education_amt) / flowsum,
		sum(flow*station$time) / flowsum)
}