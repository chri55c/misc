detach(station)
attach(station)
station = station[relation=="source",]
flow = weight*population_per_km
flowsum = sum(flow)
column = c(sum(flow*imd_amt) / flowsum,
sum(flow*income_amt) / flowsum,
sum(flow*employment_amt) / flowsum,
sum(flow*health_amt) / flowsum,
sum(flow*housing_amt) / flowsum,
sum(flow*crime_amt) / flowsum,
sum(flow*living_environment_amt) / flowsum,
sum(flow*education_amt) / flowsum)
column = cbind(column)
row.names(column) = c("imd","income","employment","health","housing","crime","environment","education")
column