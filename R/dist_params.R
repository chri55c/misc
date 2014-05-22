

folder = "lsoa_results/"
df = lsoa.pc
for (j in c(3, 6, 10)) {
	for (i in 12:19) {
		xy = df[with(df, order(df[, i])), ]
		x = as.data.frame(normalise(xy[2:(j + 1)]))

		y = xy[i]
		q = quantile(y[, 1])
		y = ifelse(y[, 1] > q[4], 1, 0)

		scores.lm = crossval.lm.classify(x, y)
		scores.lm2 = crossval.lm2.classify(x, y)
		scores.svm = crossval.svm.classify(x, y)

		fname = paste(j, "_pc_", names(df)[i], sep = "")
		write.csv(scores.lm, paste(folder, fname, "_lm.csv", sep = ""))
		write.csv(scores.lm2, paste(folder, fname, "_lm2.csv", sep = ""))
		write.csv(scores.svm, paste(folder, fname, "_svm.csv", sep = ""))
	}
}
df = lsoa.r

for (j in c(3, 6, 10)) {
	for (i in 12:19) {
		xy = df[with(df, order(df[, i])), ]
		x = as.data.frame(normalise(xy[2:(j + 1)]))

		y = xy[i]
		q = quantile(y[, 1])
		y = ifelse(y[, 1] > q[4], 1, 0)

		scores.lm = crossval.lm.classify(x, y)
		scores.lm2 = crossval.lm2.classify(x, y)
		scores.svm = crossval.svm.classify(x, y)

		fname = paste(j, "_r_", names(df)[i], sep = "")
		write.csv(scores.lm, paste(folder, fname, "_lm.csv", sep = ""))
		write.csv(scores.lm2, paste(folder, fname, "_lm2.csv", sep = ""))
		write.csv(scores.svm, paste(folder, fname, "_svm.csv", sep = ""))
	}
}
