cum_val <- data.frame()
icd9Dict <- unique(cmshcc_map$icd9)
for(i in seq(from = 1000000, to = 3000000, by = 1000000)) {
	PERSON <- generateTestPERSON(size = i)
	DIAG <- generateTestDIAG(i, 2, 10, icd9Dict)
	val <- benchmark(icd9RiskAdjCMSHCC(), replications = 10)
	val <- val$elapsed
	cum_val <- c(cum_val, val)
}