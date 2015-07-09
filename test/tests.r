split_wrapper <- function(df = NULL, fact = NULL) {
fact <- as.factor(fact)
mysplit <- split(x = df, f = fact)
mysplit
}

perform_tests <- function() {
	cum_val <- data.frame()
	icd9Dict <- unique(cmshcc_map$icd9)
	for(i in seq(from = 1000000, to = 3000000, by = 1000000)) {
		PERSON <- generateTestPERSON(size = i)
		DIAG <- generateTestDIAG(i, 2, 10, icd9Dict)
		val <- benchmark(icd9RiskAdjCMSHCC(), replications = 10)
		val <- val$elapsed
		cum_val <- c(cum_val, val)
	}
	diag <- generateTestDIAG(size = 1000, cmshcc_map = cmshcc_map)
	benchmark(split_diag <- split_wrapper(df = diag, fact = diag$HICNO))
}