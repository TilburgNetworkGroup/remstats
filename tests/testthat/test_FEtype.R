library(remify)
library(remstats)

test_that("FEtype effect in remstats", {
	
	colnames(history)[4] <- "type"
	tomres <- tomstats(~ FEtype(), edgelist = history)
	stats <- tomres$statistics
	riskset <- tomres$riskset
	riskset[,3] <- ifelse(riskset[,3] == "work", 1, 0)
	expect_equal(riskset[,3]*nrow(history), colSums(stats[,,"FEtype"]))
	expect_equal(rep(nrow(riskset)/2, nrow(history)), rowSums(stats[,,"FEtype"]))
})




