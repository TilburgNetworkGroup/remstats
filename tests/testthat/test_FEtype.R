library(remstats)

test_that("FEtype effect in remstats", {
	
	colnames(history)[4] <- "type"
	reh <- remify::remify(history, model = "tie")
	tie_stats <- tomstats(~ FEtype(), reh = reh)
	stats <- tie_stats
	riskset <- attr(tie_stats, "riskset")
	riskset[,3] <- ifelse(riskset[,3] == "work", 1, 0)
	expect_equal(riskset[,3]*nrow(history), colSums(stats[,,"FEtype"]))
	expect_equal(rep(nrow(riskset)/2, nrow(history)), rowSums(stats[,,"FEtype"]))
})




