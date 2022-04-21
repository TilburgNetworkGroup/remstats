library(remify)
library(remstats)

# Test
test_that("event", {
	skip("event not yet implemented in new version")
	
	data(history)
	work <- ifelse(history$setting == "work", 1, 0)
	tomres <- tomstats(~ event(work, variableName = "work"), edgelist = history)
	stats <- tomres$statistics
	riskset <- tomres$riskset
	
	expect_equal(stats[,,"event.work"], replicate(n = nrow(riskset), work))
})