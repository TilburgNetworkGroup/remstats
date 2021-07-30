library(remify)
library(remstats)

# Test
test_that("event", {
	
	history$work <- ifelse(history$setting == "work", 1, 0)
	tomres <- tomstats(~ event(history$work, variableName = "work"), edgelist = history)
	stats <- tomres$statistics
	riskset <- tomres$riskset
	
	expect_equal(stats[,,"event.work"], 
		replicate(n = nrow(riskset), history$work))
})