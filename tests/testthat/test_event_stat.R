context("event stat")

# Compute statistics
out <- remstats(~ event(history$setting), edgelist = history)
stats <- out$statistics
riskset <- out$riskset

# Test
test_that("event stat", {
    expect_equal(stats[,,1], 
    	replicate(n = nrow(riskset), match(history$setting, c("work", "social"))-1))
})