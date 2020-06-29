context("interact")

library(remstats)

out <- remstats(~ inertia():reciprocity(), edgelist = history)
stats <- out$statistics

test_that("multiplication", {
		expect_equal(stats[,,3], stats[,,1]*stats[,,2])	
})