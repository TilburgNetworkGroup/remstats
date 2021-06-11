library(remify)
library(remstats)

test_that("baseline", {
	
	tomres <- tomstats(~ 1, edgelist = history)
	aomres <- aomstats(rateEffects = ~ 1, edgelist = history)
	 
	expect_true(all(tomres$statistics == 1))
	expect_true(all(aomres$statistics$rate == 1))
})

