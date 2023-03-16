library(remify)
library(remstats)

test_that("baseline", {
	
	tomres <- tomstats(~ 1, reh = history)
	aomres <- aomstats(sender_effects = ~ 1, reh = history)
	 
	expect_true(all(tomres$statistics == 1))
	expect_true(all(aomres$statistics$rate == 1))
})

