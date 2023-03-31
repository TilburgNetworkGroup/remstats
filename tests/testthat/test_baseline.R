library(remstats)

test_that("baseline", {
	
	reh_tie <- remify::remify(history, model = "tie")
	tomres <- tomstats(~ 1, reh = reh_tie)
	reh_actor <- remify::remify(history, model = "actor")
	aomres <- aomstats(sender_effects = ~ 1, reh = reh_actor)
	 
	expect_true(all(tomres$statistics == 1))
	expect_true(all(aomres$rate == 1))
})

