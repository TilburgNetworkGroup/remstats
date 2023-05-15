library(remstats)

test_that("baseline", {
	
	reh_tie <- remify::remify(history, model = "tie")
	tie_stats <- tomstats(~ 1, reh = reh_tie)
	reh_actor <- remify::remify(history, model = "actor")
	aomres <- aomstats(sender_effects = ~ 1, reh = reh_actor)
	 
	expect_true(all(tie_stats == 1))
	expect_true(all(aomres$rate == 1))
})

