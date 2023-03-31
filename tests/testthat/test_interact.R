library(remstats)

test_that("interaction effects", {
	form <- ~ inertia():reciprocity() + FEtype():inertia()
	history$type <- history$setting
	reh <- remify::remify(history, model = "tie")
	out <- tomstats(form, reh = reh)
	stats <- out$statistics
	expect_equal(stats[,,5], stats[,,2]*stats[,,3])	
	expect_equal(stats[,,6], stats[,,2]*stats[,,4])	
	
	reh <- remify::remify(history[,c(1:3)], model = "actor")
	form <- ~ inertia():reciprocity()
	aomres <- aomstats(receiver_effects = form, reh = reh)
	stats <- aomres$receiver_stats
	expect_equal(stats[,,3], stats[,,2]*stats[,,1])
	
	form <- ~ indegreeSender()*recencySendSender()
	aomres <- aomstats(sender_effects = form, reh = reh)
	stats <- aomres$sender_stats
	expect_equal(stats[,,4], stats[,,3]*stats[,,2])
})

