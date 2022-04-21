library(remify)
library(remstats)

test_that("interaction effects", {
	skip("interaction effects not yet implemented in new version")
	form <- ~ inertia():reciprocity() + FEtype():inertia()
	history$type <- history$setting
	out <- tomstats(form, edgelist = history[,-5])
	stats <- out$statistics
	expect_equal(stats[,,5], stats[,,2]*stats[,,3])	
	expect_equal(stats[,,6], stats[,,2]*stats[,,4])	
	
	data(history)
	form <- ~ inertia():reciprocity()
	aomres <- aomstats(receiver_effects = form, edgelist = history[,c(1:3)])
	stats <- aomres$statistics$receiver_stats
	expect_equal(stats[,,3], stats[,,2]*stats[,,1])
	
	form <- ~ indegreeSender()*recencySendSender()
	aomres <- aomstats(sender_effects = form, edgelist = history[,c(1:3)])
	stats <- aomres$statistics$sender_stats
	expect_equal(stats[,,4], stats[,,3]*stats[,,2])
})

