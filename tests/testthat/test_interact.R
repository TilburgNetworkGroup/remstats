library(remify)
library(remstats)

test_that("interaction effects", {
	form <- ~ inertia():reciprocity() + FEtype():inertia()
	history$type <- history$setting
	out <- tomstats(form, edgelist = history[,-5])
	stats <- out$statistics
	expect_equal(stats[,,5], stats[,,2]*stats[,,3])	
	expect_equal(stats[,,6], stats[,,2]*stats[,,4])	
	
	data(history)
	form <- ~ inertia():reciprocity()
	aomres <- aomstats(choiceEffects = form, edgelist = history[,c(1:3)])
	stats <- aomres$statistics$choice
	expect_equal(stats[,,3], stats[,,2]*stats[,,1])
	
	form <- ~ indegreeSender()*recencySendSender()
	aomres <- aomstats(rateEffects = form, edgelist = history[,c(1:3)])
	stats <- aomres$statistics$rate
	expect_equal(stats[,,4], stats[,,3]*stats[,,2])
})

