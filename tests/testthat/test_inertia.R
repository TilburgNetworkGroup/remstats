library(remify)
library(remstats)

test_that("inertia", {
	data(history)
	history$weight <- 1
	
	effects <- ~ inertia()
	tomres <- tomstats(effects, reh = history)
	aomres <- aomstats(receiver_effects = effects, reh = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]), 0:(nrow(history)-1))
	expect_true(all(sapply(1:nrow(aomres$statistics$receiver_stats), function(i) {
		aomres$statistics$receiver_stats[i,,] %in% tomres$statistics[i,,2]
	})))
	
	effects <- ~ inertia(scaling = "prop") + inertia() + outdegreeSender()
	receiver_effects <- ~ inertia(scaling = "prop") + inertia() 
	sender_effects <- ~ outdegreeSender()
	tomres <- tomstats(effects, reh = history)
	aomres <- aomstats(receiver_effects = receiver_effects, sender_effects = sender_effects,
		reh = history)
	
	temp <- tomres$statistics[,,3]/tomres$statistics[,,4]
	temp[is.na(temp)] <- 1/9
	expect_equal(tomres$statistics[,,2], temp)
	expect_equal(rowSums(aomres$statistics$receiver_stats[,,1]), rep(1, nrow(history)))

	effects <- ~ inertia(scaling = "std") 
	tomres <- tomstats(effects, reh = history)
	aomres <- aomstats(receiver_effects = effects, reh = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$receiver_stats), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	effects <- ~ inertia(consider_type = TRUE) 
	tomres <- tomstats(effects, reh = history)
	expect_equal(rowSums(tomres$statistics[,,2]), 0:(nrow(history)-1))
})