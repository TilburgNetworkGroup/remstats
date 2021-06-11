library(remify)
library(remstats)

test_that("inertia", {
	data(history)
	history$weight <- 1
	
	effects <- ~ inertia()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]), 0:(nrow(history)-1))
	expect_true(all(sapply(1:nrow(aomres$statistics$choice), function(i) {
		aomres$statistics$choice[i,,] %in% tomres$statistics[i,,2]
	})))
	
	effects <- ~ inertia(scaling = "prop") + inertia() + outdegreeSender()
	choiceEffects <- ~ inertia(scaling = "prop") + inertia() 
	rateEffects <- ~ outdegreeSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = choiceEffects, rateEffects = rateEffects,
		edgelist = history)
	
	temp <- tomres$statistics[,,3]/tomres$statistics[,,4]
	temp[is.na(temp)] <- 1/9
	expect_equal(tomres$statistics[,,2], temp)
	expect_equal(rowSums(aomres$statistics$choice[,,1]), rep(1, nrow(history)))

	effects <- ~ inertia(scaling = "std") 
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	effects <- ~ inertia(consider_type = TRUE) 
	tomres <- tomstats(effects, edgelist = history)
	expect_equal(rowSums(tomres$statistics[,,2]), 0:(nrow(history)-1))
})