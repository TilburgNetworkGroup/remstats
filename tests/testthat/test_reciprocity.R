library(remify)
library(remstats)

test_that("reciprocity", {
	data(history)
	history$weight <- 1
	
	effects <- ~ reciprocity()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]), 0:(nrow(history)-1))
	expect_true(all(sapply(1:nrow(aomres$statistics$choice), function(i) {
		aomres$statistics$choice[i,,] %in% c(tomres$statistics[i,,2], 0)
	})))
	
	effects <- ~ reciprocity(scaling = "prop") + reciprocity() + indegreeSender()
	choiceEffects <- ~ reciprocity(scaling = "prop") + reciprocity() 
	rateEffects <- ~ outdegreeSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = choiceEffects, rateEffects = rateEffects,
		edgelist = history)
	
	temp <- tomres$statistics[,,3]/tomres$statistics[,,4]
	temp[is.na(temp)] <- 1/9
	expect_equal(tomres$statistics[,,2], temp)
	expect_equal(rowSums(aomres$statistics$choice[,,1]), rep(1, nrow(history)))

	effects <- ~ reciprocity(scaling = "std") 
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	effects <- ~ reciprocity(consider_type = TRUE) 
	tomres <- tomstats(effects, edgelist = history)
	expect_equal(rowSums(tomres$statistics[,,2]), 0:(nrow(history)-1))
})