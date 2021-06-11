library(remify)
library(remstats)

test_that("send", {
	data(history)
	data(info)
	
	effects <- ~ send("extraversion")
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(rateEffects = effects, edgelist = history, attributes = info)
	
	expect_true(all(tomres$statistics[,,2] %in% info$extraversion))
	expect_true(all(aomres$statistics$rate[,,2] %in% info$extraversion))
	
	effects <- ~ send("extraversion", scaling = "std")
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(rateEffects = effects, edgelist = history, attributes = info)
	
	expect_equal(rowSums(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowSums(aomres$statistics$rate[,,2]), rep(0, nrow(history)))
	
	effects <- ~ send("extraversion", attributes = info)
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_true(all(tomres$statistics[,,2] %in% info$extraversion))
	expect_true(all(aomres$statistics$rate[,,2] %in% info$extraversion))
})

test_that("receive", {
	data(history)
	data(info)
	
	effects <- ~ receive("extraversion")
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(choiceEffects = effects, edgelist = history, attributes = info)
	
	expect_true(all(tomres$statistics[,,2] %in% info$extraversion))
	expect_true(all(aomres$statistics$choice %in% info$extraversion))
	
	effects <- ~ receive("extraversion", scaling = "std")
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(choiceEffects = effects, edgelist = history, attributes = info)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(apply(aomres$statistics$choice[,,1], 1, function(x) mean(x[x!=0])), 
		rep(0, nrow(history)))
	
	effects <- ~ receive("extraversion", attributes = info)
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_true(all(tomres$statistics[,,2] %in% info$extraversion))
	expect_true(all(aomres$statistics$choice %in% info$extraversion))
})