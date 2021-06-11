library(remify)
library(remstats)

test_that("indegreeSender", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	effects <- ~ indegreeSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$rate[,,2]),
		seq(0, nrow(history)-1, 1))
	
	effects <- ~ indegreeSender(scaling = "std")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$rate[,,2]), rep(0, nrow(history)))
	
	effects <- ~ indegreeSender(scaling = "prop")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$rate[-1,,2]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ indegreeSender(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
}) 

test_that("outdegreeSender", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	effects <- ~ outdegreeSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$rate[,,2]),
		seq(0, nrow(history)-1, 1))
	
	effects <- ~ outdegreeSender(scaling = "std")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$rate[,,2]), rep(0, nrow(history)))
	
	effects <- ~ outdegreeSender(scaling = "prop")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$rate[-1,,2]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ outdegreeSender(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
}) 

test_that("indegreeReceiver", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	effects <- ~ indegreeReceiver()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$choice),
		seq(0, nrow(history)-1, 1))
	
	effects <- ~ indegreeReceiver(scaling = "std")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice), rep(0, nrow(history)))
	
	effects <- ~ indegreeReceiver(scaling = "prop")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$choice[-1,,]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ indegreeReceiver(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
}) 

test_that("totaldegreeSender", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	effects <- ~ totaldegreeSender() + indegreeSender() + outdegreeSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(aomres$statistics$rate[,,2]),
		seq(0, (nrow(history)-1)*2, 2))
	expect_equal(tomres$statistics[,,2], 
		tomres$statistics[,,3] + tomres$statistics[,,4])
	expect_equal(aomres$statistics$rate[,,2], 
		aomres$statistics$rate[,,3] + aomres$statistics$rate[,,4])
	
	effects <- ~ totaldegreeSender(scaling = "std") 
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$rate[,,2]), rep(0, nrow(history)))
	
	effects <- ~ totaldegreeSender(scaling = "prop")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(rateEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$rate[-1,,2]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ totaldegreeSender(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
}) 

test_that("totaldegreeReceiver", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	effects <- ~ totaldegreeReceiver() + indegreeReceiver() + outdegreeReceiver()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(aomres$statistics$choice[,,1]),
		seq(0, (nrow(history)-1)*2, 2))
	expect_equal(tomres$statistics[,,2], 
		tomres$statistics[,,3] + tomres$statistics[,,4])
	expect_equal(aomres$statistics$choice[,,1], 
		aomres$statistics$choice[,,2] + aomres$statistics$choice[,,3])
	
	effects <- ~ totaldegreeReceiver(scaling = "std") 
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice), rep(0, nrow(history)))
	
	effects <- ~ totaldegreeReceiver(scaling = "prop")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$choice[-1,,]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ totaldegreeReceiver(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
})