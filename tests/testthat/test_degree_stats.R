library(remstats)

test_that("indegreeSender", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	effects <- ~ indegreeSender()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$sender_stats[,,2]),
		seq(0, nrow(history)-1, 1))
	
	effects <- ~ indegreeSender(scaling = "std")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$sender_stats[,,2]), rep(0, nrow(history)))
	
	effects <- ~ indegreeSender(scaling = "prop")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$sender_stats[-1,,2]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ indegreeSender(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
}) 

test_that("outdegreeSender", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	effects <- ~ outdegreeSender()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$sender_stats[,,2]),
		seq(0, nrow(history)-1, 1))
	
	effects <- ~ outdegreeSender(scaling = "std")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$sender_stats[,,2]), rep(0, nrow(history)))
	
	effects <- ~ outdegreeSender(scaling = "prop")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$sender_stats[-1,,2]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ outdegreeSender(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
}) 

test_that("indegreeReceiver", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	effects <- ~ indegreeReceiver()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$receiver_stats),
		seq(0, nrow(history)-1, 1))
	
	effects <- ~ indegreeReceiver(scaling = "std")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats), rep(0, nrow(history)))
	
	effects <- ~ indegreeReceiver(scaling = "prop")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$receiver_stats[-1,,]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ indegreeReceiver(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*9, 9))
}) 

test_that("totaldegreeSender", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	effects <- ~ totaldegreeSender() + indegreeSender() + outdegreeSender()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(aomres$sender_stats[,,2]),
		seq(0, (nrow(history)-1)*2, 2))
	expect_equal(tomres$statistics[,,2], 
		tomres$statistics[,,3] + tomres$statistics[,,4])
	expect_equal(aomres$sender_stats[,,2], 
		aomres$sender_stats[,,3] + aomres$sender_stats[,,4])
	
	effects <- ~ totaldegreeSender(scaling = "std") 
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$sender_stats[,,2]), rep(0, nrow(history)))
	
	effects <- ~ totaldegreeSender(scaling = "prop")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(sender_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$sender_stats[-1,,2]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ totaldegreeSender(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
}) 

test_that("totaldegreeReceiver", {
	
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	effects <- ~ totaldegreeReceiver() + indegreeReceiver() + outdegreeReceiver()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(aomres$receiver_stats[,,1]),
		seq(0, (nrow(history)-1)*2, 2))
	expect_equal(tomres$statistics[,,2], 
		tomres$statistics[,,3] + tomres$statistics[,,4])
	expect_equal(aomres$receiver_stats[,,1], 
		aomres$receiver_stats[,,2] + aomres$receiver_stats[,,3])
	
	effects <- ~ totaldegreeReceiver(scaling = "std") 
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats), rep(0, nrow(history)))
	
	effects <- ~ totaldegreeReceiver(scaling = "prop")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowSums(tomres$statistics[-1,,2]), rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$receiver_stats[-1,,]), rep(1, nrow(history)-1))
	
	colnames(history)[4] <- "type"
	effects <- ~ totaldegreeReceiver(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
	expect_equal(rowSums(tomres$statistics[,,2]),
		seq(0, (nrow(history)-1)*18, 18))
})