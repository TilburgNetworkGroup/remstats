library(remify)
library(remstats)

# Data for testing
data(history)
history$weight <- rep(1, nrow(history))

test_that("degree counts", {
	
	effects <- ~ indegreeSender() + outdegreeSender() + totaldegreeSender() +
		indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver()
	effectsS <- ~ indegreeSender() + outdegreeSender() + totaldegreeSender()  
	effectsR <- ~ indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver()
	
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(sender_effects = effectsS, receiver_effects = effectsR,
		edgelist = history)
	
	# indegreeSender
	expect_equal(rowSums(tomres$statistics[,,"indegreeSender"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$sender_stats[,,"indegreeSender"]),
		seq(0, nrow(history)-1, 1))
	
	# outdegreeSender
	expect_equal(rowSums(tomres$statistics[,,"outdegreeSender"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$sender_stats[,,"outdegreeSender"]),
		seq(0, nrow(history)-1, 1))
	
	# indegreeReceiver
	expect_equal(rowSums(tomres$statistics[,,"indegreeReceiver"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$receiver_stats[,,"indegreeReceiver"]),
		seq(0, nrow(history)-1, 1))
	
	# outdegreeReceiver
	expect_equal(rowSums(tomres$statistics[,,"outdegreeReceiver"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(aomres$statistics$receiver_stats[,,"outdegreeReceiver"]),
		seq(0, nrow(history)-1, 1))
	
	# totaldegreeSender
	expect_equal(rowSums(tomres$statistics[,,"totaldegreeSender"]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(aomres$statistics$sender_stats[,,"totaldegreeSender"]),
		seq(0, (nrow(history)-1)*2, 2))
	expect_equal(tomres$statistics[,,"totaldegreeSender"], 
		tomres$statistics[,,"indegreeSender"] + 
			tomres$statistics[,,"outdegreeSender"])
	expect_equal(aomres$statistics$sender_stats[,,"totaldegreeSender"], 
		aomres$statistics$sender_stats[,,"indegreeSender"] + 
			aomres$statistics$sender_stats[,,"outdegreeSender"])
	
	# totaldegreeReceiver
	expect_equal(rowSums(tomres$statistics[,,"totaldegreeReceiver"]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(aomres$statistics$receiver_stats[,,"totaldegreeReceiver"]),
		seq(0, (nrow(history)-1)*2, 2))
	expect_equal(tomres$statistics[,,"totaldegreeReceiver"], 
		tomres$statistics[,,"indegreeReceiver"] + 
			tomres$statistics[,,"outdegreeReceiver"])
	expect_equal(aomres$statistics$receiver_stats[,,"totaldegreeReceiver"], 
		aomres$statistics$receiver_stats[,,"indegreeReceiver"] + 
			aomres$statistics$receiver_stats[,,"outdegreeReceiver"])
}) 

test_that("degree std", {
	effects <- ~ indegreeSender(scaling = "std") + 
		outdegreeSender(scaling = "std") + totaldegreeSender(scaling = "std") +
		indegreeReceiver(scaling = "std") + outdegreeReceiver(scaling = "std") + 
		totaldegreeReceiver(scaling = "std")
	effectsS <- ~ indegreeSender(scaling = "std") + 
		outdegreeSender(scaling = "std") + totaldegreeSender(scaling = "std")  
	effectsR <- ~ indegreeReceiver(scaling = "std") + 
		outdegreeReceiver(scaling = "std") + totaldegreeReceiver(scaling = "std")
	
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(sender_effects = effectsS, receiver_effects = effectsR,
		edgelist = history)
	
	# indegreeSender
	expect_equal(rowMeans(tomres$statistics[,,"indegreeSender"]), 
		rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$sender_stats[,,"indegreeSender"]), 
		rep(0, nrow(history)))
	
	# outdegreeSender
	expect_equal(rowMeans(tomres$statistics[,,"outdegreeSender"]), 
		rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$sender_stats[,,"outdegreeSender"]), 
		rep(0, nrow(history)))
	
	# indegreeReceiver
	expect_equal(rowMeans(tomres$statistics[,,"indegreeReceiver"]), 
		rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$receiver_stats[,,"indegreeReceiver"]), 
		rep(0, nrow(history)))
	
	# outdegreeReceiver
	expect_equal(rowMeans(tomres$statistics[,,"outdegreeReceiver"]), 
		rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$receiver_stats[,,"outdegreeReceiver"]), 
		rep(0, nrow(history)))
	
	# totaldegreeSender
	expect_equal(rowMeans(tomres$statistics[,,"totaldegreeSender"]), 
		rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$sender_stats[,,"totaldegreeSender"]), 
		rep(0, nrow(history)))
	
	# totaldegreeReceiver
	expect_equal(rowMeans(tomres$statistics[,,"totaldegreeReceiver"]), 
		rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$receiver_stats[,,"totaldegreeReceiver"]), 
		rep(0, nrow(history)))
})

test_that("degree prop", {
	effects <- ~ indegreeSender(scaling = "prop") + 
		outdegreeSender(scaling = "prop") + totaldegreeSender(scaling = "prop") +
		indegreeReceiver(scaling = "prop") + outdegreeReceiver(scaling = "prop") + 
		totaldegreeReceiver(scaling = "prop")
	effectsS <- ~ indegreeSender(scaling = "prop") + 
		outdegreeSender(scaling = "prop") + totaldegreeSender(scaling = "prop")  
	effectsR <- ~ indegreeReceiver(scaling = "prop") + 
		outdegreeReceiver(scaling = "prop") + totaldegreeReceiver(scaling = "prop")
	
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(sender_effects = effectsS, receiver_effects = effectsR,
		edgelist = history)
	
	# indegreeSender
	expect_equal(rowSums(tomres$statistics[-1,,"indegreeSender"]), 
		rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$sender_stats[-1,,"indegreeSender"]), 
		rep(1, nrow(history)-1))
	
	# outdegreeSender
	expect_equal(rowSums(tomres$statistics[-1,,"outdegreeSender"]), 
		rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$sender_stats[-1,,"outdegreeSender"]), 
		rep(1, nrow(history)-1))
	
	# indegreeReceiver
	expect_equal(rowSums(tomres$statistics[-1,,"indegreeReceiver"]), 
		rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$receiver_stats[-1,,"indegreeReceiver"]), 
		rep(1, nrow(history)-1))
	
	# outdegreeReceiver
	expect_equal(rowSums(tomres$statistics[-1,,"outdegreeReceiver"]), 
		rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$receiver_stats[-1,,"outdegreeReceiver"]), 
		rep(1, nrow(history)-1))
	
	# totaldegreeSender
	expect_equal(rowSums(tomres$statistics[-1,,"totaldegreeSender"]), 
		rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$sender_stats[-1,,"totaldegreeSender"]), 
		rep(1, nrow(history)-1))
	
	# totaldegreeReceiver
	expect_equal(rowSums(tomres$statistics[-1,,"totaldegreeReceiver"]), 
		rep(9, nrow(history)-1))
	expect_equal(rowSums(aomres$statistics$receiver_stats[-1,,"totaldegreeReceiver"]), 
		rep(1, nrow(history)-1))
})

test_that("degree considerType", {
	colnames(history)[4] <- "type"
	
	effects <- ~ indegreeSender(consider_type = TRUE) + 
		outdegreeSender(consider_type = TRUE) + totaldegreeSender(consider_type = TRUE) +
		indegreeReceiver(consider_type = TRUE) + outdegreeReceiver(consider_type = TRUE) + 
		totaldegreeReceiver(consider_type = TRUE)

	tomres <- tomstats(effects, edgelist = history)

	skip("considerType not yet implemented in new version")
	expect_equal(rowSums(tomres$statistics[,,"indegreeSender"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(tomres$statistics[,,"outdegreeSender"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(tomres$statistics[,,"indegreeReceiver"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(tomres$statistics[,,"outdegreeReceiver"]),
		seq(0, (nrow(history)-1)*9, 9))
	expect_equal(rowSums(tomres$statistics[,,"totaldegreeSender"]),
		seq(0, (nrow(history)-1)*18, 18))
	expect_equal(rowSums(tomres$statistics[,,"totaldegreeReceiver"]),
		seq(0, (nrow(history)-1)*18, 18))
})