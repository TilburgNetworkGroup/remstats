# Test endogenous statistics
library(remstats)

# Small edgelist
edgelist <- data.frame(
	time = 1:5,
	actor1 = c(1, 1, 2, 2, 3),
	actor2 = c(2, 3, 1, 3, 2)
)

event_types <- c(1, 1, 2, 2, 1)

# Condition 1: Directed events, tie-oriented model
test_that("Endo stats: condition 1", {
	reh <- remify::remify(edgelist, model = "tie")
	effects <- ~ 
		outdegreeSender() + outdegreeReceiver() +
		indegreeSender() + indegreeReceiver() +
		totaldegreeSender() +	totaldegreeReceiver() +
		inertia() +	reciprocity() +
		isp() +	itp() +	osp() +	otp() +
		psABBA() + psABBY() + psABAB() + psABBY() +	
		psABXA() + psABXB() + psABXY() +
		recencyContinue() +
		recencySendSender() + recencySendReceiver() +
		recencyReceiveSender() + recencyReceiveReceiver() +
		rrankSend() +	rrankReceive() 
	stats <- remstats(reh, tie_effects = effects)
	riskset <- attr(stats, "riskset")
	
	# Baseline
	expect_equal(stats[,,"baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))
	
	# outdegreeSender 
	outdegreeSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 1, 0, 0, 0, 0), 
		c(2, 2, 0, 0, 0, 0),
		c(2, 2, 1, 1, 0, 0),
		c(2, 2, 2, 2, 0, 0))
	expect_equal(stats[,,"outdegreeSender"], outdegreeSender)
	
	# outdegreeReceiver
	outdegreeReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 1, 0), 
		c(0, 0, 2, 0, 2, 0), 
		c(1, 0, 2, 0, 2, 1), 
		c(2, 0, 2, 0, 2, 2)) 
	expect_equal(stats[,,"outdegreeReceiver"], outdegreeReceiver)
	
	# indegreeSender
	indegreeSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 1, 0, 0), 
		c(0, 0, 1, 1, 1, 1), 
		c(1, 1, 1, 1, 1, 1), 
		c(1, 1, 1, 1, 2, 2)) 
	expect_equal(stats[,,"indegreeSender"], indegreeSender)
	
	# indegreeReceiver
	indegreeReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 1), 
		c(1, 1, 0, 1, 0, 1), 
		c(1, 1, 1, 1, 1, 1), 
		c(1, 2, 1, 2, 1, 1))
	expect_equal(stats[,,"indegreeReceiver"], indegreeReceiver)
	
	# totaldegreeSender
	totaldegreeSender <- indegreeSender + outdegreeSender
	expect_equal(stats[,,"totaldegreeSender"], totaldegreeSender)
	
	# totaldegreeReceiver
	totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
	expect_equal(stats[,,"totaldegreeReceiver"], totaldegreeReceiver)
	
	# inertia
	inertia <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 0, 0, 0), 
		c(1, 1, 1, 0, 0, 0), 
		c(1, 1, 1, 1, 0, 0))
	expect_equal(stats[,,"inertia"], inertia)
	
	# reciprocity
	reciprocity <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 1, 0),
		c(1, 0, 1, 0, 1, 0),
		c(1, 0, 1, 0, 1, 1))
	expect_equal(stats[,,"reciprocity"], reciprocity)
	
	# itp
	itp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 1), 
		c(0, 0, 0, 0, 1, 1))
	expect_equal(stats[,,"itp"], itp)
	
	# otp
	otp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 0), 
		c(0, 1, 0, 1, 0, 0))
	expect_equal(stats[,,"otp"], otp)
	
	# isp
	isp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 1), 
		c(0, 0, 0, 1, 0, 1), 
		c(0, 1, 0, 1, 1, 1))
	expect_equal(stats[,,"isp"], isp)
	
	# osp
	osp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0), 
		c(1, 0, 1, 0, 0, 0))
	expect_equal(stats[,,"osp"], osp)
	
	# psABBA
	psABBA <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0), 
		c(0, 0, 0, 0, 1, 0), 
		c(1, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 1))
	expect_equal(stats[,,"psABBA"], psABBA)
	
	# psABBY
	psABBY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 1, 0, 0), 
		c(0, 0, 0, 0, 0, 1), 
		c(0, 1, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 1, 0))
	expect_equal(stats[,,"psABBY"], psABBY)
	
	# psABAB
	psABAB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0), 
		c(0, 1, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 0))
	expect_equal(stats[,,"psABAB"], psABAB)
	
	# psABXB
	psABXB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 1), 
		c(0, 0, 0, 1, 0, 0), 
		c(0, 0, 0, 0, 1, 0), 
		c(0, 1, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXB"], psABXB)
	
	# psABXY
	psABXY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXY"], psABXY)
	
	# recencyContinue
	recencyContinue <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0, 0, 0, 0, 0), 
		c(1/3, 1/2, 0, 0, 0, 0), 
		c(1/4, 1/3, 1/2, 0, 0, 0), 
		c(1/5, 1/4, 1/3, 1/2, 0, 0))
	expect_equal(stats[,,"recencyContinue"], recencyContinue)
	
	# recencySendSender
	recencySendSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 1/2, 0, 0, 0, 0), 
		c(1/2, 1/2, 0, 0, 0, 0), 
		c(1/3, 1/3, 1/2, 1/2, 0, 0), 
		c(1/4, 1/4, 1/2, 1/2, 0, 0))
	expect_equal(stats[,,"recencySendSender"], recencySendSender)
	
	# recencySendReceiver
	recencySendReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 0, 1/2, 0), 
		c(0, 0, 1/2, 0, 1/2, 0), 
		c(1/2, 0, 1/3, 0, 1/3, 1/2), 
		c(1/2, 0, 1/4, 0, 1/4, 1/2))
	expect_equal(stats[,,"recencySendReceiver"], recencySendReceiver)
	
	# recencyReceiveSender
	recencyReceiveSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 1/2, 0, 0), 
		c(0, 0, 1/3, 1/3, 1/2, 1/2), 
		c(1/2, 1/2, 1/4, 1/4, 1/3, 1/3), 
		c(1/3, 1/3, 1/5, 1/5, 1/2, 1/2))
	expect_equal(stats[,,"recencyReceiveSender"], recencyReceiveSender)
	
	# recencyReceiveReceiver
	recencyReceiveReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0, 0, 0, 0, 1/2), 
		c(1/3, 1/2, 0, 1/2, 0, 1/3), 
		c(1/4, 1/3, 1/2, 1/3, 1/2, 1/4), 
		c(1/5, 1/2, 1/3, 1/2, 1/3, 1/5))
	expect_equal(stats[,,"recencyReceiveReceiver"], recencyReceiveReceiver)
	
	# rrankSend
	rrankSend <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0), 
		c(1/2, 1, 0, 0, 0, 0), 
		c(1/2, 1, 1, 0, 0, 0), 
		c(1/2, 1, 1/2, 1, 0, 0))
	expect_equal(stats[,,"rrankSend"], rrankSend)
	
	# rrankReceive
	rrankReceive <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 1, 0), 
		c(1, 0, 1, 0, 1, 0), 
		c(1, 0, 1, 0, 1/2, 1))
	expect_equal(stats[,,"rrankReceive"], rrankReceive)
})

# Condition 2: Undirected events, tie-oriented model
test_that("Endo stats: condition 2", {
	reh <- remify::remify(edgelist, model = "tie", directed = FALSE)
	effects <- ~ 	inertia() +	sp() + spUnique() +	psABAB() + psABAY() 
	stats <- remstats(reh, tie_effects = effects)
	riskset <- attr(stats, "riskset")
	
	# baseline
	expect_equal(stats[,,"baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))
	
	# inertia
	inertia <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0), 
		c(1, 1, 0), 
		c(2, 1, 0),
		c(2, 1, 1))
	expect_equal(stats[,,"inertia"], inertia)
	
	# sp
	sp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0), 
		c(0, 0, 1), 
		c(0, 0, 1),
		c(1, 1, 1))
	expect_equal(stats[,,"sp"], sp)
	
	# spUnique
	spUnique <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0), 
		c(0, 0, 1), 
		c(0, 0, 1),
		c(1, 1, 1))
	expect_equal(stats[,,"spUnique"], spUnique)
	
	# psABAB
	psABAB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0), 
		c(0, 1, 0), 
		c(1, 0, 0),
		c(0, 0, 1))
	expect_equal(stats[,,"psABAB"], psABAB)
	
	# psABAY
	psABAY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 1, 1), 
		c(1, 0, 1), 
		c(0, 1, 1),
		c(1, 1, 0))
	expect_equal(stats[,,"psABAY"], psABAY)
	
})

# Condition 3: Directed evens with types, tie-oriented model
test_that("Endo stats: condition 3", {
	edgelist$type <- event_types
	reh <- remify::remify(edgelist, model = "tie")
	effects <- ~ 
		outdegreeSender() + outdegreeReceiver() +
		indegreeSender() + indegreeReceiver() +
		totaldegreeSender() +	totaldegreeReceiver() +
		inertia() +	reciprocity() +
		isp() +	itp() +	osp() +	otp() +
		psABBA() + psABBY() + psABAB() + psABBY() +	
		psABXA() + psABXB() + psABXY() +
		recencyContinue() +
		recencySendSender() + recencySendReceiver() +
		recencyReceiveSender() + recencyReceiveReceiver() +
		rrankSend() +	rrankReceive() + 
		outdegreeSender(consider_type = TRUE) + 
		outdegreeReceiver(consider_type = TRUE) +
		indegreeSender(consider_type = TRUE) + 
		indegreeReceiver(consider_type = TRUE) +
		totaldegreeSender(consider_type = TRUE) +	
		totaldegreeReceiver(consider_type = TRUE) +
		inertia(consider_type = TRUE) +	reciprocity(consider_type = TRUE) +
		isp(consider_type = TRUE) +	itp(consider_type = TRUE) +	
		osp(consider_type = TRUE) +	otp(consider_type = TRUE) +
		psABBA(consider_type = TRUE) + psABBY(consider_type = TRUE) + 
		psABAB(consider_type = TRUE) + psABBY(consider_type = TRUE) +	
		psABXA(consider_type = TRUE) + psABXB(consider_type = TRUE) + 
		psABXY(consider_type = TRUE) +
		recencyContinue(consider_type = TRUE) +
		recencySendSender(consider_type = TRUE) + 
		recencySendReceiver(consider_type = TRUE) +
		recencyReceiveSender(consider_type = TRUE) + 
		recencyReceiveReceiver(consider_type = TRUE) +
		rrankSend(consider_type = TRUE) +	rrankReceive(consider_type = TRUE)
	stats <- remstats(reh, tie_effects = effects)
	riskset <- attr(stats, "riskset")
	
	# Baseline
	expect_equal(stats[,,"baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))
	
	# outdegreeSender 
	outdegreeSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0), 
		c(2, 2, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0),
		c(2, 2, 1, 1, 0, 0, 2, 2, 1, 1, 0, 0),
		c(2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 0))
	expect_equal(stats[,,"outdegreeSender"], outdegreeSender)
	
	# outdegreeSender.type 
	outdegreeSender.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(2, 2, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0), 
		c(2, 2, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0))
	expect_equal(stats[,,"outdegreeSender.type"], outdegreeSender.type)
	
	# outdegreeReceiver
	outdegreeReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0), 
		c(0, 0, 2, 0, 2, 0, 0, 0, 2, 0, 2, 0), 
		c(1, 0, 2, 0, 2, 1, 1, 0, 2, 0, 2, 1), 
		c(2, 0, 2, 0, 2, 2, 2, 0, 2, 0, 2, 2)) 
	expect_equal(stats[,,"outdegreeReceiver"], outdegreeReceiver)
	
	# outdegreeReceiver.type
	outdegreeReceiver.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 2, 0, 2, 0, 1, 0, 0, 0, 0, 1), 
		c(0, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 2)) 
	expect_equal(stats[,,"outdegreeReceiver.type"], outdegreeReceiver.type)
	
	# indegreeSender
	indegreeSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0), 
		c(0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1), 
		c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
		c(1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2))
	expect_equal(stats[,,"indegreeSender"], indegreeSender)
	
	# indegreeSender.type
	indegreeSender.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0), 
		c(0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1))
	expect_equal(stats[,,"indegreeSender.type"], indegreeSender.type)
	
	# indegreeReceiver
	indegreeReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1), 
		c(1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1), 
		c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
		c(1, 2, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1))
	expect_equal(stats[,,"indegreeReceiver"], indegreeReceiver)
	
	# indegreeReceiver.type
	indegreeReceiver.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0), 
		c(1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0))
	expect_equal(stats[,,"indegreeReceiver.type"], indegreeReceiver.type)
	
	# totaldegreeSender
	totaldegreeSender <- indegreeSender + outdegreeSender
	expect_equal(stats[,,"totaldegreeSender"], totaldegreeSender)
	
	# totaldegreeSender.type
	totaldegreeSender.type <- indegreeSender.type + outdegreeSender.type
	expect_equal(stats[,,"totaldegreeSender.type"], totaldegreeSender.type)
	
	# totaldegreeReceiver
	totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
	expect_equal(stats[,,"totaldegreeReceiver"], totaldegreeReceiver)
	
	# totaldegreeReceiver.type
	totaldegreeReceiver.type <- indegreeReceiver.type + outdegreeReceiver.type
	expect_equal(stats[,,"totaldegreeReceiver.type"], totaldegreeReceiver.type)
	
	# inertia
	inertia <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0), 
		c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0), 
		c(1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0))
	expect_equal(stats[,,"inertia"], inertia)
	
	# inertia.type 
	inertia.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
		c(1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0))
	expect_equal(stats[,,"inertia.type"], inertia.type)
	
	# reciprocity
	reciprocity <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0),
		c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
		c(1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1))
	expect_equal(stats[,,"reciprocity"], reciprocity)
	
	# reciprocity.type 
	reciprocity.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
		c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0),
		c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1))
	expect_equal(stats[,,"reciprocity.type"], reciprocity.type)
	
	# itp
	itp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), 
		c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1))
	expect_equal(stats[,,"itp"], itp)
	
	# itp.type 
	itp.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(stats[,,"itp.type"], itp.type)
	
	# otp
	otp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0), 
		c(0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0))
	expect_equal(stats[,,"otp"], otp)
	
	# otp.type 
	otp.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(stats[,,"otp.type"], otp.type)
	
	# isp
	isp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1), 
		c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1), 
		c(0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1))
	expect_equal(stats[,,"isp"], isp)
	
	# isp.type
	isp.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0))
	expect_equal(stats[,,"isp.type"], isp.type)
	
	# osp
	osp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0))
	expect_equal(stats[,,"osp"], osp)
	
	# osp.type 
	osp.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(stats[,,"osp.type"], osp.type)
	
	# psABBA
	psABBA <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
		c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0), 
		c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1))
	expect_equal(stats[,,"psABBA"], psABBA)
	
	# psABBA.type 
	psABBA.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
	expect_equal(stats[,,"psABBA.type"], psABBA.type)
	
	# psABBY
	psABBY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0), 
		c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), 
		c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0))
	expect_equal(stats[,,"psABBY"], psABBY)
	
	# psABBY.type 
	psABBY.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0))
	expect_equal(stats[,,"psABBY.type"], psABBY.type)
	
	# psABAB
	psABAB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 
		c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0))
	expect_equal(stats[,,"psABAB"], psABAB)
	
	# psABAB.type
	psABAB.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0))
	expect_equal(stats[,,"psABAB.type"], psABAB.type)
	
	# psABXB
	psABXB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), 
		c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0), 
		c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0), 
		c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXB"], psABXB)
	
	# psABXB.type
	psABXB.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXB.type"], psABXB.type)
	
	# psABXY
	psABXY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXY"], psABXY)
	
	# psABXY.type
	psABXY.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXY.type"], psABXY.type)
	
	# recencyContinue
	recencyContinue <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0, 0, 0, 0, 0, 1/2, 0, 0, 0, 0, 0), 
		c(1/3, 1/2, 0, 0, 0, 0, 1/3, 1/2, 0, 0, 0, 0), 
		c(1/4, 1/3, 1/2, 0, 0, 0, 1/4, 1/3, 1/2, 0, 0, 0), 
		c(1/5, 1/4, 1/3, 1/2, 0, 0, 1/5, 1/4, 1/3, 1/2, 0, 0))
	expect_equal(stats[,,"recencyContinue"], recencyContinue)
	
	# recencyContinue.type
	recencyContinue.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1/3, 1/2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1/4, 1/3, 0, 0, 0, 0, 0,0, 1/2, 0, 0, 0), 
		c(1/5, 1/4, 0, 0, 0, 0, 0, 0, 1/3, 1/2, 0, 0))
	expect_equal(stats[,,"recencyContinue.type"], recencyContinue.type)
	
	# recencySendSender
	recencySendSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 1/2, 0, 0, 0, 0, 1/2, 1/2, 0, 0, 0, 0), 
		c(1/2, 1/2, 0, 0, 0, 0, 1/2, 1/2, 0, 0, 0, 0), 
		c(1/3, 1/3, 1/2, 1/2, 0, 0, 1/3, 1/3, 1/2, 1/2, 0, 0), 
		c(1/4, 1/4, 1/2, 1/2, 0, 0, 1/4, 1/4, 1/2, 1/2, 0, 0))
	expect_equal(stats[,,"recencySendSender"], recencySendSender)
	
	# recencySendSender.type
	recencySendSender.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 1/2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1/2, 1/2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(1/3, 1/3, 0, 0, 0, 0, 0, 0, 1/2, 1/2, 0, 0), 
		c(1/4, 1/4, 0, 0, 0, 0, 0, 0, 1/2, 1/2, 0, 0))
	expect_equal(stats[,,"recencySendSender.type"], recencySendSender.type)
	
	# recencySendReceiver
	recencySendReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 0, 1/2, 0, 0, 0, 1/2, 0, 1/2, 0), 
		c(0, 0, 1/2, 0, 1/2, 0, 0, 0, 1/2, 0, 1/2, 0), 
		c(1/2, 0, 1/3, 0, 1/3, 1/2, 1/2, 0, 1/3, 0, 1/3, 1/2), 
		c(1/2, 0, 1/4, 0, 1/4, 1/2, 1/2, 0, 1/4, 0, 1/4, 1/2))
	expect_equal(stats[,,"recencySendReceiver"], recencySendReceiver)
	
	# recencySendReceiver.type 
	recencySendReceiver.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 0, 1/2, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1/2, 0, 1/2, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1/3, 0, 1/3, 0, 1/2, 0, 0, 0, 0, 1/2), 
		c(0, 0, 1/4, 0, 1/4, 0, 1/2, 0, 0, 0, 0, 1/2))
	expect_equal(stats[,,"recencySendReceiver.type"], recencySendReceiver.type)
	
	# recencyReceiveSender
	recencyReceiveSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 1/2, 0, 0, 0, 0, 1/2, 1/2, 0, 0), 
		c(0, 0, 1/3, 1/3, 1/2, 1/2, 0, 0, 1/3, 1/3, 1/2, 1/2), 
		c(1/2, 1/2, 1/4, 1/4, 1/3, 1/3, 1/2, 1/2, 1/4, 1/4, 1/3, 1/3), 
		c(1/3, 1/3, 1/5, 1/5, 1/2, 1/2, 1/3, 1/3, 1/5, 1/5, 1/2, 1/2))
	expect_equal(stats[,,"recencyReceiveSender"], recencyReceiveSender)
	
	# recencyReceiveSender.type 
	recencyReceiveSender.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 1/2, 0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1/3, 1/3, 1/2, 1/2, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1/4, 1/4, 1/3, 1/3, 1/2, 1/2, 0, 0, 0, 0), 
		c(0, 0, 1/5, 1/5, 1/4, 1/4, 1/3, 1/3, 0, 0, 1/2, 1/2))
	expect_equal(stats[,,"recencyReceiveSender.type"], recencyReceiveSender.type)
	
	# recencyReceiveReceiver
	recencyReceiveReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0.0, 0.0, 0.0, 0.0, 1/2, 1/2, 0.0, 0.0, 0.0, 0.0, 1/2), 
		c(1/3, 1/2, 0.0, 1/2, 0.0, 1/3, 1/3, 1/2, 0.0, 1/2, 0.0, 1/3), 
		c(1/4, 1/3, 1/2, 1/3, 1/2, 1/4, 1/4, 1/3, 1/2, 1/3, 1/2, 1/4), 
		c(1/5, 1/2, 1/3, 1/2, 1/3, 1/5, 1/5, 1/2, 1/3, 1/2, 1/3, 1/5))
	expect_equal(stats[,,"recencyReceiveReceiver"], recencyReceiveReceiver)
	
	# recencyReceiveReceiver.type
	recencyReceiveReceiver.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0.0, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0, 0.0, 0.0, 0), 
		c(1/3, 1/2, 0.0, 1/2, 0.0, 1/3, 0.0, 0.0, 0.0, 0.0, 0.0, 0), 
		c(1/4, 1/3, 0.0, 1/3, 0.0, 1/4, 0.0, 0.0, 1/2, 0.0, 1/2, 0), 
		c(1/5, 1/4, 0.0, 1/4, 0.0, 1/5, 0.0, 1/2, 1/3, 1/2, 1/3, 0))
	expect_equal(stats[,,"recencyReceiveReceiver.type"], recencyReceiveReceiver.type)
	
	# rrankSend
	rrankSend <- rbind(matrix(0.0, ncol = nrow(riskset)),
		c(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), 
		c(1/2, 1.0, 0.0, 0.0, 0.0, 0.0, 1/2, 1.0, 0.0, 0.0, 0.0, 0.0), 
		c(1/2, 1.0, 1.0, 0.0, 0.0, 0.0, 1/2, 1.0, 1.0, 0.0, 0.0, 0.0), 
		c(1/2, 1.0, 1/2, 1.0, 0.0, 0.0, 1/2, 1.0, 1/2, 1.0, 0.0, 0.0))
	expect_equal(stats[,,"rrankSend"], rrankSend)
	
	# rrankSend.type
	rrankSend.type <- rbind(matrix(0.0, ncol = nrow(riskset)),
		c(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), 
		c(1/2, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), 
		c(1/2, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0), 
		c(1/2, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1/2, 1.0, 0.0, 0.0))
	expect_equal(stats[,,"rrankSend.type"], rrankSend.type)
	
	# rrankReceive
	rrankReceive <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0), 
		c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), 
		c(1, 0, 1, 0, 1/2, 1, 1, 0, 1, 0, 1/2, 1))
	expect_equal(stats[,,"rrankReceive"], rrankReceive)
	
	# rrankReceive.type
	rrankReceive.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0.0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 1.0, 0, 0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 1.0, 0, 1, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 1/2, 0, 1, 0, 0, 0, 0, 1))
	expect_equal(stats[,,"rrankReceive.type"], rrankReceive.type)
})

# Condition 4: Undirected events with types, tie-oriented model
test_that("Endo stats: condition 4", {
	edgelist$type <- event_types
	reh <- remify::remify(edgelist, model = "tie", directed = FALSE)
	effects <- ~ inertia() + sp() + spUnique() +	psABAB() + psABAY() +
		inertia(consider_type = TRUE) + 
		sp(consider_type = TRUE) + spUnique(consider_type = TRUE) +	
		psABAB(consider_type = TRUE) + psABAY(consider_type = TRUE) 
	stats <- remstats(reh, tie_effects = effects)
	riskset <- attr(stats, "riskset")
	
	# baseline
	expect_equal(stats[,,"baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))
	
	# inertia
	inertia <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 1, 0, 0), 
		c(1, 1, 0, 1, 1, 0), 
		c(2, 1, 0, 2, 1, 0),
		c(2, 1, 1, 2, 1, 1))
	expect_equal(stats[,,"inertia"], inertia)
	
	# inertia.type
	inertia.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0), 
		c(1, 1, 0, 0, 0, 0), 
		c(1, 1, 0, 1, 0, 0),
		c(1, 1, 0, 1, 0, 1))
	expect_equal(stats[,,"inertia.type"], inertia.type)
	
	# sp
	sp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 1), 
		c(0, 0, 1, 0, 0, 1),
		c(1, 1, 1, 1, 1, 1))
	expect_equal(stats[,,"sp"], sp)
	
	# sp.type
	sp.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 0),
		c(0, 0, 1, 0, 1, 0))
	expect_equal(stats[,,"sp.type"], sp.type)
	
	# spUnique
	spUnique <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 1), 
		c(0, 0, 1, 0, 0, 1),
		c(1, 1, 1, 1, 1, 1))
	expect_equal(stats[,,"spUnique"], spUnique)
	
	# spUnique.type
	spUnique.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 0, 0),
		c(0, 0, 1, 0, 1, 0))
	expect_equal(stats[,,"spUnique.type"], spUnique.type)
	
	# psABAB
	psABAB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 1, 0, 0), 
		c(0, 1, 0, 0, 1, 0), 
		c(1, 0, 0, 1, 0, 0),
		c(0, 0, 1, 0, 0, 1))
	expect_equal(stats[,,"psABAB"], psABAB)
	
	# psABAB.type
	psABAB.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0, 0), 
		c(0, 1, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0, 0),
		c(0, 0, 0, 0, 0, 1))
	expect_equal(stats[,,"psABAB.type"], psABAB.type)
	
	# psABAY
	psABAY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 1, 1, 0, 1, 1), 
		c(1, 0, 1, 1, 0, 1), 
		c(0, 1, 1, 0, 1, 1),
		c(1, 1, 0, 1, 1, 0))
	expect_equal(stats[,,"psABAY"], psABAY)
	
	# psABAY.type
	psABAY.type <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 1, 1, 0, 0, 0), 
		c(1, 0, 1, 0, 0, 0), 
		c(0, 0, 0, 0, 1, 1),
		c(0, 0, 0, 1, 1, 0))
	expect_equal(stats[,,"psABAY.type"], psABAY.type)
})

# Condition 5: Directed events, tie-oriented model with active risk set
test_that("Endo stats: condition 5", {
	reh <- remify::remify(edgelist, model = "tie", riskset = "active")
	effects <- ~ 
		outdegreeSender() + outdegreeReceiver() +
		indegreeSender() + indegreeReceiver() +
		totaldegreeSender() +	totaldegreeReceiver() +
		inertia() +	reciprocity() +
		isp() +	itp() +	osp() +	otp() +
		psABBA() + psABBY() + psABAB() + psABBY() +	
		psABXA() + psABXB() + psABXY() +
		recencyContinue() +
		recencySendSender() + recencySendReceiver() +
		recencyReceiveSender() + recencyReceiveReceiver() +
		rrankSend() +	rrankReceive() 
	stats <- remstats(reh, tie_effects = effects)
	riskset <- attr(stats, "riskset")
	
	# Baseline
	expect_equal(stats[,,"baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))
	
	# outdegreeSender 
	outdegreeSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 1, 0, 0, 0), 
		c(2, 2, 0, 0, 0),
		c(2, 2, 1, 1, 0),
		c(2, 2, 2, 2, 0))
	expect_equal(stats[,,"outdegreeSender"], outdegreeSender)
	
	# outdegreeReceiver
	outdegreeReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0), 
		c(0, 0, 2, 0, 0), 
		c(1, 0, 2, 0, 1), 
		c(2, 0, 2, 0, 2)) 
	expect_equal(stats[,,"outdegreeReceiver"], outdegreeReceiver)
	
	# indegreeSender
	indegreeSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 1, 0), 
		c(0, 0, 1, 1, 1), 
		c(1, 1, 1, 1, 1), 
		c(1, 1, 1, 1, 2)) 
	expect_equal(stats[,,"indegreeSender"], indegreeSender)
	
	# indegreeReceiver
	indegreeReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 1), 
		c(1, 1, 0, 1, 1), 
		c(1, 1, 1, 1, 1), 
		c(1, 2, 1, 2, 1))
	expect_equal(stats[,,"indegreeReceiver"], indegreeReceiver)
	
	# totaldegreeSender
	totaldegreeSender <- indegreeSender + outdegreeSender
	expect_equal(stats[,,"totaldegreeSender"], totaldegreeSender)
	
	# totaldegreeReceiver
	totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
	expect_equal(stats[,,"totaldegreeReceiver"], totaldegreeReceiver)
	
	# inertia
	inertia <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0), 
		c(1, 1, 0, 0, 0), 
		c(1, 1, 1, 0, 0), 
		c(1, 1, 1, 1, 0))
	expect_equal(stats[,,"inertia"], inertia)
	
	# reciprocity
	reciprocity <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0), 
		c(0, 0, 1, 0, 0),
		c(1, 0, 1, 0, 0),
		c(1, 0, 1, 0, 1))
	expect_equal(stats[,,"reciprocity"], reciprocity)
	
	# itp
	itp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 1), 
		c(0, 0, 0, 0, 1))
	expect_equal(stats[,,"itp"], itp)
	
	# otp
	otp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 0), 
		c(0, 1, 0, 1, 0))
	expect_equal(stats[,,"otp"], otp)
	
	# isp
	isp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 1, 1), 
		c(0, 0, 0, 1, 1), 
		c(0, 1, 0, 1, 1))
	expect_equal(stats[,,"isp"], isp)
	
	# osp
	osp <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(1, 0, 1, 0, 0))
	expect_equal(stats[,,"osp"], osp)
	
	# psABBA
	psABBA <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(1, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 1))
	expect_equal(stats[,,"psABBA"], psABBA)
	
	# psABBY
	psABBY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 1, 0), 
		c(0, 0, 0, 0, 1), 
		c(0, 1, 0, 0, 0), 
		c(0, 0, 0, 0, 0))
	expect_equal(stats[,,"psABBY"], psABBY)
	
	# psABAB
	psABAB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0), 
		c(0, 1, 0, 0, 0), 
		c(0, 0, 1, 0, 0), 
		c(0, 0, 0, 1, 0))
	expect_equal(stats[,,"psABAB"], psABAB)
	
	# psABXB
	psABXB <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 1), 
		c(0, 0, 0, 1, 0), 
		c(0, 0, 0, 0, 0), 
		c(0, 1, 0, 0, 0))
	expect_equal(stats[,,"psABXB"], psABXB)
	
	# psABXY
	psABXY <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0), 
		c(0, 0, 0, 0, 0))
	expect_equal(stats[,,"psABXY"], psABXY)
	
	# recencyContinue
	recencyContinue <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0, 0, 0, 0), 
		c(1/3, 1/2, 0, 0, 0), 
		c(1/4, 1/3, 1/2, 0, 0), 
		c(1/5, 1/4, 1/3, 1/2, 0))
	expect_equal(stats[,,"recencyContinue"], recencyContinue)
	
	# recencySendSender
	recencySendSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 1/2, 0, 0, 0), 
		c(1/2, 1/2, 0, 0, 0), 
		c(1/3, 1/3, 1/2, 1/2, 0), 
		c(1/4, 1/4, 1/2, 1/2, 0))
	expect_equal(stats[,,"recencySendSender"], recencySendSender)
	
	# recencySendReceiver
	recencySendReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 0, 0), 
		c(0, 0, 1/2, 0, 0), 
		c(1/2, 0, 1/3, 0, 1/2), 
		c(1/2, 0, 1/4, 0, 1/2))
	expect_equal(stats[,,"recencySendReceiver"], recencySendReceiver)
	
	# recencyReceiveSender
	recencyReceiveSender <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1/2, 1/2, 0), 
		c(0, 0, 1/3, 1/3, 1/2), 
		c(1/2, 1/2, 1/4, 1/4, 1/3), 
		c(1/3, 1/3, 1/5, 1/5, 1/2))
	expect_equal(stats[,,"recencyReceiveSender"], recencyReceiveSender)
	
	# recencyReceiveReceiver
	recencyReceiveReceiver <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1/2, 0, 0, 0, 1/2), 
		c(1/3, 1/2, 0, 1/2, 1/3), 
		c(1/4, 1/3, 1/2, 1/3, 1/4), 
		c(1/5, 1/2, 1/3, 1/2, 1/5))
	expect_equal(stats[,,"recencyReceiveReceiver"], recencyReceiveReceiver)
	
	# rrankSend
	rrankSend <- rbind(matrix(0, ncol = nrow(riskset)),
		c(1, 0, 0, 0, 0), 
		c(1/2, 1, 0, 0, 0), 
		c(1/2, 1, 1, 0, 0), 
		c(1/2, 1, 1/2, 1, 0))
	expect_equal(stats[,,"rrankSend"], rrankSend)
	
	# rrankReceive
	rrankReceive <- rbind(matrix(0, ncol = nrow(riskset)),
		c(0, 0, 1, 0, 0), 
		c(0, 0, 1, 0, 0), 
		c(1, 0, 1, 0, 0), 
		c(1, 0, 1, 0, 1))
	expect_equal(stats[,,"rrankReceive"], rrankReceive)
})

# Condition 9: Actor-oriented model
test_that("Endo stats: condition 9", {
	reh <- remify::remify(edgelist, model = "actor")
	sender_effects <- ~ 
		indegreeSender() + outdegreeSender() + totaldegreeSender() +
		recencySendSender() + recencyReceiveSender()
	receiver_effects <- ~ 
		indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
		inertia() + reciprocity() +
		isp() +	itp() +	osp() +	otp() +
		recencyContinue() + recencySendReceiver() + recencyReceiveReceiver() +
		rrankSend() +	rrankReceive()  
	stats <- remstats(reh, sender_effects = sender_effects, 
		receiver_effects = receiver_effects)
	sender_stats <- stats$sender_stats
	receiver_stats <- stats$receiver_stats
	actors <- attr(reh, "dictionary")$actors
	
	# baseline
	expect_equal(sender_stats[,,"baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(actors)))
	
	# outdegreeSender 
	outdegreeSender <- rbind(matrix(0, ncol = nrow(actors)),
		c(1, 0, 0), 
		c(2, 0, 0), 
		c(2, 1, 0), 
		c(2, 2, 0))
	expect_equal(sender_stats[,,"outdegreeSender"], outdegreeSender)
	
	# indegreeSender 
	indegreeSender <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 1, 0), 
		c(0, 1, 1), 
		c(1, 1, 1), 
		c(1, 1, 2))
	expect_equal(sender_stats[,,"indegreeSender"], indegreeSender)
	
	# totaldegreeSender
	totaldegreeSender <- indegreeSender + outdegreeSender
	expect_equal(sender_stats[,,"totaldegreeSender"], totaldegreeSender)
	
	# recencySendSender 
	recencySendSender <- rbind(matrix(0, ncol = nrow(actors)),
		c(1/2, 0, 0), 
		c(1/2, 0, 0), 
		c(1/3, 1/2, 0), 
		c(1/4, 1/2, 0))
	expect_equal(sender_stats[,,"recencySendSender"], recencySendSender)
	
	# recencyReceiveSender 
	recencyReceiveSender <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 1/2, 0), 
		c(0, 1/3, 1/2), 
		c(1/2, 1/4, 1/3), 
		c(1/3, 1/5, 1/2))
	expect_equal(sender_stats[,,"recencyReceiveSender"], recencyReceiveSender)
	
	# outdegreeReceiver 
	outdegreeReceiver <- rbind(matrix(0, ncol = nrow(actors)),
		c(1, 0, 0), 
		c(2, 0, 0), 
		c(2, 1, 0), 
		c(2, 2, 0))
	expect_equal(receiver_stats[,,"outdegreeReceiver"], outdegreeReceiver)
	
	# indegreeReceiver 
	indegreeReceiver <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 1, 0), 
		c(0, 1, 1), 
		c(1, 1, 1), 
		c(1, 1, 2))
	expect_equal(receiver_stats[,,"indegreeReceiver"], indegreeReceiver)
	
	# totaldegreeReceiver
	totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
	expect_equal(receiver_stats[,,"totaldegreeReceiver"], totaldegreeReceiver)
	
	# itp 
	itp <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 0, 0), 
		c(0, 0, 0), 
		c(0, 0, 0), 
		c(1, 1, 0))
	expect_equal(receiver_stats[,,"itp"], itp)
	
	# otp 
	otp <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 0, 0), 
		c(0, 0, 0), 
		c(0, 0, 1), 
		c(0, 0, 0))
	expect_equal(receiver_stats[,,"otp"], otp)
	
	# isp 
	isp <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 0, 0), 
		c(0, 0, 1), 
		c(0, 0, 1), 
		c(1, 1, 0))
	expect_equal(receiver_stats[,,"isp"], isp)
	
	# osp 
	osp <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 0, 0), 
		c(0, 0, 0), 
		c(0, 0, 0), 
		c(0, 0, 0))
	expect_equal(receiver_stats[,,"osp"], osp)
	
	# recencyContinue 
	recencyContinue <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 1/2, 0), 
		c(0, 0, 0), 
		c(1/2, 0, 0), 
		c(0, 0, 0))
	expect_equal(receiver_stats[,,"recencyContinue"], recencyContinue)
	
	# recencySendReceiver 
	recencySendReceiver <- rbind(matrix(0, ncol = nrow(actors)),
		c(1/2, 0, 0), 
		c(1/2, 0, 0), 
		c(1/3, 1/2, 0), 
		c(1/4, 1/2, 0))
	expect_equal(receiver_stats[,,"recencySendReceiver"], recencySendReceiver)
	
	# recencyReceiveReceiver 
	recencyReceiveReceiver <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 1/2, 0), 
		c(0, 1/3, 1/2), 
		c(1/2, 1/4, 1/3), 
		c(1/3, 1/5, 1/2))
	expect_equal(receiver_stats[,,"recencyReceiveReceiver"], recencyReceiveReceiver)
	
	# rrankSend 
	rrankSend <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 1, 0), 
		c(0, 0, 0), 
		c(1, 0, 0), 
		c(0, 0, 0))
	expect_equal(receiver_stats[,,"rrankSend"], rrankSend)
	
	# rrankReceive 
	rrankReceive <- rbind(matrix(0, ncol = nrow(actors)),
		c(0, 0, 0), 
		c(1, 0, 0), 
		c(1, 0, 0), 
		c(1/2, 1, 0))
	expect_equal(receiver_stats[,,"rrankReceive"], rrankReceive)
})

