# Small edgelist
edgelist <- data.frame(
	time = c(1, 2, 3, 4, 5, 5, 7, 8, 9, 10),
	actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
	actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1),
	type = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
)

# Actor info
info <- data.frame(
	name = 1:4,
	time = rep(0, 4),
	x1 = c(10, 20, 30, 40),
	x2 = c(0, 1, 1, 0)
)

info2 <- data.frame(
	name = 1:4,
	time = rep(3, 4),
	x1 = c(100, 200, 300, 400),
	x2 = c(1, 1, 0, 0)
)

info <- rbind(info, info2)

# Function to transform a tomstats object to tomstats_sample
transform <- function(x, caseControls) {
	t(sapply(1:nrow(caseControls), function(i) {
		x[i,caseControls[i,]]
	}))
}

# Default settings ----------------------------------------------------
# Prep edgelist
reh <- remify::remify(edgelist = edgelist[,1:3], model = "tie")

# Define the effects
effects <- ~ inertia() + reciprocity() + otp() + itp() + isp() + osp() +
	outdegreeSender() + indegreeSender() + outdegreeReceiver() +
	indegreeReceiver() + totaldegreeSender() + totaldegreeReceiver() +
	psABBA() + psABAB() + psABBY() + psABXA() + psABXB() + psABAY() +
	psABXY() + rrankSend() + rrankReceive() + recencyContinue() +
	recencySendSender() + recencySendReceiver() + recencyReceiveSender() +
	recencyReceiveReceiver() + send(variable = "x1") + receive(variable = "x1") + 
	average(variable = "x1") + difference(variable = "x1") +
	maximum(variable = "x1") + minimum(variable = "x1") +
	same(variable = "x2") 

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh, attr_actors = info)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5,
																attr_actors = info)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"inertia"], 
						 transform(stats[,,"inertia"], caseControls))
expect_equal(sample_stats[,,"reciprocity"], 
						 transform(stats[,,"reciprocity"], caseControls))
expect_equal(sample_stats[,,"otp"], 
						 transform(stats[,,"otp"], caseControls))
expect_equal(sample_stats[,,"itp"], 
						 transform(stats[,,"itp"], caseControls))
expect_equal(sample_stats[,,"isp"], 
						 transform(stats[,,"isp"], caseControls))
expect_equal(sample_stats[,,"osp"], 
						 transform(stats[,,"osp"], caseControls))
expect_equal(sample_stats[,,"outdegreeSender"], 
						 transform(stats[,,"outdegreeSender"], caseControls))
expect_equal(sample_stats[,,"indegreeSender"], 
						 transform(stats[,,"indegreeSender"], caseControls))
expect_equal(sample_stats[,,"outdegreeReceiver"], 
						 transform(stats[,,"outdegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"indegreeReceiver"], 
						 transform(stats[,,"indegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"totaldegreeSender"], 
						 transform(stats[,,"totaldegreeSender"], caseControls))
expect_equal(sample_stats[,,"psABBA"], 
						 transform(stats[,,"psABBA"], caseControls))
expect_equal(sample_stats[,,"psABAB"], 
						 transform(stats[,,"psABAB"], caseControls))
expect_equal(sample_stats[,,"psABBY"], 
						 transform(stats[,,"psABBY"], caseControls))
expect_equal(sample_stats[,,"psABXA"], 
						 transform(stats[,,"psABXA"], caseControls))
expect_equal(sample_stats[,,"psABXB"], 
						 transform(stats[,,"psABXB"], caseControls))
expect_equal(sample_stats[,,"psABAY"], 
						 transform(stats[,,"psABAY"], caseControls))
expect_equal(sample_stats[,,"psABXY"], 
						 transform(stats[,,"psABXY"], caseControls)) 
expect_equal(sample_stats[,,"rrankSend"], 
						 transform(stats[,,"rrankSend"], caseControls))
expect_equal(sample_stats[,,"rrankReceive"], 
						 transform(stats[,,"rrankReceive"], caseControls)) 
expect_equal(sample_stats[,,"recencyContinue"], 
						 transform(stats[,,"recencyContinue"], caseControls)) 
expect_equal(sample_stats[,,"recencySendSender"], 
						 transform(stats[,,"recencySendSender"], caseControls)) 
expect_equal(sample_stats[,,"recencySendReceiver"], 
						 transform(stats[,,"recencySendReceiver"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveSender"], 
						 transform(stats[,,"recencyReceiveSender"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveReceiver"], 
						 transform(stats[,,"recencyReceiveReceiver"], caseControls))
expect_equal(sample_stats[,,"send_x1"], 
						 transform(stats[,,"send_x1"], caseControls))
expect_equal(sample_stats[,,"receive_x1"], 
						 transform(stats[,,"receive_x1"], caseControls))
expect_equal(sample_stats[,,"average_x1"], 
						 transform(stats[,,"average_x1"], caseControls))
expect_equal(sample_stats[,,"difference_x1"], 
						 transform(stats[,,"difference_x1"], caseControls))
expect_equal(sample_stats[,,"maximum_x1"], 
						 transform(stats[,,"maximum_x1"], caseControls))
expect_equal(sample_stats[,,"minimum_x1"], 
						 transform(stats[,,"minimum_x1"], caseControls))
expect_equal(sample_stats[,,"same_x2"], 
						 transform(stats[,,"same_x2"], caseControls))

# Scaling -------------------------------------------------------------
# Prep edgelist
reh <- remify::remify(edgelist = edgelist[,1:3], model = "tie")

# Define the effects
effects <- ~ inertia(scaling = "prop") + reciprocity(scaling = "prop") + 
	otp(unique = TRUE) + itp(unique = TRUE) + 
	isp(unique = TRUE) + osp(unique = TRUE) +
	outdegreeSender(scaling = "prop") + indegreeSender(scaling = "prop") + 
	outdegreeReceiver(scaling = "prop") + indegreeReceiver(scaling = "prop") + 
	totaldegreeSender(scaling = "prop") + totaldegreeReceiver(scaling = "prop") 

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"inertia"], 
						 transform(stats[,,"inertia"], caseControls))
expect_equal(sample_stats[,,"reciprocity"], 
						 transform(stats[,,"reciprocity"], caseControls))
expect_equal(sample_stats[,,"otp.unique"], 
						 transform(stats[,,"otp.unique"], caseControls))
expect_equal(sample_stats[,,"itp.unique"], 
						 transform(stats[,,"itp.unique"], caseControls))
expect_equal(sample_stats[,,"isp.unique"], 
						 transform(stats[,,"isp.unique"], caseControls))
expect_equal(sample_stats[,,"osp.unique"], 
						 transform(stats[,,"osp.unique"], caseControls))
expect_equal(sample_stats[,,"outdegreeSender"], 
						 transform(stats[,,"outdegreeSender"], caseControls))
expect_equal(sample_stats[,,"indegreeSender"], 
						 transform(stats[,,"indegreeSender"], caseControls))
expect_equal(sample_stats[,,"outdegreeReceiver"], 
						 transform(stats[,,"outdegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"indegreeReceiver"], 
						 transform(stats[,,"indegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"totaldegreeSender"], 
						 transform(stats[,,"totaldegreeSender"], caseControls))

# Method is 'pe' ----------------------------------------------------
# Prep edgelist
reh <- remify::remify(edgelist = edgelist[,1:3], model = "tie")

# Define the effects
effects <- ~ inertia() + reciprocity() + otp() + itp() + isp() + osp() +
	outdegreeSender() + indegreeSender() + outdegreeReceiver() +
	indegreeReceiver() + totaldegreeSender() + totaldegreeReceiver() +
	psABBA() + psABAB() + psABBY() + psABXA() + psABXB() + psABAY() +
	psABXY() + rrankSend() + rrankReceive() + recencyContinue() +
	recencySendSender() + recencySendReceiver() + recencyReceiveSender() +
	recencyReceiveReceiver() + send(variable = "x1") + receive(variable = "x1") + 
	average(variable = "x1") + difference(variable = "x1") +
	maximum(variable = "x1") + minimum(variable = "x1") +
	same(variable = "x2") 

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh, method = "pe", 
									attr_actors = info)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5,
																method = "pe", attr_actors = info)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"inertia"], 
						 transform(stats[,,"inertia"], caseControls))
expect_equal(sample_stats[,,"reciprocity"], 
						 transform(stats[,,"reciprocity"], caseControls))
expect_equal(sample_stats[,,"otp"], 
						 transform(stats[,,"otp"], caseControls))
expect_equal(sample_stats[,,"itp"], 
						 transform(stats[,,"itp"], caseControls))
expect_equal(sample_stats[,,"isp"], 
						 transform(stats[,,"isp"], caseControls))
expect_equal(sample_stats[,,"osp"], 
						 transform(stats[,,"osp"], caseControls))
expect_equal(sample_stats[,,"outdegreeSender"], 
						 transform(stats[,,"outdegreeSender"], caseControls))
expect_equal(sample_stats[,,"indegreeSender"], 
						 transform(stats[,,"indegreeSender"], caseControls))
expect_equal(sample_stats[,,"outdegreeReceiver"], 
						 transform(stats[,,"outdegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"indegreeReceiver"], 
						 transform(stats[,,"indegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"totaldegreeSender"], 
						 transform(stats[,,"totaldegreeSender"], caseControls))
expect_equal(sample_stats[,,"psABBA"], 
						 transform(stats[,,"psABBA"], caseControls))
expect_equal(sample_stats[,,"psABAB"], 
						 transform(stats[,,"psABAB"], caseControls))
expect_equal(sample_stats[,,"psABBY"], 
						 transform(stats[,,"psABBY"], caseControls))
expect_equal(sample_stats[,,"psABXA"], 
						 transform(stats[,,"psABXA"], caseControls))
expect_equal(sample_stats[,,"psABXB"], 
						 transform(stats[,,"psABXB"], caseControls))
expect_equal(sample_stats[,,"psABAY"], 
						 transform(stats[,,"psABAY"], caseControls))
expect_equal(sample_stats[,,"psABXY"], 
						 transform(stats[,,"psABXY"], caseControls)) 
expect_equal(sample_stats[,,"rrankSend"], 
						 transform(stats[,,"rrankSend"], caseControls))
expect_equal(sample_stats[,,"rrankReceive"], 
						 transform(stats[,,"rrankReceive"], caseControls)) 
expect_equal(sample_stats[,,"recencyContinue"], 
						 transform(stats[,,"recencyContinue"], caseControls)) 
expect_equal(sample_stats[,,"recencySendSender"], 
						 transform(stats[,,"recencySendSender"], caseControls)) 
expect_equal(sample_stats[,,"recencySendReceiver"], 
						 transform(stats[,,"recencySendReceiver"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveSender"], 
						 transform(stats[,,"recencyReceiveSender"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveReceiver"], 
						 transform(stats[,,"recencyReceiveReceiver"], caseControls)) 
expect_equal(sample_stats[,,"send_x1"], 
						 transform(stats[,,"send_x1"], caseControls))
expect_equal(sample_stats[,,"receive_x1"], 
						 transform(stats[,,"receive_x1"], caseControls))
expect_equal(sample_stats[,,"average_x1"], 
						 transform(stats[,,"average_x1"], caseControls))
expect_equal(sample_stats[,,"difference_x1"], 
						 transform(stats[,,"difference_x1"], caseControls))
expect_equal(sample_stats[,,"maximum_x1"], 
						 transform(stats[,,"maximum_x1"], caseControls))
expect_equal(sample_stats[,,"minimum_x1"], 
						 transform(stats[,,"minimum_x1"], caseControls))
expect_equal(sample_stats[,,"same_x2"], 
						 transform(stats[,,"same_x2"], caseControls))

# Event types, consider_type = TRUE ------------------------------------------
# Prep edgelist
reh <- remify::remify(edgelist = edgelist, model = "tie")

# Define the effects
effects <- ~ inertia() + reciprocity() + otp() + itp() + isp() + osp() +
	outdegreeSender() + indegreeSender() + outdegreeReceiver() +
	indegreeReceiver() + totaldegreeSender() + totaldegreeReceiver() +
	psABBA() + psABAB() + psABBY() + psABXA() + psABXB() + psABAY() +
	psABXY() + rrankSend() + rrankReceive() + recencyContinue() +
	recencySendSender() + recencySendReceiver() + recencyReceiveSender() +
	recencyReceiveReceiver() + FEtype() + send(variable = "x1") + receive(variable = "x1") + 
	average(variable = "x1") + difference(variable = "x1") +
	maximum(variable = "x1") + minimum(variable = "x1") +
	same(variable = "x2") 

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh, attr_actors = info)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5,
																attr_actors = info)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"inertia"], 
						 transform(stats[,,"inertia"], caseControls))
expect_equal(sample_stats[,,"reciprocity"], 
						 transform(stats[,,"reciprocity"], caseControls))
expect_equal(sample_stats[,,"otp"], 
						 transform(stats[,,"otp"], caseControls))
expect_equal(sample_stats[,,"itp"], 
						 transform(stats[,,"itp"], caseControls))
expect_equal(sample_stats[,,"isp"], 
						 transform(stats[,,"isp"], caseControls))
expect_equal(sample_stats[,,"osp"], 
						 transform(stats[,,"osp"], caseControls))
expect_equal(sample_stats[,,"outdegreeSender"], 
						 transform(stats[,,"outdegreeSender"], caseControls))
expect_equal(sample_stats[,,"indegreeSender"], 
						 transform(stats[,,"indegreeSender"], caseControls))
expect_equal(sample_stats[,,"outdegreeReceiver"], 
						 transform(stats[,,"outdegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"indegreeReceiver"], 
						 transform(stats[,,"indegreeReceiver"], caseControls))
expect_equal(sample_stats[,,"totaldegreeSender"], 
						 transform(stats[,,"totaldegreeSender"], caseControls))
expect_equal(sample_stats[,,"psABBA"], 
						 transform(stats[,,"psABBA"], caseControls))
expect_equal(sample_stats[,,"psABAB"], 
						 transform(stats[,,"psABAB"], caseControls))
expect_equal(sample_stats[,,"psABBY"], 
						 transform(stats[,,"psABBY"], caseControls))
expect_equal(sample_stats[,,"psABXA"], 
						 transform(stats[,,"psABXA"], caseControls))
expect_equal(sample_stats[,,"psABXB"], 
						 transform(stats[,,"psABXB"], caseControls))
expect_equal(sample_stats[,,"psABAY"], 
						 transform(stats[,,"psABAY"], caseControls))
expect_equal(sample_stats[,,"psABXY"], 
						 transform(stats[,,"psABXY"], caseControls)) 
expect_equal(sample_stats[,,"rrankSend"], 
						 transform(stats[,,"rrankSend"], caseControls))
expect_equal(sample_stats[,,"rrankReceive"], 
						 transform(stats[,,"rrankReceive"], caseControls)) 
expect_equal(sample_stats[,,"recencyContinue"], 
						 transform(stats[,,"recencyContinue"], caseControls)) 
expect_equal(sample_stats[,,"recencySendSender"], 
						 transform(stats[,,"recencySendSender"], caseControls)) 
expect_equal(sample_stats[,,"recencySendReceiver"], 
						 transform(stats[,,"recencySendReceiver"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveSender"], 
						 transform(stats[,,"recencyReceiveSender"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveReceiver"], 
						 transform(stats[,,"recencyReceiveReceiver"], caseControls))
expect_equal(sample_stats[,,"FEtype_2"], 
						 transform(stats[,,"FEtype_2"], caseControls))
expect_equal(sample_stats[,,"FEtype_3"], 
						 transform(stats[,,"FEtype_3"], caseControls))
expect_equal(sample_stats[,,"send_x1"], 
						 transform(stats[,,"send_x1"], caseControls))
expect_equal(sample_stats[,,"receive_x1"], 
						 transform(stats[,,"receive_x1"], caseControls))
expect_equal(sample_stats[,,"average_x1"], 
						 transform(stats[,,"average_x1"], caseControls))
expect_equal(sample_stats[,,"difference_x1"], 
						 transform(stats[,,"difference_x1"], caseControls))
expect_equal(sample_stats[,,"maximum_x1"], 
						 transform(stats[,,"maximum_x1"], caseControls))
expect_equal(sample_stats[,,"minimum_x1"], 
						 transform(stats[,,"minimum_x1"], caseControls))
expect_equal(sample_stats[,,"same_x2"], 
						 transform(stats[,,"same_x2"], caseControls))

# Event types, consider_type = FALSE ------------------------------------------
# Prep edgelist
reh <- remify::remify(edgelist = edgelist, model = "tie")

# Define the effects
effects <- ~ inertia(consider_type = FALSE) + 
	reciprocity(consider_type = FALSE) + otp(consider_type = FALSE) + 
	itp(consider_type = FALSE) + isp(consider_type = FALSE) + 
	osp(consider_type = FALSE) +outdegreeSender(consider_type = FALSE) + 
	indegreeSender(consider_type = FALSE) + 
	outdegreeReceiver(consider_type = FALSE) +
	indegreeReceiver(consider_type = FALSE) + 
	totaldegreeSender(consider_type = FALSE) + 
	totaldegreeReceiver(consider_type = FALSE) +
	psABBA(consider_type = FALSE) + psABAB(consider_type = FALSE) + 
	psABBY(consider_type = FALSE) + psABXA(consider_type = FALSE) + 
	psABXB(consider_type = FALSE) + psABAY(consider_type = FALSE) +
	psABXY(consider_type = FALSE) + rrankSend(consider_type = FALSE) + 
	rrankReceive(consider_type = FALSE) + recencyContinue(consider_type = FALSE) +
	recencySendSender(consider_type = FALSE) + 
	recencySendReceiver(consider_type = FALSE) + 
	recencyReceiveSender(consider_type = FALSE) +
	recencyReceiveReceiver(consider_type = FALSE) 

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"inertia.TypeAgg"], 
						 transform(stats[,,"inertia.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"reciprocity.TypeAgg"], 
						 transform(stats[,,"reciprocity.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"otp.TypeAgg"], 
						 transform(stats[,,"otp.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"itp.TypeAgg"], 
						 transform(stats[,,"itp.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"isp.TypeAgg"], 
						 transform(stats[,,"isp.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"osp.TypeAgg"], 
						 transform(stats[,,"osp.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"outdegreeSender.TypeAgg"], 
						 transform(stats[,,"outdegreeSender.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"indegreeSender.TypeAgg"], 
						 transform(stats[,,"indegreeSender.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"outdegreeReceiver.TypeAgg"], 
						 transform(stats[,,"outdegreeReceiver.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"indegreeReceiver.TypeAgg"], 
						 transform(stats[,,"indegreeReceiver.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"totaldegreeSender.TypeAgg"], 
						 transform(stats[,,"totaldegreeSender.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABBA.TypeAgg"], 
						 transform(stats[,,"psABBA.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABAB.TypeAgg"], 
						 transform(stats[,,"psABAB.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABBY.TypeAgg"], 
						 transform(stats[,,"psABBY.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABXA.TypeAgg"], 
						 transform(stats[,,"psABXA.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABXB.TypeAgg"], 
						 transform(stats[,,"psABXB.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABAY.TypeAgg"], 
						 transform(stats[,,"psABAY.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"psABXY.TypeAgg"], 
						 transform(stats[,,"psABXY.TypeAgg"], caseControls)) 
expect_equal(sample_stats[,,"rrankSend.TypeAgg"], 
						 transform(stats[,,"rrankSend.TypeAgg"], caseControls))
expect_equal(sample_stats[,,"rrankReceive.TypeAgg"], 
						 transform(stats[,,"rrankReceive.TypeAgg"], caseControls)) 
expect_equal(sample_stats[,,"recencyContinue.TypeAgg"], 
						 transform(stats[,,"recencyContinue.TypeAgg"], caseControls)) 
expect_equal(sample_stats[,,"recencySendSender.TypeAgg"], 
						 transform(stats[,,"recencySendSender.TypeAgg"], caseControls)) 
expect_equal(sample_stats[,,"recencySendReceiver.TypeAgg"], 
						 transform(stats[,,"recencySendReceiver.TypeAgg"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveSender.TypeAgg"], 
						 transform(stats[,,"recencyReceiveSender.TypeAgg"], caseControls)) 
expect_equal(sample_stats[,,"recencyReceiveReceiver.TypeAgg"], 
						 transform(stats[,,"recencyReceiveReceiver.TypeAgg"], caseControls)) 

# Undirected ----------------------------------------------------------
# Prep edgelist
reh <- remify::remify(edgelist = edgelist[,1:3], model = "tie", 
											directed = FALSE)

# Define the effects
effects <- ~ inertia() + sp() + degreeMin() + degreeDiff() + degreeMax() +
	totaldegreeDyad() + psABAB() + psABAY() + recencyContinue() +
	average(variable = "x1") + difference(variable = "x1") +
	maximum(variable = "x1") + minimum(variable = "x1") +
	same(variable = "x2") + sp(unique = TRUE)

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh, attr_actors = info)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5,
																attr_actors = info)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"inertia"], 
						 transform(stats[,,"inertia"], caseControls))
expect_equal(sample_stats[,,"sp"], 
						 transform(stats[,,"sp"], caseControls)) 
expect_equal(sample_stats[,,"degreeMin"], 
						 transform(stats[,,"degreeMin"], caseControls))
expect_equal(sample_stats[,,"degreeDiff"], 
						 transform(stats[,,"degreeDiff"], caseControls))
expect_equal(sample_stats[,,"degreeMax"], 
						 transform(stats[,,"degreeMax"], caseControls))
expect_equal(sample_stats[,,"totaldegreeDyad"], 
						 transform(stats[,,"totaldegreeDyad"], caseControls))
expect_equal(sample_stats[,,"psABAB"], 
						 transform(stats[,,"psABAB"], caseControls))
expect_equal(sample_stats[,,"psABAY"], 
						 transform(stats[,,"psABAY"], caseControls))  # FAILED
expect_equal(sample_stats[,,"recencyContinue"], 
						 transform(stats[,,"recencyContinue"], caseControls)) 
expect_equal(sample_stats[,,"average_x1"], 
						 transform(stats[,,"average_x1"], caseControls))
expect_equal(sample_stats[,,"difference_x1"], 
						 transform(stats[,,"difference_x1"], caseControls))
expect_equal(sample_stats[,,"maximum_x1"], 
						 transform(stats[,,"maximum_x1"], caseControls))
expect_equal(sample_stats[,,"minimum_x1"], 
						 transform(stats[,,"minimum_x1"], caseControls))
expect_equal(sample_stats[,,"same_x2"], 
						 transform(stats[,,"same_x2"], caseControls))
expect_equal(sample_stats[,,"sp.unique"], 
						 transform(stats[,,"sp.unique"], caseControls)) 

# attr_dyads wide ---------------------------------------------
# Dyads info 
tie_wide <- matrix(1:16, nrow = 4, ncol = 4)

# Prep edgelist
reh <- remify::remify(edgelist = edgelist[,1:3], model = "tie")

# Define the effects
effects <- ~ tie(variable = "x")

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh, attr_dyads = tie_wide)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5,
																attr_dyads = tie_wide)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"tie_x"], 
						 transform(stats[,,"tie_x"], caseControls))

# attr_dyads long ---------------------------------------------
# Dyads info 
tie_long <- data.frame(actor1 = rep(1:4, times = 4),
											 actor2 = rep(1:4, each = 4),
											 x = 1:16)

# Prep edgelist
reh <- remify::remify(edgelist = edgelist[,1:3], model = "tie")

# Define the effects
effects <- ~ tie(variable = "x")

# Calculate the statistics with and without sampling
stats <- tomstats(effects = effects, reh = reh, attr_dyads = tie_long)
sample_stats <- tomstats_sample(effects = effects, reh = reh, controls = .5,
																attr_dyads = tie_long)

# Extract the caseControls
caseControls <- attr(sample_stats, "caseControls")

# Compare
expect_equal(sample_stats[,,"tie_x"], 
						 transform(stats[,,"tie_x"], caseControls))
