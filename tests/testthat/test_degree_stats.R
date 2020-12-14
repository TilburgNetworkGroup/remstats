context("degree stats")

library(remstats)

test_that("scaling is counts", {
	# Specify the effects
	form <- ~ indegreeSender() + indegreeReceiver() + outdegreeSender() + 
		outdegreeReceiver() + totaldegreeSender() + totaldegreeReceiver()

	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset
	edgelist <- out$edgelist
	
	# Tests
	# Test the last row
	expect_true(all(stats[nrow(stats),, "indegreeSender"] == 	
		apply(riskset, 1, function(x) {
			sum(edgelist[-nrow(edgelist),3] == x[1])
		})
	))
	expect_true(all(stats[nrow(stats),, "indegreeReceiver"] == 	
		apply(riskset, 1, function(x) {
			sum(edgelist[-nrow(edgelist),3] == x[2])
		})
	))
	expect_true(all(stats[nrow(stats),, "outdegreeSender"] == 	
		apply(riskset, 1, function(x) {
			sum(edgelist[-nrow(edgelist),2] == x[1])
		})
	))
	expect_true(all(stats[nrow(stats),, "outdegreeReceiver"] == 	
		apply(riskset, 1, function(x) {
			sum(edgelist[-nrow(edgelist),2] == x[2])
		})
	))
	expect_true(all(stats[nrow(stats),, "totaldegreeSender"] == 	
		apply(riskset, 1, function(x) {
			sum(edgelist[-nrow(edgelist),2] == x[1] | 
						edgelist[-nrow(edgelist),3] == x[1])
		})
	))
	expect_true(all(stats[nrow(stats),, "totaldegreeReceiver"] == 	
		apply(riskset, 1, function(x) {
			sum(edgelist[-nrow(edgelist),2] == x[2] | 
						edgelist[-nrow(edgelist),3] == x[2])
		})
	))
	
	# Test the rowsums
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"indegreeSender"]), 
		seq(0, (nrow(history)-1)*(n-1), n-1))
	expect_equal(rowSums(stats[,,"indegreeReceiver"]), 
		seq(0, (nrow(history)-1)*(n-1), n-1))
	expect_equal(rowSums(stats[,,"outdegreeSender"]), 
		seq(0, (nrow(history)-1)*(n-1), n-1))
	expect_equal(rowSums(stats[,,"outdegreeReceiver"]), 
		seq(0, (nrow(history)-1)*(n-1), n-1))
	expect_equal(rowSums(stats[,,"totaldegreeSender"]), 
		seq(0, (nrow(history)-1)*(n-1), n-1)*2)
	expect_equal(rowSums(stats[,,"totaldegreeReceiver"]), 
		seq(0, (nrow(history)-1)*(n-1), n-1)*2)
})

test_that("scaling is total", {
	# Specify the effects
	form <- ~ indegreeSender(scaling = "total") + 
		indegreeReceiver(scaling = "total") + 
		outdegreeSender(scaling = "total") + 
		outdegreeReceiver(scaling = "total") + 
		totaldegreeSender(scaling = "total") + 
		totaldegreeReceiver(scaling = "total")

	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Test the rowsums
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"indegreeSender"]), 
		c(0, rep(n-1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"indegreeReceiver"]), 
		c(0, rep(n-1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"outdegreeSender"]), 
		c(0, rep(n-1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"outdegreeReceiver"]), 
		c(0, rep(n-1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"totaldegreeSender"]), 
		c(0, rep(n-1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"totaldegreeReceiver"]), 
		c(0, rep(n-1, nrow(stats)-1)))
})

test_that("scaling is standardize", {
	# Specify the effects
	form <- ~ indegreeSender(scaling = "standardize") + 
		indegreeReceiver(scaling = "standardize") + 
		outdegreeSender(scaling = "standardize") + 
		outdegreeReceiver(scaling = "standardize") + 
		totaldegreeSender(scaling = "standardize") + 
		totaldegreeReceiver(scaling = "standardize")

	# Compute statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Test rowmeans
	expect_equal(rowMeans(stats[,,"indegreeSender"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"indegreeReceiver"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"outdegreeSender"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"outdegreeReceiver"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"totaldegreeSender"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"totaldegreeReceiver"]), rep(0, nrow(stats)))
})

test_that("memory_value", {
	# Specify the effects
	form <- ~ indegreeSender(memory_value = 10000) + 
		indegreeReceiver(memory_value = 10000) + 
		outdegreeSender(memory_value = 10000) + 
		outdegreeReceiver(memory_value = 10000) + 
		totaldegreeSender(memory_value = 10000) + 
		totaldegreeReceiver(memory_value = 10000)

	# Compute statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	n <- length(unique(info$id))
	expect_true(all(diff(rowSums(stats[,,"indegreeSender"])) <= (n-1)))
	expect_true(all(diff(rowSums(stats[,,"indegreeReceiver"])) <= (n-1)))
	expect_true(all(diff(rowSums(stats[,,"outdegreeSender"])) <= (n-1)))
	expect_true(all(diff(rowSums(stats[,,"outdegreeReceiver"])) <= (n-1)))
	expect_true(all(diff(rowSums(stats[,,"totaldegreeSender"])) <= (n-1)*2))
	expect_true(all(diff(rowSums(stats[,,"totaldegreeReceiver"])) <= (n-1)*2))
	
	expect_true(!all(diff(rowSums(stats[,,"indegreeSender"])) == (n-1)))
	expect_true(!all(diff(rowSums(stats[,,"indegreeReceiver"])) == (n-1)))
	expect_true(!all(diff(rowSums(stats[,,"outdegreeSender"])) == (n-1)))
	expect_true(!all(diff(rowSums(stats[,,"outdegreeReceiver"])) == (n-1)))
	expect_true(!all(diff(rowSums(stats[,,"totaldegreeSender"])) == (n-1)*2))
	expect_true(!all(diff(rowSums(stats[,,"totaldegreeReceiver"])) == (n-1)*2))
})

test_that("with type", {
	# Specify the effects
	form <- ~ indegreeSender(with_type = TRUE) + 
		indegreeReceiver(with_type = TRUE) + 
		outdegreeSender(with_type = TRUE) + 
		outdegreeReceiver(with_type = TRUE) + 
		totaldegreeSender(with_type = TRUE) + 
		totaldegreeReceiver(with_type = TRUE)

	# Compute statistics
	names(history)[4] <- "type"
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"indegreeSender"]), 
		0:(nrow(history)-1)*(n-1))
	expect_equal(rowSums(stats[,,"indegreeReceiver"]), 
		0:(nrow(history)-1)*(n-1))
	expect_equal(rowSums(stats[,,"outdegreeSender"]), 
		0:(nrow(history)-1)*(n-1))
	expect_equal(rowSums(stats[,,"outdegreeReceiver"]), 
		0:(nrow(history)-1)*(n-1))
	expect_equal(rowSums(stats[,,"totaldegreeSender"]), 
		0:(nrow(history)-1)*(n-1)*2)
	expect_equal(rowSums(stats[,,"totaldegreeReceiver"]), 
		0:(nrow(history)-1)*(n-1)*2)
	
	expect_equal(ncol(stats), n*(n-1)*2)
})

test_that("event weights", {
	# Specify the effects
	form <- ~ indegreeSender(event_weights = history$weight) + 
		indegreeReceiver(event_weights = history$weight) + 
		outdegreeSender(event_weights = history$weight) + 
		outdegreeReceiver(event_weights = history$weight) + 
		totaldegreeSender(event_weights = history$weight) + 
		totaldegreeReceiver(event_weights = history$weight)
	
	# Compute statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"indegreeSender"]), 
		c(0, head(cumsum(history$weight)*(n-1), 
		n = nrow(history)-1)))
	expect_equal(rowSums(stats[,,"indegreeReceiver"]), 
		c(0, head(cumsum(history$weight)*(n-1), 
		n = nrow(history)-1)))
	expect_equal(rowSums(stats[,,"outdegreeSender"]), 
		c(0, head(cumsum(history$weight)*(n-1), 
		n = nrow(history)-1)))
	expect_equal(rowSums(stats[,,"outdegreeReceiver"]), 
		c(0, head(cumsum(history$weight)*(n-1), 
		n = nrow(history)-1)))
	expect_equal(rowSums(stats[,,"totaldegreeSender"]), 
		c(0, head(cumsum(history$weight)*(n-1)*2, 
		n = nrow(history)-1)))
	expect_equal(rowSums(stats[,,"totaldegreeReceiver"]), 
		c(0, head(cumsum(history$weight)*(n-1)*2, 
		n = nrow(history)-1)))
})