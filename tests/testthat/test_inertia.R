context("inertia")

library(remstats)

test_that("scaling is counts", {
	# Specify the effect and compute 
	out <- remstats(~inertia(), edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_equal(rowSums(stats), 0:(nrow(history)-1))
})

test_that("scaling is outdegreeSender", {
	# Specify effects
	form <- ~ inertia(scaling = "outdegreeSender") + 
		inertia() + outdegreeSender()

	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	temp <- stats[,,2]/stats[,,3]
	temp[is.na(temp)] <- 0
	expect_equal(stats[,,1], temp)
})

test_that("scaling is standardize", {
	# Specify effects
	form <- ~ inertia(scaling = "standardize") + 
		inertia()

	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	temp <- t(apply(stats[,,2], 1, function(x) (x-mean(x))/sd(x)))
	temp[is.na(temp)] <- 0
	expect_equal(stats[,,1], temp)
})

test_that("memory_value", {
	# Specify the effect and compute
	out <- remstats(~ inertia(memory_value = 1000), edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_true(all(diff(rowSums(stats)) <= 1))
	expect_true(!all(diff(rowSums(stats)) == 1))
})

test_that("with type", {
	# Specify the effect and compute
	out <- remstats(~ inertia(with_type = TRUE), edgelist = history, 
		with_type = TRUE)
	stats <- out$statistics
	
	# Tests
	expect_equal(rowSums(stats), 0:(nrow(history)-1))
	n <- length(unique(info$id))
	expect_equal(ncol(stats), n*(n-1)*2)
})

test_that("event weights", {
	# Specify the effect and compute
	out <- remstats(~ inertia(event_weights = history$weight), 
		edgelist = history)
	stats <- out$statistics

	# Tests
	expect_equal(rowSums(stats), 
		c(0, head(cumsum(history$weight), n = nrow(history)-1)))
})