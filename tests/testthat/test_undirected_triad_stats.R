context("undirected triad stats")

library(remstats)

n <- length(unique(info$id))

test_that("scaling is counts", {
	# Compute the stats
	out <- remstats(~ sp() + spUnique(), edgelist = history, directed = FALSE)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats[,,"sp"] >= stats[,,"spUnique"]))
	expect_true(max(stats[,,"spUnique"]) <= n)
})

test_that("scaling is standardize", {
	# Specify the effects
	form <- ~ sp(scaling = "standardize") + spUnique(scaling = "standardize") + 
		sp() + spUnique()

	# Compute the stats
	out <- remstats(form, edgelist = history, directed = FALSE)
	stats <- out$statistics

	# Tests
	temp <- t(apply(stats[,,3], 1, function(x) (x-mean(x))/sd(x)))
	temp[is.na(temp)] <- 0
	expect_equal(stats[,,1], temp)

	temp <- t(apply(stats[,,4], 1, function(x) (x-mean(x))/sd(x)))
	temp[is.na(temp)] <- 0
	expect_equal(stats[,,2], temp)
})

test_that("memory_value", {
	# Specify the effects
	form <- ~ sp(memory_value = 1000) + spUnique(memory_value = 1000) 

	# Compute the stats
	out <- remstats(form, edgelist = history, directed = FALSE)
	stats <- out$statistics

	# Tests
	expect_true(all(stats[,,"sp"] >= stats[,,"spUnique"]))
	expect_true(max(stats[,,"spUnique"]) <= n)
})

test_that("with type", {
	# Specify the effects
	form <- ~ sp(with_type = TRUE) + sp() + spUnique(with_type = TRUE) +
		spUnique()

	# Compute the stats
	out <- remstats(form, edgelist = history, directed = FALSE, 
		with_type = TRUE)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats[,,2]>=stats[,,1]))
	expect_true(all(stats[,,4]>=stats[,,3]))
})

test_that("event weights", {
	# Specify the effects
	form <- ~ sp(event_weights = history$weight) + 
		sp() 

	# Compute the stats
	out <- remstats(form, edgelist = history, directed = FALSE)
	stats <- out$statistics
	
	# Test
	expect_true(all(stats[,,1]>=stats[,,2]))
})