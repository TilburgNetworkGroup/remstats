context("directed triad stats")

library(remstats)

test_that("scaling is counts", {
	# Specify the stats
	form <- ~ otp() + itp() + osp() + isp()

	# Compute the stats
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset
	
	# Tests
	reverse_dyads <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(stats[,,"otp"], stats[,reverse_dyads,"itp"])
	expect_equal(stats[,,"osp"], stats[,reverse_dyads,"osp"])
	expect_equal(stats[,,"isp"], stats[,reverse_dyads,"isp"])
	
	expect_true(all(diff(rowSums(stats[,,"otp"]))>=0))
	expect_true(all(diff(rowSums(stats[,,"itp"]))>=0))
	expect_true(all(diff(rowSums(stats[,,"osp"]))>=0))
	expect_true(all(diff(rowSums(stats[,,"isp"]))>=0))
})

test_that("scaling is standardize", {
	# Specify the stats
	form <- ~ otp(scaling = "standardize") + itp(scaling = "standardize") + 
		osp(scaling = "standardize") + isp(scaling = "standardize")

	# Compute the stats
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset
	
	# Tests
	reverse_dyads <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(stats[,,"otp"], stats[,reverse_dyads,"itp"])
	expect_equal(stats[,,"osp"], stats[,reverse_dyads,"osp"])
	expect_equal(stats[,,"isp"], stats[,reverse_dyads,"isp"])
	
	expect_equal(rowMeans(stats[,,"otp"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"itp"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"osp"]), rep(0, nrow(stats)))
	expect_equal(rowMeans(stats[,,"isp"]), rep(0, nrow(stats)))
})

test_that("memory is window", {
	# Specify the stats
	form <- ~ otp(memory_value = 10000) + itp(memory_value = 10000) + 
		osp(memory_value = 10000) + isp(memory_value = 10000)

	# Compute the stats
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset

	# Tests	
	reverse_dyads <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(stats[,,"otp"], stats[,reverse_dyads,"itp"])
	expect_equal(stats[,,"osp"], stats[,reverse_dyads,"osp"])
	expect_equal(stats[,,"isp"], stats[,reverse_dyads,"isp"])
})

test_that("with type", {
	# Specify the stats
	form <- ~ otp() + otp(with_type = TRUE) + itp() + itp(with_type = TRUE) + 
		osp() + osp(with_type = TRUE) + isp() + isp(with_type = TRUE)
	
	# Make type a column name in the history
	names(history)[4] <- "type"

	# Compute the stats	
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset
	
	# Tests	
	expect_true(all(stats[,,1]>=stats[,,2]))
	expect_true(all(stats[,,3]>=stats[,,4]))
	expect_true(all(stats[,,5]>=stats[,,6]))
	expect_true(all(stats[,,7]>=stats[,,8]))
})

test_that("event weights", {
	# Specify the stats
	form <- ~ otp(event_weights = history$weight) + 
		itp(event_weights = history$weight) + 
		osp(event_weights = history$weight) + 
		isp(event_weights = history$weight)

	# Compute the stats	
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset
	
	# Tests
	reverse_dyads <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(stats[,,"otp"], stats[,reverse_dyads,"itp"])
	expect_equal(stats[,,"osp"], stats[,reverse_dyads,"osp"])
	expect_equal(stats[,,"isp"], stats[,reverse_dyads,"isp"])
})