context("pshifts")

library(remstats)

test_that("pshifts", {
	# Specify the statistics
	form <- ~ psABBA() + psABBY() + psABXA() + psABXB() + psABXY() + psABAY()
	
	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats %in% c(0,1)))
	expect_equal(rowSums(stats[,,"psABBA"]), c(0, rep(1, nrow(stats)-1)))
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"psABBY"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXA"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXB"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXY"]), 
		c(0, rep((n-2)*(n-3), nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABAY"]), 
		c(0, rep(n-2, nrow(stats)-1)))
})

test_that("pshifts with type", {
	# Specify the statistics
	form <- ~ psABBA(with_type = TRUE) + psABBY(with_type = TRUE) + 
		psABXA(with_type = TRUE) + psABXB(with_type = TRUE) + 
		psABXY(with_type = TRUE) + psABAY(with_type = TRUE)


	# Compute the statistics
	out <- remstats(form, edgelist = history, with_type = TRUE)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats %in% c(0,1)))
	expect_equal(rowSums(stats[,,"psABBA"]), c(0, rep(1, nrow(stats)-1)))
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"psABBY"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXA"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXB"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXY"]), 
		c(0, rep((n-2)*(n-3), nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABAY"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(ncol(stats), n*(n-1)*length(unique(history$setting)))
})