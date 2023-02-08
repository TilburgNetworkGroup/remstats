library(remify)
library(remstats)

test_that("pshifts", {
	
	# Specify the statistics
	form <- ~ psABBA() + psABBY() + psABXA() + psABXB() + psABXY() + psABAY() +
		psABAB() 
	
	# Compute the statistics
	out <- tomstats(form, edgelist = history[,c(1:3)])
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats %in% c(0,1)))
	expect_equal(rowSums(stats[,,"psABBA"]), c(0, rep(1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABAB"]), c(0, rep(1, nrow(stats)-1)))
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"psABBY"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXA"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXB"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXY"]), 
		c(0, rep((n-2)*(n-3), nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABAY"]), 
		c(0, rep(n-2, nrow(stats)-1)))
	
	# Make sure type is a column in the history
	names(history)[4] <- "type"
	
	# Specify the statistics
	form <- ~ psABBA(consider_type = TRUE) + psABBY(consider_type = TRUE) + 
		psABXA(consider_type = TRUE) + psABXB(consider_type = TRUE) + 
		psABXY(consider_type = TRUE) + psABAY(consider_type = TRUE) + 
		psABAB(consider_type = TRUE)
	
	# Compute the statistics
	out <- tomstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats %in% c(0,1)))
	expect_equal(rowSums(stats[,,"psABBA.type"]), c(0, rep(1, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABAB.type"]), c(0, rep(1, nrow(stats)-1)))
	n <- length(unique(info$id))
	expect_equal(rowSums(stats[,,"psABBY.type"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXA.type"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXB.type"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABXY.type"]), 
		c(0, rep((n-2)*(n-3), nrow(stats)-1)))
	expect_equal(rowSums(stats[,,"psABAY.type"]), c(0, rep(n-2, nrow(stats)-1)))
	expect_equal(ncol(stats), n*(n-1)*length(unique(history$type)))
})