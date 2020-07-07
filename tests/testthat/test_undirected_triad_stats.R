context("undirected triad stats")

library(remstats)

n <- length(unique(info$id))

test_that("scaling is counts", {
	# Compute the stats
	out <- remstats(~ sp() + spUnique(), edgelist = history, directed = FALSE)
	edgelist <- out$edgelist
	riskset <- out$riskset
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats[,,"sp"] >= stats[,,"spUnique"]))
	expect_true(max(stats[,,"spUnique"]) <= n)
	
	statrow1 <- apply(riskset, 1, function(x) {
		min1 <- edgelist[-nrow(edgelist),]
		
		sub1 <- min1[min1[,2] == x[1] | min1[,3] == x[1],]
		sub2 <- min1[min1[,2] == x[2] | min1[,3] == x[2],]
		
		p1 <- c(sub1[,2], sub1[,3])
		p1 <- p1[p1!=x[1] & p1!=x[2]]
		p2 <- c(sub2[,2], sub2[,3])
		p2 <- p2[p2!=x[1] & p2!=x[2]]
		
		sum(unlist(sapply(unique(p1), function(y) {
			if(y %in% p2) {
				min(c(sum(p1 == y), sum(p2 == y)))
			}
		})))
	})
	
	expect_equal(stats[nrow(stats),,"sp"], statrow1)
	
	statrow2 <- apply(riskset, 1, function(x) {
		min1 <- edgelist[-nrow(edgelist),]
		
		sub1 <- min1[min1[,2] == x[1] | min1[,3] == x[1],]
		sub2 <- min1[min1[,2] == x[2] | min1[,3] == x[2],]
		
		p1 <- unique(c(sub1[,2], sub1[,3]))
		p1 <- p1[p1!=x[1] & p1!=x[2]]
		p2 <- unique(c(sub2[,2], sub2[,3]))
		p2 <- p2[p2!=x[1] & p2!=x[2]]
		
		sum(p1 %in% p2)
	})
	
	expect_equal(stats[nrow(stats),,"spUnique"], statrow2)
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