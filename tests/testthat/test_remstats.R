library(remify)
library(remstats)

test_that("output tomstats", {
	out <- tomstats(~ send("extraversion"):inertia(),
		edgelist = history[,c(1:3)], attributes = info)
	
	stats <- out$statistics
	edgelist <- out$edgelist
	riskset <- out$riskset
	evls <- out$evls
	adjmat <- out$adjmat
	#actors <- out$actors
	#types <- out$types
	
	expect_output(str(out), "List of 7")
	expect_equal(dim(stats), c(nrow(edgelist), nrow(riskset), 4))
	expect_output(str(evls), "num[1:nrow(edgelist), 2]")
	expect_equal(dim(adjmat), c(nrow(edgelist), nrow(riskset)))
})

test_that("output aomstats", {
	out <- aomstats(sender_effects = ~ send("extraversion"), 
		receiver_effects = ~ inertia(), edgelist = history[,c(1:3)], attributes = info)
	
	rstats <- out$statistics$sender_stats
	cstats <- out$statistics$receiver_stats
	edgelist <- out$edgelist
	riskset <- out$riskset
	adjmat <- out$adjmat
	#actors <- out$actors
	
	expect_output(str(out), "List of 5")
	expect_equal(dim(rstats), c(nrow(edgelist), length(unique(info$id)), 2))
	expect_equal(dim(cstats), c(nrow(edgelist), length(unique(info$id)), 1))
	expect_equal(dim(adjmat), c(nrow(edgelist), nrow(riskset)))
})

test_that("output remstats", {
	
	# Tie-oriented model
	out <- remstats(tie_effects = ~ send("extraversion"):inertia(),
		edgelist = history[,c(1:3)], attributes = info)
	
	stats <- out$statistics
	edgelist <- out$edgelist
	riskset <- out$riskset
	evls <- out$evls
	adjmat <- out$adjmat
	#actors <- out$actors
	#types <- out$types
	
	expect_output(str(out), "List of 7")
	expect_equal(dim(stats), c(nrow(edgelist), nrow(riskset), 4))
	expect_output(str(evls), "num[1:nrow(edgelist), 2]")
	expect_equal(dim(adjmat), c(nrow(edgelist), nrow(riskset)))
	
	# Actor-oriented model
	out <- remstats(sender_effects = ~ send("extraversion"), 
		receiver_effects = ~ inertia(), edgelist = history[,c(1:3)], attributes = info)
	
	rstats <- out$statistics$sender_stats
	cstats <- out$statistics$receiver_stats
	edgelist <- out$edgelist
	riskset <- out$riskset
	adjmat <- out$adjmat
	#actors <- out$actors
	
	expect_output(str(out), "List of 5")
	expect_equal(dim(rstats), c(nrow(edgelist), length(unique(info$id)), 2))
	expect_equal(dim(cstats), c(nrow(edgelist), length(unique(info$id)), 1))
	expect_equal(dim(adjmat), c(nrow(edgelist), nrow(riskset)))
})
