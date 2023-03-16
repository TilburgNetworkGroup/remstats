library(remify)
library(remstats)

test_that("output tomstats", {
	out <- tomstats(~ send("extraversion"):inertia(),
		reh = history[,c(1:3)], attributes = info)
	
	stats <- out$statistics
	reh <- out$reh
	riskset <- out$riskset
	adjmat <- out$adjmat
	#actors <- out$actors
	#types <- out$types
	
	expect_output(str(out), "List of 6")
	expect_equal(dim(stats), c(nrow(reh), nrow(riskset), 4))
	expect_equal(dim(adjmat), c(nrow(reh), nrow(riskset)))
})

test_that("output aomstats", {
	out <- aomstats(sender_effects = ~ send("extraversion"), 
		receiver_effects = ~ inertia(), reh = history[,c(1:3)], attributes = info)
	
	rstats <- out$statistics$sender_stats
	cstats <- out$statistics$receiver_stats
	reh <- out$reh
	riskset <- out$riskset
	#actors <- out$actors
	
	expect_output(str(out), "List of 4")
	expect_equal(dim(rstats), c(nrow(reh), length(unique(info$id)), 2))
	expect_equal(dim(cstats), c(nrow(reh), length(unique(info$id)), 1))
})

test_that("output remstats", {
	
	# Tie-oriented model
	out <- remstats(tie_effects = ~ send("extraversion"):inertia(),
		reh = history[,c(1:3)], attributes = info)
	
	stats <- out$statistics
	reh <- out$reh
	riskset <- out$riskset
	adjmat <- out$adjmat
	#actors <- out$actors
	#types <- out$types
	
	expect_output(str(out), "List of 6")
	expect_equal(dim(stats), c(nrow(reh), nrow(riskset), 4))
	expect_equal(dim(adjmat), c(nrow(reh), nrow(riskset)))
	
	# Actor-oriented model
	out <- remstats(sender_effects = ~ send("extraversion"), 
		receiver_effects = ~ inertia(), reh = history[,c(1:3)], attributes = info)
	
	rstats <- out$statistics$sender_stats
	cstats <- out$statistics$receiver_stats
	reh <- out$reh
	riskset <- out$riskset
	#actors <- out$actors
	
	expect_output(str(out), "List of 4")
	expect_equal(dim(rstats), c(nrow(reh), length(unique(info$id)), 2))
	expect_equal(dim(cstats), c(nrow(reh), length(unique(info$id)), 1))
})
