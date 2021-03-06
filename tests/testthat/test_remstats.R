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
	
	expect_output(str(out), "List of 5")
	expect_equal(dim(stats), c(nrow(edgelist), nrow(riskset), 4))
	expect_output(str(evls), "num[1:nrow(edgelist), 2]")
	expect_equal(dim(adjmat), c(nrow(edgelist), nrow(riskset)))
})

test_that("output aomstats", {
	out <- aomstats(rateEffects = ~ send("extraversion"), 
		choiceEffects = ~ inertia(), edgelist = history[,c(1:3)], attributes = info)
	
	rstats <- out$statistics$rate
	cstats <- out$statistics$choice
	edgelist <- out$edgelist
	riskset <- out$riskset
	adjmat <- out$adjmat
	
	expect_output(str(out), "List of 4")
	expect_equal(dim(rstats), c(nrow(edgelist), length(unique(info$id)), 2))
	expect_equal(dim(cstats), c(nrow(edgelist), length(unique(info$id)), 1))
	expect_equal(dim(adjmat), c(nrow(edgelist), nrow(riskset)))
})