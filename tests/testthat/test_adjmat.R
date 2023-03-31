library(remstats)

test_that("adjacency matrix", {
	# Full memory
	history$weight <- 1
	reh <- remify::remify(history, model = "tie")
	out <- tomstats(effects = ~ inertia(), reh = reh)
	adjmat <- out$adjmat
	expect_equal(rowSums(adjmat), 0:(nrow(history)-1))
	
	# Start and stop
	out <- tomstats(effects = ~ inertia(), reh = reh, 
		start = 5, stop = 10)
	adjmat <- out$adjmat
	expect_equal(rowSums(adjmat), 4:9)
	
	# Windowed memory
	out <- tomstats(effects = ~ inertia(), reh = reh, 
		memory = "window", memory_value = 400)
	adjmat <- out$adjmat
	expect_equal(rowSums(adjmat),
		apply(history, 1, function(x) {
			length(which(history$time < as.numeric(x[1]) & history$time > as.numeric(x[1])-400))
		}))
	
	# Exponential decay memory
	out <- tomstats(effects = ~ inertia(), reh = reh, 
		memory = "decay", memory_value = 400)
	adjmat <- out$adjmat
	expect_equal(rowSums(adjmat),
		apply(history, 1, function(x) {
			past <- history[history$time < as.numeric(x[1]),]
			sum(past$weight * exp(- (as.numeric(x[1]) - as.numeric(past$time)) * (log(2)/400)) * log(2)/400)
		}))
	
	# Event weights
	history$weight <- rnorm(nrow(history))
	reh <- remify::remify(history, model = "tie")
	out <- tomstats(effects = ~ inertia(), reh = reh, start = 2)
	adjmat <- out$adjmat
	expect_equal(rowSums(adjmat), cumsum(history$weight[-nrow(history)]))
})