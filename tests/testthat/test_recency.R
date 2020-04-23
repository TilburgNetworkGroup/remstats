context("recency")

library(remstats)

data("edgelistD")

effects <- c("rrank_send", "rrank_receive")
out <- remstats(edgelistD, effects)
obs <- out$edgelist
act <- out$actors$id
rs <- out$riskset

#Preparing the statistics
recencyOut1 <- recency(obs, act, 1)
recencyIn1 <- recency(obs, act, 2)

#Test the dimensions of the output
test_that("Test that output dimensions are the same as expected", {
    expect_output(str(recencyOut1), "num[1:nrow(obs), 1:nrow(rs)]")
    expect_output(str(recencyIn1), "num[1:nrow(obs), 1:nrow(rs)]")
})

#Test the ranks
test_that("Test that ranks are smaller than the maximum number of actors", {
    test1 <- apply(recencyOut1, 1, function(x) 1/x[x != 0])
    test2 <- apply(recencyIn1, 1, function(x) 1/x[x != 0])
    
    expect_equal(sum(sapply(test1, function(x) sum(x >= max(act)))), 0)
    expect_equal(sum(sapply(test2, function(x) sum(x >= max(act)))), 0)
})

#Test the value of recency statistic, it should be between 0 and 1
test_that("Test that no values are bigger than 1", {
    expect_equal(sum(apply(recencyOut1, 1, function(x) x > 1 | x < 0)), 0)
    expect_equal(sum(apply(recencyIn1, 1, function(x) x > 1 | x < 0)), 0)
})

#Test if the statistic is in the right place, e.g. if the dyads in In are the reciprocal of the dyads in Out
test_that("Test that all values are in the right place", {
    In <- sapply(apply(recencyIn1[-1,], 1, function(x) which(x > 0)), function(x) cbind(rs[x,1], rs[x,2])[order(rs[x,1]),])
    Out <- sapply(apply(recencyOut1[-1,], 1, function(x) which(x > 0)), function(x) cbind(rs[x,2], rs[x,1]))
    
    expect_equal(length(setdiff(In, Out)), 0)
})
    
    