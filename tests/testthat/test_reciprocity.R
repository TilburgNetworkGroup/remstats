context("reciprocity output")

require(remstats)

test_that("dimensions reciprocity output", {
    # Test for directed relational events 
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    stat <- reciprocity(el, rs)

    expect_output(str(stat), "num[1:nrow(evls), 1:nrow(rs)]")
})

test_that("content reciprocity output", {
    # Test for directed relational events 
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    stat <- reciprocity(el, rs)

    # Do the rowsums run from 0 to M-1?
    expect_equal(rowSums(stat), seq(0, nrow(el)-1))
    # Are the final counts all in the inertia statistic?
    evls <- prepEvls(el, rs, FALSE)
    statInertia <- inertia(evls, rs, rep(1, nrow(el)))
    expect_true(all(stat[nrow(el),] %in% statInertia[nrow(evls),]))
    # Is the statistic not equal to the inertia statistic?
    expect_true(!all(stat[nrow(el),] == statInertia[nrow(evls),]))
    # Check for a random event
    event <- el[sample(1:nrow(el), 1),]
    sender <- event[2]
    receiver <- event[3]
    count <- length(which((el[1:(nrow(el)-1),2] == sender) & (el[1:(nrow(el)-1),3] == receiver)))
    reciprocal <- which(rs[,1] == receiver & rs[,2] == sender)
    expect_true(count==stat[nrow(evls), reciprocal])
})