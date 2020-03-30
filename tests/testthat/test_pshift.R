context("pshifts")

require(remstats)

# Test for directed relational events
data(edgelistD)
out <- prepER(edgelistD)
edgelist <- out$edgelist
riskset <- out$riskset
actors <- out$actors

test_that("p-shift AB-BA for directed relational events", {
    stat <- pshift(edgelist = edgelist, riskset = riskset, type = 1)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelist), 1:nrow(riskset)]")
    # All but the first rowSums are expected to be equal to one
    expect_equal(rowSums(stat), c(0, rep(1, nrow(edgelist)-1)))
})

test_that("p-shift AB-BY for directed relational events", {
    stat <- pshift(edgelist = edgelist, riskset = riskset, type = 2)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelist), 1:nrow(riskset)]")
    # All but the first rowSums are expected to be equal to n-2
    expect_equal(rowSums(stat), c(0, rep(nrow(actors)-2, nrow(edgelist)-1)))
})

test_that("p-shift AB-XA for directed relational events", {
    stat <- pshift(edgelist = edgelist, riskset = riskset, type = 3)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelist), 1:nrow(riskset)]")
    # All but the first rowSums are expected to be equal to n-2
    expect_equal(rowSums(stat), c(0, rep(nrow(actors)-2, nrow(edgelist)-1)))
})

test_that("p-shift AB-XB for directed relational events", {
    stat <- pshift(edgelist = edgelist, riskset = riskset, type = 3)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelist), 1:nrow(riskset)]")
    # All but the first rowSums are expected to be equal to n-2
    expect_equal(rowSums(stat), c(0, rep(nrow(actors)-2, nrow(edgelist)-1)))
})

test_that("p-shift AB-XY for directed relational events", {
    stat <- pshift(edgelist = edgelist, riskset = riskset, type = 5)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelist), 1:nrow(riskset)]")
    # All but the first rowSums are expected to be equal to (n-2)*(n-3)
    expect_equal(rowSums(stat), 
        c(0, rep((nrow(actors)-2)*(nrow(actors)-3), nrow(edgelist)-1)))
})

test_that("p-shift AB-AY for directed relational events", {
    stat <- pshift(edgelist = edgelist, riskset = riskset, type = 6)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelist), 1:nrow(riskset)]")
    # All but the first rowSums are expected to be equal to n-2
    expect_equal(rowSums(stat), c(0, rep(nrow(actors)-2, nrow(edgelist)-1)))
})

# Test for directed relational events with types
data(edgelistDT)
out <- prepER(edgelistDT, type = TRUE)
edgelistT <- out$edgelist
risksetT <- out$riskset
actorsT <- out$actors
types <- out$types

test_that("p-shift AB-BA for directed relational events with types", {
    stat <- pshift(edgelist = edgelistT, riskset = risksetT, type = 1)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelistT), 1:nrow(risksetT)]")
    # All but the first rowSums are expected to be equal to one times the 
    # number of event types
    expect_equal(rowSums(stat), c(0, rep(1*nrow(types), nrow(edgelistT)-1)))
})

test_that("p-shift AB-BY for directed relational events with types", {
    stat <- pshift(edgelist = edgelistT, riskset = risksetT, type = 2)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelistT), 1:nrow(riskset)]")

    # All but the first rowSums are expected to be equal to n-2 times the 
    # number of event types
    expect_equal(rowSums(stat), 
        c(0, rep((nrow(actorsT)-2)*nrow(types), nrow(edgelistT)-1)))
})

test_that("p-shift AB-XA for directed relational events with types", {
    stat <- pshift(edgelist = edgelistT, riskset = risksetT, type = 3)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelistT), 1:nrow(risksetT)]")

    # All but the first rowSums are expected to be equal to n-2 times the 
    # number of event types
    expect_equal(rowSums(stat), 
        c(0, rep((nrow(actorsT)-2)*nrow(types), nrow(edgelistT)-1)))
})

test_that("p-shift AB-XB for directed relational events with types", {
    stat <- pshift(edgelist = edgelistT, riskset = risksetT, type = 3)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelistT), 1:nrow(risksetT)]")

    # All but the first rowSums are expected to be equal to n-2 times the 
    # number of event types
    expect_equal(rowSums(stat), 
        c(0, rep((nrow(actorsT)-2)*nrow(types), nrow(edgelistT)-1)))
})

test_that("p-shift AB-XY for directed relational events with types", {
    stat <- pshift(edgelist = edgelistT, riskset = risksetT, type = 5)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelistT), 1:nrow(risksetT)]")

    # All but the first rowSums are expected to be equal to (n-2)*(n-3) times 
    # the number of event types
    expect_equal(rowSums(stat), 
        c(0, rep(((nrow(actorsT)-2)*(nrow(actorsT)-3))*nrow(types), 
            nrow(edgelistT)-1)))
})

test_that("p-shift AB-AY for directed relational events with types", {
    stat <- pshift(edgelist = edgelistT, riskset = risksetT, type = 6)

    # Output
    expect_output(str(stat), "num[1:nrow(edgelistT), 1:nrow(risksetT)]")

    # All but the first rowSums are expected to be equal to n-2 times the 
    # number of event types
    expect_equal(rowSums(stat), 
        c(0, rep((nrow(actorsT)-2)*nrow(types), nrow(edgelistT)-1)))
})