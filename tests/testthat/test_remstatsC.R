context("remStatsC")

require(remstats)

test_that("remStatsC output for directed dyadic relational events", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- sort(unique(c(rs[,1], rs[,2])))

    evls <- prepEvls(el, rs)

    effects <- c(0, 1:8, 13:16, 24)

    stats <- remStatsC(effects, el, rs, evls, ac, rep(1, nrow(evls))) 

    # Dimensions
    expect_output(str(stats), 
    "num[1:nrow(evls), 1:nrow(rs), 1:length(effects)]")

    # Output
    expect_true(all(stats[,,1]==1))
    expect_equal(stats[,,2], inertia(evls, rs, rep(1, nrow(evls))))
    expect_equal(stats[,,3], reciprocity(el, rs))
    expect_equal(stats[,,4], degree(el, rs, 1))
    expect_equal(stats[,,5], degree(el, rs, 2))
    expect_equal(stats[,,6], degree(el, rs, 3))
    expect_equal(stats[,,7], degree(el, rs, 4))
    expect_equal(stats[,,8], degree(el, rs, 5))
    expect_equal(stats[,,9], degree(el, rs, 6))
    expect_equal(stats[,,10], triad(ac, el, rs, 1))
    expect_equal(stats[,,11], triad(ac, el, rs, 2))
    expect_equal(stats[,,12], triad(ac, el, rs, 3))
    expect_equal(stats[,,13], triad(ac, el, rs, 4))
    expect_equal(stats[,,14], stats[,,2])
})

test_that("remStatsC output for undirected dyadic relational events", {
    # Test for undirected relational events
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- sort(unique(c(rs[,1], rs[,2])))

    evls <- prepEvls(el, rs)

    effects <- c(0, 1, 17, 24, 25)

    stats <- remStatsC(effects, el, rs, evls, ac, rep(1, nrow(evls))) 

    # Dimensions
    expect_output(str(stats), 
    "num[1:nrow(evls), 1:nrow(rs), 1:length(effects)]")

    # Output
    expect_true(all(stats[,,1]==1))
    expect_equal(stats[,,2], inertia(evls, rs, rep(1, nrow(evls))))
    expect_equal(stats[,,3], triadU(ac, el, rs, FALSE))
    expect_equal(stats[,,4], stats[,,2])
    expect_equal(stats[,,5], triadU(ac, el, rs, TRUE))
})