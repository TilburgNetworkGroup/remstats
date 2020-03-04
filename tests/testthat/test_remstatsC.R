context("remStatsC")

require(remstats)

test_that("remStatsC output for directed dyadic relational events", {
    # Test for directed relational events
    data(edgelistD)
    data(covar)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    evls <- prepEvls(el, rs)

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    effects <- c(0, 1, 1:5, 7:12, 17:20)

    stats <- remStatsC(effects, el, rs, evls, ac[,1], covar, covar[,c(1:3)], rep(1, nrow(evls))) 

    # Dimensions
    expect_output(str(stats), 
    "num[1:nrow(evls), 1:nrow(rs), 1:length(effects)]")

    # Output
    expect_true(all(stats[,,1]==1))
    expect_equal(stats[,,2], actorStat(covar[,c(1:3)], 1, el, rs))
    expect_equal(stats[,,3], actorStat(covar[,c(1:2, 4)], 1, el, rs))
    expect_equal(stats[,,4], actorStat(covar[,c(1:3)], 2, el, rs))
    expect_equal(stats[,,5], inertia(evls, rs, rep(1, nrow(evls))))
    expect_equal(stats[,,6], stats[,,5])
    expect_equal(stats[,,7], reciprocity(el, rs))
    expect_equal(stats[,,8], degree(el, rs, 1))
    expect_equal(stats[,,9], degree(el, rs, 2))
    expect_equal(stats[,,10], degree(el, rs, 3))
    expect_equal(stats[,,11], degree(el, rs, 4))
    expect_equal(stats[,,12], degree(el, rs, 5))
    expect_equal(stats[,,13], degree(el, rs, 6))
    expect_equal(stats[,,14], triad(ac[,1], el, rs, 1))
    expect_equal(stats[,,15], triad(ac[,1], el, rs, 2))
    expect_equal(stats[,,16], triad(ac[,1], el, rs, 3))
    expect_equal(stats[,,17], triad(ac[,1], el, rs, 4))

})

test_that("remStatsC output for undirected dyadic relational events", {
    # Test for undirected relational events
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    evls <- prepEvls(el, rs)

    effects <- c(0, 3, 4, 21, 22)

    stats <- remStatsC(effects, el, rs, evls, ac, matrix(0, 1, 1), 
        matrix(0, 1, 1), rep(1, nrow(evls))) 

    # Dimensions
    expect_output(str(stats), 
    "num[1:nrow(evls), 1:nrow(rs), 1:length(effects)]")

    # Output
    expect_true(all(stats[,,1]==1))
    expect_equal(stats[,,2], inertia(evls, rs, rep(1, nrow(evls))))
    expect_equal(stats[,,3], stats[,,2])
    expect_equal(stats[,,4], triadU(ac, el, rs, FALSE))
    expect_equal(stats[,,5], triadU(ac, el, rs, TRUE))
})