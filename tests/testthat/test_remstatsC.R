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

    effects <- c(0, 1, 1, 2, 3, 4, 4, 5, 6, 7, 8, 9:11, 13:18, 23:26)
    covariates <- list(sender_effect = covar, 
        receiver_effect = covar[,c(1:3)],
		same = covar[,c(1:2, 4)],
		difference = covar,
		mean = covar[,c(1:3)],
		max = covar[,c(1:3)],
		min = covar[,c(1:3)],
		both_equal_to = covar[,c(1:2, 4)])

    stats <- remStatsC(effects, el, rs, evls, ac[,1], covariates, 
									 rep(1, nrow(evls)), 0) 

    # Dimensions
    expect_output(str(stats), 
    "num[1:nrow(evls), 1:nrow(rs), 1:length(effects)]")

    # Output
    # Baseline
    expect_true(all(stats[,,1]==1))
    # x1 sender effect
    expect_equal(stats[,,2], actorStat(covar[,c(1:3)], 1, el, rs))
    # x2 sender effect
    expect_equal(stats[,,3], actorStat(covar[,c(1:2, 4)], 1, el, rs))
    # x1 receiver effect
    expect_equal(stats[,,4], actorStat(covar[,c(1:3)], 2, el, rs))
    # x2 same effect
    expect_equal(stats[,,5], dyadstat(covar[,c(1:2, 4)], 1, el, rs, 0))
    # x1 difference effect
    expect_equal(stats[,,6], dyadstat(covar[,c(1:3)], 2, el, rs, 0))
    # x2 difference effect
    expect_equal(stats[,,7], dyadstat(covar[,c(1:2, 4)], 2, el, rs, 0))
    # x1 mean effect
    expect_equal(stats[,,8], dyadstat(covar[,c(1:3)], 3, el, rs, 0))
    # x1 min effect
    expect_equal(stats[,,9], dyadstat(covar[,c(1:3)], 4, el, rs, 0))
    # x1 max effect
    expect_equal(stats[,,10], dyadstat(covar[,c(1:3)], 5, el, rs, 0))
    # x2 both_equal_to 0 effect
    expect_equal(stats[,,11], dyadstat(covar[,c(1:2, 4)], 6, el, rs, 0))
    # inertia 
    expect_equal(stats[,,12], inertia(evls, rs, rep(1, nrow(evls))))
    # inertia_weighted
    expect_equal(stats[,,13], stats[,,12])
    # reciprocity
    expect_equal(stats[,,14], reciprocity(el, rs))
    # indegree_sender
    expect_equal(stats[,,15], degree(el, rs, 1))
    # indegree_receiver
    expect_equal(stats[,,16], degree(el, rs, 2))
    # outdegree_sender
    expect_equal(stats[,,17], degree(el, rs, 3))
    # outdegree_receiver
    expect_equal(stats[,,18], degree(el, rs, 4))
    # totaldegree_sender
    expect_equal(stats[,,19], degree(el, rs, 5))
    # totaldegree_receiver
    expect_equal(stats[,,20], degree(el, rs, 6))
    # OTP
    expect_equal(stats[,,21], triad(ac[,1], el, rs, 1))
    # ITP
    expect_equal(stats[,,22], triad(ac[,1], el, rs, 2))
    # OSP
    expect_equal(stats[,,23], triad(ac[,1], el, rs, 3))
    # ISP
    expect_equal(stats[,,24], triad(ac[,1], el, rs, 4))
})

test_that("remStatsC output for undirected dyadic relational events", {
    # Test for undirected relational events
    data(edgelistU)
    data(covar)

    out <- prepER(edgelistU, directed = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    evls <- prepEvls(el, rs)

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    effects <- c(0, 3, 4, 4, 5, 6, 7, 8, 9:10, 27:28)

    covariates <- list(sender_effect = matrix(0, 1, 1),
        receiver_effect = matrix(0, 1, 1),
        same = covar[,c(1:2, 4)],
        difference = covar,
        mean = covar[,c(1:3)],
        max = covar[,c(1:3)],
        min = covar[,c(1:3)],
        both_equal_to = covar[,c(1:2, 4)])

    stats <- remStatsC(effects, el, rs, evls, ac[,1], covariates, 
                                        rep(1, nrow(evls)), 0) 

    # Dimensions
    expect_output(str(stats), 
        "num[1:nrow(evls), 1:nrow(rs), 1:length(effects)]")

    # Output
    # Baseline
    expect_true(all(stats[,,1]==1))
    # x2 same effect
    expect_equal(stats[,,2], dyadstat(covar[,c(1:2, 4)], 1, el, rs, 0))
    # x1 difference effect
    expect_equal(stats[,,3], dyadstat(covar[,c(1:3)], 2, el, rs, 0))
    # x2 difference effect
    expect_equal(stats[,,4], dyadstat(covar[,c(1:2, 4)], 2, el, rs, 0))
    # x1 mean effect
    expect_equal(stats[,,5], dyadstat(covar[,c(1:3)], 3, el, rs, 0))
    # x1 min effect
    expect_equal(stats[,,6], dyadstat(covar[,c(1:3)], 4, el, rs, 0))
    # x1 max effect
    expect_equal(stats[,,7], dyadstat(covar[,c(1:3)], 5, el, rs, 0))
    # x2 both_equal_to 0 effect
    expect_equal(stats[,,8], dyadstat(covar[,c(1:2, 4)], 6, el, rs, 0))
    # inertia
    expect_equal(stats[,,9], inertia(evls, rs, rep(1, nrow(evls))))
    # inertia_weighted
    expect_equal(stats[,,10], stats[,,9])
    # shared_partners
    expect_equal(stats[,,11], triadU(ac[,1], el, rs, FALSE))
    # unique_partners
    expect_equal(stats[,,12], triadU(ac[,1], el, rs, TRUE))
})