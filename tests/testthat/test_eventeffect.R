context("event_effect in remStatsC")

require(remstats)

test_that("event_effect in remStatsC", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    evls <- prepEvls(el, rs)

    effects <- 9
    event_effect <- as.matrix(sample(c(0, 1), nrow(el), replace = TRUE))
    covariates <- rep(list(matrix(0, 1, 1)), 8)

    stats <- remStatsC(effects = effects, edgelist = el, riskset = rs, evls = evls, actors = ac[,1], covariates = covariates, event_effect = event_effect, weights = rep(1, nrow(evls)), equal_val = 0, int_positions = matrix(0, 1, 1))

    expect_equal(stats[,1,1], stats[,2,1])
    expect_equal(as.matrix(stats[,1,1]), event_effect)
})