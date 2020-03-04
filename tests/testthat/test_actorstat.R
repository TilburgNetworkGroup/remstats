context("actorStat output")

require(remstats)

test_that("dimensions actorStat output", {
    # Test for directed relational events
    data(edgelistD)
    data(covar)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    stat1 <- actorStat(values = covar[,c(1:3)], type = 1, el, rs)
    stat2 <- actorStat(values = covar[,c(1:2, 4)], type = 2, el, rs)
    
    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]") 
})

test_that("content actorStat output", {
    # Test for directed relational events
    data(edgelistD)
    data(covar)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    stat1 <- actorStat(values = covar[,c(1:3)], type = 1, el, rs)
    stat2 <- actorStat(values = covar[,c(1:2, 4)], type = 2, el, rs)

    # Test whether all starting values are accounted for
    expect_true(all(stat1[1,] %in% covar[covar[,2]==0,3]))
    expect_true(all(covar[covar[,2]==0,3] %in% stat1[1,]))
    expect_true(all(stat2[1,] %in% covar[covar[,2]==0,4]))
    expect_true(all(covar[covar[,2]==0,4] %in% stat2[1,]))   

    # Test for two dyads their values
    expect_true(all(stat1[,1] %in% covar[covar[,1]==rs[1,1], 3]))
    expect_true(all(covar[covar[,1]==rs[1,1], 3] %in% stat1[,1]))
    expect_true(all(stat1[,15] %in% covar[covar[,1]==rs[15,1], 3]))
    expect_true(all(covar[covar[,1]==rs[15,1], 3] %in% stat1[,15]))

    expect_true(all(stat2[,1] %in% covar[covar[,1]==rs[1,2], 4]))
    expect_true(all(covar[covar[,1]==rs[1,2], 4] %in% stat2[,1]))
    expect_true(all(stat2[,15] %in% covar[covar[,1]==rs[15,2], 4]))
    expect_true(all(covar[covar[,1]==rs[15,2], 4] %in% stat2[,15]))
})