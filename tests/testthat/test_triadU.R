context("triadU output")

require(remstats)

test_that("dimensions triadU output", {
    data(edgelistU)

    out <- prepER(edgelistD, riskset = NULL, directed = FALSE, type = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- unique(c(rs[,1], rs[,2]))

    stat <- triadU(actors = ac, edgelist = el, riskset = rs)
    expect_output(str(stat), "num[1:nrow(el), 1:nrow(rs)]")
}) 

test_that("content triadU output", {
    data(edgelistU)

    out <- prepER(edgelistD, riskset = NULL, directed = FALSE, type = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- unique(c(rs[,1], rs[,2]))

    stat <- triadU(actors = ac, edgelist = el, riskset = rs)

    # The difference between rowsums should not be negative
    expect_true(all(diff(rowSums(stat))>=0))

    # The statistic should be equal to the sum of the four directed triad 
    # effects
    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4)

    expect_equal(stat, stat1+stat2+stat3+stat4)
})