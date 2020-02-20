context("triad output")

require(remstats)

test_that("dimensions triad output", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, riskset = NULL, directed = TRUE, type = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- unique(c(rs[,1], rs[,2]))

    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4)

    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat3), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat4), "num[1:nrow(el), 1:nrow(rs)]")
})

test_that("test content triad output with edgelistD", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, riskset = NULL, directed = TRUE, type = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- unique(c(rs[,1], rs[,2]))

    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4)

    # The difference between rowsums should not be negative
    expect_true(all(diff(rowSums(stat1))>=0))
    expect_true(all(diff(rowSums(stat2))>=0))
    expect_true(all(diff(rowSums(stat3))>=0))
    expect_true(all(diff(rowSums(stat4))>=0))
    # An outgoing two-path for (i,j) is an incoming twopath for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
        stat1[nrow(stat1), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat2[nrow(stat2), which(rs[,1] == x[2] & rs[,2] == x[1])]
        })))
    # An inbound shared partner for (i,j) is an inbound shared partner for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
	    stat3[nrow(stat3), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat3[nrow(stat3), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
    # An outbound shared partner for (i,j) is an outbound shared partner for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
	    stat4[nrow(stat4), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat4[nrow(stat4), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
})
