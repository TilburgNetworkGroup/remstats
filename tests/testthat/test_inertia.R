context("inertia output")

require(remstats)

test_that("dimensions inertia output", {
    # Test for directed relational events 
    data(edgelistD)

    out <- prepER(edgelistD, NULL, TRUE, FALSE)
    el <- out$edgelist
    rs <- out$riskset

    evls <- prepEvls(el, rs, FALSE)

    stat <- inertia(evls, rs)

    expect_output(str(stat), "num[1:nrow(evls), 1:nrow(rs)]")

    # Test for undirected relational events 
    data(edgelistU)

    out2 <- prepER(edgelistU, NULL, FALSE, FALSE)
    el2 <- out2$edgelist
    rs2 <- out2$riskset

    evls2 <- prepEvls(el2, rs2, FALSE)

    stat2 <- inertia(evls2, rs2)

    expect_output(str(stat2), "num[1:nrow(evls2), 1:nrow(rs2)]")
})

test_that("content inertia output", {
    # Test for directed relational events 
    data(edgelistD)

    out <- prepER(edgelistD, NULL, TRUE, FALSE)
    el <- out$edgelist
    rs <- out$riskset

    evls <- prepEvls(el, rs, FALSE)

    stat <- inertia(evls, rs)

    # Do the rowsums run from 0 to M-1?
    expect_equal(rowSums(stat), seq(0, nrow(evls)-1))
    # Is the final row equal to the adjacency table?
    expect_true(all(table(factor(evls[1:(nrow(evls)-1),1], levels = 1:nrow(rs))) == stat[nrow(evls),]))

    # Test for undirected relational events 
    data(edgelistU)

    out2 <- prepER(edgelistU, NULL, FALSE, FALSE)
    el2 <- out2$edgelist
    rs2 <- out2$riskset

    evls2 <- prepEvls(el2, rs2, FALSE)

    stat2 <- inertia(evls2, rs2)

    # Do the rowsums run from 0 to M-1?
    expect_equal(rowSums(stat2), seq(0, nrow(evls2)-1))
    # Is the final row equal to the adjacency table?
    expect_true(all(table(factor(evls2[1:(nrow(evls2)-1),1], levels = 1:nrow(rs2))) == stat2[nrow(evls2),]))
})