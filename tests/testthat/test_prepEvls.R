context("prepEvls output")

require(remstats)

test_that("prepEvls output for different types of relational events in the edgelist input", {
    # Test for relational events that don't consider event type
    data(edgelistD) 

    out1 <- prepER(edgelistD, riskset = NULL, directed = TRUE, type = FALSE)
    evls1 <- prepEvls(out1$edgelist, out1$riskset, type = FALSE)

    expect_output(str(evls1), "num[1:nrow(edgelistD), 1:2]")

    # Test for relational events that consider event type
    data(edgelistDT) 

    out2 <- prepER(edgelistDT, riskset = NULL, directed = TRUE, type = TRUE)
    evls2 <- prepEvls(out2$edgelist, out2$riskset, type = TRUE)

    expect_output(str(evls2), "num[1:nrow(edgelistD), 1:2]")
    }
)

test_that("prepEvls time equal to edgelist time", {
    data(edgelistD) 

    out <- prepER(edgelistD, riskset = NULL, directed = TRUE, type = FALSE)
    evls <- prepEvls(out$edgelist, out$riskset, type = FALSE)

    expect_identical(evls[,2], out$edgelist[,1])
})

test_that("prepEvls event IDs in riskset", {
    data(edgelistD) 

    out <- prepER(edgelistD, riskset = NULL, directed = TRUE, type = FALSE)
    evls <- prepEvls(out$edgelist, out$riskset, type = FALSE)

    expect_true(all(evls[,1] >= 1 & evls[,1] <= nrow(out$riskset)))
})