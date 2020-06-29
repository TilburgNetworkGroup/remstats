context("remstats")

library(remstats)

out <- remstats(~ baseline() + send("extraversion", info):inertia(),
    edgelist = history)

stats <- out$statistics
edgelist <- out$edgelist
riskset <- out$riskset
evls <- out$evls

test_that("output", {
    expect_output(str(out), "List of 4")
    expect_equal(dim(stats), c(nrow(edgelist), nrow(riskset), length(out)))
    expect_output(str(evls), "num[1:nrow(edgelist), 2]")
})