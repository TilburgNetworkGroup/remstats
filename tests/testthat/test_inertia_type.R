context("inertia_type")

library(remstats)

# Prepare for directed relational events with types
data(edgelistDT)

out <- prepER(edgelistDT, type = TRUE)
el <- out$edgelist
rs <- out$riskset

# Prepare for undirected relational events with types
data(edgelistUT)

out2 <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el2 <- out2$edgelist
rs2 <- out2$riskset

# Weights
equal_weights <- rep(1, nrow(el))

# Statistics
statA <- inertia_type(edgelist = el, riskset = rs, weights = equal_weights, 
    standardize = FALSE)
statB <- inertia_type(edgelist = el2, riskset = rs2, weights = equal_weights, 
    standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el2), 1:nrow(rs2)]")
})

test_that("the rowsums for the raw inertia counts sum from 0 to the total 
    number of events (M) - 1", {

    expect_equal(rowSums(statA), seq(0, nrow(el)-1))
    expect_equal(rowSums(statB), seq(0, nrow(el2)-1)) 
})

test_that("the statistics are different for dyads of different type", {
    expect_true(!all(statA[,1:650]==statA[,651:1300]))
    expect_true(!all(statA[,1:650]==statA[,1301:1950]))

    expect_true(!all(statB[,1:325]==statB[,326:650]))
    expect_true(!all(statB[,1:325]==statB[,651:975]))
})

test_that("the final counts are correct", {
    sel <- el[1:(nrow(el)-1),]
    expect_equal(apply(rs, 1, function(x) {
        sum(sel[,2]==x[1]&sel[,3]==x[2]&sel[,4]==x[3])
    }), statA[nrow(statA),])

    sel2 <- el2[1:(nrow(el2)-1),]
    expect_equal(apply(rs2, 1, function(x) {
        sum(sel2[,2]==x[1]&sel2[,3]==x[2]&sel2[,4]==x[3])
    }), statB[nrow(statB),])
})