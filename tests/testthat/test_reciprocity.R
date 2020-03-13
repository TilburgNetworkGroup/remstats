context("reciprocity")

library(remstats)

# Prepare for directed relational events
data(edgelistD)

out <- prepER(edgelistD)
el <- out$edgelist
rs <- out$riskset
ac <- out$actors

# Prepare for directed relational events with types
data(edgelistDT)

out2 <- prepER(edgelistDT, type = TRUE)
el2 <- out2$edgelist
rs2 <- out2$riskset

# Weights 
equal_weights <- rep(1, nrow(el))
weights <- rnorm(nrow(el), 0, 1)

# Statistics
statA <- reciprocity(edgelist = el, riskset = rs, weights = equal_weights, 
    standardize = FALSE)
statB <- reciprocity(edgelist = el, riskset = rs, weights = weights, 
    standardize = FALSE)
statC <- reciprocity(edgelist = el2, riskset = rs2, weights = equal_weights, 
    standardize = FALSE)
statD <- reciprocity(edgelist = el2, riskset = rs2, weights = weights, 
    standardize = FALSE)

statA_std <- reciprocity(edgelist = el, riskset = rs, weights = equal_weights, 
    standardize = TRUE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statC), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statD), "num[1:nrow(el2), 1:nrow(rs2)]")

    expect_output(str(statA_std), "num[1:nrow(el), 1:nrow(rs)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(statC[,1:650], statA) 
    expect_equal(statD[,1:650], statB) 

    expect_equal(statC[,1:650], statC[,651:1300])
    expect_equal(statC[,1:650], statC[,1301:1950])

    expect_equal(statD[,1:650], statD[,651:1300])
    expect_equal(statD[,1:650], statD[,1301:1950])
})

test_that("the rowsums for the raw reciprocity counts sum from 0 to the total 
    number of events (M) - 1", {

    expect_equal(rowSums(statA), seq(0, nrow(el)-1))
    expect_equal(rowSums(statC), seq(0, nrow(el2)-1)*3) # with types
})

test_that("the rowsums for the weighted reciprocity count is equal to the cumulative sum of the weights", {

    expect_equal(rowSums(statB), c(0, cumsum(weights[-nrow(el)])))
    expect_equal(rowSums(statD), c(0, cumsum(weights[-nrow(el2)]))*3)
})

test_that("compare the raw reciprocity counts with the raw inertia counts", {
    stat_inertia <- inertia(edgelist = el, riskset = rs, weights = 
        equal_weights, standardize = FALSE)
    
    expect_true(all(statA %in% stat_inertia))
    expect_true(!all(statA == stat_inertia))
})

test_that("standardization", {
    compare <- rbind(statA[1,], 
        t(apply(statA[-1,], 1, function(x) (x-mean(x))/sd(x))))

    expect_equal(compare, statA_std)
})