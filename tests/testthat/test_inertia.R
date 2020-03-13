context("inertia")

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

# Prepare for undirected relational events
data(edgelistU)

out3 <- prepER(edgelistU, directed = FALSE)
el3 <- out3$edgelist
rs3 <- out3$riskset

# Prepare for undirected relational events with types
data(edgelistUT)

out4 <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el4 <- out4$edgelist
rs4 <- out4$riskset

# Weights 
equal_weights <- rep(1, nrow(el))
weights <- rnorm(nrow(el), 0, 1)

# Statistics
statA <- inertia(edgelist = el, riskset = rs, weights = equal_weights, 
    standardize = FALSE)
statB <- inertia(edgelist = el2, riskset = rs2, weights = equal_weights, 
    standardize = FALSE)
statC <- inertia(edgelist = el3, riskset = rs3, weights = equal_weights, 
    standardize = FALSE)
statD <- inertia(edgelist = el4, riskset = rs4, weights = equal_weights, 
    standardize = FALSE)

statA_std <- inertia(edgelist = el, riskset = rs, weights = equal_weights, 
    standardize = TRUE)

stat1 <- inertia(edgelist = el, riskset = rs, weights = weights, 
    standardize = FALSE)
stat2 <- inertia(edgelist = el2, riskset = rs2, weights = weights, 
    standardize = FALSE)
stat3 <- inertia(edgelist = el3, riskset = rs3, weights = weights, 
    standardize = FALSE)
stat4 <- inertia(edgelist = el4, riskset = rs4, weights = weights, 
    standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statC), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(statD), "num[1:nrow(el4), 1:nrow(rs4)]")

    expect_output(str(statA_std), "num[1:nrow(el), 1:nrow(rs)]")

    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat3), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(stat4), "num[1:nrow(el4), 1:nrow(rs4)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(statB[,1:650], statA) 
    expect_equal(statD[,1:325], statC) 
    expect_equal(stat2[,1:650], stat1) 
    expect_equal(stat4[,1:325], stat3) 

    expect_equal(statB[,1:650], statB[,651:1300])
    expect_equal(statB[,1:650], statB[,1301:1950])

    expect_equal(statD[,1:325], statD[,326:650])
    expect_equal(statD[,1:325], statD[,651:975])

     expect_equal(stat2[,1:650], stat2[,651:1300])
    expect_equal(stat2[,1:650], stat2[,1301:1950])

    expect_equal(stat4[,1:325], stat4[,326:650])
    expect_equal(stat4[,1:325], stat4[,651:975])
})

test_that("the rowsums for the raw inertia counts sum from 0 to the total 
    number of events (M) - 1", {

    expect_equal(rowSums(statA), seq(0, nrow(el)-1))
    expect_equal(rowSums(statB), seq(0, nrow(el2)-1)*3) # with types
    expect_equal(rowSums(statC), seq(0, nrow(el3)-1))
    expect_equal(rowSums(statD), seq(0, nrow(el4)-1)*3) # with types
})

test_that("the rowsums for the weighted inertia count is equal to the cumulative sum of the weights", {

    expect_equal(rowSums(stat1), c(0, cumsum(weights[-nrow(el)])))
    expect_equal(rowSums(stat2), c(0, cumsum(weights[-nrow(el2)]))*3)
    expect_equal(rowSums(stat3), c(0, cumsum(weights[-nrow(el3)])))
    expect_equal(rowSums(stat4), c(0, cumsum(weights[-nrow(el4)]))*3)
})

test_that("the raw final counts are correct", {
    sel <- el[1:(nrow(el)-1),]
    expect_equal(apply(rs, 1, function(x) {
        sum(sel[,2]==x[1]&sel[,3]==x[2])
    }), statA[nrow(statA),])

    sel2 <- el2[1:(nrow(el2)-1),]
    expect_equal(apply(rs2, 1, function(x) {
        sum(sel2[,2]==x[1]&sel2[,3]==x[2])
    }), statB[nrow(statB),])

    sel3 <- el3[1:(nrow(el3)-1),]
    expect_equal(apply(rs3, 1, function(x) {
        sum(sel3[,2]==x[1]&sel3[,3]==x[2])
    }), statC[nrow(statC),])

    sel4 <- el4[1:(nrow(el4)-1),]
    expect_equal(apply(rs4, 1, function(x) {
        sum(sel4[,2]==x[1]&sel4[,3]==x[2])
    }), statD[nrow(statD),])
})

test_that("the weighted final counts are correct", {
    sel <- el[1:(nrow(el)-1),]
    expect_equal(apply(rs, 1, function(x) {
        sum(weights[which(sel[,2]==x[1]&sel[,3]==x[2])])
    }), stat1[nrow(stat1),])

    sel2 <- el2[1:(nrow(el2)-1),]
    expect_equal(apply(rs2, 1, function(x) {
        sum(weights[which(sel2[,2]==x[1]&sel2[,3]==x[2])])
    }), stat2[nrow(stat2),])

    sel3 <- el3[1:(nrow(el3)-1),]
    expect_equal(apply(rs3, 1, function(x) {
        sum(weights[which(sel3[,2]==x[1]&sel3[,3]==x[2])])
    }), stat3[nrow(stat3),])

    sel4 <- el4[1:(nrow(el4)-1),]
    expect_equal(apply(rs4, 1, function(x) {
        sum(weights[which(sel4[,2]==x[1]&sel4[,3]==x[2])])
    }), stat4[nrow(stat4),])
})

test_that("standardization", {
    compare <- rbind(statA[1,], 
        t(apply(statA[-1,], 1, function(x) (x-mean(x))/sd(x))))

    expect_equal(compare, statA_std)
})

