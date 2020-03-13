context("typestat")

library(remstats)

# Prepare for directed relational events with types
data(edgelistDT)

out <- prepER(edgelistDT, type = TRUE)
el <- out$edgelist
rs <- out$riskset
types <- sort(unique(rs[,3]))

# Prepare for undirected relational events with types
data(edgelistUT)

out2 <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el2 <- out2$edgelist
rs2 <- out2$riskset
types2 <- sort(unique(rs2[,3]))

# Statistics
statA1 <- typestat(edgelist = el, riskset = rs, type = types[1])
statA2 <- typestat(edgelist = el, riskset = rs, type = types[2])
statB1 <- typestat(edgelist = el2, riskset = rs2, type = types[1])
statB2 <- typestat(edgelist = el2, riskset = rs2, type = types[2])

# Tests
test_that("dimensions", {
    expect_output(str(statA1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statA2), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB1), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statB2), "num[1:nrow(el2), 1:nrow(rs2)]")
})

test_that("every row is equal", {
    expect_true(all(sapply(1:(nrow(statA1)-1), function(x) 
        {statA1[x,] == statA1[x+1,]})))
    expect_true(all(sapply(1:(nrow(statA2)-1), function(x) 
        {statA2[x,] == statA2[x+1,]})))
    expect_true(all(sapply(1:(nrow(statB1)-1), function(x) 
        {statB1[x,] == statB1[x+1,]})))
    expect_true(all(sapply(1:(nrow(statB2)-1), function(x) 
        {statB2[x,] == statB2[x+1,]})))
})

test_that("the first row contains the correct values", {
    expect_equal(apply(rs, 1, function(x) {
        ifelse(x[3]==types[1], 1, 0)
    }), statA1[1,])
    expect_equal(apply(rs, 1, function(x) {
        ifelse(x[3]==types[2], 1, 0)
    }), statA2[1,])
    expect_equal(apply(rs2, 1, function(x) {
        ifelse(x[3]==types[1], 1, 0)
    }), statB1[1,])
    expect_equal(apply(rs2, 1, function(x) {
        ifelse(x[3]==types[2], 1, 0)
    }), statB2[1,])
})