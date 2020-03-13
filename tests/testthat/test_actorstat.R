context("actorstat")

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

# Prepare covariates
data(covar)

covar$id <- ac$id[match(covar$id, ac$name)]
covar <- as.matrix(covar)
zero_values <- covar[covar[,2]==0,]

# Statistics
statA <- actorstat(values = covar[,c(1:3)], type = 1, edgelist = el,
    riskset = rs) # Sender effect
statB <- actorstat(values = covar[,c(1, 2, 4)], type = 2, edgelist = el,
    riskset = rs) # Receiver effect

statC <- actorstat(values = covar[,c(1:3)], type = 1, edgelist = el2,
    riskset = rs2) # Sender effect
statD <- actorstat(values = covar[,c(1, 2, 4)], type = 2, edgelist = el2,
    riskset = rs2) # Receiver effect

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statC), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statD), "num[1:nrow(el2), 1:nrow(rs2)]")
})

test_that("the starting values for the statitics", {
    expect_true(all(zero_values[,3] %in% statA[1,]))
    expect_true(all(zero_values[,4] %in% statB[1,]))
    expect_true(all(zero_values[,3] %in% statC[1,]))
    expect_true(all(zero_values[,4] %in% statD[1,]))
})

test_that("the statistics change over time", {
    expect_true(!all(diff(rowSums(statA))==0))
    expect_true(!all(diff(rowSums(statB))==0))
    expect_true(!all(diff(rowSums(statC))==0))
    expect_true(!all(diff(rowSums(statD))==0))
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(statC[,1:650], statA)
    expect_equal(statD[,1:650], statB)

    expect_equal(statC[,1:650], statC[,651:1300])
    expect_equal(statC[,1:650], statC[,1301:1950])

    expect_equal(statD[,1:650], statD[,651:1300])
    expect_equal(statD[,1:650], statD[,1301:1950])
})