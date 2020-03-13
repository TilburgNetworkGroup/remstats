context("dyadstat")

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

# Prepare covariates
data(covar)

covar$id <- ac$id[match(covar$id, ac$name)]
covar <- as.matrix(covar)
zero_values <- covar[covar[,2]==0,]

# Statistics
stat1 <- dyadstat(values = covar[,c(1, 2, 4)], type = 1, edgelist = el,
    riskset = rs, equal_val = 0) # same
stat2 <- dyadstat(values = covar[,c(1, 2, 3)], type = 2, edgelist = el,
    riskset = rs, equal_val = 0) # difference
stat3 <- dyadstat(values = covar[,c(1, 2, 3)], type = 3, edgelist = el,
    riskset = rs, equal_val = 0) # mean
stat4 <- dyadstat(values = covar[,c(1, 2, 3)], type = 4, edgelist = el,
    riskset = rs, equal_val = 0) # min
stat5 <- dyadstat(values = covar[,c(1, 2, 3)], type = 5, edgelist = el,
    riskset = rs, equal_val = 0) # max
stat6 <- dyadstat(values = covar[,c(1, 2, 4)], type = 6, edgelist = el,
    riskset = rs, equal_val = 0) # both_equal_to

statA <- dyadstat(values = covar[,c(1, 2, 4)], type = 1, edgelist = el2,
    riskset = rs2, equal_val = 0) # same
statB <- dyadstat(values = covar[,c(1, 2, 3)], type = 2, edgelist = el2,
    riskset = rs2, equal_val = 0) # difference
statC <- dyadstat(values = covar[,c(1, 2, 3)], type = 3, edgelist = el2,
    riskset = rs2, equal_val = 0) # mean
statD <- dyadstat(values = covar[,c(1, 2, 3)], type = 4, edgelist = el2,
    riskset = rs2, equal_val = 0) # min
statE <- dyadstat(values = covar[,c(1, 2, 3)], type = 5, edgelist = el2,
    riskset = rs2, equal_val = 0) # max
statF <- dyadstat(values = covar[,c(1, 2, 4)], type = 6, edgelist = el2,
    riskset = rs2, equal_val = 0) # both_equal_to

stat11 <- dyadstat(values = covar[,c(1, 2, 4)], type = 1, edgelist = el3,
    riskset = rs3, equal_val = 0) # same
stat22 <- dyadstat(values = covar[,c(1, 2, 3)], type = 2, edgelist = el3,
    riskset = rs3, equal_val = 0) # difference
stat33 <- dyadstat(values = covar[,c(1, 2, 3)], type = 3, edgelist = el3,
    riskset = rs3, equal_val = 0) # mean
stat44 <- dyadstat(values = covar[,c(1, 2, 3)], type = 4, edgelist = el3,
    riskset = rs3, equal_val = 0) # min
stat55 <- dyadstat(values = covar[,c(1, 2, 3)], type = 5, edgelist = el3,
    riskset = rs3, equal_val = 0) # max
stat66 <- dyadstat(values = covar[,c(1, 2, 4)], type = 6, edgelist = el3,
    riskset = rs3, equal_val = 0) # both_equal_to

statAA <- dyadstat(values = covar[,c(1, 2, 4)], type = 1, edgelist = el4,
    riskset = rs4, equal_val = 0) # same
statBB <- dyadstat(values = covar[,c(1, 2, 3)], type = 2, edgelist = el4,
    riskset = rs4, equal_val = 0) # difference
statCC <- dyadstat(values = covar[,c(1, 2, 3)], type = 3, edgelist = el4,
    riskset = rs4, equal_val = 0) # mean
statDD <- dyadstat(values = covar[,c(1, 2, 3)], type = 4, edgelist = el4,
    riskset = rs4, equal_val = 0) # min
statEE <- dyadstat(values = covar[,c(1, 2, 3)], type = 5, edgelist = el4,
    riskset = rs4, equal_val = 0) # max
statFF <- dyadstat(values = covar[,c(1, 2, 4)], type = 6, edgelist = el4,
    riskset = rs4, equal_val = 0) # both_equal_to

# Tests
test_that("dimensions", {
    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat3), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat4), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat5), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat6), "num[1:nrow(el), 1:nrow(rs)]")

    expect_output(str(statA), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statB), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statC), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statD), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statE), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statF), "num[1:nrow(el2), 1:nrow(rs2)]")

    expect_output(str(stat11), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(stat22), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(stat33), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(stat44), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(stat55), "num[1:nrow(el3), 1:nrow(rs3)]")
    expect_output(str(stat66), "num[1:nrow(el3), 1:nrow(rs3)]")

    expect_output(str(statAA), "num[1:nrow(el4), 1:nrow(rs4)]")
    expect_output(str(statBB), "num[1:nrow(el4), 1:nrow(rs4)]")
    expect_output(str(statCC), "num[1:nrow(el4), 1:nrow(rs4)]")
    expect_output(str(statDD), "num[1:nrow(el4), 1:nrow(rs4)]")
    expect_output(str(statEE), "num[1:nrow(el4), 1:nrow(rs4)]")
    expect_output(str(statFF), "num[1:nrow(el4), 1:nrow(rs4)]")
})

test_that("the starting values for the statitics", {
    # same
    expect_equal(apply(rs, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 
            zero_values[zero_values[,1] == x[2], 4], 1, 0)}), stat1[1,])
    expect_equal(apply(rs2, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 
            zero_values[zero_values[,1] == x[2], 4], 1, 0)}), statA[1,])
    expect_equal(apply(rs3, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 
            zero_values[zero_values[,1] == x[2], 4], 1, 0)}), stat11[1,])
    expect_equal(apply(rs4, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 
            zero_values[zero_values[,1] == x[2], 4], 1, 0)}), statAA[1,])

    # difference
    expect_equal(apply(rs, 1, function(x) {
        abs(zero_values[zero_values[,1] == x[1], 3] - 
            zero_values[zero_values[,1] == x[2], 3])}), stat2[1,])
    expect_equal(apply(rs2, 1, function(x) {
        abs(zero_values[zero_values[,1] == x[1], 3] - 
            zero_values[zero_values[,1] == x[2], 3])}), statB[1,])
    expect_equal(apply(rs3, 1, function(x) {
        abs(zero_values[zero_values[,1] == x[1], 3] - 
            zero_values[zero_values[,1] == x[2], 3])}), stat22[1,])
    expect_equal(apply(rs4, 1, function(x) {
        abs(zero_values[zero_values[,1] == x[1], 3] - 
            zero_values[zero_values[,1] == x[2], 3])}), statBB[1,])

    # mean
    expect_equal(apply(rs, 1, function(x) {
        mean(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), stat3[1,])
    expect_equal(apply(rs2, 1, function(x) {
        mean(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), statC[1,])
    expect_equal(apply(rs3, 1, function(x) {
        mean(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), stat33[1,])
    expect_equal(apply(rs4, 1, function(x) {
        mean(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), statCC[1,])          

    # min
    expect_equal(apply(rs, 1, function(x) {
        min(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), stat4[1,])
    expect_equal(apply(rs2, 1, function(x) {
        min(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), statD[1,])
    expect_equal(apply(rs3, 1, function(x) {
        min(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), stat44[1,])
    expect_equal(apply(rs4, 1, function(x) {
        min(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), statDD[1,])        

    # max
    expect_equal(apply(rs, 1, function(x) {
        max(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), stat5[1,])
    expect_equal(apply(rs2, 1, function(x) {
        max(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), statE[1,])
    expect_equal(apply(rs3, 1, function(x) {
        max(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), stat55[1,])
    expect_equal(apply(rs4, 1, function(x) {
        max(c(zero_values[zero_values[,1] == x[1], 3], 
            zero_values[zero_values[,1] == x[2], 3]))}), statEE[1,])

    # both_equal_to
    expect_equal(apply(rs, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 0 &
            zero_values[zero_values[,1] == x[2], 4] == 0, 1, 0)}), stat6[1,])
    expect_equal(apply(rs2, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 0 &
            zero_values[zero_values[,1] == x[2], 4] == 0, 1, 0)}), statF[1,])
    expect_equal(apply(rs3, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 0 &
            zero_values[zero_values[,1] == x[2], 4] == 0, 1, 0)}), stat66[1,])
    expect_equal(apply(rs4, 1, function(x) {
        ifelse(zero_values[zero_values[,1] == x[1], 4] == 0 &
            zero_values[zero_values[,1] == x[2], 4] == 0, 1, 0)}), statFF[1,])
})

test_that("the statistics change over time", {
    expect_true(!all(diff(rowSums(stat1))==0))
    expect_true(!all(diff(rowSums(stat2))==0))
    expect_true(!all(diff(rowSums(stat3))==0))
    expect_true(!all(diff(rowSums(stat4))==0))
    expect_true(!all(diff(rowSums(stat5))==0))
    expect_true(!all(diff(rowSums(stat6))==0))

    expect_true(!all(diff(rowSums(statA))==0))
    expect_true(!all(diff(rowSums(statB))==0))
    expect_true(!all(diff(rowSums(statC))==0))
    expect_true(!all(diff(rowSums(statD))==0))
    expect_true(!all(diff(rowSums(statE))==0))
    expect_true(!all(diff(rowSums(statF))==0))

    expect_true(!all(diff(rowSums(stat11))==0))
    expect_true(!all(diff(rowSums(stat22))==0))
    expect_true(!all(diff(rowSums(stat33))==0))
    expect_true(!all(diff(rowSums(stat44))==0))
    expect_true(!all(diff(rowSums(stat55))==0))
    expect_true(!all(diff(rowSums(stat66))==0))

    expect_true(!all(diff(rowSums(statAA))==0))
    expect_true(!all(diff(rowSums(statBB))==0))
    expect_true(!all(diff(rowSums(statCC))==0))
    expect_true(!all(diff(rowSums(statDD))==0))
    expect_true(!all(diff(rowSums(statEE))==0))
    expect_true(!all(diff(rowSums(statFF))==0))
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(statA[,1:650], stat1)
    expect_equal(statB[,1:650], stat2)
    expect_equal(statC[,1:650], stat3)
    expect_equal(statD[,1:650], stat4)
    expect_equal(statE[,1:650], stat5)
    expect_equal(statF[,1:650], stat6)

    expect_equal(statA[,1:650], statA[,651:1300])
    expect_equal(statA[,1:650], statA[,1301:1950])

    expect_equal(statB[,1:650], statB[,651:1300])
    expect_equal(statB[,1:650], statB[,1301:1950])

    expect_equal(statC[,1:650], statC[,651:1300])
    expect_equal(statC[,1:650], statC[,1301:1950])

    expect_equal(statD[,1:650], statD[,651:1300])
    expect_equal(statD[,1:650], statD[,1301:1950])

    expect_equal(statE[,1:650], statE[,651:1300])
    expect_equal(statE[,1:650], statE[,1301:1950])

    expect_equal(statF[,1:650], statF[,651:1300])
    expect_equal(statF[,1:650], statF[,1301:1950])

    expect_equal(statAA[,1:325], stat11)
    expect_equal(statBB[,1:325], stat22)
    expect_equal(statCC[,1:325], stat33)
    expect_equal(statDD[,1:325], stat44)
    expect_equal(statEE[,1:325], stat55)
    expect_equal(statFF[,1:325], stat66)

    expect_equal(statAA[,1:325], statAA[,326:650])
    expect_equal(statAA[,1:325], statAA[,651:975])

    expect_equal(statBB[,1:325], statBB[,326:650])
    expect_equal(statBB[,1:325], statBB[,651:975])

    expect_equal(statCC[,1:325], statCC[,326:650])
    expect_equal(statCC[,1:325], statCC[,651:975])

    expect_equal(statDD[,1:325], statDD[,326:650])
    expect_equal(statDD[,1:325], statDD[,651:975])

    expect_equal(statEE[,1:325], statEE[,326:650])
    expect_equal(statEE[,1:325], statEE[,651:975])

    expect_equal(statFF[,1:325], statFF[,326:650])
    expect_equal(statFF[,1:325], statFF[,651:975])
})