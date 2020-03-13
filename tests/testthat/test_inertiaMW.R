context("inertiaMW")

library(remstats)

# Prepare windows
windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))

# Prepare for directed relational events
data(edgelistD)

w1 <- edgelistD[edgelistD$time > windows$start[1] & 
    edgelistD$time <= windows$end[1],]
w2 <- edgelistD[edgelistD$time > windows$start[2] & 
    edgelistD$time <= windows$end[2],]

out <- prepER(edgelistD)
el <- out$edgelist
rs <- out$riskset
ac <- out$actors

elw1 <- prepER(w1, actors = ac[,2])$edgelist
elw2 <- prepER(w2, actors = ac[,2])$edgelist

# Prepare for directed relational events with types
data(edgelistDT)

w1 <- edgelistDT[edgelistDT$time > windows$start[1] & 
    edgelistDT$time <= windows$end[1],]
w2 <- edgelistDT[edgelistDT$time > windows$start[2] & 
    edgelistDT$time <= windows$end[2],]

out2 <- prepER(edgelistDT, type = TRUE)
el2 <- out2$edgelist
rs2 <- out2$riskset

el2w1 <- prepER(w1, type = TRUE, actors = ac[,2])$edgelist
el2w2 <- prepER(w2, type = TRUE, actors = ac[,2])$edgelist

# Weights 
full_equal_weights <- rep(1, nrow(el))

# Statistics
statA <- inertia(edgelist = el, riskset = rs, weights = full_equal_weights, 
    standardize = FALSE)
statAw1 <- inertiaMW(full_edgelist = el, window_edgelist = elw1, 
    window_length = 100, riskset = rs, full_weights = full_equal_weights, 
    standardize = FALSE)
statAw2 <- inertiaMW(full_edgelist = el, window_edgelist = elw2, 
    window_length = 100, riskset = rs, full_weights = full_equal_weights, 
    standardize = FALSE)

statB <- inertia(edgelist = el2, riskset = rs2, weights = full_equal_weights, 
    standardize = FALSE)
statBw1 <- inertiaMW(full_edgelist = el2, window_edgelist = el2w1, 
    window_length = 100, riskset = rs2, full_weights = full_equal_weights, 
    standardize = FALSE)
statBw2 <- inertiaMW(full_edgelist = el2, window_edgelist = el2w2, 
    window_length = 100, riskset = rs2, full_weights = full_equal_weights, 
    standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statAw1), "num[1:nrow(elw1), 1:nrow(rs)]")
    expect_output(str(statAw2), "num[1:nrow(elw2), 1:nrow(rs)]")

    expect_output(str(statB), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(statBw1), "num[1:nrow(el2w1), 1:nrow(rs2)]")
    expect_output(str(statBw2), "num[1:nrow(el2w2), 1:nrow(rs2)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(statB[,1:650], statA) 
    expect_equal(statBw1[,1:650], statAw1) 
    expect_equal(statBw2[,1:650], statAw2) 
    
    expect_equal(statB[,1:650], statB[,651:1300])
    expect_equal(statB[,1:650], statB[,1301:1950])

    expect_equal(statBw1[,1:650], statBw1[,651:1300])
    expect_equal(statBw1[,1:650], statBw1[,1301:1950])

    expect_equal(statBw2[,1:650], statBw2[,651:1300])
    expect_equal(statBw2[,1:650], statBw2[,1301:1950])
})

test_that("the values in the first window are equal to the first values for the 
    full edgelist", {

    expect_equal(statAw1, statA[1:nrow(statAw1),])
    expect_equal(statBw1, statB[1:nrow(statBw1),])
})

test_that("after 100 time units, the first event is no longer counted", {

    dyad <- which(statAw1[2,]==1)
    time <- min(which(w2$time > 100 + w1$time[1],))
    
    expect_true(statAw2[time-1, dyad]!=0)
    expect_true(statAw2[time, dyad]==statAw2[time-1, dyad]-1)
})

test_that("the final counts are correct", {
    pel1 <- el[el[,1] > (max(elw2[,1]) - 100) & el[,1] < max(elw2[,1]),]
    expect_equal(apply(rs, 1, function(x) {
        sum(pel1[,2]==x[1]&pel1[,3]==x[2])
    }), statAw2[nrow(statAw2),])

    pel2 <- el2[el2[,1] > (max(el2w2[,1]) - 100) & el2[,1] < max(el2w2[,1]),]
    expect_equal(apply(rs2, 1, function(x) {
        sum(pel2[,2]==x[1]&pel2[,3]==x[2])
    }), statBw2[nrow(statBw2),])
})