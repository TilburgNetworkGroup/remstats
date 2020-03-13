context("inertia_typeMW")

library(remstats)

# Prepare windows
windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))

# Prepare for directed relational events with types
data(edgelistDT)

w1 <- edgelistDT[edgelistDT$time > windows$start[1] & 
    edgelistDT$time <= windows$end[1],]
w2 <- edgelistDT[edgelistDT$time > windows$start[2] & 
    edgelistDT$time <= windows$end[2],]

out <- prepER(edgelistDT, type = TRUE)
el1 <- out$edgelist
rs1 <- out$riskset
ac <- out$actors

el1w1 <- prepER(w1, type = TRUE, actors = ac[,2])$edgelist
el1w2 <- prepER(w2, type = TRUE, actors = ac[,2])$edgelist

# Prepare for directed relational events with types
data(edgelistUT)

w1 <- edgelistUT[edgelistUT$time > windows$start[1] & 
    edgelistUT$time <= windows$end[1],]
w2 <- edgelistUT[edgelistUT$time > windows$start[2] & 
    edgelistUT$time <= windows$end[2],]

out2 <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el2 <- out2$edgelist
rs2 <- out2$riskset

el2w1 <- prepER(w1, directed = FALSE, type = TRUE, actors = ac[,2])$edgelist
el2w2 <- prepER(w2, directed = FALSE, type = TRUE, actors = ac[,2])$edgelist

# Weights 
full_equal_weights <- rep(1, nrow(el1))

# Statistics
statA <- inertia_type(edgelist = el1, riskset = rs1, 
    weights = full_equal_weights, standardize = FALSE)
statAw1 <- inertia_typeMW(full_edgelist = el1, window_edgelist = el1w1, 
    window_length = 100, riskset = rs1, full_weights = full_equal_weights, 
    standardize = FALSE)
statAw2 <- inertia_typeMW(full_edgelist = el1, window_edgelist = el1w2, 
    window_length = 100, riskset = rs1, full_weights = full_equal_weights, 
    standardize = FALSE)

statB <- inertia_type(edgelist = el2, riskset = rs2, 
    weights = full_equal_weights, standardize = FALSE)
statBw1 <- inertia_typeMW(full_edgelist = el2, window_edgelist = el2w1, 
    window_length = 100, riskset = rs2, full_weights = full_equal_weights, 
    standardize = FALSE)
statBw2 <- inertia_typeMW(full_edgelist = el2, window_edgelist = el2w2, 
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

test_that("the statistics are different for dyads of different type", {
    expect_true(!all(statA[,1:650]==statA[,651:1300]))
    expect_true(!all(statA[,1:650]==statA[,1301:1950]))

    expect_true(!all(statB[,1:325]==statB[,326:650]))
    expect_true(!all(statB[,1:325]==statB[,651:975]))
})

test_that("the values in the first window are equal to the first values for the 
    full edgelist", {

    expect_equal(statAw1, statA[1:nrow(statAw1),])
    expect_equal(statBw1, statB[1:nrow(statBw1),])
})

test_that("the final counts are correct", {
    pel1 <- el1[el1[,1] > (max(el1w2[,1]) - 100) & el1[,1] < max(el1w2[,1]),]
    expect_equal(apply(rs1, 1, function(x) {
        sum(pel1[,2]==x[1]&pel1[,3]==x[2]&pel1[,4]==x[3])
    }), statAw2[nrow(statAw2),])

    pel2 <- el2[el2[,1] > (max(el2w2[,1]) - 100) & el2[,1] < max(el2w2[,1]),]
    expect_equal(apply(rs2, 1, function(x) {
        sum(pel2[,2]==x[1]&pel2[,3]==x[2]&pel2[,4]==x[3])
    }), statBw2[nrow(statBw2),])
})

