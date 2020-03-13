context("triadUMW")

library(remstats)

# Prepare windows
windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))

# Prepare for undirected relational events
data(edgelistU)

w1 <- edgelistU[edgelistU$time > windows$start[1] & 
    edgelistU$time <= windows$end[1],]
w2 <- edgelistU[edgelistU$time > windows$start[2] & 
    edgelistU$time <= windows$end[2],]

out <- prepER(edgelistU, directed = FALSE)
el <- out$edgelist
rs <- out$riskset
ac <- out$actors

elw1 <- prepER(w1, directed = FALSE, actors = ac[,2])$edgelist
elw2 <- prepER(w2, directed = FALSE, actors = ac[,2])$edgelist

# Prepare for undirected relational events with types
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

# Statistics
statA <- triadU(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)
statAw1 <- triadUMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw1, window_length = 100, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)
statAw2 <- triadUMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw2, window_length = 100, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)

statB <- triadU(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)
statBw1 <- triadUMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw1, window_length = 100, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)
statBw2 <- triadUMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw2, window_length = 100, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)

stat1 <- triadU(actors = ac[,1], edgelist = el2, riskset = rs2, 
    unique_sp = FALSE, standardize = FALSE)
stat1w1 <- triadUMW(actors = ac[,1], full_edgelist = el2, 
    window_edgelist = el2w1, window_length = 100, riskset = rs2, 
    unique_sp = FALSE, standardize = FALSE)
stat1w2 <- triadUMW(actors = ac[,1], full_edgelist = el2, 
    window_edgelist = el2w2, window_length = 100, riskset = rs2, 
    unique_sp = FALSE, standardize = FALSE)

stat2 <- triadU(actors = ac[,1], edgelist = el2, riskset = rs2, 
    unique_sp = TRUE, standardize = FALSE)
stat2w1 <- triadUMW(actors = ac[,1], full_edgelist = el2, 
    window_edgelist = el2w1, window_length = 100, riskset = rs2, 
    unique_sp = TRUE, standardize = FALSE)
stat2w2 <- triadUMW(actors = ac[,1], full_edgelist = el2, 
    window_edgelist = el2w2, window_length = 100, riskset = rs2, 
    unique_sp = TRUE, standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statAw1), "num[1:nrow(elw1), 1:nrow(rs)]")
    expect_output(str(statAw2), "num[1:nrow(elw2), 1:nrow(rs)]")

    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statBw1), "num[1:nrow(elw1), 1:nrow(rs)]")
    expect_output(str(statBw2), "num[1:nrow(elw2), 1:nrow(rs)]")

    expect_output(str(stat1), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat1w1), "num[1:nrow(el2w1), 1:nrow(rs2)]")
    expect_output(str(stat1w2), "num[1:nrow(el2w2), 1:nrow(rs2)]")

    expect_output(str(stat2), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat2w1), "num[1:nrow(el2w1), 1:nrow(rs2)]")
    expect_output(str(stat2w2), "num[1:nrow(el2w2), 1:nrow(rs2)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(stat1[,1:325], statA) 
    expect_equal(stat1w1[,1:325], statAw1) 
    expect_equal(stat1w2[,1:325], statAw2) 
    
    expect_equal(stat1[,1:325], stat1[,326:650])
    expect_equal(stat1[,1:325], stat1[,651:975])

    expect_equal(stat1w1[,1:325], stat1w1[,326:650])
    expect_equal(stat1w1[,1:325], stat1w1[,651:975])

    expect_equal(stat1w2[,1:325], stat1w2[,326:650])
    expect_equal(stat1w2[,1:325], stat1w2[,651:975])

    expect_equal(stat2[,1:325], statB) 
    expect_equal(stat2w1[,1:325], statBw1) 
    expect_equal(stat2w2[,1:325], statBw2) 
    
    expect_equal(stat2[,1:325], stat2[,326:650])
    expect_equal(stat2[,1:325], stat2[,651:975])

    expect_equal(stat2w1[,1:325], stat2w1[,326:650])
    expect_equal(stat2w1[,1:325], stat2w1[,651:975])

    expect_equal(stat2w2[,1:325], stat2w2[,326:650])
    expect_equal(stat2w2[,1:325], stat2w2[,651:975])
})

test_that("the values in the first window are equal to the first values for the 
    full edgelist", {

    expect_equal(statAw1, statA[1:nrow(statAw1),])
    expect_equal(statBw1, statB[1:nrow(statBw1),])
    expect_equal(stat1w1, stat1[1:nrow(stat1w1),])
    expect_equal(stat2w1, stat2[1:nrow(stat2w1),])
})

test_that("after 100 time units, the first event is no longer counted", {

    time <- min(which(statAw1==1, arr.ind = T)[,1])
    dyad <- which(statAw1[time,]==1)[1]
    newtime <- min(which(w2$time > 100 + w1$time[elw1[,2] == rs[dyad, 1] | elw1[,2] == rs[dyad, 2] | elw1[,3] == rs[dyad, 1] | elw1[,3] == rs[dyad, 2]][1],))
    
    expect_true(statAw2[newtime-1, dyad]!=0)
    expect_true(statAw2[newtime, dyad]==statAw2[time-1, dyad]-1)
})

test_that("the final counts are correct", {
    pel <- el[el[,1] > (max(elw2[,1]) - 100) & el[,1] < max(elw2[,1]),]

    expect_equal(apply(rs, 1, function(x) {
        del1 <- pel[(pel[,2] == x[1] | pel[,3] == x[1]),]
        del2 <- pel[(pel[,2] == x[2] | pel[,3] == x[2]),]
        if(is.null(nrow(del1))) {
            p1 <- c(del1[2], del1[3])
        } else {
            p1 <- c(del1[,2], del1[,3])
        }
        if(is.null(nrow(del2))) {
            p2 <- c(del2[2], del2[3])
        } else {
            p2 <- c(del2[,2], del2[,3])
        }
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sp <- 0
        for(i in unique(p1)) {
            sp <- sp + min(c(sum(p1==i), sum(p2==i)))
        }
        sp}), statAw2[nrow(statAw2),])

    expect_equal(apply(rs, 1, function(x) {
        del1 <- pel[(pel[,2] == x[1] | pel[,3] == x[1]),]
        del2 <- pel[(pel[,2] == x[2] | pel[,3] == x[2]),]
        if(is.null(nrow(del1))) {
            p1 <- unique(c(del1[2], del1[3]))
        } else {
            p1 <- unique(c(del1[,2], del1[,3]))
        }
        if(is.null(nrow(del2))) {
            p2 <- unique(c(del2[2], del2[3]))
        } else {
            p2 <- unique(c(del2[,2], del2[,3]))
        }
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sum(p1 %in% p2)}), statBw2[nrow(statBw2),])

    pel2 <- el2[el2[,1] > (max(el2w2[,1]) - 100) & el2[,1] < max(el2w2[,1]),]

    expect_equal(apply(rs2, 1, function(x) {
        del1 <- pel2[(pel2[,2] == x[1] | pel2[,3] == x[1]),]
        del2 <- pel2[(pel2[,2] == x[2] | pel2[,3] == x[2]),]
        if(is.null(nrow(del1))) {
            p1 <- c(del1[2], del1[3])
        } else {
            p1 <- c(del1[,2], del1[,3])
        }
        if(is.null(nrow(del2))) {
            p2 <- c(del2[2], del2[3])
        } else {
            p2 <- c(del2[,2], del2[,3])
        }
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sp <- 0
        for(i in unique(p1)) {
            sp <- sp + min(c(sum(p1==i), sum(p2==i)))
        }
        sp}), stat1w2[nrow(stat1w2),])

    expect_equal(apply(rs2, 1, function(x) {
        del1 <- pel2[(pel2[,2] == x[1] | pel2[,3] == x[1]),]
        del2 <- pel2[(pel2[,2] == x[2] | pel2[,3] == x[2]),]
        if(is.null(nrow(del1))) {
            p1 <- unique(c(del1[2], del1[3]))
        } else {
            p1 <- unique(c(del1[,2], del1[,3]))
        }
        if(is.null(nrow(del2))) {
            p2 <- unique(c(del2[2], del2[3]))
        } else {
            p2 <- unique(c(del2[,2], del2[,3]))
        }
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sum(p1 %in% p2)}), stat2w2[nrow(stat2w2),])
})