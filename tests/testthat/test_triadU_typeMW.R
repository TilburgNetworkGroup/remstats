context("triadU_typeMW")

library(remstats)

# Prepare windows
windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))

# Prepare for undirected relational events with types
data(edgelistUT)

w1 <- edgelistUT[edgelistUT$time > windows$start[1] & 
    edgelistUT$time <= windows$end[1],]
w2 <- edgelistUT[edgelistUT$time > windows$start[2] & 
    edgelistUT$time <= windows$end[2],]

out <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el <- out$edgelist
rs <- out$riskset
ac <- out$actors

elw1 <- prepER(w1, directed = FALSE, type = TRUE, actors = ac[,2])$edgelist
elw2 <- prepER(w2, directed = FALSE, type = TRUE, actors = ac[,2])$edgelist

# Statistics
statA <- triadU_type(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)
statAw1 <- triadU_typeMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw1, window_length = 100, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)
statAw2 <- triadU_typeMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw2, window_length = 100, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)

statB <- triadU_type(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)
statBw1 <- triadU_typeMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw1, window_length = 100, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)
statBw2 <- triadU_typeMW(actors = ac[,1], full_edgelist = el, 
    window_edgelist = elw2, window_length = 100, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statAw1), "num[1:nrow(elw1), 1:nrow(rs)]")
    expect_output(str(statAw2), "num[1:nrow(elw2), 1:nrow(rs)]")

    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statBw1), "num[1:nrow(elw1), 1:nrow(rs)]")
    expect_output(str(statBw2), "num[1:nrow(elw2), 1:nrow(rs)]")
})

test_that("the values in the first window are equal to the first values for the 
    full edgelist", {

    expect_equal(statAw1, statA[1:nrow(statAw1),])
    expect_equal(statBw1, statB[1:nrow(statBw1),])
})

test_that("the final counts are correct", {
    pel <- el[el[,1] > (max(elw2[,1]) - 100) & el[,1] < max(elw2[,1]),]

    expect_equal(apply(rs, 1, function(x) {
        del1 <- pel[(pel[,2] == x[1] | pel[,3] == x[1]) & pel[,4] == x[3],]
        del2 <- pel[(pel[,2] == x[2] | pel[,3] == x[2]) & pel[,4] == x[3],]
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
        del1 <- pel[(pel[,2] == x[1] | pel[,3] == x[1]) & pel[,4] == x[3],]
        del2 <- pel[(pel[,2] == x[2] | pel[,3] == x[2]) & pel[,4] == x[3],]
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
})

