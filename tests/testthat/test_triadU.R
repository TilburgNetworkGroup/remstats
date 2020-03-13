context("triadU")

library(remstats)

# Prepare for undirected relational events
data(edgelistU)

out <- prepER(edgelistU, directed = FALSE)
el <- out$edgelist
rs <- out$riskset
ac <- out$actors

# Prepare for undirected relational events with types
data(edgelistUT)

out2 <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el2 <- out2$edgelist
rs2 <- out2$riskset

# Statistics
statA <- triadU(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)
statB <- triadU(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)

statA_std <- triadU(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = FALSE, standardize = TRUE)

stat1 <- triadU(actors = ac[,1], edgelist = el2, riskset = rs2, 
    unique_sp = FALSE, standardize = FALSE)
stat2 <- triadU(actors = ac[,1], edgelist = el2, riskset = rs2, 
    unique_sp = TRUE, standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    
    expect_output(str(stat1), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat2), "num[1:nrow(el2), 1:nrow(rs2)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(stat1[,1:325], statA)
    expect_equal(stat2[,1:325], statB)

    expect_equal(stat1[,1:325], stat1[,326:650])
    expect_equal(stat1[,1:325], stat1[,651:975])

    expect_equal(stat2[,1:325], stat2[,326:650])
    expect_equal(stat2[,1:325], stat2[,651:975])
})

test_that("the difference between rowSums is not negative", {
    expect_true(all(diff(rowSums(statA))>=0))
    expect_true(all(diff(rowSums(statB))>=0))

    expect_true(all(diff(rowSums(stat1))>=0))
    expect_true(all(diff(rowSums(stat2))>=0))
})

test_that("the shared_partners effect is equal to the sum of the four directed 
    triad effects", {
        OTP1 <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 1, 
            standardize = FALSE)
        ITP1 <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 2, 
            standardize = FALSE)
        OSP1 <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 3, 
            standardize = FALSE)
        ISP1 <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 4, 
            standardize = FALSE)

        OTP2 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, 
            type = 1, standardize = FALSE)
        ITP2 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, 
            type = 2, standardize = FALSE)
        OSP2 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, 
            type = 3, standardize = FALSE)
        ISP2 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, 
            type = 4, standardize = FALSE)

        expect_equal(statA, OTP1+ITP1+OSP1+ISP1)
        expect_equal(stat1, OTP2+ITP2+OSP2+ISP2)
})

test_that("values unique_sp effect", {
    expect_true(max(statB)<=(max(ac[,1])-2))
    expect_true(max(stat2)<=(max(ac[,1])-2))

    expect_true(all(statB<=statA))
    expect_true(all(stat2<=stat1))
})

test_that("the final counts are correct", {
    sel <- el[1:(nrow(el)-1),]

    expect_equal(apply(rs, 1, function(x) {
        del1 <- sel[(sel[,2] == x[1] | sel[,3] == x[1]),]
        del2 <- sel[(sel[,2] == x[2] | sel[,3] == x[2]),]
        p1 <- c(del1[,2], del1[,3])
        p2 <- c(del2[,2], del2[,3])
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sp <- 0
        for(i in unique(p1)) {
            sp <- sp + min(c(sum(p1==i), sum(p2==i)))
        }
        sp}), statA[nrow(statA),])

    expect_equal(apply(rs, 1, function(x) {
        del1 <- sel[(sel[,2] == x[1] | sel[,3] == x[1]),]
        del2 <- sel[(sel[,2] == x[2] | sel[,3] == x[2]),]
        p1 <- unique(c(del1[,2], del1[,3]))
        p2 <- unique(c(del2[,2], del2[,3]))
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sum(p1 %in% p2)}), statB[nrow(statB),])

    sel2 <- el2[1:(nrow(el2)-1),]

    expect_equal(apply(rs2, 1, function(x) {
        del1 <- sel2[(sel2[,2] == x[1] | sel2[,3] == x[1]),]
        del2 <- sel2[(sel2[,2] == x[2] | sel2[,3] == x[2]),]
        p1 <- c(del1[,2], del1[,3])
        p2 <- c(del2[,2], del2[,3])
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sp <- 0
        for(i in unique(p1)) {
            sp <- sp + min(c(sum(p1==i), sum(p2==i)))
        }
        sp}), stat1[nrow(stat1),])

    expect_equal(apply(rs2, 1, function(x) {
        del1 <- sel2[(sel2[,2] == x[1] | sel2[,3] == x[1]),]
        del2 <- sel2[(sel2[,2] == x[2] | sel2[,3] == x[2]),]
        p1 <- unique(c(del1[,2], del1[,3]))
        p2 <- unique(c(del2[,2], del2[,3]))
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sum(p1 %in% p2)}), stat2[nrow(stat2),])
})

test_that("standardization", {
    compare <- rbind(statA[1,], 
        t(apply(statA[-1,], 1, function(x) (x-mean(x))/sd(x))))
    compare[is.na(compare)] <- 0

    expect_equal(compare, statA_std)
})
