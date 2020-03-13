context("triad")

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

# Statistics
statA <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 1, 
    standardize = FALSE)
statB <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 2, 
    standardize = FALSE)
statC <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 3, 
    standardize = FALSE)
statD <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 4, 
    standardize = FALSE)

statA_std <- triad(actors = ac[,1], edgelist = el, riskset = rs, type = 1, 
    standardize = TRUE)

stat1 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, type = 1, 
    standardize = FALSE)
stat2 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, type = 2, 
    standardize = FALSE)
stat3 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, type = 3, 
    standardize = FALSE)
stat4 <- triad(actors = ac[,1], edgelist = el2, riskset = rs2, type = 4,        
    standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statC), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statD), "num[1:nrow(el), 1:nrow(rs)]")

    expect_output(str(stat1), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat2), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat3), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat4), "num[1:nrow(el2), 1:nrow(rs2)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(stat1[,1:650], statA)
    expect_equal(stat2[,1:650], statB)
    expect_equal(stat3[,1:650], statC)
    expect_equal(stat4[,1:650], statD)

    expect_equal(stat1[,1:650], stat1[,651:1300])
    expect_equal(stat1[,1:650], stat1[,1301:1950])

    expect_equal(stat2[,1:650], stat2[,651:1300])
    expect_equal(stat2[,1:650], stat2[,1301:1950])

    expect_equal(stat3[,1:650], stat3[,651:1300])
    expect_equal(stat3[,1:650], stat3[,1301:1950])

    expect_equal(stat4[,1:650], stat4[,651:1300])
    expect_equal(stat4[,1:650], stat4[,1301:1950])
})

test_that("the difference between rowSums is not negative", {
    expect_true(all(diff(rowSums(statA))>=0))
    expect_true(all(diff(rowSums(statB))>=0))
    expect_true(all(diff(rowSums(statC))>=0))
    expect_true(all(diff(rowSums(statD))>=0))

    expect_true(all(diff(rowSums(stat1))>=0))
    expect_true(all(diff(rowSums(stat2))>=0))
    expect_true(all(diff(rowSums(stat3))>=0))
    expect_true(all(diff(rowSums(stat4))>=0))
})

test_that("triad values", {
    # An outgoing two-path for (i,j) is an incoming twopath for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
        statA[nrow(statA), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		statB[nrow(statB), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
    expect_true(all(apply(rs2, 1, function(x) {
        stat1[nrow(stat1), which(rs2[,1] == x[1] & rs2[,2] == x[2])] ==
		stat2[nrow(stat2), which(rs2[,1] == x[2] & rs2[,2] == x[1])]
    })))
        
    # An inbound shared partner for (i,j) is an inbound shared partner for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
	    stat3[nrow(statC), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat3[nrow(statC), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
    expect_true(all(apply(rs2, 1, function(x) {
	    stat3[nrow(stat3), which(rs2[,1] == x[1] & rs2[,2] == x[2])] ==
		stat3[nrow(stat3), which(rs2[,1] == x[2] & rs2[,2] == x[1])]
    })))

    # An outbound shared partner for (i,j) is an outbound shared partner for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
	    statD[nrow(statD), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		statD[nrow(statD), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
    expect_true(all(apply(rs2, 1, function(x) {
	    stat4[nrow(stat4), which(rs2[,1] == x[1] & rs2[,2] == x[2])] ==
		stat4[nrow(stat4), which(rs2[,1] == x[2] & rs2[,2] == x[1])]
    })))
})

test_that("standardization", {
    compare <- rbind(statA[1,], 
        t(apply(statA[-1,], 1, function(x) (x-mean(x))/sd(x))))
    compare[is.na(compare)] <- 0

    expect_equal(compare, statA_std)
})