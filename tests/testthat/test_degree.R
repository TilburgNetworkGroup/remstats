context("degree")

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
statA <- degree(edgelist = el, riskset = rs, type = 1, standardize = FALSE)
statB <- degree(edgelist = el, riskset = rs, type = 2, standardize = FALSE)
statC <- degree(edgelist = el, riskset = rs, type = 3, standardize = FALSE)
statD <- degree(edgelist = el, riskset = rs, type = 4, standardize = FALSE)
statE <- degree(edgelist = el, riskset = rs, type = 5, standardize = FALSE)
statF <- degree(edgelist = el, riskset = rs, type = 6, standardize = FALSE)

statA_std <- degree(edgelist = el, riskset = rs, type = 1, standardize = TRUE)

stat1 <- degree(edgelist = el2, riskset = rs2, type = 1, standardize = FALSE)
stat2 <- degree(edgelist = el2, riskset = rs2, type = 2, standardize = FALSE)
stat3 <- degree(edgelist = el2, riskset = rs2, type = 3, standardize = FALSE)
stat4 <- degree(edgelist = el2, riskset = rs2, type = 4, standardize = FALSE)
stat5 <- degree(edgelist = el2, riskset = rs2, type = 5, standardize = FALSE)
stat6 <- degree(edgelist = el2, riskset = rs2, type = 6, standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statC), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statD), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statE), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statF), "num[1:nrow(el), 1:nrow(rs)]")

    expect_output(str(stat1), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat2), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat3), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat4), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat5), "num[1:nrow(el2), 1:nrow(rs2)]")
    expect_output(str(stat6), "num[1:nrow(el2), 1:nrow(rs2)]")
})

test_that("the statistics are equal for dyads regardless of type", {
    expect_equal(stat1[,1:650], statA)
    expect_equal(stat2[,1:650], statB)
    expect_equal(stat3[,1:650], statC)
    expect_equal(stat4[,1:650], statD)
    expect_equal(stat5[,1:650], statE)
    expect_equal(stat6[,1:650], statF)

    expect_equal(stat1[,1:650], stat1[,651:1300])
    expect_equal(stat1[,1:650], stat1[,1301:1950])

    expect_equal(stat2[,1:650], stat2[,651:1300])
    expect_equal(stat2[,1:650], stat2[,1301:1950])

    expect_equal(stat3[,1:650], stat3[,651:1300])
    expect_equal(stat3[,1:650], stat3[,1301:1950])

    expect_equal(stat4[,1:650], stat4[,651:1300])
    expect_equal(stat4[,1:650], stat4[,1301:1950])

    expect_equal(stat5[,1:650], stat5[,651:1300])
    expect_equal(stat5[,1:650], stat5[,1301:1950])

    expect_equal(stat6[,1:650], stat6[,651:1300])
    expect_equal(stat6[,1:650], stat6[,1301:1950])
})

test_that("indegree values", {
    final_indegree_values <- table(el[1:(nrow(el)-1),3])

    expect_true(all(final_indegree_values %in% statA[nrow(el),]))
    expect_true(all(final_indegree_values %in% stat1[nrow(el2),]))
    expect_true(all(final_indegree_values %in% statB[nrow(el),]))
    expect_true(all(final_indegree_values %in% stat2[nrow(el2),]))

    expect_true(all(statA[nrow(el),] %in% statB[nrow(el),]))
    expect_true(all(stat1[nrow(el2),] %in% stat2[nrow(el2),]))

    expect_true(!all(statA == statB))
    expect_true(!all(stat1 == stat2))
})

test_that("outdegree values", {
    final_outdegree_values <- table(el[1:(nrow(el)-1),2])

    expect_true(all(final_outdegree_values %in% statC[nrow(el),]))
    expect_true(all(final_outdegree_values %in% stat3[nrow(el2),]))
    expect_true(all(final_outdegree_values %in% statD[nrow(el),]))
    expect_true(all(final_outdegree_values %in% stat4[nrow(el2),]))

    expect_true(all(statC[nrow(el),] %in% statD[nrow(el),]))
    expect_true(all(stat3[nrow(el2),] %in% stat4[nrow(el2),]))

    expect_true(!all(statC == statD))
    expect_true(!all(stat3 == stat4))
})

test_that("totaldegree values", {
    expect_equal(statE, statA + statC)
    expect_equal(statF, statB + statD)
    expect_equal(stat5, stat1 + stat3)
    expect_equal(stat6, stat2 + stat4)
})

test_that("standardization", {
    compare <- rbind(statA[1,], 
        t(apply(statA[-1,], 1, function(x) (x-mean(x))/sd(x))))

    expect_equal(compare, statA_std)
})