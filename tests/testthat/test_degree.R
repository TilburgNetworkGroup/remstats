context("degree output")

require(remstats)

test_that("dimensions degree output", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)

    el <- out$edgelist
    rs <- out$riskset

    stat1 <- degree(edgelist = el, riskset = rs, type = 1, standardize = FALSE)
    stat2 <- degree(edgelist = el, riskset = rs, type = 2, standardize = FALSE)
    stat3 <- degree(edgelist = el, riskset = rs, type = 3, standardize = FALSE)
    stat4 <- degree(edgelist = el, riskset = rs, type = 4, standardize = FALSE)
    stat5 <- degree(edgelist = el, riskset = rs, type = 5, standardize = FALSE)
    stat6 <- degree(edgelist = el, riskset = rs, type = 6, standardize = FALSE)

    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat3), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat4), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat5), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat6), "num[1:nrow(el), 1:nrow(rs)]")
})

test_that("content degree output", {
    # Tests for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test indegreeSnd
    indegreeSnd <- degree(el, rs, 1, standardize = FALSE)
    expect_true(all(table(el[1:(nrow(el)-1),3]) %in% indegreeSnd[nrow(el),]))

    # Test indegreeRec
    indegreeRec <- degree(el, rs, 2, standardize = FALSE)
    expect_true(all(table(el[1:(nrow(el)-1),3]) %in% indegreeRec[nrow(el),]))

    # Test both indegreeSnd and indegreeRec
    expect_true(all(indegreeSnd[nrow(el),] %in% indegreeRec[nrow(el),]))
    expect_true(!all(indegreeSnd == indegreeRec))

    # Test outdegreeSnd
    outdegreeSnd <- degree(el, rs, 3, standardize = FALSE)
    expect_true(all(table(el[1:(nrow(el)-1),2]) %in% outdegreeSnd[nrow(el),]))

    # Test outdegreeRec
    outdegreeRec <- degree(el, rs, 4, standardize = FALSE)
    expect_true(all(table(el[1:(nrow(el)-1),2]) %in% outdegreeRec[nrow(el),]))

    # Test both indegreeSnd and indegreeRec
    expect_true(all(indegreeSnd[nrow(el),] %in% indegreeRec[nrow(el),]))
    expect_true(!all(indegreeSnd == indegreeRec))

    # Test totaldegreeSnd
    totaldegreeSnd <- degree(el, rs, 5, standardize = FALSE)
    expect_true(all(totaldegreeSnd[nrow(el),] == (indegreeSnd[nrow(el),] + outdegreeSnd[nrow(el),])))

    # Test totaldegreeRec
    totaldegreeRec <- degree(el, rs, 6, standardize = FALSE)
    expect_true(all(totaldegreeRec[nrow(el),] == (indegreeRec[nrow(el),] + outdegreeRec[nrow(el),])))

    # Test both totaldegreeSnd and totaldegreeRec
    expect_true(all(totaldegreeSnd[nrow(el),] %in% totaldegreeRec[nrow(el),]))
    expect_true(!all(totaldegreeSnd == totaldegreeRec))
})

test_that("Standardization degree", {
    # Test for directed relational events 
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    stat1 <- degree(edgelist = el, riskset = rs, type = 1, standardize = FALSE)
    stat2 <- degree(edgelist = el, riskset = rs, type = 2, standardize = FALSE)
    stat3 <- degree(edgelist = el, riskset = rs, type = 3, standardize = FALSE)
    stat4 <- degree(edgelist = el, riskset = rs, type = 4, standardize = FALSE)
    stat5 <- degree(edgelist = el, riskset = rs, type = 5, standardize = FALSE)
    stat6 <- degree(edgelist = el, riskset = rs, type = 6, standardize = FALSE)

    stat11 <- degree(edgelist = el, riskset = rs, type = 1, standardize = TRUE)
    stat12 <- degree(edgelist = el, riskset = rs, type = 2, standardize = TRUE)
    stat13 <- degree(edgelist = el, riskset = rs, type = 3, standardize = TRUE)
    stat14 <- degree(edgelist = el, riskset = rs, type = 4, standardize = TRUE)
    stat15 <- degree(edgelist = el, riskset = rs, type = 5, standardize = TRUE)
    stat16 <- degree(edgelist = el, riskset = rs, type = 6, standardize = TRUE)

    test1 <- rbind(stat1[1,], 
        t(apply(stat1[-1,], 1, function(x) (x-mean(x))/sd(x))))
    expect_equal(test1, stat11)
    
    test2 <- rbind(stat2[1,], 
        t(apply(stat2[-1,], 1, function(x) (x-mean(x))/sd(x))))
    expect_equal(test2, stat12)

    test3 <- rbind(stat3[1,], 
        t(apply(stat3[-1,], 1, function(x) (x-mean(x))/sd(x))))
    expect_equal(test3, stat13)

    test4 <- rbind(stat4[1,], 
        t(apply(stat4[-1,], 1, function(x) (x-mean(x))/sd(x))))
    expect_equal(test4, stat14)

    test5 <- rbind(stat5[1,], 
        t(apply(stat5[-1,], 1, function(x) (x-mean(x))/sd(x))))
    expect_equal(test5, stat15)

    test6 <- rbind(stat6[1,], 
        t(apply(stat6[-1,], 1, function(x) (x-mean(x))/sd(x))))
    expect_equal(test6, stat16)
})