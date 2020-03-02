context("degree output")

require(remstats)

test_that("dimensions degree output", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)

    el <- out$edgelist
    rs <- out$riskset

    stat1 <- degree(edgelist = el, riskset = rs, type = 1)
    stat2 <- degree(edgelist = el, riskset = rs, type = 2)
    stat3 <- degree(edgelist = el, riskset = rs, type = 3)
    stat4 <- degree(edgelist = el, riskset = rs, type = 4)
    stat5 <- degree(edgelist = el, riskset = rs, type = 5)
    stat6 <- degree(edgelist = el, riskset = rs, type = 6)

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
    indegreeSnd <- degree(el, rs, 1)
    expect_true(all(table(el[1:(nrow(el)-1),3]) %in% indegreeSnd[nrow(el),]))

    # Test indegreeRec
    indegreeRec <- degree(el, rs, 2)
    expect_true(all(table(el[1:(nrow(el)-1),3]) %in% indegreeRec[nrow(el),]))

    # Test both indegreeSnd and indegreeRec
    expect_true(all(indegreeSnd[nrow(el),] %in% indegreeRec[nrow(el),]))
    expect_true(!all(indegreeSnd == indegreeRec))

    # Test outdegreeSnd
    outdegreeSnd <- degree(el, rs, 3)
    expect_true(all(table(el[1:(nrow(el)-1),2]) %in% outdegreeSnd[nrow(el),]))

    # Test outdegreeRec
    outdegreeRec <- degree(el, rs, 4)
    expect_true(all(table(el[1:(nrow(el)-1),2]) %in% outdegreeRec[nrow(el),]))

    # Test both indegreeSnd and indegreeRec
    expect_true(all(indegreeSnd[nrow(el),] %in% indegreeRec[nrow(el),]))
    expect_true(!all(indegreeSnd == indegreeRec))

    # Test totaldegreeSnd
    totaldegreeSnd <- degree(el, rs, 5)
    expect_true(all(totaldegreeSnd[nrow(el),] == (indegreeSnd[nrow(el),] + outdegreeSnd[nrow(el),])))

    # Test totaldegreeRec
    totaldegreeRec <- degree(el, rs, 6)
    expect_true(all(totaldegreeRec[nrow(el),] == (indegreeRec[nrow(el),] + outdegreeRec[nrow(el),])))

    # Test both totaldegreeSnd and totaldegreeRec
    expect_true(all(totaldegreeSnd[nrow(el),] %in% totaldegreeRec[nrow(el),]))
    expect_true(!all(totaldegreeSnd == totaldegreeRec))
})