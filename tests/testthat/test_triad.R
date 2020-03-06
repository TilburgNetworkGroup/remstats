context("triad output")

require(remstats)

test_that("dimensions triad output", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1, 
        standardize = FALSE)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2, 
        standardize = FALSE)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3, 
        standardize = FALSE)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4, 
        standardize = FALSE)

    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat3), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat4), "num[1:nrow(el), 1:nrow(rs)]")
})

test_that("test content triad output with edgelistD", {
    # Test for directed relational events
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1, 
        standardize = FALSE)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2, 
        standardize = FALSE)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3, 
        standardize = FALSE)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4, 
        standardize = FALSE)

    # The difference between rowSums should not be negative
    expect_true(all(diff(rowSums(stat1))>=0))
    expect_true(all(diff(rowSums(stat2))>=0))
    expect_true(all(diff(rowSums(stat3))>=0))
    expect_true(all(diff(rowSums(stat4))>=0))
    # An outgoing two-path for (i,j) is an incoming twopath for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
        stat1[nrow(stat1), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat2[nrow(stat2), which(rs[,1] == x[2] & rs[,2] == x[1])]
        })))
    # An inbound shared partner for (i,j) is an inbound shared partner for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
	    stat3[nrow(stat3), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat3[nrow(stat3), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
    # An outbound shared partner for (i,j) is an outbound shared partner for (j,i)
    expect_true(all(apply(rs, 1, function(x) {
	    stat4[nrow(stat4), which(rs[,1] == x[1] & rs[,2] == x[2])] ==
		stat4[nrow(stat4), which(rs[,1] == x[2] & rs[,2] == x[1])]
    })))
})

test_that("Standardization triad", {
    # Test for directed relational events 
    data(edgelistD)

    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1, 
        standardize = FALSE)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2, 
        standardize = FALSE)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3, 
        standardize = FALSE)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4, 
        standardize = FALSE)

    stat11 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1, 
        standardize = TRUE)
    stat12 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2, 
        standardize = TRUE)
    stat13 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3, 
        standardize = TRUE)
    stat14 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4, 
        standardize = TRUE)

    test1 <- rbind(stat1[which(rowSums(stat1)==0),], 
        t(apply(stat1[-which(rowSums(stat1)==0),], 1, 
            function(x) (x-mean(x))/sd(x))))
    expect_equal(test1, stat11)
    
    test2 <- rbind(stat2[which(rowSums(stat2)==0),], 
        t(apply(stat2[-which(rowSums(stat2)==0),], 1, 
            function(x) (x-mean(x))/sd(x))))
    expect_equal(test2, stat12)

    test3 <- rbind(stat3[which(rowSums(stat3)==0),], 
        t(apply(stat3[-which(rowSums(stat3)==0),], 1, 
            function(x) (x-mean(x))/sd(x))))
    expect_equal(test3, stat13)

    test4 <- rbind(stat4[which(rowSums(stat4)==0),], 
        t(apply(stat4[-which(rowSums(stat4)==0),], 1, 
            function(x) (x-mean(x))/sd(x))))
    expect_equal(test4, stat14)
})
