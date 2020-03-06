context("triadU output")

require(remstats)

test_that("dimensions triadU output, shared_partners effect", {
    # Test for undirected relational events
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat <- triadU(actors = ac, edgelist = el, riskset = rs, FALSE, FALSE)
    expect_output(str(stat), "num[1:nrow(el), 1:nrow(rs)]")
}) 

test_that("content triadU output, shared_partners effect", {
    # Test for undirected relational events
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat <- triadU(actors = ac, edgelist = el, riskset = rs, FALSE, FALSE)

    # The difference between rowSums should not be negative
    expect_true(all(diff(rowSums(stat))>=0))

    # The statistic should be equal to the sum of the four directed triad 
    # effects
    stat1 <- triad(actors = ac, edgelist = el, riskset = rs, type = 1, FALSE)
    stat2 <- triad(actors = ac, edgelist = el, riskset = rs, type = 2, FALSE)
    stat3 <- triad(actors = ac, edgelist = el, riskset = rs, type = 3, FALSE)
    stat4 <- triad(actors = ac, edgelist = el, riskset = rs, type = 4, FALSE)

    expect_equal(stat, stat1+stat2+stat3+stat4)
})

context("triadU output")

require(remstats)

test_that("dimensions triadU output, unique_sp effect", {
    # Test for undirected relational events
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat <- triadU(actors = ac, edgelist = el, riskset = rs, TRUE, FALSE)
    expect_output(str(stat), "num[1:nrow(el), 1:nrow(rs)]")
}) 

test_that("content triadU output, unique_sp effect", {
    # Test for undirected relational events
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat <- triadU(actors = ac, edgelist = el, riskset = rs, TRUE, FALSE)

    # The difference between rowSums should not be negative
    expect_true(all(diff(rowSums(stat))>=0))

    # The count can be at maximum equal to the number of actors - 2
    expect_true(max(stat)<=(max(ac)-2))

    # The count should always be equal or lower than the shared_partners effect
    stat2 <- triadU(actors = ac, edgelist = el, riskset = rs, FALSE, FALSE)
    expect_true(all(stat <= stat2))

    # Check a random row
    m <- sample(1:nrow(el)-1, 1)
    expect_equal(apply(rs, 1, function(x) {
	    partners1 <- unique(c(el[which(el[1:m,2]==x[1]),3], 
            el[which(el[1:m,3]==x[1]),2]))
	    partners2 <- unique(c(el[which(el[1:m,2]==x[2]),3], 
            el[which(el[1:m,3]==x[2]),2]))
	    sum(partners1 %in% partners2)	
    }), stat[m+1,])
})

test_that("Standardization triadU", {
    data(edgelistU)

    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors[,1]

    stat1 <- triadU(actors = ac, edgelist = el, riskset = rs, 
        unique_sp = FALSE, standardize = FALSE)
    stat2 <- triadU(actors = ac, edgelist = el, riskset = rs, 
        unique_sp = TRUE, standardize = FALSE)
    
    stat11 <- triadU(actors = ac, edgelist = el, riskset = rs, 
        unique_sp = FALSE, standardize = TRUE)
    stat12 <- triadU(actors = ac, edgelist = el, riskset = rs, 
        unique_sp = TRUE, standardize = TRUE)

    test1 <- rbind(stat1[which(rowSums(stat1)==0),], 
        t(apply(stat1[-which(rowSums(stat1)==0),], 1, 
            function(x) (x-mean(x))/sd(x))))
    expect_equal(test1, stat11)
    
    test2 <- rbind(stat2[which(rowSums(stat2)==0),], 
        t(apply(stat2[-which(rowSums(stat2)==0),], 1, 
            function(x) (x-mean(x))/sd(x))))
    expect_equal(test2, stat12)
})