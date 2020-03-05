context("dyadstat output")

require(remstats)

test_that("dimensions dyadstat output", {
    # Test for directed relational events
    data(edgelistD)
    data(covar)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    stat1 <- dyadstat(values = covar[,c(1:2, 4)], type = 1, el, rs, 0)
    stat2 <- dyadstat(values = covar[,c(1:3)], type = 2, el, rs, 0)
    stat3 <- dyadstat(values = covar[,c(1:3)], type = 3, el, rs, 0)
    stat4 <- dyadstat(values = covar[,c(1:3)], type = 4, el, rs, 0)
    stat5 <- dyadstat(values = covar[,c(1:3)], type = 5, el, rs, 0)
    stat6 <- dyadstat(values = covar[,c(1:2, 4)], type = 6, el, rs, 0)
    
    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]") 
    expect_output(str(stat3), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat4), "num[1:nrow(el), 1:nrow(rs)]") 
    expect_output(str(stat5), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat6), "num[1:nrow(el), 1:nrow(rs)]") 

    # Test for undirected relational events
    data(edgelistU)
    data(covar)

    out <- prepER(edgelistU, directed = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    stat1 <- dyadstat(values = covar[,c(1:2, 4)], type = 1, el, rs, 0)
    stat2 <- dyadstat(values = covar[,c(1:3)], type = 2, el, rs, 0)
    stat3 <- dyadstat(values = covar[,c(1:3)], type = 3, el, rs, 0)
    stat4 <- dyadstat(values = covar[,c(1:3)], type = 4, el, rs, 0)
    stat5 <- dyadstat(values = covar[,c(1:3)], type = 5, el, rs, 0)
    stat6 <- dyadstat(values = covar[,c(1:2, 4)], type = 6, el, rs, 0)
    
    expect_output(str(stat1), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat2), "num[1:nrow(el), 1:nrow(rs)]") 
    expect_output(str(stat3), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat4), "num[1:nrow(el), 1:nrow(rs)]") 
    expect_output(str(stat5), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(stat6), "num[1:nrow(el), 1:nrow(rs)]") 
})

test_that("starting values dyadstat output", {
    # Test for directed relational events
    data(edgelistD)
    data(covar)

    out <- prepER(edgelistD)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    stat1 <- dyadstat(values = covar[,c(1:2, 4)], type = 1, el, rs, 0)
    stat2 <- dyadstat(values = covar[,c(1:3)], type = 2, el, rs, 0)
    stat3 <- dyadstat(values = covar[,c(1:3)], type = 3, el, rs, 0)
    stat4 <- dyadstat(values = covar[,c(1:3)], type = 4, el, rs, 0)
    stat5 <- dyadstat(values = covar[,c(1:3)], type = 5, el, rs, 0)
    stat6 <- dyadstat(values = covar[,c(1:2, 4)], type = 6, el, rs, 0)

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    # Stat 1: same
    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 4] == 
            zerovalue[zerovalue[,1] == x[2], 4], 1, 0)}), stat1[1,])

    # Stat 2: difference
    expect_equal(apply(rs, 1, function(x) {
        abs(zerovalue[zerovalue[,1] == x[1], 3] - 
            zerovalue[zerovalue[,1] == x[2], 3])}), stat2[1,])

    # Stat 3: mean
    expect_equal(apply(rs, 1, function(x) {
        mean(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stat3[1,])

    # Stat 4: min
    expect_equal(apply(rs, 1, function(x) {
        min(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stat4[1,])
    
    # Stat 5: max
    expect_equal(apply(rs, 1, function(x) {
        max(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stat5[1,])
    
    # Stat 6: both equal to
    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 4] == 0 &
            zerovalue[zerovalue[,1] == x[2], 4] == 0, 1, 0)}), stat6[1,])

    # Test for undirected relational events
    data(edgelistU)
    data(covar)

    out <- prepER(edgelistU, directed = FALSE)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    covar$id <- ac$id[match(covar$id, ac$name)]
    covar <- as.matrix(covar)

    stat1 <- dyadstat(values = covar[,c(1:2, 4)], type = 1, el, rs, 0)
    stat2 <- dyadstat(values = covar[,c(1:3)], type = 2, el, rs, 0)
    stat3 <- dyadstat(values = covar[,c(1:3)], type = 3, el, rs, 0)
    stat4 <- dyadstat(values = covar[,c(1:3)], type = 4, el, rs, 0)
    stat5 <- dyadstat(values = covar[,c(1:3)], type = 5, el, rs, 0)
    stat6 <- dyadstat(values = covar[,c(1:2, 4)], type = 6, el, rs, 0)

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    # Stat 1: same
    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 4] == 
            zerovalue[zerovalue[,1] == x[2], 4], 1, 0)}), stat1[1,])

    # Stat 2: difference
    expect_equal(apply(rs, 1, function(x) {
        abs(zerovalue[zerovalue[,1] == x[1], 3] - 
            zerovalue[zerovalue[,1] == x[2], 3])}), stat2[1,])

    # Stat 3: mean
    expect_equal(apply(rs, 1, function(x) {
        mean(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stat3[1,])

    # Stat 4: min
    expect_equal(apply(rs, 1, function(x) {
        min(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stat4[1,])
    
    # Stat 5: max
    expect_equal(apply(rs, 1, function(x) {
        max(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stat5[1,])
    
    # Stat 6: both equal to
    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 4] == 0 &
            zerovalue[zerovalue[,1] == x[2], 4] == 0, 1, 0)}), stat6[1,])
})