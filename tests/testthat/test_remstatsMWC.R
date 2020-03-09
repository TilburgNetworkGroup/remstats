context("remstatsMWC")

require(remstats)

# Set things up
data(edgelistD)
data(covar)

windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))

full_edgelist <- edgelistD
w1_edgelist <- edgelistD[edgelistD$time > windows$start[1] & 
    edgelistD$time <= windows$end[1],]
w2_edgelist <- edgelistD[edgelistD$time > windows$start[2] & 
    edgelistD$time <= windows$end[2],]

out <- prepER(full_edgelist)
full_el <- out$edgelist
rs <- out$riskset
ac <- out$actors

out <- prepER(w1_edgelist, actors = ac[,2])
w1_el <- out$edgelist
out <- prepER(w2_edgelist, actors = ac[,2])
w2_el <- out$edgelist 

full_evls <- prepEvls(full_el, rs)
w1_evls <- prepEvls(w1_el, rs)
w2_evls <- prepEvls(w2_el, rs)

covar$id <- ac$id[match(covar$id, ac$name)]
covar <- as.matrix(covar)
    
covariates <- rep(list(matrix(0, 1, 1)), 8)

# Tests
test_that("remstatsMWC sender_effect", {
    effects <- c(0, 1, 1)
    covariates[[1]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # The first row for the first window should contain all values for t = 0
    expect_true(all(stats[1,,2] %in% covar[covar[,2]==0,3]))
    expect_true(all(covar[covar[,2]==0,3] %in% stats[1,,2]))

    expect_true(all(stats[1,,3] %in% covar[covar[,2]==0,4]))
    expect_true(all(covar[covar[,2]==0,4] %in% stats[1,,3]))

    # The first row for the second window should be different from the first 
    # row of the second window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC receiver_effect", {
    effects <- c(0, 2, 2)
    covariates[[2]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # The first row for the first window should contain all values for t = 0
    expect_true(all(stats[1,,2] %in% covar[covar[,2]==0,3]))
    expect_true(all(covar[covar[,2]==0,3] %in% stats[1,,2]))

    expect_true(all(stats[1,,3] %in% covar[covar[,2]==0,4]))
    expect_true(all(covar[covar[,2]==0,4] %in% stats[1,,3]))

    # The first row for the second window should be different from the first 
    # row of the second window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC same effect", {
    effects <- c(0, 3, 3)
    covariates[[3]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 3] == 
            zerovalue[zerovalue[,1] == x[2], 3], 1, 0)}), stats[1,,2])

    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 4] == 
            zerovalue[zerovalue[,1] == x[2], 4], 1, 0)}), stats[1,,3])

    # The first row for the second window should be different from the first 
    # row of the first window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))  
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC difference effect", {
    effects <- c(0, 4, 4)
    covariates[[4]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    expect_equal(apply(rs, 1, function(x) {
        abs(zerovalue[zerovalue[,1] == x[1], 3] - 
            zerovalue[zerovalue[,1] == x[2], 3])}), stats[1,,2])
    
    expect_equal(apply(rs, 1, function(x) {
        abs(zerovalue[zerovalue[,1] == x[1], 4] - 
            zerovalue[zerovalue[,1] == x[2], 4])}), stats[1,,3])

    # The first row for the second window should be different from the first 
    # row of the first window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))  
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC mean effect", {
    effects <- c(0, 5, 5)
    covariates[[5]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    expect_equal(apply(rs, 1, function(x) {
        mean(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stats[1,,2])

    expect_equal(apply(rs, 1, function(x) {
        mean(c(zerovalue[zerovalue[,1] == x[1], 4], 
            zerovalue[zerovalue[,1] == x[2], 4]))}), stats[1,,3])

    # The first row for the second window should be different from the first 
    # row of the first window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))  
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC min effect", {
    effects <- c(0, 6, 6)    
    covariates[[6]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    expect_equal(apply(rs, 1, function(x) {
        min(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stats[1,,2])
    
    expect_equal(apply(rs, 1, function(x) {
        min(c(zerovalue[zerovalue[,1] == x[1], 4], 
            zerovalue[zerovalue[,1] == x[2], 4]))}), stats[1,,3])
    
    # The first row for the second window should be different from the first 
    # row of the first window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))  
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC max effect", {
    effects <- c(0, 7, 7)
    covariates[[7]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    expect_equal(apply(rs, 1, function(x) {
        max(c(zerovalue[zerovalue[,1] == x[1], 3], 
            zerovalue[zerovalue[,1] == x[2], 3]))}), stats[1,,2])
    
    expect_equal(apply(rs, 1, function(x) {
        max(c(zerovalue[zerovalue[,1] == x[1], 4], 
            zerovalue[zerovalue[,1] == x[2], 4]))}), stats[1,,3])
    
    # The first row for the second window should be different from the first 
    # row of the first window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))  
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC both_equal_to effect", {
    effects <- c(0, 8, 8)
    covariates[[8]] <- covar

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = c(0,0), 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, equal_val = c(0,0), 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:3]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:3]")

    # Test whether the starting values are correct
    zerovalue <- covar[covar[,2]==0,]

    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 3] == 0 &
            zerovalue[zerovalue[,1] == x[2], 3] == 0, 1, 0)}), stats[1,,2])

    expect_equal(apply(rs, 1, function(x) {
        ifelse(zerovalue[zerovalue[,1] == x[1], 4] == 0 &
            zerovalue[zerovalue[,1] == x[2], 4] == 0, 1, 0)}), stats[1,,3])
    
    # The first row for the second window should be different from the first 
    # row of the first window
    expect_true(!all(stats2[1,,2] == stats[1,,2]))  
    expect_true(!all(stats2[1,,3] == stats[1,,3]))
})

test_that("remstatsMWC event_effect", {
    effects <- c(0, 9)
    event_effect1 <- as.matrix(sample(c(0,1), nrow(w1_el), replace = TRUE))
    event_effect2 <- as.matrix(sample(c(0,1), nrow(w2_el), replace = TRUE))

    stats <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = event_effect1, full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))
    
    stats2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = event_effect2, full_weights = 0, equal_val = 0, 
        int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stats), "num[1:nrow(w1_evls), 1:nrow(rs), 1:2]")
    expect_output(str(stats2), "num[1:nrow(w2_evls), 1:nrow(rs), 1:2]")

    # Columns should be equal to the event_effect matrices
    expect_equal(as.matrix(stats[,1,2]), event_effect1)
    expect_equal(as.matrix(stats2[,1,2]), event_effect2)

    # All columns should be equal
    expect_true(all(diff(colSums(stats[,,2]))==0))
    expect_true(all(diff(colSums(stats2[,,2]))==0))
})

test_that("remstatsMWC inertia", {
    effects <- 10

    full_weights <- rep(1, nrow(full_evls))

    stat <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = full_weights, 
        equal_val = 0, int_positions = matrix(0, 1, 1))
    
    stat2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = full_weights, 
        equal_val = 0, int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stat), "num[1:nrow(w1_evls), 1:nrow(rs), 1]")
    expect_output(str(stat2), "num[1:nrow(w2_evls), 1:nrow(rs), 1]")

    # All values for w1_evls should be equal to the values for full_evls
    comp <- inertia(full_evls, rs, full_weights, FALSE)
    expect_equal(stat[,,1], comp[1:nrow(stat),])

    # At a certain point in the 2nd window, the rowsums should not always be 
    # equal to the previous row sum plus one
    expect_true(!all(diff(rowSums(stat2[,,1]))==1))
})

test_that("remstatsMWC inertia_weighted", {
    effects <- 11

    full_weights <- rep(1, nrow(full_evls))

    stat <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = full_weights, 
        equal_val = 0, int_positions = matrix(0, 1, 1))
    
    stat2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = full_weights, 
        equal_val = 0, int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stat), "num[1:nrow(w1_evls), 1:nrow(rs), 1]")
    expect_output(str(stat2), "num[1:nrow(w2_evls), 1:nrow(rs), 1]")

    # All values should be equal to moving wnidow inertia values
    comp <- inertiaMW(full_evls, w1_evls, 100, rs, full_weights, FALSE)
    comp2 <- inertiaMW(full_evls, w2_evls, 100, rs, full_weights, FALSE)

    expect_equal(stat[,,1], comp)
    expect_equal(stat2[,,1], comp2)
})

test_that("remstatsMWC shared_partners", {
    effects <- 28

    stat <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, 
        equal_val = 0, int_positions = matrix(0, 1, 1))
    
    stat2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, 
        equal_val = 0, int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stat), "num[1:nrow(w1_evls), 1:nrow(rs), 1]")
    expect_output(str(stat2), "num[1:nrow(w2_evls), 1:nrow(rs), 1]")

    # All values for w1_evls should be equal to the values for full_evls
    comp <- triadU(ac[,1], full_el, rs, FALSE, FALSE)
    expect_equal(stat[,,1], comp[1:nrow(stat),])
})

test_that("remstatsMWC unique_sp", {
    effects <- 29

    stat <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w1_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w1_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, 
        equal_val = 0, int_positions = matrix(0, 1, 1))
    
    stat2 <- remStatsMWC(effects = effects, standardize = FALSE, 
        full_edgelist = full_el, window_edgelist = w2_el, 
        window_length = 100, riskset = rs, full_evls = full_evls, 
        window_evls = w2_evls, actors = ac[,1], covariates = covariates, 
        event_effect = matrix(0, 1, 1), full_weights = 0, 
        equal_val = 0, int_positions = matrix(0, 1, 1))

    # Test output dimensions
    expect_output(str(stat), "num[1:nrow(w1_evls), 1:nrow(rs), 1]")
    expect_output(str(stat2), "num[1:nrow(w2_evls), 1:nrow(rs), 1]")

    # All values for w1_evls should be equal to the values for full_evls
    comp <- triadU(ac[,1], full_el, rs, TRUE, FALSE)
    expect_equal(stat[,,1], comp[1:nrow(stat),])

    # The count can be at maximum equal to the number of actors - 2
    expect_true(max(stat[,,1])<=(max(ac[,1])-2))
    expect_true(max(stat2[,,1])<=(max(ac[,1])-2))

    # The count should always be equal or lower than the shared_partners effect
    comp <- triadUMW(ac[,1], full_el, w1_el, 100, rs, FALSE, FALSE)
    comp2 <- triadUMW(ac[,1], full_el, w2_el, 100, rs, FALSE, FALSE)
    
    expect_true(all(stat[,,1] <= comp))
    expect_true(all(stat2[,,1] <= comp2))
})