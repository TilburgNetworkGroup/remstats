library(remstats)

actors <- unique(info$id)
age <- info[match(actors, info$id), "age"]
both_old <- sapply(seq_along(actors), function(i) {
    sapply(seq_along(actors), function(j) {
        ifelse(age[i] == 1 & age[j] == 1 & i != j, 1, 0)
    })
})
rownames(both_old) <- colnames(both_old) <- actors

test_that("expected errors and warnings", {
    # Expected errors for sender effects
    mod <- ~ tie(x = both_old)
    expect_error(
        remstats(edgelist = history, sender_effects = mod, attributes = info),
        "not defined for the sender activity model"
    )

    # Expected error for missing row names
    temp <- both_old
    rownames(temp) <- NULL
    mod <- ~ tie(x = temp)
    expect_error(
        remstats(edgelist = history, tie_effects = mod),
        "Name rows and columns"
    )
    expect_error(
        remstats(edgelist = history, receiver_effects = mod),
        "Name rows and columns"
    )

    # Expected error for missing column names
    temp <- both_old
    colnames(temp) <- NULL
    expect_error(
        remstats(edgelist = history, tie_effects = mod),
        "Name rows and columns"
    )
    expect_error(
        remstats(edgelist = history, receiver_effects = mod),
        "Name rows and columns"
    )

    # Expected error for missing actors
    temp <- both_old[-1, ]
    expect_error(
        remstats(edgelist = history, tie_effects = mod),
        "include values for all actors in the network"
    )
    expect_error(
        remstats(edgelist = history, receiver_effects = mod),
        "include values for all actors in the network"
    )

    temp <- both_old[, -1]
    expect_error(
        remstats(edgelist = history, tie_effects = mod),
        "include values for all actors in the network"
    )
    expect_error(
        remstats(edgelist = history, receiver_effects = mod),
        "include values for all actors in the network"
    )

    # Expected error for missing values on the off-diagonal
    temp <- both_old
    temp[3, 1] <- NA
    expect_error(
        remstats(edgelist = history, tie_effects = mod),
        "missing values"
    )
    expect_error(
        remstats(edgelist = history, receiver_effects = mod),
        "missing values"
    )

    # No error expected for missing values on the diagonal
    temp <- both_old
    temp[1, 1] <- NA
    expect_no_error(remstats(edgelist = history, tie_effects = mod))
    expect_no_error(remstats(edgelist = history, receiver_effects = mod))

    # Expected error for non-symmetrical matrix and undirected events
    temp <- both_old
    temp[3, 1] <- 5
    expect_error(
        remstats(edgelist = history, tie_effects = mod, directed = FALSE),
        "expected to be symmetric"
    )

    # No error expected for diagonal matrix and undirected events
    temp <- both_old
    temp[upper.tri(temp)] <- NA
    expect_no_error(
        remstats(edgelist = history, tie_effects = mod, directed = FALSE)
    )

    # No error expected for symmetrical matrix and undirected events
    mod <- ~ tie(x = both_old)
    expect_no_error(
        remstats(edgelist = history, tie_effects = mod, directed = FALSE)
    )
})

test_that("expected output from tie()", {
    # Expected standard output
    out <- list(
        effect = "tie", x = both_old, variable = NULL, scaling = 1
    )
    expect_equal(tie(x = both_old), out)

    # Expected output with "std" scaling
    out$scaling <- 2
    expect_equal(tie(x = both_old, scaling = "std"), out)

    # Expected output with variableName
    out$scaling <- 1
    out$variable <- "test"
    expect_equal(tie(x = both_old, variableName = "test"), out)
})

test_that("expected statistic tie-oriented model", {
    # Expected name of the statistic
    mod <- ~ tie(x = both_old)
    tomres <- remstats(edgelist = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "tie")

    mod <- ~ tie(x = both_old) + tie(x = t(both_old))
    tomres <- remstats(edgelist = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "tie1")
    expect_equal(dimnames(tomres$statistics)[[3]][3], "tie2")

    mod <- ~ tie(x = both_old, variableName = "both_old")
    tomres <- remstats(edgelist = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "both_old")

    mod <- ~ tie(x = both_old, variableName = "test") +
        tie(x = both_old, variableName = "check")
    tomres <- remstats(edgelist = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "test")
    expect_equal(dimnames(tomres$statistics)[[3]][3], "check")

    # Expected statistic
    mod <- ~ tie(x = both_old)
    tomres <- remstats(edgelist = history, tie_effects = mod)

    dyads <- which(tomres$riskset[, 1] %in% actors[age == 1] &
        tomres$riskset[, 2] %in% actors[age == 1])
    nondyads <- (1:nrow(tomres$riskset))[-dyads]

    expect_true(all(tomres$statistics[, dyads, 2] == 1))
    expect_true(all(tomres$statistics[, nondyads, 2] == 0))

    # Expected "std" statistic
    mod <- ~ tie(x = both_old, scaling = "std")
    tomres <- remstats(edgelist = history, tie_effects = mod)
    stat <- matrix(0, nrow = 1, ncol = nrow(tomres$riskset))
    stat[dyads] <- 1
    stat <- as.numeric(scale(as.numeric(stat)))
    expect_true(all(sapply(1:nrow(history), function(x) {
        all.equal(stat, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Repeat for undirected events
    mod <- ~ tie(x = both_old)
    tomres <- remstats(edgelist = history, tie_effects = mod, directed = FALSE)

    dyads <- which(tomres$riskset[, 1] %in% actors[age == 1] &
        tomres$riskset[, 2] %in% actors[age == 1])
    nondyads <- (1:nrow(tomres$riskset))[-dyads]

    expect_true(all(tomres$statistics[, dyads, 2] == 1))
    expect_true(all(tomres$statistics[, nondyads, 2] == 0))

    mod <- ~ tie(x = both_old, scaling = "std")
    tomres <- remstats(edgelist = history, tie_effects = mod, directed = FALSE)
    stat <- matrix(0, nrow = 1, ncol = nrow(tomres$riskset))
    stat[dyads] <- 1
    stat <- as.numeric(scale(as.numeric(stat)))
    expect_true(all(sapply(1:nrow(history), function(x) {
        all.equal(stat, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Repeat for typed events
    mod <- ~ tie(x = both_old)
    history$type <- history$setting
    tomres <- remstats(edgelist = history, tie_effects = mod)

    dyads <- which(tomres$riskset[, 1] %in% actors[age == 1] &
        tomres$riskset[, 2] %in% actors[age == 1])
    nondyads <- (1:nrow(tomres$riskset))[-dyads]

    expect_true(all(tomres$statistics[, dyads, 2] == 1))
    expect_true(all(tomres$statistics[, nondyads, 2] == 0))

    mod <- ~ tie(x = both_old, scaling = "std")
    tomres <- remstats(edgelist = history, tie_effects = mod)
    stat <- matrix(0, nrow = 1, ncol = nrow(tomres$riskset))
    stat[dyads] <- 1
    stat <- as.numeric(scale(as.numeric(stat)))
    expect_true(all(sapply(1:nrow(history), function(x) {
        all.equal(stat, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))
})

test_that("expected statistic actor-oriented model", {
    # Expected name of the statistic
    mod <- ~ tie(x = both_old)
    aomres <- remstats(edgelist = history, receiver_effects = mod)
    expect_equal(dimnames(aomres$statistics$receiver_stats)[[3]][1], "tie")

    mod <- ~ tie(x = both_old) + tie(x = t(both_old))
    aomres <- remstats(edgelist = history, receiver_effects = mod)
    expect_equal(dimnames(aomres$statistics$receiver_stats)[[3]][1], "tie1")
    expect_equal(dimnames(aomres$statistics$receiver_stats)[[3]][2], "tie2")

    mod <- ~ tie(x = both_old, variableName = "both_old")
    aomres <- remstats(edgelist = history, receiver_effects = mod)
    expect_equal(dimnames(aomres$statistics$receiver_stats)[[3]][1], "both_old")

    mod <- ~ tie(x = both_old, variableName = "test") +
        tie(x = both_old, variableName = "check")
    aomres <- remstats(edgelist = history, receiver_effects = mod)
    expect_equal(dimnames(aomres$statistics$receiver_stats)[[3]][1], "test")
    expect_equal(dimnames(aomres$statistics$receiver_stats)[[3]][2], "check")

    # Expected statistic
    mod <- ~ tie(x = both_old)
    aomres <- remstats(edgelist = history, receiver_effects = mod)
    stat <- lapply(1:nrow(history), function(i) {
        sender <- as.numeric(history[i, 2])
        both_old[which(aomres$actors == sender), ]
    })
    stat <- do.call(rbind, stat)
    expect_true(
        all.equal(stat, aomres$statistics$receiver_stats[, , 1],
            check.attributes = FALSE
        )
    )

    # Expected "std" statistic
    mod <- ~ tie(x = both_old, scaling = "std")
    aomres <- remstats(edgelist = history, receiver_effects = mod)
    std_stat <- t(sapply(1:nrow(history), function(x) {
        # Scale stat without receiver
        sender <- which(aomres$actors == history$actor1[x])
        std_row <- scale(stat[x, -sender])
        std_row <- append(std_row, 0, after = sender - 1)
        std_row[is.nan(std_row)] <- 0
        std_row
    }))
    expect_true(
        all.equal(std_stat, aomres$statistics$receiver_stats[, , 1],
            check.attributes = FALSE
        )
    )
})
