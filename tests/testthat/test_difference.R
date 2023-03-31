library(remstats)

test_that("expected errors and warnings", {
    # Expected error for missing argument "variable"
    expect_error(difference(), "missing")

    # Expected errors for wrong "variable"
    expect_error(
        difference(variable = "test", attributes = info),
        "not in attributes"
    )

    mod <- ~ difference(variable = "test")
    reh_tie <- remify::remify(history, model = "tie")
    reh_actor <- remify::remify(history, model = "actor")
    expect_error(
        remstats(reh = reh_tie, tie_effects = mod, attributes = info),
        "not in attributes"
    )

    expect_error(
        remstats(reh = reh_actor, receiver_effects = mod, attributes = info),
        "not in attributes"
    )

    # Expected errors for missing time variable
    expect_error(
        difference(variable = "extraversion", attributes = info[, -2]),
        "time variable is missing"
    )

    mod <- ~ difference(variable = "extraversion")
    attr <- info[, -2]
    expect_error(
        remstats(reh = reh_tie, tie_effects = mod, attributes = attr),
        "time variable is missing"
    )

    expect_error(
        remstats(reh = reh_actor, receiver_effects = mod, attributes = attr),
        "time variable is missing"
    )

    # Expected errors for sender effects
    expect_error(
        remstats(reh = reh_actor, sender_effects = mod, attributes = info),
        "not defined for the sender activity model"
    )

    # Expect warning for missing values
    attr <- info
    attr$extraversion[1] <- NA
    expect_warning(
        difference(variable = "extraversion", attributes = attr),
        "unexpected behavior"
    )

    expect_warning(
        remstats(reh = reh_tie, tie_effects = mod, attributes = attr),
        "unexpected behavior"
    )

    expect_warning(
        remstats(reh = reh_actor, receiver_effects = mod, attributes = attr),
        "unexpected behavior"
    )

    # Expect error for missing time values
    attr <- info
    attr$time[1] <- NA
    expect_error(
        difference(variable = "extraversion", attributes = attr),
        "cannot have missing values"
    )

    expect_error(
        remstats(reh = reh_tie, tie_effects = mod, attributes = attr),
        "cannot have missing values"
    )

    expect_error(
        remstats(reh = reh_actor, receiver_effects = mod, attributes = attr),
        "cannot have missing values"
    )

    # Expect warning for extra actor
    attr <- rbind(info, info[1, ])
    attr[nrow(attr), 1] <- 999
    expect_warning(
        remstats(reh = reh_tie, tie_effects = mod, attributes = attr),
        "actors that are not in the risk set"
    )

    expect_warning(
        remstats(reh = reh_actor, receiver_effects = mod, attributes = attr),
        "actors that are not in the risk set"
    )

    mod <- ~ difference(variable = "extraversion", attributes = attr)
    expect_warning(
        remstats(reh = reh_tie, tie_effects = mod),
        "actors that are not in the risk set"
    )

    expect_warning(
        remstats(reh = reh_actor, receiver_effects = mod),
        "actors that are not in the risk set"
    )

    # Missing actor
    attr <- subset(info, name != 101)
    mod <- ~ difference(variable = "extraversion")
    expect_error(
        remstats(reh = reh_tie, tie_effects = mod, attributes = attr),
        "Missing actors"
    )

    expect_error(
        remstats(reh = reh_actor, receiver_effects = mod, attributes = attr),
        "Missing actors"
    )

    mod <- ~ difference(variable = "extraversion", attributes = attr)
    expect_error(
        remstats(reh = reh_tie, tie_effects = mod),
        "Missing actors"
    )

    expect_error(
        remstats(reh = reh_actor, receiver_effects = mod),
        "Missing actors"
    )
})

test_that("expected output from difference()", {
    # Expected standard output
    out <- list(
        effect = "difference", variable = "extraversion", x = NULL,
        scaling = 2
    )
    expect_equal(difference(variable = "extraversion"), out)

    # Expected output with absolute = FALSE
    out$scaling <- 1
    expect_equal(difference(variable = "extraversion", absolute = FALSE), out)

    # Expected output with "std" scaling
    out$scaling <- 4
    expect_equal(difference(variable = "extraversion", scaling = "std"), out)

    # Expected output with "std" scaling and absolute = FALSE
    out$scaling <- 3
    expect_equal(
        difference(
            variable = "extraversion", scaling = "std",
            absolute = FALSE
        ), out
    )

    # Expected output with object supplied to "attributes" argument
    out$scaling <- 2
    out$x <- info[, c("name", "time", "extraversion")]
    expect_equal(difference(variable = "extraversion", attributes = info), out)
})

test_that("expected statistic tie-oriented model", {
    set.seed(191)
    info$x <- sample(1:5, size = nrow(info), replace = TRUE)
    mod <- ~ difference("x")
    reh_tie <- remify::remify(history, model = "tie")
    tomres <- remstats(reh = reh_tie, tie_effects = mod, attributes = info)

    # Expected name of the statistic
    expect_equal(dimnames(tomres$statistics)[[3]][2], "difference_x")

    # The first 40 rows are expected to be equal to the following row
    first_info <- subset(info, time == 0)
    riskset <- tomres$riskset
    stat1 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(first_info$x[first_info$name == sender] -
            first_info$x[first_info$name == receiver])
    }))
    expect_true(all(sapply(1:40, function(x) {
        all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 41 to 71 are expected to be equal to the following row
    second_info <- subset(info, time == 9432)
    stat2 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(second_info$x[second_info$name == sender] -
            second_info$x[second_info$name == receiver])
    }))
    expect_true(all(sapply(41:71, function(x) {
        all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 72 to 115 are expected to be equal to the following row
    third_info <- subset(info, time == 18864)
    stat3 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(third_info$x[third_info$name == sender] -
            third_info$x[third_info$name == receiver])
    }))
    expect_true(all(sapply(72:115, function(x) {
        all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Repeat for "std" scaling
    mod <- ~ difference("x", scaling = "std")
    tomres <- remstats(reh = reh_tie, tie_effects = mod, attributes = info)

    # Expected name of the statistic
    expect_equal(dimnames(tomres$statistics)[[3]][2], "difference_x")

    # The first 40 rows are expected to be equal to the following row
    stat1 <- scale(stat1)
    expect_true(all(sapply(1:40, function(x) {
        all.equal(as.numeric(stat1),
            tomres$statistics[x, , 2],
            check.attributes = FALSE
        )
    })))

    # Rows 41 to 71 are expected to be equal to the following row
    stat2 <- scale(stat2)
    expect_true(all(sapply(41:71, function(x) {
        all.equal(as.numeric(stat2),
            tomres$statistics[x, , 2],
            check.attributes = FALSE
        )
    })))

    # Rows 72 to 115 are expected to be equal to the following row
    stat3 <- scale(stat3)
    expect_true(all(sapply(72:115, function(x) {
        all.equal(as.numeric(stat3),
            tomres$statistics[x, , 2],
            check.attributes = FALSE
        )
    })))

    # Repeat for absolute = FALSE
    mod <- ~ difference("x", absolute = FALSE)
    tomres <- remstats(reh = reh_tie, tie_effects = mod, attributes = info)

    # Expected name of the statistic
    expect_equal(dimnames(tomres$statistics)[[3]][2], "difference_x")

    # The first 40 rows are expected to be equal to the following row
    stat1 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        first_info$x[first_info$name == sender] -
            first_info$x[first_info$name == receiver]
    }))
    expect_true(all(sapply(1:40, function(x) {
        all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 41 to 71 are expected to be equal to the following row
    stat2 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        second_info$x[second_info$name == sender] -
            second_info$x[second_info$name == receiver]
    }))
    expect_true(all(sapply(41:71, function(x) {
        all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 72 to 115 are expected to be equal to the following row
    stat3 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        third_info$x[third_info$name == sender] -
            third_info$x[third_info$name == receiver]
    }))
    expect_true(all(sapply(72:115, function(x) {
        all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Repeat for absolute = FALSE and "std" scaling
    mod <- ~ difference("x", scaling = "std", absolute = FALSE)
    tomres <- remstats(reh = reh_tie, tie_effects = mod, attributes = info)

    # Expected name of the statistic
    expect_equal(dimnames(tomres$statistics)[[3]][2], "difference_x")

    # The first 40 rows are expected to be equal to the following row
    stat1 <- scale(stat1)
    expect_true(all(sapply(1:40, function(x) {
        all.equal(as.numeric(stat1),
            tomres$statistics[x, , 2],
            check.attributes = FALSE
        )
    })))

    # Rows 41 to 71 are expected to be equal to the following row
    stat2 <- scale(stat2)
    expect_true(all(sapply(41:71, function(x) {
        all.equal(as.numeric(stat2),
            tomres$statistics[x, , 2],
            check.attributes = FALSE
        )
    })))

    # Rows 72 to 115 are expected to be equal to the following row
    stat3 <- scale(stat3)
    expect_true(all(sapply(72:115, function(x) {
        all.equal(as.numeric(stat3),
            tomres$statistics[x, , 2],
            check.attributes = FALSE
        )
    })))

    # Repeat for undirected events
    mod <- ~ difference("x")
    reh_undirected <- remify::remify(history, model = "tie", 
        directed = FALSE)
    tomres <- remstats(
        reh = reh_undirected, tie_effects = mod, attributes = info
    )

    # Expected name of the statistic
    expect_equal(dimnames(tomres$statistics)[[3]][2], "difference_x")

    # The first 40 rows are expected to be equal to the following row
    riskset <- tomres$riskset
    stat1 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(first_info$x[first_info$name == sender] -
            first_info$x[first_info$name == receiver])
    }))
    expect_true(all(sapply(1:40, function(x) {
        all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 41 to 71 are expected to be equal to the following row
    stat2 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(second_info$x[second_info$name == sender] -
            second_info$x[second_info$name == receiver])
    }))
    expect_true(all(sapply(41:71, function(x) {
        all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 72 to 115 are expected to be equal to the following row
    stat3 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(third_info$x[third_info$name == sender] -
            third_info$x[third_info$name == receiver])
    }))
    expect_true(all(sapply(72:115, function(x) {
        all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Repeat for typed events
    history$type <- history$setting
    tomres <- remstats(reh = reh_tie, tie_effects = mod, attributes = info)

    # Expected name of the statistic
    expect_equal(dimnames(tomres$statistics)[[3]][2], "difference_x")

    # The first 40 rows are expected to be equal to the following row
    riskset <- tomres$riskset
    stat1 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(first_info$x[first_info$name == sender] -
            first_info$x[first_info$name == receiver])
    }))
    expect_true(all(sapply(1:40, function(x) {
        all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 41 to 71 are expected to be equal to the following row
    stat2 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(second_info$x[second_info$name == sender] -
            second_info$x[second_info$name == receiver])
    }))
    expect_true(all(sapply(41:71, function(x) {
        all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))

    # Rows 72 to 115 are expected to be equal to the following row
    stat3 <- as.numeric(apply(riskset, 1, function(x) {
        sender <- as.numeric(x[1])
        receiver <- as.numeric(x[2])
        abs(third_info$x[third_info$name == sender] -
            third_info$x[third_info$name == receiver])
    }))
    expect_true(all(sapply(72:115, function(x) {
        all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
    })))
})

test_that("expected statistic actor-oriented model", {
    set.seed(191)
    info$x <- sample(1:5, size = nrow(info), replace = T)
    mod <- ~ difference("x")
    reh_actor <- remify::remify(history, model = "actor")
    actors <- attr(reh_actor, "dictionary")$actors
    aomres <- remstats(
        reh = reh_actor, receiver_effects = mod, attributes = info
    )

    # Expected name of the statistic
    expect_equal(
        dimnames(aomres$receiver_stats)[[3]][1],
        "difference_x"
    )

    # The first 40 rows are expected to be equal to the following row
    first_info <- subset(info, time == 0)
    stat1 <- lapply(1:40, function(i) {
        x <- history[i, ]
        sender <- as.numeric(x[2])
        as.numeric(sapply(actors[,1], function(y) {
            abs(first_info$x[first_info$name == sender] -
                first_info$x[first_info$name == as.numeric(y)])
        }))
    })
    stat1 <- do.call(rbind, stat1)
    expect_equal(stat1, aomres$receiver_stats[1:40, , 1])

    # Rows 41 to 71 are expected to be equal to the following row
    second_info <- subset(info, time == 9432)
    stat2 <- lapply(41:71, function(i) {
        x <- history[i, ]
        sender <- as.numeric(x[2])
        as.numeric(sapply(actors[,1], function(y) {
            abs(second_info$x[first_info$name == sender] -
                second_info$x[first_info$name == as.numeric(y)])
        }))
    })
    stat2 <- do.call(rbind, stat2)
    expect_equal(stat2, aomres$receiver_stats[41:71, , 1])

    # Rows 72 to 115 are expected to be equal to the following row
    third_info <- subset(info, time == 18864)
    stat3 <- lapply(72:115, function(i) {
        x <- history[i, ]
        sender <- as.numeric(x[2])
        as.numeric(sapply(actors[,1], function(y) {
            abs(third_info$x[first_info$name == sender] -
                third_info$x[first_info$name == as.numeric(y)])
        }))
    })
    stat3 <- do.call(rbind, stat3)
    expect_equal(stat3, aomres$receiver_stats[72:115, , 1])

    # Repeat for scaling = "std" -----------------------------------------------
    mod <- ~ difference("x", scaling = "std")
    aomres <- remstats(
        reh = reh_actor, receiver_effects = mod, attributes = info
    )

    # Expected name of the statistic
    expect_equal(
        dimnames(aomres$receiver_stats)[[3]][1],
        "difference_x"
    )

    # The first 40 rows are expected to be equal to the following row
    std_stat1 <- t(sapply(1:40, function(x) {
        # Scale stat 1 without receiver
        sender <- which(actors[,1] == history$actor1[x])
        std_stat <- scale(stat1[x, -sender])
        std_stat <- append(std_stat, 0, after = sender - 1)
        std_stat
    }))

    expect_equal(aomres$receiver_stats[1:40, , ], std_stat1)

    # Rows 41 to 71 are expected to be equal to the following row
    std_stat2 <- t(sapply(41:71, function(x) {
        # Scale stat 2 without receiver
        sender <- which(actors[,1] == history$actor1[x])
        std_stat <- scale(stat2[x - 40, -sender])
        std_stat <- append(std_stat, 0, after = sender - 1)
        std_stat
    }))

    expect_equal(aomres$receiver_stats[41:71, , ], std_stat2)

    # Rows 72 to 115 are expected to be equal to the following row
    std_stat3 <- t(sapply(72:115, function(x) {
        # Scale stat 2 without receiver
        sender <- which(actors[,1] == history$actor1[x])
        std_stat <- scale(stat3[x - 71, -sender])
        std_stat <- append(std_stat, 0, after = sender - 1)
        std_stat
    }))

    expect_equal(aomres$receiver_stats[72:115, , ], std_stat3)

    # Repeat for absolute = FALSE ----------------------------------------------
    mod <- ~ difference("x", absolute = FALSE)
    aomres <- remstats(
        reh = reh_actor, receiver_effects = mod, attributes = info
    )

    # Expected name of the statistic
    expect_equal(
        dimnames(aomres$receiver_stats)[[3]][1],
        "difference_x"
    )

    # The first 40 rows are expected to be equal to the following row
    first_info <- subset(info, time == 0)
    stat1 <- lapply(1:40, function(i) {
        x <- history[i, ]
        sender <- as.numeric(x[2])
        as.numeric(sapply(actors[,1], function(y) {
            (first_info$x[first_info$name == sender] -
                first_info$x[first_info$name == as.numeric(y)])
        }))
    })
    stat1 <- do.call(rbind, stat1)
    expect_equal(stat1, aomres$receiver_stats[1:40, , 1])

    # Rows 41 to 71 are expected to be equal to the following row
    second_info <- subset(info, time == 9432)
    stat2 <- lapply(41:71, function(i) {
        x <- history[i, ]
        sender <- as.numeric(x[2])
        as.numeric(sapply(actors[,1], function(y) {
            (second_info$x[first_info$name == sender] -
                second_info$x[first_info$name == as.numeric(y)])
        }))
    })
    stat2 <- do.call(rbind, stat2)
    expect_equal(stat2, aomres$receiver_stats[41:71, , 1])

    # Rows 72 to 115 are expected to be equal to the following row
    third_info <- subset(info, time == 18864)
    stat3 <- lapply(72:115, function(i) {
        x <- history[i, ]
        sender <- as.numeric(x[2])
        as.numeric(sapply(actors[,1], function(y) {
            (third_info$x[first_info$name == sender] -
                third_info$x[first_info$name == as.numeric(y)])
        }))
    })
    stat3 <- do.call(rbind, stat3)
    expect_equal(stat3, aomres$receiver_stats[72:115, , 1])

    # Repeat for absolute = FALSE and scaling = "std" --------------------------
    mod <- ~ difference("x", absolute = FALSE, scaling = "std")
    aomres <- remstats(
        reh = reh_actor, receiver_effects = mod, attributes = info
    )

    # Expected name of the statistic
    expect_equal(
        dimnames(aomres$receiver_stats)[[3]][1],
        "difference_x"
    )

    # The first 40 rows are expected to be equal to the following row
    std_stat1 <- t(sapply(1:40, function(x) {
        # Scale stat 1 without receiver
        sender <- which(actors[,1] == history$actor1[x])
        std_stat <- scale(stat1[x, -sender])
        std_stat <- append(std_stat, 0, after = sender - 1)
        std_stat
    }))

    expect_equal(aomres$receiver_stats[1:40, , ], std_stat1)

    # Rows 41 to 71 are expected to be equal to the following row
    std_stat2 <- t(sapply(41:71, function(x) {
        # Scale stat 2 without receiver
        sender <- which(actors[,1] == history$actor1[x])
        std_stat <- scale(stat2[x - 40, -sender])
        std_stat <- append(std_stat, 0, after = sender - 1)
        std_stat
    }))

    expect_equal(aomres$receiver_stats[41:71, , ], std_stat2)

    # Rows 72 to 115 are expected to be equal to the following row
    std_stat3 <- t(sapply(72:115, function(x) {
        # Scale stat 2 without receiver
        sender <- which(actors[,1] == history$actor1[x])
        std_stat <- scale(stat3[x - 71, -sender])
        std_stat <- append(std_stat, 0, after = sender - 1)
        std_stat
    }))

    expect_equal(aomres$receiver_stats[72:115, , ], std_stat3)
})
