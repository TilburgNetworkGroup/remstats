library(remstats)

test_that("expected errors and warnings", {
    data(history)
    
    # Expected errors for sender effects
    work <- ifelse(history$setting == "work", 1, 0)
    mod <- ~ event(x = work)
    reh_actor <- remify::remify(history, model = "actor")
    expect_error(
        remstats(reh = reh_actor, sender_effects = mod),
        "not defined for the sender activity model"
    )

    # Expected errors for receiver effects
    expect_error(
        remstats(reh = reh_actor, receiver_effects = mod),
        "not defined for the receiver choice model"
    )

    # Expected error for unequal number of rows
    temp <- work[1:10]
    mod <- ~ event(x = temp)
    reh <- remify::remify(history, model = "tie")
    expect_error(
        remstats(reh = reh, tie_effects = mod),
    )


    # Expected error for missing values
    temp <- work
    temp[1] <- NA

    expect_error(event(temp))

    expect_error(
        remstats(reh = reh, tie_effects = mod),
    )
})

test_that("expected output from event()", {
    data(history)
    work <- ifelse(history$setting == "work", 1, 0)
    
    # Expected standard output
    out <- list(
        effect = "event", x = work, variable = NULL, scaling = 1
    )
    expect_equal(event(x = work), out)

    # Expected output with variableName
    out$variable <- "test"
    expect_equal(event(x = work, variableName = "test"), out)
})

test_that("expected statistic", {
    data(history)
    work <- ifelse(history$setting == "work", 1, 0)
    
    # Expected name of the statistic
    mod <- ~ event(x = work)
    reh <- remify::remify(history, model = "tie")
    tie_stats <- remstats(reh = reh, tie_effects = mod)
    expect_equal(dimnames(tie_stats)[[3]][2], "event")

    extra <- work + 1
    mod <- ~ event(x = work) + event(x = extra)
    tie_stats <- remstats(reh = reh, tie_effects = mod)
    expect_equal(dimnames(tie_stats)[[3]][2], "event1")
    expect_equal(dimnames(tie_stats)[[3]][3], "event2")

    mod <- ~ event(x = work, variableName = "work")
    tie_stats <- remstats(reh = reh, tie_effects = mod)
    expect_equal(dimnames(tie_stats)[[3]][2], "work")

    mod <- ~ event(x = work, variableName = "test") +
        event(x = work, variableName = "check")
    tie_stats <- remstats(reh = reh, tie_effects = mod)
    expect_equal(dimnames(tie_stats)[[3]][2], "test")
    expect_equal(dimnames(tie_stats)[[3]][3], "check")

    # Expected statistic
    mod <- ~ event(x = work)
    tie_stats <- remstats(reh = reh, tie_effects = mod)
    stat <- replicate(n = nrow(attr(tie_stats, "riskset")), work)
    expect_equal(stat, tie_stats[, , 2])

    # Repeat with start and stop values
    tie_stats <- remstats(
        reh = reh, tie_effects = mod,
        start = 5, stop = 10
    )
    expect_equal(stat[5:10, ], tie_stats[, , 2])
})
