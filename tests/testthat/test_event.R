library(remstats)

work <- ifelse(history$setting == "work", 1, 0)

test_that("expected errors and warnings", {
    # Expected errors for sender effects
    mod <- ~ event(x = work)
    expect_error(
        remstats(reh = history, sender_effects = mod),
        "not defined for the sender activity model"
    )

    # Expected errors for receiver effects
    expect_error(
        remstats(reh = history, receiver_effects = mod),
        "not defined for the receiver choice model"
    )

    # Expected error for unequal number of rows
    temp <- work[1:10]
    mod <- ~ event(x = temp)

    expect_error(
        remstats(reh = history, tie_effects = mod),
        "does not match number of events"
    )


    # Expected error for missing values
    temp <- work
    temp[1] <- NA

    expect_error(event(temp), "missing values")

    expect_error(
        remstats(reh = history, tie_effects = mod),
        "missing values"
    )
})

test_that("expected output from event()", {
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
    # Expected name of the statistic
    mod <- ~ event(x = work)
    tomres <- remstats(reh = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "event")

    mod <- ~ event(x = work) + event(x = work + 1)
    tomres <- remstats(reh = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "event1")
    expect_equal(dimnames(tomres$statistics)[[3]][3], "event2")

    mod <- ~ event(x = work, variableName = "work")
    tomres <- remstats(reh = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "work")

    mod <- ~ event(x = work, variableName = "test") +
        event(x = work, variableName = "check")
    tomres <- remstats(reh = history, tie_effects = mod)
    expect_equal(dimnames(tomres$statistics)[[3]][2], "test")
    expect_equal(dimnames(tomres$statistics)[[3]][3], "check")

    # Expected statistic
    mod <- ~ event(x = work)
    tomres <- remstats(reh = history, tie_effects = mod)
    stat <- replicate(n = nrow(tomres$riskset), work)
    expect_equal(stat, tomres$statistics[, , 2])

    # Repeat with start and stop values
    tomres <- remstats(
        reh = history, tie_effects = mod,
        start = 5, stop = 10
    )
    expect_equal(stat[5:10, ], tomres$statistics[, , 2])
})
