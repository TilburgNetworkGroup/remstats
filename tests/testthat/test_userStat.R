library(remstats)

reh_tie <- remify::remify(history, model = "tie")
dummy <- remstats(reh = reh_tie, tie_effects = ~1)
riskset <- dummy$riskset
same_age <- as.numeric(
    apply(riskset, 1, function(x) {
        info$age[info$name == as.numeric(x[1]) & info$time == 0] ==
            info$age[info$name == as.numeric(x[2]) & info$time == 0]
    })
)
same_age <- t(replicate(n = nrow(history), same_age))

test_that("expected errors and warnings", {
    # Expected error for unequal number of rows
    temp <- same_age[1:10, ]
    mod <- ~ userStat(x = temp)

    expect_error(
        remstats(reh = reh_tie, tie_effects = mod),
        "does not match number of events"
    )
    # ADD for sender_effects and receiver_effects

    # Expected error for unequal number of columns
    temp <- same_age[, 1:10]
    mod <- ~ userStat(x = temp)

    expect_error(
        remstats(reh = reh_tie, tie_effects = mod),
        "does not match number of dyads"
    )
    # ADD for sender_effects and receiver_effects

    # Expected warning for missing values
    temp <- same_age
    temp[1, 1] <- NA

    expect_warning(userStat(x = temp), "missing values")

    expect_warning(
        remstats(reh = reh_tie, tie_effects = mod),
        "missing values"
    )
})

test_that("expected output from userStat()", {
    # Expected standard output
    out <- list(
        effect = "userStat", x = same_age, variable = NULL, scaling = 1
    )
    expect_equal(userStat(x = same_age), out)

    # Expected output with variableName
    out$variable <- "test"
    expect_equal(userStat(x = same_age, variableName = "test"), out)
})
