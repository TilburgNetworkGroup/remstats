library(remstats)

test_that("inertia", {
    data(history)
    history$weight <- 1

    effects <- ~ inertia()
    reh_tie <- remify::remify(history, model = "tie")
    reh_actor <- remify::remify(history, model = "actor")
    tie_stats <- tomstats(effects, reh = reh_tie)
    aomres <- aomstats(receiver_effects = effects, reh = reh_actor)

    expect_equal(rowSums(tie_stats[, , 2]), 0:(nrow(history) - 1))
    expect_true(all(sapply(1:nrow(aomres$receiver_stats), function(i) {
        aomres$receiver_stats[i, , ] %in% tie_stats[i, , 2]
    })))

    effects <- ~ inertia(scaling = "prop") + inertia() + outdegreeSender()
    receiver_effects <- ~ inertia(scaling = "prop") + inertia()
    sender_effects <- ~ outdegreeSender()
    tie_stats <- tomstats(effects, reh = reh_tie)
    aomres <- aomstats(
        receiver_effects = receiver_effects,
        sender_effects = sender_effects,
        reh = reh_actor
    )

    temp <- tie_stats[, , 3] / tie_stats[, , 4]
    temp[is.na(temp)] <- 1 / 9
    expect_equal(tie_stats[, , 2], temp)
    expect_equal(rowSums(aomres$receiver_stats[, , 1]), rep(1, nrow(history)))

    effects <- ~ inertia(scaling = "std")
    tie_stats <- tomstats(effects, reh = reh_tie)
    aomres <- aomstats(receiver_effects = effects, reh = reh_actor)

    expect_equal(rowMeans(tie_stats[, , 2]), rep(0, nrow(history)))
    expect_equal(rowMeans(aomres$receiver_stats), rep(0, nrow(history)))

    colnames(history)[4] <- "type"
    effects <- ~ inertia(consider_type = TRUE)
    tie_stats <- tomstats(effects, reh = reh_tie)
    expect_equal(rowSums(tie_stats[, , 2]), 0:(nrow(history) - 1))
})
