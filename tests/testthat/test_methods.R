library(remstats)

test_that("summary method", {
    reh_tie <- remify::remify(history, model = "tie")
    tie_stats <- remstats(reh = reh_tie, tie_effects = ~ 1)
    expect_silent(summary(tie_stats))

    tie_stats <- tomstats(reh = reh_tie, effects = ~ 1)
    expect_silent(summary(tie_stats))

    reh_actor <- remify::remify(history, model = "actor")
    actor_stats <- remstats(reh = reh_actor, sender_effects = ~ 1)
    expect_silent(summary(actor_stats)) 

    actor_stats <- remstats(reh = reh_actor, receiver_effects = ~ inertia())
    expect_silent(summary(actor_stats)) 

    actor_stats <- remstats(reh = reh_actor, sender_effects = ~ 1, 
      receiver_effects = ~ inertia())
    expect_silent(summary(actor_stats)) 

    actor_stats <- aomstats(reh = reh_actor, sender_effects = ~ 1)
    expect_silent(summary(actor_stats)) 

    actor_stats <- aomstats(reh = reh_actor, receiver_effects = ~ inertia())
    expect_silent(summary(actor_stats)) 

    actor_stats <- aomstats(reh = reh_actor, sender_effects = ~ 1, 
      receiver_effects = ~ inertia())
    expect_silent(summary(actor_stats)) 
})