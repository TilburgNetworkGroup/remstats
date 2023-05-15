library(remstats)

test_that("expected errors and warnings", {
  reh_tie <- remify::remify(history, model = "tie")
  expect_error(
    remstats(reh_tie),
    "Supply effects"
    )

  expect_error(
    remstats(reh_tie, tie_effects = ~ inertia(), 
      receiver_effects = ~ inertia()),
    "Supply effects"
    )

  reh_actor <- remify::remify(history, directed = FALSE, model = "actor")
  expect_error(
    remstats(reh_actor, receiver_effects = ~ inertia()),
    "Undirected events are not defined"
    )

  expect_error(
    remstats(reh_tie, receiver_effects = ~ inertia()), 
    "The reh object should be prepared"
  )

  expect_error(
    remstats(reh_actor, tie_effects = ~ 1), 
    "The reh object should be prepared"
  )
})

test_that("output tomstats", {
  reh_tie <- remify::remify(history, model = "tie")
  stats <- tomstats(~ send("extraversion"):inertia(),
    reh = reh_tie,
    attributes = info,
    get_adjmat = TRUE
  )

  riskset <- attr(stats, "riskset")
  adjmat <- attr(stats, "adjmat")

  expect_output(str(stats), "List of 3")
  expect_equal(dim(stats), c(nrow(reh_tie$edgelist), nrow(riskset), 4))
  expect_equal(dim(adjmat), c(nrow(reh_tie$edgelist), nrow(riskset)))
})

test_that("output aomstats", {
  reh_actor <- remify::remify(history, model = "actor")
  out <- aomstats(
    sender_effects = ~ send("extraversion"),
    receiver_effects = ~ inertia(), reh = reh_actor, attributes = info
  )

  rstats <- out$sender_stats
  cstats <- out$receiver_stats

  expect_output(str(out), "List of 2")
  expect_equal(
    dim(rstats),
    c(nrow(reh_actor$edgelist), length(unique(info$name)), 2)
  )
  expect_equal(
    dim(cstats),
    c(nrow(reh_actor$edgelist), length(unique(info$name)), 1)
  )
})

test_that("output remstats", {
  # Tie-oriented model
  reh_tie <- remify::remify(history, model = "tie")
  stats <- remstats(
    tie_effects = ~ send("extraversion"):inertia(),
    reh = reh_tie, attributes = info, get_adjmat = TRUE
  )

  riskset <- attr(stats, "riskset")
  adjmat <- attr(stats, "adjmat")


  expect_output(str(stats), "List of 3")
  expect_equal(dim(stats), c(nrow(reh_tie$edgelist), nrow(riskset), 4))
  expect_equal(dim(adjmat), c(nrow(reh_tie$edgelist), nrow(riskset)))

  # Actor-oriented model
  reh_actor <- remify::remify(history, model = "actor")
  out <- remstats(
    sender_effects = ~ send("extraversion"),
    receiver_effects = ~ inertia(), reh = reh_actor, attributes = info
  )

  rstats <- out$sender_stats
  cstats <- out$receiver_stats

  expect_output(str(out), "List of 2")
  expect_equal(
    dim(rstats),
    c(nrow(reh_actor$edgelist), length(unique(info$name)), 2)
  )
  expect_equal(
    dim(cstats),
    c(nrow(reh_actor$edgelist), length(unique(info$name)), 1)
  )
})
