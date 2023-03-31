library(remstats)

test_that("expected errors and warnings", {
  reh_actor <- remify::remify(history, model = "actor")

  # Expected errors for sender effects
  mod <- ~ otp()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  mod <- ~ itp()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  mod <- ~ osp()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  mod <- ~ isp()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  # Expect errors for undirected events
  reh_tie <- remify::remify(history, model = "tie", directed = FALSE)

  mod <- ~ otp()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  mod <- ~ itp()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  mod <- ~ osp()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  mod <- ~ isp()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  # Expect errors for directed events
  reh_tie <- remify::remify(history, model = "tie")

  mod <- ~ sp()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "directed events"
  )

  mod <- ~ spUnique()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "directed events"
  )

})

test_that("expected output from otp() etc.", {
    # Expected standard output
  out <- list(effect = "otp", scaling = 1)
  expect_equal(otp(), out)

  out$effect <- "osp"
  expect_equal(osp(), out)

  out$effect <- "itp"
  expect_equal(itp(), out)

  out$effect <- "isp"
  expect_equal(isp(), out)

  out$effect <- "sp"
  expect_equal(sp(), out)

  out$effect <- "spUnique"
  expect_equal(spUnique(), out)

  # Expected output with consider_type = TRUE
  out$effect <- "otp.type"
  expect_equal(otp(consider_type = TRUE), out)

  out$effect <- "osp.type"
  expect_equal(osp(consider_type = TRUE), out)

  out$effect <- "isp.type"
  expect_equal(isp(consider_type = TRUE), out)

  out$effect <- "itp.type"
  expect_equal(itp(consider_type = TRUE), out)

  out$effect <- "sp.type"
  expect_equal(sp(consider_type = TRUE), out)

  out$effect <- "spUnique.type"
  expect_equal(spUnique(consider_type = TRUE), out)
})

test_that("expected statistic tie-oriented model", {
  # Directed events --------------------------------------
  history$weight <- 1
  reh_tie <- remify::remify(history, model = "tie")

  # Undirected events -----------------------------------
  reh_tie <- remify::remify(history, model = "tie", directed = FALSE)

  # Event types (directed and undirected) ----------------
  history$type <- history$setting
  reh_tie <- remify::remify(history, model = "tie")

})

test_that("expected statistic actor-oriented model", {
  reh_actor <- remify::remify(history, model = "actor")
  
  # Full memory --------------------------------------

  # Interval memory ----------------------------------

  # Decay memory -------------------------------------

})