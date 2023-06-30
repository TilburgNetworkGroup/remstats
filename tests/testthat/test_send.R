library(remstats)

test_that("expected errors and warnings", {
  # Expected error for missing argument "variable"
  expect_error(send(), "missing")

  # Expected errors for wrong "variable"
  expect_error(
    send(variable = "test", attr_data = info),
    "not in attr_data"
  )

  mod <- ~ send(variable = "test")
  reh_tie <- remify::remify(history, model = "tie")
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = info),
    "not in attr_data"
  )

  reh_actor <- remify::remify(history, model = "actor")
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = info),
    "not in attr_data"
  )

  # Expected errors for missing time variable
  expect_error(
    send(variable = "extraversion", attr_data = info[, -2]),
    "time variable is missing"
  )

  mod <- ~ send(variable = "extraversion")
  attr_object <- info[, -2]
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "time variable is missing"
  )

  expect_error(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = attr_object),
    "time variable is missing"
  )

  # Expected errors for undirected events
  reh_undirected <- remify::remify(history, model = "tie", directed = FALSE)
  expect_error(
    remstats(reh = reh_undirected, tie_effects = mod, attr_data = info),
    "defined for undirected events"
  )

  # Expected errors for receiver effects
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = info),
    "not defined for the receiver choice model"
  )

  # Expect warning for missing values
  attr_object <- info
  attr_object$extraversion[1] <- NA
  expect_warning(
    send(variable = "extraversion", attr_data = attr_object),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = attr_object),
    "unexpected behavior"
  )

  # Expect error for missing time values
  attr_object <- info
  attr_object$time[1] <- NA
  expect_error(
    send(variable = "extraversion", attr_data = attr_object),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = attr_object),
    "cannot have missing values"
  )

  # Expect warning for extra actor
  attr_object <- rbind(info, info[1, ])
  attr_object[nrow(attr_object), 1] <- 999
  expect_warning(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "actors that are not in the risk set"
  )

  expect_warning(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = attr_object),
    "actors that are not in the risk set"
  )

  mod <- ~ send(variable = "extraversion", attr_data = attr_object)
  expect_warning(
    remstats(reh = reh_tie, tie_effects = mod),
    "actors that are not in the risk set"
  )

  expect_warning(
    remstats(reh = reh_actor, sender_effects = mod),
    "actors that are not in the risk set"
  )

  # Missing actor
  attr_object <- subset(info, name != 101)
  mod <- ~ send(variable = "extraversion")
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "Missing actors"
  )

  expect_error(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = attr_object),
    "Missing actors"
  )

  mod <- ~ send(variable = "extraversion", attr_data = attr_object)
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "Missing actors"
  )

  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "Missing actors"
  )
})

test_that("expected output from send()", {
  # Expected standard output
  out <- list(effect = "send", variable = "extraversion", x = NULL, scaling = 1)
  expect_equal(send(variable = "extraversion"), out)

  # Expected output with "std" scaling
  out$scaling <- 2
  expect_equal(send(variable = "extraversion", scaling = "std"), out)

  # Expected output with object supplied to "attr_data" argument
  out$scaling <- 1
  out$x <- info[, c("name", "time", "extraversion")]
  expect_equal(send(variable = "extraversion", attr_data = info), out)
})

test_that("expected statistic tie-oriented model", {
  mod <- ~ send("extraversion")
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "send_extraversion")

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(attr(tie_stats, "riskset")$sender, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(attr(tie_stats, "riskset")$sender, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(attr(tie_stats, "riskset")$sender, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Repeat for standardized effects
  mod <- ~ send("extraversion", scaling = "std")
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "send_extraversion")

  # The first 40 rows are expected to be equal to the following row
  stat1 <- as.numeric(scale(stat1))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(scale(stat2))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(scale(stat3))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Repeat for typed events
  mod <- ~ send("extraversion")
  history$type <- history$setting
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "send_extraversion")

  # The first 40 rows are expected to be equal to the following row
  stat1 <- first_info$extraversion[match(attr(tie_stats, "riskset")$sender, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- second_info$extraversion[match(attr(tie_stats, "riskset")$sender, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- third_info$extraversion[match(attr(tie_stats, "riskset")$sender, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
})

test_that("expected statistic actor-oriented model", {
  mod <- ~ send("extraversion")
  reh_actor <- remify::remify(history, model = "actor")
  actors <- attr(reh_actor, "dictionary")$actors
  aomres <- remstats(
    reh = reh_actor, sender_effects = mod, attr_data = info
  )

  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$sender_stats)[[3]][2],
    "send_extraversion"
  )

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(actors[, 1], first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, aomres$sender_stats[x, , 2],
      check.attr_data = FALSE
    )
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(actors[, 1], second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, aomres$sender_stats[x, , 2],
      check.attr_data = FALSE
    )
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(actors[, 1], third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, aomres$sender_stats[x, , 2],
      check.attr_data = FALSE
    )
  })))

  # Repeat for standardized effects
  mod <- ~ send("extraversion", scaling = "std")
  aomres <- remstats(reh = reh_actor, sender_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$sender_stats)[[3]][2],
    "send_extraversion"
  )

  # The first 40 rows are expected to be equal to the following row
  stat1 <- as.numeric(scale(stat1))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, aomres$sender_stats[x, , 2],
      check.attr_data = FALSE
    )
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(scale(stat2))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, aomres$sender_stats[x, , 2],
      check.attr_data = FALSE
    )
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(scale(stat3))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, aomres$sender_stats[x, , 2],
      check.attr_data = FALSE
    )
  })))
})
