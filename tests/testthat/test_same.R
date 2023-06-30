library(remstats)

test_that("expected errors and warnings", {
  # Expected error for missing argument "variable"
  expect_error(same(), "missing")

  # Expected errors for wrong "variable"
  expect_error(
    same(variable = "test", attr_data = info),
    "not in attr_data"
  )

  mod <- ~ same(variable = "test")
  reh_tie <- remify::remify(history, model = "tie")
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = info),
    "not in attr_data"
  )

  reh_actor <- remify::remify(history, model = "actor")
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = info),
    "not in attr_data"
  )

  # Expected errors for missing time variable
  expect_error(
    same(variable = "extraversion", attr_data = info[, -2]),
    "time variable is missing"
  )

  mod <- ~ same(variable = "extraversion")
  attr_object <- info[, -2]
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "time variable is missing"
  )

  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
    "time variable is missing"
  )

  # Expected errors for sender effects
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod, attr_data = info),
    "not defined for the sender activity model"
  )

  # Expect warning for missing values
  attr_object <- info
  attr_object$extraversion[1] <- NA
  expect_warning(
    same(variable = "extraversion", attr_data = attr_object),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
    "unexpected behavior"
  )

  # Expect error for missing time values
  attr_object <- info
  attr_object$time[1] <- NA
  expect_error(
    same(variable = "extraversion", attr_data = attr_object),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
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
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
    "actors that are not in the risk set"
  )

  mod <- ~ same(variable = "extraversion", attr_data = attr_object)
  expect_warning(
    remstats(reh = reh_tie, tie_effects = mod),
    "actors that are not in the risk set"
  )

  expect_warning(
    remstats(reh = reh_actor, receiver_effects = mod),
    "actors that are not in the risk set"
  )

  # Missing actor
  attr_object <- subset(info, name != 101)
  mod <- ~ same(variable = "extraversion")
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "Missing actors"
  )

  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
    "Missing actors"
  )

  mod <- ~ same(variable = "extraversion", attr_data = attr_object)
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "Missing actors"
  )

  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod),
    "Missing actors"
  )
})

test_that("expected output from same()", {
  # Expected standard output
  out <- list(effect = "same", variable = "extraversion", x = NULL, scaling = 1)
  expect_equal(same(variable = "extraversion"), out)

  # Expected output with object supplied to "attr_data" argument
  out$x <- info[, c("name", "time", "extraversion")]
  expect_equal(same(variable = "extraversion", attr_data = info), out)
})

test_that("expected statistic tie-oriented model", {
  set.seed(191)
  info$x <- sample(1:5, size = nrow(info), replace = T)
  mod <- ~ same("x")
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "same_x")

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  riskset <- attr(tie_stats, "riskset")
  stat1 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    first_info$x[first_info$name == sender] ==
      first_info$x[first_info$name == receiver]
  }))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    second_info$x[second_info$name == sender] ==
      second_info$x[second_info$name == receiver]
  }))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    third_info$x[third_info$name == sender] ==
      third_info$x[third_info$name == receiver]
  }))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Repeat for undirected events
  reh_undirected <- remify::remify(history, model = "tie", directed = FALSE)
  tie_stats <- remstats(reh = reh_undirected, tie_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "same_x")

  # The first 40 rows are expected to be equal to the following row
  riskset <- attr(tie_stats, "riskset")
  stat1 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    first_info$x[first_info$name == sender] ==
      first_info$x[first_info$name == receiver]
  }))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    second_info$x[second_info$name == sender] ==
      second_info$x[second_info$name == receiver]
  }))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    third_info$x[third_info$name == sender] ==
      third_info$x[third_info$name == receiver]
  }))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Repeat for typed events
  history$type <- history$setting
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "same_x")

  # The first 40 rows are expected to be equal to the following row
  riskset <- attr(tie_stats, "riskset")
  stat1 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    first_info$x[first_info$name == sender] ==
      first_info$x[first_info$name == receiver]
  }))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    second_info$x[second_info$name == sender] ==
      second_info$x[second_info$name == receiver]
  }))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    third_info$x[third_info$name == sender] ==
      third_info$x[third_info$name == receiver]
  }))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
})

test_that("expected statistic actor-oriented model", {
  set.seed(191)
  info$x <- sample(1:5, size = nrow(info), replace = T)
  mod <- ~ same("x")
  reh_actor <- remify::remify(history, model = "actor")
  actors <- attr(reh_actor, "dictionary")$actors
  aomres <- remstats(
    reh = reh_actor, receiver_effects = mod, attr_data = info
  )

  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$receiver_stats)[[3]][1],
    "same_x"
  )

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- lapply(1:40, function(i) {
    x <- history[i, ]
    sender <- as.numeric(x[2])
    as.numeric(sapply(actors[, 1], function(y) {
      first_info$x[first_info$name == sender] ==
        first_info$x[first_info$name == as.numeric(y)]
    }))
  })
  stat1 <- do.call(rbind, stat1)
  expect_equal(stat1, aomres$receiver_stats[1:40, , 1])

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- lapply(41:71, function(i) {
    x <- history[i, ]
    sender <- as.numeric(x[2])
    as.numeric(sapply(actors[, 1], function(y) {
      second_info$x[first_info$name == sender] ==
        second_info$x[first_info$name == as.numeric(y)]
    }))
  })
  stat2 <- do.call(rbind, stat2)
  expect_equal(stat2, aomres$receiver_stats[41:71, , 1])

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- lapply(72:115, function(i) {
    x <- history[i, ]
    sender <- as.numeric(x[2])
    as.numeric(sapply(actors[, 1], function(y) {
      third_info$x[first_info$name == sender] ==
        third_info$x[first_info$name == as.numeric(y)]
    }))
  })
  stat3 <- do.call(rbind, stat3)
  expect_equal(stat3, aomres$receiver_stats[72:115, , 1])
})
