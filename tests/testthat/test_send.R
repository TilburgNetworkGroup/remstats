library(remstats)

test_that("expected errors and warnings", {
  # Expected error for missing argument "variable"
  expect_error(send(), "missing")

  # Expected errors for wrong "variable"
  expect_error(
    send(variable = "test", attributes = info),
    "not in attributes"
  )

  mod <- ~ send(variable = "test")
  expect_error(
    remstats(reh = history, tie_effects = mod, attributes = info),
    "not in attributes"
  )

  expect_error(
    remstats(reh = history, sender_effects = mod, attributes = info),
    "not in attributes"
  )

  # Expected errors for missing time variable
  expect_error(
    send(variable = "extraversion", attributes = info[, -2]),
    "time variable is missing"
  )

  mod <- ~ send(variable = "extraversion")
  attr <- info[, -2]
  expect_error(
    remstats(reh = history, tie_effects = mod, attributes = attr),
    "time variable is missing"
  )

  expect_error(
    remstats(reh = history, sender_effects = mod, attributes = attr),
    "time variable is missing"
  )

  # Expected errors for undirected events
  expect_error(
    remstats(
      reh = history, tie_effects = mod, attributes = info,
      directed = FALSE
    ),
    "defined for undirected events"
  )

  # Expected errors for receiver effects
  expect_error(
    remstats(reh = history, receiver_effects = mod, attributes = info),
    "not defined for the receiver choice model"
  )

  # Expect warning for missing values
  attr <- info
  attr$extraversion[1] <- NA
  expect_warning(
    send(variable = "extraversion", attributes = attr),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = history, tie_effects = mod, attributes = attr),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = history, sender_effects = mod, attributes = attr),
    "unexpected behavior"
  )

  # Expect error for missing time values
  attr <- info
  attr$time[1] <- NA
  expect_error(
    send(variable = "extraversion", attributes = attr),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = history, tie_effects = mod, attributes = attr),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = history, sender_effects = mod, attributes = attr),
    "cannot have missing values"
  )

  # Expect warning for extra actor
  attr <- rbind(info, info[1, ])
  attr[nrow(attr), 1] <- 999
  expect_warning(
    remstats(reh = history, tie_effects = mod, attributes = attr),
    "actors that are not in the risk set"
  )

  expect_warning(
    remstats(reh = history, sender_effects = mod, attributes = attr),
    "actors that are not in the risk set"
  )

  mod <- ~ send(variable = "extraversion", attributes = attr)
  expect_warning(
    remstats(reh = history, tie_effects = mod),
    "actors that are not in the risk set"
  )

  expect_warning(
    remstats(reh = history, sender_effects = mod),
    "actors that are not in the risk set"
  )

  # Missing actor
  attr <- subset(info, name != 101)
  mod <- ~ send(variable = "extraversion")
  expect_error(
    remstats(reh = history, tie_effects = mod, attributes = attr),
    "Missing actors"
  )

  expect_error(
    remstats(reh = history, sender_effects = mod, attributes = attr),
    "Missing actors"
  )

  mod <- ~ send(variable = "extraversion", attributes = attr)
  expect_error(
    remstats(reh = history, tie_effects = mod),
    "Missing actors"
  )

  expect_error(
    remstats(reh = history, sender_effects = mod),
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

  # Expected output with object supplied to "attributes" argument
  out$scaling <- 1
  out$x <- info[, c("name", "time", "extraversion")]
  expect_equal(send(variable = "extraversion", attributes = info), out)
})

test_that("expected statistic tie-oriented model", {
  mod <- ~ send("extraversion")
  tomres <- remstats(reh = history, tie_effects = mod, attributes = info)

  # Expected name of the statistic
  expect_equal(dimnames(tomres$statistics)[[3]][2], "send_extraversion")

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(tomres$riskset$sender, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(tomres$riskset$sender, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(tomres$riskset$sender, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Repeat for standardized effects
  mod <- ~ send("extraversion", scaling = "std")
  tomres <- remstats(reh = history, tie_effects = mod, attributes = info)

  # Expected name of the statistic
  expect_equal(dimnames(tomres$statistics)[[3]][2], "send_extraversion")

  # The first 40 rows are expected to be equal to the following row
  stat1 <- as.numeric(scale(stat1))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(scale(stat2))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(scale(stat3))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Repeat for typed events
  mod <- ~ send("extraversion")
  history$type <- history$setting
  tomres <- remstats(reh = history, tie_effects = mod, attributes = info)

  # Expected name of the statistic
  expect_equal(dimnames(tomres$statistics)[[3]][2], "send_extraversion")

  # The first 40 rows are expected to be equal to the following row
  stat1 <- first_info$extraversion[match(tomres$riskset$sender, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- second_info$extraversion[match(tomres$riskset$sender, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- third_info$extraversion[match(tomres$riskset$sender, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
})

test_that("expected statistic actor-oriented model", {
  mod <- ~ send("extraversion")
  aomres <- remstats(
    reh = history, sender_effects = mod, attributes = info
  )

  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$statistics$sender_stats)[[3]][2],
    "send_extraversion"
  )

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(aomres$actors, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, aomres$statistics$sender_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(aomres$actors, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, aomres$statistics$sender_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(aomres$actors, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, aomres$statistics$sender_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Repeat for standardized effects
  mod <- ~ send("extraversion", scaling = "std")
  aomres <- remstats(reh = history, sender_effects = mod, attributes = info)

  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$statistics$sender_stats)[[3]][2],
    "send_extraversion"
  )

  # The first 40 rows are expected to be equal to the following row
  stat1 <- as.numeric(scale(stat1))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, aomres$statistics$sender_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(scale(stat2))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, aomres$statistics$sender_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(scale(stat3))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, aomres$statistics$sender_stats[x, , 2],
      check.attributes = FALSE
    )
  })))
})
