library(remstats)

test_that("expected errors and warnings", {
  # Expected error for missing argument "variable"
  expect_error(maximum(), "missing")

  # Expected errors for wrong "variable"
  expect_error(
    maximum(variable = "test", attributes = info),
    "not in attributes"
  )

  mod <- ~ maximum(variable = "test")
  reh <- remify::remify(history, model = "tie")
  expect_error(
    remstats(reh = reh, tie_effects = mod, attributes = info),
    "not in attributes"
  )

  # Expected errors for missing time variable
  expect_error(
    maximum(variable = "extraversion", attributes = info[, -2]),
    "time variable is missing"
  )

  mod <- ~ maximum(variable = "extraversion")
  attr <- info[, -2]
  expect_error(
    remstats(reh = reh, tie_effects = mod, attributes = attr),
    "time variable is missing"
  )

  # Expected errors for actor-oriented model
  reh_actor <- remify::remify(history, model = "actor")
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod, attributes = info),
    "not defined for the sender activity model"
  )

  # Expect warning for missing values
  attr <- info
  attr$extraversion[1] <- NA
  expect_warning(
    maximum(variable = "extraversion", attributes = attr),
    "unexpected behavior"
  )

  expect_warning(
    remstats(reh = reh, tie_effects = mod, attributes = attr),
    "unexpected behavior"
  )

  # Expect error for missing time values
  attr <- info
  attr$time[1] <- NA
  expect_error(
    maximum(variable = "extraversion", attributes = attr),
    "cannot have missing values"
  )

  expect_error(
    remstats(reh = reh, tie_effects = mod, attributes = attr),
    "cannot have missing values"
  )

  # Expect warning for extra actor
  attr <- rbind(info, info[1, ])
  attr[nrow(attr), 1] <- 999
  expect_warning(
    remstats(reh = reh, tie_effects = mod, attributes = attr),
    "actors that are not in the risk set"
  )

  mod <- ~ maximum(variable = "extraversion", attributes = attr)
  expect_warning(
    remstats(reh = reh, tie_effects = mod),
    "actors that are not in the risk set"
  )

  # Missing actor
  attr <- subset(info, name != 101)
  mod <- ~ maximum(variable = "extraversion")
  expect_error(
    remstats(reh = reh, tie_effects = mod, attributes = attr),
    "Missing actors"
  )

  mod <- ~ maximum(variable = "extraversion", attributes = attr)
  expect_error(
    remstats(reh = reh, tie_effects = mod),
    "Missing actors"
  )
})

test_that("expected output from maximum()", {
  # Expected standard output
  out <- list(
    effect = "maximum", variable = "extraversion", x = NULL,
    scaling = 1
  )
  expect_equal(maximum(variable = "extraversion"), out)

  # Expected output with "std" scaling
  out$scaling <- 2
  expect_equal(maximum(variable = "extraversion", scaling = "std"), out)

  # Expected output with object supplied to "attributes" argument
  out$scaling <- 1
  out$x <- info[, c("name", "time", "extraversion")]
  expect_equal(maximum(variable = "extraversion", attributes = info), out)
})

test_that("expected statistic tie-oriented model", {
  mod <- ~ maximum("extraversion")
  reh <- remify::remify(history, model = "tie")
  tie_stats <- remstats(reh = reh, tie_effects = mod, attributes = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "maximum_extraversion")

  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  riskset <- attr(tie_stats, "riskset")
  stat1 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      first_info$extraversion[first_info$name == sender],
      first_info$extraversion[first_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      second_info$extraversion[second_info$name == sender],
      second_info$extraversion[second_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      third_info$extraversion[third_info$name == sender],
      third_info$extraversion[third_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Repeat for undirected events ---------------------------------------------
  mod <- ~ maximum("extraversion")
  reh_undirected <- remify::remify(history, model = "tie", directed = FALSE)
  tie_stats <- remstats(
    reh = reh_undirected, tie_effects = mod, attributes = info,
  )

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "maximum_extraversion")

  # The first 40 rows are expected to be equal to the following row
  riskset <- attr(tie_stats, "riskset")
  stat1 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      first_info$extraversion[first_info$name == sender],
      first_info$extraversion[first_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      second_info$extraversion[second_info$name == sender],
      second_info$extraversion[second_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      third_info$extraversion[third_info$name == sender],
      third_info$extraversion[third_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Repeat for "std" scaling -------------------------------------------------
  mod <- ~ maximum("extraversion", scaling = "std")
  tie_stats <- remstats(
    reh = reh_undirected, tie_effects = mod, attributes = info,
  )

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "maximum_extraversion")

  # The first 40 rows are expected to be equal to the following row
  stat1 <- scale(stat1)
  expect_true(all(sapply(1:40, function(x) {
    all.equal(as.numeric(stat1),
      tie_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- scale(stat2)
  expect_true(all(sapply(41:71, function(x) {
    all.equal(as.numeric(stat2),
      tie_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- scale(stat3)
  expect_true(all(sapply(72:115, function(x) {
    all.equal(as.numeric(stat3),
      tie_stats[x, , 2],
      check.attributes = FALSE
    )
  })))

  # Repeat for typed events --------------------------------------------------
  history$type <- history$setting
  mod <- ~ maximum("extraversion")
  tie_stats <- remstats(reh = reh, tie_effects = mod, attributes = info)

  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "maximum_extraversion")

  # The first 40 rows are expected to be equal to the following row
  riskset <- attr(tie_stats, "riskset")
  stat1 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      first_info$extraversion[first_info$name == sender],
      first_info$extraversion[first_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      second_info$extraversion[second_info$name == sender],
      second_info$extraversion[second_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attributes = FALSE)
  })))

  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- as.numeric(apply(riskset, 1, function(x) {
    sender <- as.numeric(x[1])
    receiver <- as.numeric(x[2])
    max(c(
      third_info$extraversion[third_info$name == sender],
      third_info$extraversion[third_info$name == receiver]
    ))
  }))
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attributes = FALSE)
  })))
})
