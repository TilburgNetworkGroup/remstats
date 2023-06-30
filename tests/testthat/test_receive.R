library(remstats)

test_that("expected errors and warnings", {
  
  # Expected errors for wrong "variable"
  expect_error(
    receive(variable = "test", attr_data = info),
    "not in attr_data"
  )
  
  mod <- ~ receive(variable = "test")
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
    receive(variable = "extraversion", attr_data = info[, -2]),
    "time variable is missing"
  )
  
  mod <- ~ receive(variable = "extraversion")
  attr_object <- info[, -2]
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "time variable is missing"
  )
  
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
    "time variable is missing"
  )
  
  # Expected errors for undirected events
  reh_undirected <- remify::remify(history, model = "tie", directed = FALSE)
  expect_error(
    remstats(reh = reh_undirected, tie_effects = mod, attr_data = info),
    "defined for undirected events"
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
    receive(variable = "extraversion", attr_data = attr_object),
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
    receive(variable = "extraversion", attr_data = attr_object),
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
  
  mod <- ~ receive(variable = "extraversion", attr_data = attr_object)
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
  mod <- ~ receive(variable = "extraversion")
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod, attr_data = attr_object),
    "Missing actors"
  )
  
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod, attr_data = attr_object),
    "Missing actors"
  )
  
  mod <- ~ receive(variable = "extraversion", attr_data = attr_object)
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "Missing actors"
  )
  
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod),
    "Missing actors"
  )
})

test_that("expected output from receive()", {
  # Expected standard output
  out <- list(effect = "receive", variable = "extraversion", x = NULL, 
    scaling = 1)
  expect_equal(receive(variable = "extraversion"), out)
  
  # Expected output with "std" scaling
  out$scaling <- 2
  expect_equal(receive(variable = "extraversion", scaling = "std"), out)
  
  # Expected output with object supplied to "attr_data" argument
  out$scaling <- 1
  out$x <- info[, c("name", "time", "extraversion")]
  expect_equal(receive(variable = "extraversion", attr_data = info), out)
})

test_that("expected statistic tie-oriented model", {
  mod <- ~ receive("extraversion")
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)
  
  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "receive_extraversion")
  
  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(attr_object(tie_stats, "riskset")$receiver, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
  
  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(attr_object(tie_stats, "riskset")$receiver, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
  
  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(attr_object(tie_stats, "riskset")$receiver, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
  
  # Repeat for standardized effects
  mod <- ~ receive("extraversion", scaling = "std")
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)
  
  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "receive_extraversion")
  
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
  mod <- ~ receive("extraversion")
  history$type <- history$setting
  tie_stats <- remstats(reh = reh_tie, tie_effects = mod, attr_data = info)
  
  # Expected name of the statistic
  expect_equal(dimnames(tie_stats)[[3]][2], "receive_extraversion")
  
  # The first 40 rows are expected to be equal to the following row
  stat1 <- first_info$extraversion[match(attr_object(tie_stats, "riskset")$receiver, first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
  
  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- second_info$extraversion[match(attr_object(tie_stats, "riskset")$receiver, second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
  
  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- third_info$extraversion[match(attr_object(tie_stats, "riskset")$receiver, third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tie_stats[x, , 2], check.attr_data = FALSE)
  })))
})

test_that("expected statistic actor-oriented model", {
  reh_actor <- remify::remify(history, model = "actor")
  mod <- ~ receive("extraversion")
  actors <- attr_object(reh_actor, "dictionary")$actors
  aomres <- remstats(
    reh = reh_actor, receiver_effects = mod, attr_data = info
  )
  
  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$receiver_stats)[[3]][1],
    "receive_extraversion"
  )
  
  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(actors[,1], first_info$name)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, aomres$receiver_stats[x, , 1],
      check.attr_data = FALSE
    )
  })))
  
  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(actors[,1], second_info$name)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, aomres$receiver_stats[x, , 1],
      check.attr_data = FALSE
    )
  })))
  
  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(actors[,1], third_info$name)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, aomres$receiver_stats[x, , 1],
      check.attr_data = FALSE
    )
  })))
  
  # Repeat for standardized effects
  mod <- ~ receive("extraversion", scaling = "std")
  aomres <- remstats(reh = reh_actor, receiver_effects = mod, attr_data = info)
  
  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$receiver_stats)[[3]][1],
    "receive_extraversion"
  )
  
  # The first 40 rows are expected to be equal to the following row
  std_stat1 <- t(sapply(1:40, function(x) {
    # Scale stat 1 without receiver 
    sender <- which(actors[,1] == history$actor1[x])
    std_stat <- scale(stat1[-sender])
    std_stat <- append(std_stat, 0, after = sender-1)
    std_stat
  }))
  
  expect_equal(aomres$receiver_stats[1:40,,], std_stat1)
  
  # Rows 41 to 71 are expected to be equal to the following row
  std_stat2 <- t(sapply(41:71, function(x) {
    # Scale stat 2 without receiver 
    sender <- which(actors[,1] == history$actor1[x])
    std_stat <- scale(stat2[-sender])
    std_stat <- append(std_stat, 0, after = sender-1)
    std_stat
  }))
  
  expect_equal(aomres$receiver_stats[41:71,,], std_stat2)
  
  # Rows 72 to 115 are expected to be equal to the following row
  std_stat3 <- t(sapply(72:115, function(x) {
    # Scale stat 2 without receiver 
    sender <- which(actors[,1] == history$actor1[x])
    std_stat <- scale(stat3[-sender])
    std_stat <- append(std_stat, 0, after = sender-1)
    std_stat
  }))
  
  expect_equal(aomres$receiver_stats[72:115,,], std_stat3)
})
