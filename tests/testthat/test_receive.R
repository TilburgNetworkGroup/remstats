library(remstats)

test_that("expected errors and warnings", {
  
  # Expected errors for wrong "variable"
  expect_error(
    receive(variable = "test", attributes = info),
    "not in attributes"
  )
  
  mod <- ~ receive(variable = "test")
  expect_error(
    remstats(edgelist = history, tie_effects = mod, attributes = info),
    "not in attributes"
  )
  
  expect_error(
    remstats(edgelist = history, receiver_effects = mod, attributes = info),
    "not in attributes"
  )
  
  # Expected errors for missing time variable
  expect_error(
    receive(variable = "extraversion", attributes = info[, -2]),
    "time variable is missing"
  )
  
  mod <- ~ receive(variable = "extraversion")
  attr <- info[, -2]
  expect_error(
    remstats(edgelist = history, tie_effects = mod, attributes = attr),
    "time variable is missing"
  )
  
  expect_error(
    remstats(edgelist = history, receiver_effects = mod, attributes = attr),
    "time variable is missing"
  )
  
  # Expected errors for undirected events
  expect_error(
    remstats(
      edgelist = history, tie_effects = mod, attributes = info,
      directed = FALSE
    ),
    "defined for undirected events"
  )
  
  # Expected errors for sender effects
  expect_error(
    remstats(edgelist = history, sender_effects = mod, attributes = info),
    "not defined for the sender activity model"
  )
  
  # Expect warning for missing values
  attr <- info
  attr$extraversion[1] <- NA
  expect_warning(
    receive(variable = "extraversion", attributes = attr),
    "unexpected behavior"
  )
  
  expect_warning(
    remstats(edgelist = history, tie_effects = mod, attributes = attr),
    "unexpected behavior"
  )
  
  expect_warning(
    remstats(edgelist = history, receiver_effects = mod, attributes = attr),
    "unexpected behavior"
  )
  
  # Expect error for missing time values
  attr <- info
  attr$time[1] <- NA
  expect_error(
    receive(variable = "extraversion", attributes = attr),
    "cannot have missing values"
  )
  
  expect_error(
    remstats(edgelist = history, tie_effects = mod, attributes = attr),
    "cannot have missing values"
  )
  
  expect_error(
    remstats(edgelist = history, receiver_effects = mod, attributes = attr),
    "cannot have missing values"
  )
  
  # Expect warning for extra actor
  attr <- rbind(info, info[1, ])
  attr[nrow(attr), 1] <- 999
  expect_warning(
    remstats(edgelist = history, tie_effects = mod, attributes = attr),
    "actors that are not in the risk set"
  )
  
  expect_warning(
    remstats(edgelist = history, receiver_effects = mod, attributes = attr),
    "actors that are not in the risk set"
  )
  
  mod <- ~ receive(variable = "extraversion", attributes = attr)
  expect_warning(
    remstats(edgelist = history, tie_effects = mod),
    "actors that are not in the risk set"
  )
  
  expect_warning(
    remstats(edgelist = history, receiver_effects = mod),
    "actors that are not in the risk set"
  )
  
  # Missing actor
  attr <- subset(info, id != 101)
  mod <- ~ receive(variable = "extraversion")
  expect_error(
    remstats(edgelist = history, tie_effects = mod, attributes = attr),
    "Missing actors"
  )
  
  expect_error(
    remstats(edgelist = history, receiver_effects = mod, attributes = attr),
    "Missing actors"
  )
  
  mod <- ~ receive(variable = "extraversion", attributes = attr)
  expect_error(
    remstats(edgelist = history, tie_effects = mod),
    "Missing actors"
  )
  
  expect_error(
    remstats(edgelist = history, receiver_effects = mod),
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
  
  # Expected output with object supplied to "attributes" argument
  out$scaling <- 1
  out$x <- info[, c("id", "time", "extraversion")]
  expect_equal(receive(variable = "extraversion", attributes = info), out)
})

test_that("expected statistic tie-oriented model", {
  mod <- ~ receive("extraversion")
  tomres <- remstats(edgelist = history, tie_effects = mod, attributes = info)
  
  # Expected name of the statistic
  expect_equal(dimnames(tomres$statistics)[[3]][2], "receive_extraversion")
  
  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(tomres$riskset$receiver, first_info$id)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
  
  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(tomres$riskset$receiver, second_info$id)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
  
  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(tomres$riskset$receiver, third_info$id)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
  
  # Repeat for standardized effects
  mod <- ~ receive("extraversion", scaling = "std")
  tomres <- remstats(edgelist = history, tie_effects = mod, attributes = info)
  
  # Expected name of the statistic
  expect_equal(dimnames(tomres$statistics)[[3]][2], "receive_extraversion")
  
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
  mod <- ~ receive("extraversion")
  history$type <- history$setting
  tomres <- remstats(edgelist = history, tie_effects = mod, attributes = info)
  
  # Expected name of the statistic
  expect_equal(dimnames(tomres$statistics)[[3]][2], "receive_extraversion")
  
  # The first 40 rows are expected to be equal to the following row
  stat1 <- first_info$extraversion[match(tomres$riskset$receiver, first_info$id)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
  
  # Rows 41 to 71 are expected to be equal to the following row
  stat2 <- second_info$extraversion[match(tomres$riskset$receiver, second_info$id)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
  
  # Rows 72 to 115 are expected to be equal to the following row
  stat3 <- third_info$extraversion[match(tomres$riskset$receiver, third_info$id)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, tomres$statistics[x, , 2], check.attributes = FALSE)
  })))
})

test_that("expected statistic actor-oriented model", {
  mod <- ~ receive("extraversion")
  aomres <- remstats(
    edgelist = history, receiver_effects = mod, attributes = info
  )
  
  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$statistics$receiver_stats)[[3]][1],
    "receive_extraversion"
  )
  
  # The first 40 rows are expected to be equal to the following row
  first_info <- subset(info, time == 0)
  stat1 <- first_info$extraversion[match(aomres$actors, first_info$id)]
  expect_true(all(sapply(1:40, function(x) {
    all.equal(stat1, aomres$statistics$receiver_stats[x, , 1],
      check.attributes = FALSE
    )
  })))
  
  # Rows 41 to 71 are expected to be equal to the following row
  second_info <- subset(info, time == 9432)
  stat2 <- second_info$extraversion[match(aomres$actors, second_info$id)]
  expect_true(all(sapply(41:71, function(x) {
    all.equal(stat2, aomres$statistics$receiver_stats[x, , 1],
      check.attributes = FALSE
    )
  })))
  
  # Rows 72 to 115 are expected to be equal to the following row
  third_info <- subset(info, time == 18864)
  stat3 <- third_info$extraversion[match(aomres$actors, third_info$id)]
  expect_true(all(sapply(72:115, function(x) {
    all.equal(stat3, aomres$statistics$receiver_stats[x, , 1],
      check.attributes = FALSE
    )
  })))
  
  # Repeat for standardized effects
  mod <- ~ receive("extraversion", scaling = "std")
  aomres <- remstats(edgelist = history, receiver_effects = mod, attributes = info)
  
  # Expected name of the statistic
  expect_equal(
    dimnames(aomres$statistics$receiver_stats)[[3]][1],
    "receive_extraversion"
  )
  
  # The first 40 rows are expected to be equal to the following row
  std_stat1 <- t(sapply(1:40, function(x) {
    # Scale stat 1 without receiver 
    sender <- which(aomres$actors == history$actor1[x])
    std_stat <- scale(stat1[-sender])
    std_stat <- append(std_stat, 0, after = sender-1)
    std_stat
  }))
  
  expect_equal(aomres$statistics$receiver_stats[1:40,,], std_stat1)
  
  # Rows 41 to 71 are expected to be equal to the following row
  std_stat2 <- t(sapply(41:71, function(x) {
    # Scale stat 2 without receiver 
    sender <- which(aomres$actors == history$actor1[x])
    std_stat <- scale(stat2[-sender])
    std_stat <- append(std_stat, 0, after = sender-1)
    std_stat
  }))
  
  expect_equal(aomres$statistics$receiver_stats[41:71,,], std_stat2)
  
  # Rows 72 to 115 are expected to be equal to the following row
  std_stat3 <- t(sapply(72:115, function(x) {
    # Scale stat 2 without receiver 
    sender <- which(aomres$actors == history$actor1[x])
    std_stat <- scale(stat3[-sender])
    std_stat <- append(std_stat, 0, after = sender-1)
    std_stat
  }))
  
  expect_equal(aomres$statistics$receiver_stats[72:115,,], std_stat3)
})
