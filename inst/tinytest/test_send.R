library(remstats)

################################################################################
# Expected errors and warnings [helpers.R / effects.R] -------------------------
################################################################################
# Expect error for missing argument "variable"
expect_error(send(), pattern = "missing")
# Expect error for wrong "variable"
expect_error(send(variable = "extroversion", attributes = info),
  pattern = "not in attributes"
)
# Expect error for missing time variable
expect_error(send(variable = "extraversion", attributes = info[, -2]),
  pattern = "time variable is missing"
)
# Expect warning for missing values
info$extraversion[1] <- NA
expect_warning(send(variable = "extraversion", attributes = info),
  pattern = "unexpected behavior"
)
# Re-load info
data(info)
# Expect warning for missing values
info$time[1] <- NA
expect_warning(send(variable = "extraversion", attributes = info),
  pattern = "unexpected behavior"
)
# Re-load info
data(info)

################################################################################
# Expected errors and warnings [tomstats.R] -------------------------------
################################################################################
# Expect error for wrong "variable"
mod <- ~ send(variable = "extroversion")
expect_error(remstats(edgelist = history, tie_effects = mod, attributes = info),
  pattern = "not in attributes"
)
# Expect error for missing time variable
mod <- ~ send(variable = "extraversion")
expect_error(
  remstats(edgelist = history, tie_effects = mod, attributes = info[, -2]),
  pattern = "time variable is missing"
)
# Expect error for undirected events
expect_error(
  remstats(edgelist = history, tie_effects = mod, attributes = info,
  directed = FALSE),
  pattern = "defined for undirected events"
)
# Expect warning for missing values
info$extraversion[1] <- NA
expect_warning(
  remstats(edgelist = history, tie_effects = mod, attributes = info),
  pattern = "unexpected behavior"
)
# Re-load info
data(info)
# Expect warning for missing values
info$time[1] <- NA
expect_warning(
  remstats(edgelist = history, tie_effects = mod, attributes = info),
  pattern = "unexpected behavior"
)
# Re-load info
data(info)
# Expect warning for extra actor
info <- rbind(info, info[1, ])
info[nrow(info), 1] <- 999
expect_warning(
  remstats(edgelist = history, tie_effects = mod, attributes = info),
  pattern = "actors that are not in the risk set"
)
# Re-load info
data(info)
# Missing actor
info <- subset(info, id != 101)
expect_error(remstats(edgelist = history, tie_effects = mod, attributes = info),
  pattern = "Missing actors"
)
mod <- ~ send(variable = "extraversion", attributes = info)
expect_error(remstats(edgelist = history, tie_effects = mod),
  pattern = "Missing actors"
)
# Re-load info
data(info)

################################################################################
# Expected errors and warnings [aomstats.R] -------------------------------
################################################################################
# Expect error for wrong "variable"
mod <- ~ send(variable = "extroversion")
expect_error(
  remstats(edgelist = history, sender_effects = mod, attributes = info),
  pattern = "not in attributes"
)
# Expect error for missing time variable
mod <- ~ send(variable = "extraversion")
expect_error(
  remstats(edgelist = history, sender_effects = mod, attributes = info[, -2]),
  pattern = "time variable is missing"
)
# Expect error for receiver effects
expect_error(
  remstats(edgelist = history, receiver_effects = mod, attributes = info),
  pattern = "not defined for the receiver choice model"
)
# Expect warning for missing values
info$extraversion[1] <- NA
expect_warning(
  remstats(edgelist = history, sender_effects = mod, attributes = info),
  pattern = "unexpected behavior"
)
# Re-load info
data(info)
# Expect warning for missing values
info$time[1] <- NA
expect_warning(
  remstats(edgelist = history, sender_effects = mod, attributes = info),
  pattern = "unexpected behavior"
)
# Re-load info
data(info)
# Expect warning for extra actor
info <- rbind(info, info[1, ])
info[nrow(info), 1] <- 999
expect_warning(
  remstats(edgelist = history, sender_effects = mod, attributes = info),
  pattern = "actors that are not in the risk set"
)
# Re-load info
data(info)
# Missing actor
info <- subset(info, id != 101)
expect_error(
  remstats(edgelist = history, sender_effects = mod, attributes = info),
  pattern = "Missing actors"
)
mod <- ~ send(variable = "extraversion", attributes = info)
expect_error(remstats(edgelist = history, sender_effects = mod),
  pattern = "Missing actors"
)
# Re-load info
data(info)

################################################################################
# Expected output from send() [effects.R] ---------------------------------
################################################################################
# Expected standard output
out <- list(effect = "send", variable = "extraversion", x = NULL, scaling = 1)
expect_equal(send(variable = "extraversion"), out)
# Expected output with "std" scaling
out$scaling <- 2
expect_equal(send(variable = "extraversion", scaling = "std"), out)
# Expected output with object supplied to "attributes" argument
out$scaling <- 1
out$x <- info[, c("id", "time", "extraversion")]
expect_equal(send(variable = "extraversion", attributes = info), out)

################################################################################
# Expected statistic [tomstats.R] -----------------------------------------
################################################################################
mod <- ~ send("extraversion")
tomres <- remstats(edgelist = history, tie_effects = mod, attributes = info)

# Expected name of the statistic
expect_equal(dimnames(tomres$statistics)[[3]][2], "send_extraversion")

# The first 40 rows are expected to be equal to the following row
first_info <- subset(info, time == 0)
stat1 <- first_info$extraversion[match(tomres$riskset$sender, first_info$id)]
expect_true(all(sapply(1:40, function(x) {
  all.equal(stat1, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Rows 41 to 71 are expected to be equal to the following row
second_info <- subset(info, time == 9432)
stat2 <- second_info$extraversion[match(tomres$riskset$sender, second_info$id)]
expect_true(all(sapply(41:71, function(x) {
  all.equal(stat2, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Rows 72 to 115 are expected to be equal to the following row
third_info <- subset(info, time == 18864)
stat3 <- third_info$extraversion[match(tomres$riskset$sender, third_info$id)]
expect_true(all(sapply(72:115, function(x) {
  all.equal(stat3, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Repeat for standardized effects 
mod <- ~ send("extraversion", scaling = "std")
tomres <- remstats(edgelist = history, tie_effects = mod, attributes = info)

# Expected name of the statistic
expect_equal(dimnames(tomres$statistics)[[3]][2], "send_extraversion")

# The first 40 rows are expected to be equal to the following row
stat1 <- as.numeric(scale(stat1))
expect_true(all(sapply(1:40, function(x) {
	all.equal(stat1, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Rows 41 to 71 are expected to be equal to the following row
stat2 <- as.numeric(scale(stat2))
expect_true(all(sapply(41:71, function(x) {
  all.equal(stat2, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Rows 72 to 115 are expected to be equal to the following row
stat3 <- as.numeric(scale(stat3))
expect_true(all(sapply(72:115, function(x) {
  all.equal(stat3, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Repeat for typed events
mod <- ~ send("extraversion")
history$type <- history$setting
tomres <- remstats(edgelist = history, tie_effects = mod, attributes = info)

# Expected name of the statistic
expect_equal(dimnames(tomres$statistics)[[3]][2], "send_extraversion")

# The first 40 rows are expected to be equal to the following row
stat1 <- first_info$extraversion[match(tomres$riskset$sender, first_info$id)]
expect_true(all(sapply(1:40, function(x) {
  all.equal(stat1, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Rows 41 to 71 are expected to be equal to the following row
stat2 <- second_info$extraversion[match(tomres$riskset$sender, second_info$id)]
expect_true(all(sapply(41:71, function(x) {
  all.equal(stat2, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Rows 72 to 115 are expected to be equal to the following row
stat3 <- third_info$extraversion[match(tomres$riskset$sender, third_info$id)]
expect_true(all(sapply(72:115, function(x) {
  all.equal(stat3, tomres$statistics[x,,2], check.attributes = FALSE)
})))

# Reload history
data(history)

# Expected statistic [aomstats.R] -----------------------------------------
mod <- ~ send("extraversion")
aomres <- remstats(edgelist = history, sender_effects = mod, attributes = info)

# Expected name of the statistic
expect_equal(
  dimnames(aomres$statistics$sender_stats)[[3]][2],
  "send_extraversion"
)

# The first 40 rows are expected to be equal to the following row
first_info <- subset(info, time == 0)
stat1 <- first_info$extraversion[match(aomres$actors, first_info$id)]
expect_true(all(sapply(1:40, function(x) {
  all.equal(stat1, aomres$statistics$sender_stats[x,,2], 
  check.attributes = FALSE)
})))

# Rows 41 to 71 are expected to be equal to the following row
second_info <- subset(info, time == 9432)
stat2 <- second_info$extraversion[match(aomres$actors, second_info$id)]
expect_true(all(sapply(41:71, function(x) {
  all.equal(stat2, aomres$statistics$sender_stats[x,,2], 
  check.attributes = FALSE)
})))

# Rows 72 to 115 are expected to be equal to the following row
third_info <- subset(info, time == 18864)
stat3 <- third_info$extraversion[match(aomres$actors, third_info$id)]
expect_true(all(sapply(72:115, function(x) {
  all.equal(stat3, aomres$statistics$sender_stats[x,,2], 
  check.attributes = FALSE)
})))

# Repeat for standardized effects 
mod <- ~ send("extraversion", scaling = "std")
aomres <- remstats(edgelist = history, sender_effects = mod, attributes = info)

# Expected name of the statistic
expect_equal(
  dimnames(aomres$statistics$sender_stats)[[3]][2],
  "send_extraversion"
)

# The first 40 rows are expected to be equal to the following row
stat1 <- as.numeric(scale(stat1))
expect_true(all(sapply(1:40, function(x) {
	all.equal(stat1, aomres$statistics$sender_stats[x,,2], 
  check.attributes = FALSE)
})))

# Rows 41 to 71 are expected to be equal to the following row
stat2 <- as.numeric(scale(stat2))
expect_true(all(sapply(41:71, function(x) {
	all.equal(stat2, aomres$statistics$sender_stats[x,,2], 
  check.attributes = FALSE)
})))

# Rows 72 to 115 are expected to be equal to the following row
stat3 <- as.numeric(scale(stat3))
expect_true(all(sapply(72:115, function(x) {
  all.equal(stat3, aomres$statistics$sender_stats[x,,2], 
  check.attributes = FALSE)
})))

