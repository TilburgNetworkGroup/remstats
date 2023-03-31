library(remstats)

test_that("expected errors and warnings", {
  reh_actor <- remify::remify(history, model = "actor")

  # Expected errors for sender effects
  mod <- ~ recencySendReceiver()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  mod <- ~ recencyReceiveReceiver()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  mod <- ~ recencyContinue()
  expect_error(
    remstats(reh = reh_actor, sender_effects = mod),
    "not defined for the sender activity model"
  )

  # Expected errors for receiver effects
  mod <- ~ recencySendSender()
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod),
    "not defined for the receiver choice model"
  )

  mod <- ~ recencyReceiveSender()
  expect_error(
    remstats(reh = reh_actor, receiver_effects = mod),
    "not defined for the receiver choice model"
  )

  # Expect errors for undirected events
  reh_tie <- remify::remify(history, model = "tie", directed = FALSE)

  mod <- ~ recencySendReceiver()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  mod <- ~ recencySendSender()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  mod <- ~ recencyReceiveReceiver()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )

  mod <- ~ recencyReceiveSender()
  expect_error(
    remstats(reh = reh_tie, tie_effects = mod),
    "undirected events"
  )
})

test_that("expected output from recency()", {
  # Expected standard output
  out <- list(effect = "recencyContinue", scaling = 1)
  expect_equal(recencyContinue(), out)

  out$effect <- "recencySendReceiver"
  expect_equal(recencySendReceiver(), out)

  out$effect <- "recencyReceiveReceiver"
  expect_equal(recencyReceiveReceiver(), out)

  out$effect <- "recencySendSender"
  expect_equal(recencySendSender(), out)

  out$effect <- "recencyReceiveSender"
  expect_equal(recencyReceiveSender(), out)

  # Expected output with consider_type = TRUE
  out$effect <- "recencyContinue.type"
  expect_equal(recencyContinue(consider_type = TRUE), out)

  out$effect <- "recencySendReceiver.type"
  expect_equal(recencySendReceiver(consider_type = TRUE), out)

  out$effect <- "recencyReceiveReceiver.type"
  expect_equal(recencyReceiveReceiver(consider_type = TRUE), out)

  out$effect <- "recencySendSender.type"
  expect_equal(recencySendSender(consider_type = TRUE), out)

  out$effect <- "recencyReceiveSender.type"
  expect_equal(recencyReceiveSender(consider_type = TRUE), out)
})

test_that("expected statistic tie-oriented model", {
  # Standard --------------------------------------
  reh_tie <- remify::remify(history, model = "tie")
  mod <- ~ recencyContinue() + recencySendReceiver() + recencySendSender() +
    recencyReceiveReceiver() + recencyReceiveSender()
  stats <- remstats(reh = reh_tie, tie_effects = mod)
  dyads <- attr(reh_tie, "dyad")
  riskset <- attr(stats, "riskset")
  edgelist <- reh_tie$edgelist
  actors <- attr(reh_tie, "dictionary")$actors

  s1 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      idx <- which(edgelist$time < time & dyads == d)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s1, stats[, , "recencyContinue"])

  s2 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      receiverName <- riskset[d, 2]
      receiver <- actors$actorID[match(receiverName, actors$actorName)]
      idx <- which(edgelist$time < time & edgelist$actor1_ID == receiver)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s2, stats[, , "recencySendReceiver"])

  s3 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      senderName <- riskset[d, 1]
      sender <- actors$actorID[match(senderName, actors$actorName)]
      idx <- which(edgelist$time < time & edgelist$actor1_ID == sender)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s3, stats[, , "recencySendSender"])

  s4 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      receiverName <- riskset[d, 2]
      receiver <- actors$actorID[match(receiverName, actors$actorName)]
      idx <- which(edgelist$time < time & edgelist$actor2_ID == receiver)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s4, stats[, , "recencyReceiveReceiver"])

  s5 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      senderName <- riskset[d, 1]
      sender <- actors$actorID[match(senderName, actors$actorName)]
      idx <- which(edgelist$time < time & edgelist$actor2_ID == sender)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s5, stats[, , "recencyReceiveSender"])

  # Undirected events -----------------------------
  reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
  mod <- ~ recencyContinue()
  stats <- remstats(reh = reh_tie, tie_effects = mod)
  dyads <- attr(reh_tie, "dyad")
  riskset <- attr(stats, "riskset")
  edgelist <- reh_tie$edgelist
  actors <- attr(reh_tie, "dictionary")$actors

  s1 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      idx <- which(edgelist$time < time & dyads == d)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s1, stats[, , "recencyContinue"])

  # Event types -----------------------------------
  history$type <- history$setting
  reh_tie <- remify::remify(history, model = "tie")
  mod <- ~ recencyContinue(consider_type = TRUE) +
    recencySendReceiver(consider_type = TRUE) +
    recencySendSender(consider_type = TRUE) +
    recencyReceiveReceiver(consider_type = TRUE) +
    recencyReceiveSender(consider_type = TRUE)
  stats <- remstats(reh = reh_tie, tie_effects = mod)
  dyads <- attr(reh_tie, "dyad")
  riskset <- attr(stats, "riskset")
  edgelist <- reh_tie$edgelist
  actors <- attr(reh_tie, "dictionary")$actors
  types <- attr(reh_tie, "dictionary")$types

  s1 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      idx <- which(edgelist$time < time & dyads == d)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s1, stats[, , "recencyContinue.type"])

  s2 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      receiverName <- riskset[d, 2]
      receiver <- actors$actorID[match(receiverName, actors$actorName)]
      typeName <- riskset[d, 3]
      type <- types$typeID[match(typeName, types$typeName)]
      idx <- which(edgelist$time < time &
        edgelist$actor1_ID == receiver &
        edgelist$type_ID == type)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s2, stats[, , "recencySendReceiver.type"])

  s3 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      senderName <- riskset[d, 1]
      sender <- actors$actorID[match(senderName, actors$actorName)]
      typeName <- riskset[d, 3]
      type <- types$typeID[match(typeName, types$typeName)]
      idx <- which(edgelist$time < time &
        edgelist$actor1_ID == sender &
        edgelist$type_ID == type)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s3, stats[, , "recencySendSender.type"])

  s4 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      receiverName <- riskset[d, 2]
      receiver <- actors$actorID[match(receiverName, actors$actorName)]
      typeName <- riskset[d, 3]
      type <- types$typeID[match(typeName, types$typeName)]
      idx <- which(edgelist$time < time &
        edgelist$actor2_ID == receiver &
        edgelist$type_ID == type)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s4, stats[, , "recencyReceiveReceiver.type"])

  s5 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(riskset)), function(d) {
      senderName <- riskset[d, 1]
      sender <- actors$actorID[match(senderName, actors$actorName)]
      typeName <- riskset[d, 3]
      type <- types$typeID[match(typeName, types$typeName)]
      idx <- which(edgelist$time < time &
        edgelist$actor2_ID == sender &
        edgelist$type_ID == type)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s5, stats[, , "recencyReceiveSender.type"])
})

test_that("expected statistic actor-oriented model", {
  # Standard --------------------------------------
  reh_actor <- remify::remify(history, model = "actor")
  sender_mod <- ~ recencySendSender() + recencyReceiveSender()
  receiver_mod <- ~ recencyContinue() + recencySendReceiver() +
    recencyReceiveReceiver()
  stats <- remstats(
    reh = reh_actor, sender_effects = sender_mod,
    receiver_effects = receiver_mod
  )
  sender_stats <- stats$sender_stats
  receiver_stats <- stats$receiver_stats
  actors <- attr(reh_actor, "dictionary")$actors
  edgelist <- reh_actor$edgelist

  s1 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sender <- edgelist[m, 2]
    sapply(seq_len(nrow(actors)), function(r) {
      idx <- which(edgelist$time < time &
        edgelist$actor1_ID == sender &
        edgelist$actor2_ID == r)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s1, receiver_stats[, , "recencyContinue"])

  s2 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sender <- edgelist[m, 2]
    sapply(seq_len(nrow(actors)), function(r) {
      idx <- which(edgelist$time < time & edgelist$actor1_ID == r)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s2, receiver_stats[, , "recencySendReceiver"])

  s3 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(actors)), function(s) {
      idx <- which(edgelist$time < time & edgelist$actor1_ID == s)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s3, sender_stats[, , "recencySendSender"])

  s4 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sender <- edgelist[m, 2]
    sapply(seq_len(nrow(actors)), function(r) {
      idx <- which(edgelist$time < time & edgelist$actor2_ID == r)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s4, receiver_stats[, , "recencyReceiveReceiver"])

  s5 <- t(sapply(seq_len(nrow(edgelist)), function(m) {
    time <- edgelist[m, 1]
    sapply(seq_len(nrow(actors)), function(s) {
      idx <- which(edgelist$time < time & edgelist$actor2_ID == s)
      if (length(idx) > 0) {
        max_idx <- max(idx)
        idx_time <- edgelist$time[max_idx]
        1 / (time - idx_time + 1)
      } else {
        0
      }
    })
  }))

  expect_equal(s5, sender_stats[, , "recencyReceiveSender"])
})
