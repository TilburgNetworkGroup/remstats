library(remstats)

test_that("recencyContinue", {
  # Compute the statistics
  effects <- ~ recencyContinue()
  reh_tie <- remify::remify(history, model = "tie")
  reh_actor <- remify::remify(history, model = "actor")
  tie_stats <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(reh = reh_actor, receiver_effects = effects)

  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  # Test whether the statistic for this event is correct
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(attr(reh_tie, "dyad")[past] == event)
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])

  sender <- attr(tie_stats, "riskset")[event, 1]
  receivers <- sort(unique(attr(tie_stats, "riskset")[, 2]))
  receiver <- match(attr(tie_stats, "riskset")[event, 2], receivers)
  events <- which(history$actor1 == sender)
  expect_true(all(aomres$receiver_stats[events, receiver, ] %in%
    tie_stats[, event, 2]))

  # Test for consider_type
  # colnames(history)[4] <- "type"
  # effects <- ~ recencyContinue(consider_type = TRUE)
  # reh_tie <- remify::remify(history, model = "tie")
  # tie_stats <- tomstats(effects, reh = reh_tie)
  # event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  # expect_equal(sapply(1:nrow(history), function(m) {
  #   past <- which(history$time < history$time[m])
  #   past <- which(attr(reh_tie, "dyad")[past] == event)
  #   if (length(past) > 0) {
  #     last <- max(past)
  #     1 / ((history$time[m] - history$time[last]) + 1)
  #   } else {
  #     0
  #   }
  # }), tie_stats[, event, 2])
})

test_that("recencySendSender", {
  effects <- ~ recencySendSender()
  reh_tie <- remify::remify(history, model = "tie")
  reh_actor <- remify::remify(history, model = "actor")
  tie_stats <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(reh = reh_actor, sender_effects = effects)

  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 2] == attr(tie_stats, "riskset")[event, 1])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])

  senders <- sort(unique(attr(tie_stats, "riskset")[, 1]))
  sender <- match(attr(tie_stats, "riskset")[event, 1], senders)
  expect_true(all(aomres$sender_stats[, sender, 2] %in%
    tie_stats[, event, 2]))

  colnames(history)[4] <- "type"
  effects <- ~ recencySendSender(consider_type = TRUE)
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- tomstats(effects, reh = reh_tie)
  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 2] == attr(tie_stats, "riskset")[event, 1] &
      history[past, 4] == attr(tie_stats, "riskset")[event, 3])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])
})

test_that("recencySendReceiver", {
  effects <- ~ recencySendReceiver()
  reh_tie <- remify::remify(history, model = "tie")
  reh_actor <- remify::remify(history, model = "actor")
  tie_stats <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(reh = reh_actor, receiver_effects = effects)

  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 2] == attr(tie_stats, "riskset")[event, 2])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])

  receivers <- sort(unique(attr(tie_stats, "riskset")[, 2]))
  receiver <- match(attr(tie_stats, "riskset")[event, 2], receivers)
  expect_true(all(aomres$receiver_stats[, receiver, 1] %in%
    tie_stats[, event, 2]))

  colnames(history)[4] <- "type"
  effects <- ~ recencySendReceiver(consider_type = TRUE)
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- tomstats(effects, reh = reh_tie)
  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 2] == attr(tie_stats, "riskset")[event, 2] &
      history[past, 4] == attr(tie_stats, "riskset")[event, 3])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])
})

test_that("recencyReceiveSender", {
  effects <- ~ recencyReceiveSender()
  reh_tie <- remify::remify(history, model = "tie")
  reh_actor <- remify::remify(history, model = "actor")
  tie_stats <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(reh = reh_actor, sender_effects = effects)

  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 3] == attr(tie_stats, "riskset")[event, 1])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])

  senders <- sort(unique(attr(tie_stats, "riskset")[, 1]))
  sender <- match(attr(tie_stats, "riskset")[event, 1], senders)
  expect_true(all(aomres$sender_stats[, sender, 2] %in%
    tie_stats[, event, 2]))

  colnames(history)[4] <- "type"
  effects <- ~ recencyReceiveSender(consider_type = TRUE)
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- tomstats(effects, reh = reh_tie)
  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 3] == attr(tie_stats, "riskset")[event, 1] &
      history[past, 4] == attr(tie_stats, "riskset")[event, 3])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])
})

test_that("recencyReceiveReceiver", {
  effects <- ~ recencyReceiveReceiver()
  reh_tie <- remify::remify(history, model = "tie")
  reh_actor <- remify::remify(history, model = "actor")
  tie_stats <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(reh = reh_actor, receiver_effects = effects)

  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 3] == attr(tie_stats, "riskset")[event, 2])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])

  receivers <- sort(unique(attr(tie_stats, "riskset")[, 2]))
  receiver <- match(attr(tie_stats, "riskset")[event, 2], receivers)
  expect_true(all(aomres$receiver_stats[, receiver, 1] %in%
    tie_stats[, event, 2]))

  colnames(history)[4] <- "type"
  effects <- ~ recencyReceiveReceiver(consider_type = TRUE)
  reh_tie <- remify::remify(history, model = "tie")
  tie_stats <- tomstats(effects, reh = reh_tie)
  event <- sample(1:nrow(attr(tie_stats, "riskset")), 1) # randomly sample an event
  expect_equal(sapply(1:nrow(history), function(m) {
    past <- which(history$time < history$time[m])
    past <- which(history[past, 3] == attr(tie_stats, "riskset")[event, 2] &
      history[past, 4] == attr(tie_stats, "riskset")[event, 3])
    if (length(past) > 0) {
      last <- max(past)
      1 / ((history$time[m] - history$time[last]) + 1)
    } else {
      0
    }
  }), tie_stats[, event, 2])
})
