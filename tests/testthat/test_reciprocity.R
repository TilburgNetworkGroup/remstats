library(remstats)

test_that("reciprocity", {
  data(history)
  history$weight <- 1

  effects <- ~ reciprocity()
  reh_tie <- remify::remify(history, model = "tie")
  reh_actor <- remify::remify(history, model = "actor")
  tomres <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(receiver_effects = effects, reh = reh_actor)

  expect_equal(rowSums(tomres$statistics[, , 2]), 0:(nrow(history) - 1))
  expect_true(all(sapply(1:nrow(aomres$receiver_stats), function(i) {
    aomres$receiver_stats[i, , ] %in% c(tomres$statistics[i, , 2], 0)
  })))

  effects <- ~ reciprocity(scaling = "prop") + reciprocity() + indegreeSender()
  receiver_effects <- ~ reciprocity(scaling = "prop") + reciprocity()
  sender_effects <- ~ outdegreeSender()
  tomres <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(
    receiver_effects = receiver_effects,
    sender_effects = sender_effects, reh = reh_actor
  )

  temp <- tomres$statistics[, , 3] / tomres$statistics[, , 4]
  temp[is.na(temp)] <- 1 / 9
  expect_equal(tomres$statistics[, , 2], temp)
  expect_equal(rowSums(aomres$receiver_stats[, , 1]), rep(1, nrow(history)))

  effects <- ~ reciprocity(scaling = "std")
  tomres <- tomstats(effects, reh = reh_tie)
  aomres <- aomstats(receiver_effects = effects, reh = reh_actor)

  expect_equal(rowMeans(tomres$statistics[, , 2]), rep(0, nrow(history)))
  expect_equal(rowMeans(aomres$receiver_stats), rep(0, nrow(history)))

  colnames(history)[4] <- "type"
  effects <- ~ reciprocity(consider_type = TRUE)
  reh_tie <- remify::remify(history, model = "tie")
  tomres <- tomstats(effects, reh = reh_tie)
  expect_equal(rowSums(tomres$statistics[, , 2]), 0:(nrow(history) - 1))
})
