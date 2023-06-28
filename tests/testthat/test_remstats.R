library(remstats)

test_that("remstats correctly handles input combinations", {
  
  # Test case 1: Only tie_effects provided
  reh1 <- remify::remify(edgelist = history, model = "tie")
  tie_effects1 <- ~ inertia() + otp()
  
  expect_silent(remstats(reh1, tie_effects = tie_effects1))
  
  # Test case 2: Only sender_effects provided with undirected events
  reh2 <- remify::remify(edgelist = history, model = "actor", directed = FALSE)
  sender_effects2 <- ~ outdegreeSender() + recencySendSender()
  
  expect_error(remstats(reh2, sender_effects = sender_effects2),
    "Undirected events are not defined for the actor-oriented model.")
  
  receiver_effects2 <- ~ inertia() + otp()
  
  expect_error(remstats(reh2, receiver_effects = receiver_effects2),
    "Undirected events are not defined for the actor-oriented model.")
  
  # Test case 3: Only sender_effects provided with directed events
  reh3 <- remify::remify(edgelist = history, model = "actor", directed = TRUE)
  sender_effects3 <- ~ outdegreeSender() + recencySendSender()
  
  expect_silent(remstats(reh3, sender_effects = sender_effects3))
  
  receiver_effects3 <- ~ inertia() + otp()
  
  expect_silent(remstats(reh3, receiver_effects = receiver_effects3))
  
  # Test case 4: Invalid combination of arguments
  reh4 <- remify::remify(edgelist = history, model = "tie")
  tie_effects4 <- ~ inertia() + otp()
  sender_effects4 <- ~ outdegreeSender() + recencySendSender()
  
  expect_error(remstats(reh4, tie_effects = tie_effects4,
    sender_effects = sender_effects4))
})
