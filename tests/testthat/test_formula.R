# Load the necessary packages
library(remstats)

# Define the test cases
test_that("check_formula correctly identifies incorrect effects", {
	
	# Test tie_effects
	
	formula1 <- ~ inertia()
	formula2 <- ~ inertia
	formula3 <- ~ inertia() + otp
	formula4 <- ~ inertia + otp
	
	reh <- remify::remify(history, model = "tie")
	
	expect_silent(remstats(reh, tie_effects = formula1))
	
	expect_error(remstats(reh, tie_effects = formula2), 
		"The following effects are not specified as functions: inertia")
	
	expect_error(remstats(reh, tie_effects = formula3), 
		"The following effects are not specified as functions: otp")
	
	expect_error(remstats(reh, tie_effects = formula4), 
		"The following effects are not specified as functions: inertia, otp")
	
	# Test receiver_effects
	
	reh <- remify::remify(history, model = "actor")
	
	expect_silent(remstats(reh, receiver_effects = formula1))
	
	expect_error(remstats(reh, receiver_effects = formula2), 
		"The following effects are not specified as functions: inertia")
	
	expect_error(remstats(reh, receiver_effects = formula3), 
		"The following effects are not specified as functions: otp")
	
	expect_error(remstats(reh, receiver_effects = formula4), 
		"The following effects are not specified as functions: inertia, otp")
	
	# Test sender_effects
	
	formula1 <- ~ indegreeSender()
	formula2 <- ~ indegreeSender
	formula3 <- ~ indegreeSender() + recencySendSender
	formula4 <- ~ indegreeSender + recencySendSender
	
	expect_silent(remstats(reh, sender_effects = formula1))
	
	expect_error(remstats(reh, sender_effects = formula2), 
		"The following effects are not specified as functions: indegreeSender")
	
	expect_error(remstats(reh, sender_effects = formula3), 
		"The following effects are not specified as functions: recencySendSender")
	
	expect_error(remstats(reh, sender_effects = formula4), 
		"The following effects are not specified as functions: indegreeSender, recencySendSender")
	
	
})