library(remify)
library(remstats)

test_that("recencyContinue", {
	
	effects <- ~ recencyContinue()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(edgelist = history, choiceEffects = effects)
	
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$evls[past,1] == event)
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
	
	sender <- tomres$riskset$actor1[9]
	receivers <- sort(unique(tomres$riskset$actor2))
	receiver <- match(tomres$riskset$actor2[9], receivers)
	events <- which(history$actor1 == sender)
	expect_true(all(aomres$statistics$choice[events,10,] %in% 
			tomres$statistics[,9,2]))
	
	colnames(history)[4] <- "type"
	effects <- ~ recencyContinue(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$evls[past,1] == event)
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
})

test_that("recencySendSender", {
	
	effects <- ~ recencySendSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(edgelist = history, rateEffects = effects)
	
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,2] == tomres$riskset$actor1[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
	
	senders <- sort(unique(tomres$riskset$actor1))
	sender <- match(tomres$riskset$actor1[9], senders)
	expect_true(all(aomres$statistics$rate[,sender,2] %in% 
			tomres$statistics[,9,2]))
	
	colnames(history)[4] <- "type"
	effects <- ~ recencySendSender(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,2] == tomres$riskset$actor1[9] & 
				tomres$edgelist[past,4] == tomres$riskset$type[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
})

test_that("recencySendReceiver", {
	
	effects <- ~ recencySendReceiver()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(edgelist = history, choiceEffects = effects)
	
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,2] == tomres$riskset$actor2[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
	
	receivers <- sort(unique(tomres$riskset$actor2))
	receiver <- match(tomres$riskset$actor2[9], receivers)
	expect_true(all(aomres$statistics$choice[,receiver,1] %in% 
			tomres$statistics[,9,2]))
	
	colnames(history)[4] <- "type"
	effects <- ~ recencySendReceiver(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,2] == tomres$riskset$actor2[9] & 
				tomres$edgelist[past,4] == tomres$riskset$type[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
})

test_that("recencyReceiveSender", {
	
	effects <- ~ recencyReceiveSender()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(edgelist = history, rateEffects = effects)
	
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,3] == tomres$riskset$actor1[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
	
	senders <- sort(unique(tomres$riskset$actor1))
	sender <- match(tomres$riskset$actor1[9], senders)
	expect_true(all(aomres$statistics$rate[,sender,2] %in% 
			tomres$statistics[,9,2]))
	
	colnames(history)[4] <- "type"
	effects <- ~ recencyReceiveSender(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,3] == tomres$riskset$actor1[9] & 
				tomres$edgelist[past,4] == tomres$riskset$type[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
})

test_that("recencyReceiveReceiver", {
	
	effects <- ~ recencyReceiveReceiver()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(edgelist = history, choiceEffects = effects)
	
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,3] == tomres$riskset$actor2[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
	
	receivers <- sort(unique(tomres$riskset$actor2))
	receiver <- match(tomres$riskset$actor2[9], receivers)
	expect_true(all(aomres$statistics$choice[,receiver,1] %in% 
			tomres$statistics[,9,2]))
	
	colnames(history)[4] <- "type"
	effects <- ~ recencyReceiveReceiver(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	event <- 9
	expect_equal(sapply(1:nrow(history), function(m) {
		past <- which(history$time < history$time[m])
		past <- which(tomres$edgelist[past,3] == tomres$riskset$actor2[9] & 
				tomres$edgelist[past,4] == tomres$riskset$type[9])
		if(length(past)>0) {
			last <- max(past)
			1/((history$time[m] - history$time[last])+1)	
		} else {
			0
		}
	}), tomres$statistics[,9,2])
})