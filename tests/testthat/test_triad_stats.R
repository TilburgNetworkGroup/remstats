library(remify)
library(remstats)

test_that("otp and itp", {
	data(history)
	history$weight <- rep(1, nrow(history))
	
	effects <- ~ otp() + itp()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	riskset <- tomres$riskset
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(tomres$statistics[,,2], tomres$statistics[,rd,3])
	expect_true(all(sapply(1:nrow(aomres$statistics$choice[,,1]), function(i) {
		aomres$statistics$choice[i,,1] %in% c(tomres$statistics[i,,2], 0)
	})))
	expect_true(all(sapply(1:nrow(aomres$statistics$choice[,,2]), function(i) {
		aomres$statistics$choice[i,,2] %in% c(tomres$statistics[i,,3], 0)
	})))
	
	effects <- ~ otp(scaling = "std") + itp(scaling = "std")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice[,,1]), rep(0, nrow(history)))
	expect_equal(rowMeans(tomres$statistics[,,3]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice[,,2]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	effects <- ~ otp(consider_type = TRUE) + itp(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	riskset <- tomres$riskset
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1])
			& riskset[,3] == as.character(x[3]))
	})
	
	expect_equal(tomres$statistics[,,2], tomres$statistics[,rd,3])
})

test_that("osp and isp", {
	data(history)
	history$weight <- rep(1, nrow(history))
	
	effects <- ~ osp() + isp()
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	riskset <- tomres$riskset
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(tomres$statistics[,,2], tomres$statistics[,rd,2])
	expect_equal(tomres$statistics[,,3], tomres$statistics[,rd,3])
	expect_true(all(sapply(1:nrow(aomres$statistics$choice[,,1]), function(i) {
		aomres$statistics$choice[i,,1] %in% c(tomres$statistics[i,,2], 0)
	})))
	expect_true(all(sapply(1:nrow(aomres$statistics$choice[,,2]), function(i) {
		aomres$statistics$choice[i,,2] %in% c(tomres$statistics[i,,3], 0)
	})))
	
	effects <- ~ osp(scaling = "std") + isp(scaling = "std")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice[,,1]), rep(0, nrow(history)))
	expect_equal(rowMeans(tomres$statistics[,,3]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$statistics$choice[,,2]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	effects <- ~ osp(consider_type = TRUE) + isp(consider_type = TRUE)
	tomres <- tomstats(effects, edgelist = history)
	
	riskset <- tomres$riskset
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1])
			& riskset[,3] == as.character(x[3]))
	})
	
	expect_equal(tomres$statistics[,,2], tomres$statistics[,rd,2])
	expect_equal(tomres$statistics[,,3], tomres$statistics[,rd,3])
})

test_that("sp and spUnique", {
	data(history)
	history$weight <- 1
	
	effects <- ~ spUnique() + sp()
	tomres <- tomstats(effects, history, directed = F)
	expect_true(all(tomres$statistics[,,2] < 10))
	expect_true(all(tomres$statistics[,,3] >= tomres$statistics[,,2]))
	
	effects <- ~ spUnique(scaling = "std") + sp(scaling = "std")
	tomres <- tomstats(effects, history, directed = F)
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(tomres$statistics[,,3]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	effects <- ~ spUnique(consider_type = TRUE) + sp(consider_type = TRUE)
	tomres <- tomstats(effects, history, directed = F)
	expect_true(all(tomres$statistics[,,2] < 10))
	expect_true(all(tomres$statistics[,,3] >= tomres$statistics[,,2]))
})


