library(remstats)

test_that("otp and itp", {
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	
	effects <- ~ otp() + itp()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	riskset <- tomres$riskset
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(tomres$statistics[,,2], tomres$statistics[,rd,3])
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,1]), function(i) {
		aomres$receiver_stats[i,,1] %in% c(tomres$statistics[i,,2], 0)
	})))
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,2]), function(i) {
		aomres$receiver_stats[i,,2] %in% c(tomres$statistics[i,,3], 0)
	})))
	
	effects <- ~ otp(scaling = "std") + itp(scaling = "std")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,1]), rep(0, nrow(history)))
	expect_equal(rowMeans(tomres$statistics[,,3]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,2]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	reh_tie <- remify::remify(history, model = "tie")
	effects <- ~ otp(consider_type = TRUE) + itp(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
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
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	
	effects <- ~ osp() + isp()
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	riskset <- tomres$riskset
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(tomres$statistics[,,2], tomres$statistics[,rd,2])
	expect_equal(tomres$statistics[,,3], tomres$statistics[,rd,3])
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,1]), function(i) {
		aomres$receiver_stats[i,,1] %in% c(tomres$statistics[i,,2], 0)
	})))
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,2]), function(i) {
		aomres$receiver_stats[i,,2] %in% c(tomres$statistics[i,,3], 0)
	})))
	
	effects <- ~ osp(scaling = "std") + isp(scaling = "std")
	tomres <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,1]), rep(0, nrow(history)))
	expect_equal(rowMeans(tomres$statistics[,,3]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,2]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	reh_tie <- remify::remify(history, model = "tie")
	effects <- ~ osp(consider_type = TRUE) + isp(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	
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
	reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
	
	effects <- ~ spUnique() + sp()
	tomres <- tomstats(effects, reh = reh_tie)
	expect_true(all(tomres$statistics[,,2] < 10))
	expect_true(all(tomres$statistics[,,3] >= tomres$statistics[,,2]))
	
	effects <- ~ spUnique(scaling = "std") + sp(scaling = "std")
	tomres <- tomstats(effects, reh = reh_tie)
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(tomres$statistics[,,3]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
	effects <- ~ spUnique(consider_type = TRUE) + sp(consider_type = TRUE)
	tomres <- tomstats(effects, reh = reh_tie)
	expect_true(all(tomres$statistics[,,2] < 10))
	expect_true(all(tomres$statistics[,,3] >= tomres$statistics[,,2]))
})


