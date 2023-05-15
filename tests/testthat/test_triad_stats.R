library(remstats)

test_that("otp and itp", {
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	
	effects <- ~ otp() + itp()
	tie_stats <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	riskset <- attr(tie_stats, "riskset")
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(tie_stats[,,2], tie_stats[,rd,3])
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,1]), function(i) {
		aomres$receiver_stats[i,,1] %in% c(tie_stats[i,,2], 0)
	})))
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,2]), function(i) {
		aomres$receiver_stats[i,,2] %in% c(tie_stats[i,,3], 0)
	})))
	
	effects <- ~ otp(scaling = "std") + itp(scaling = "std")
	tie_stats <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tie_stats[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,1]), rep(0, nrow(history)))
	expect_equal(rowMeans(tie_stats[,,3]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,2]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	reh_tie <- remify::remify(history, model = "tie")
	effects <- ~ otp(consider_type = TRUE) + itp(consider_type = TRUE)
	tie_stats <- tomstats(effects, reh = reh_tie)
	
	riskset <- attr(tie_stats, "riskset")
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1])
			& riskset[,3] == as.character(x[3]))
	})
	
	expect_equal(tie_stats[,,2], tie_stats[,rd,3])
})

test_that("osp and isp", {
	data(history)
	history$weight <- rep(1, nrow(history))
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	
	effects <- ~ osp() + isp()
	tie_stats <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	riskset <- attr(tie_stats, "riskset")
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1]))
	})
	
	expect_equal(tie_stats[,,2], tie_stats[,rd,2])
	expect_equal(tie_stats[,,3], tie_stats[,rd,3])
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,1]), function(i) {
		aomres$receiver_stats[i,,1] %in% c(tie_stats[i,,2], 0)
	})))
	expect_true(all(sapply(1:nrow(aomres$receiver_stats[,,2]), function(i) {
		aomres$receiver_stats[i,,2] %in% c(tie_stats[i,,3], 0)
	})))
	
	effects <- ~ osp(scaling = "std") + isp(scaling = "std")
	tie_stats <- tomstats(effects, reh = reh_tie)
	aomres <- aomstats(receiver_effects = effects, reh = reh_actor)
	
	expect_equal(rowMeans(tie_stats[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,1]), rep(0, nrow(history)))
	expect_equal(rowMeans(tie_stats[,,3]), rep(0, nrow(history)))
	expect_equal(rowMeans(aomres$receiver_stats[,,2]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	reh_tie <- remify::remify(history, model = "tie")
	effects <- ~ osp(consider_type = TRUE) + isp(consider_type = TRUE)
	tie_stats <- tomstats(effects, reh = reh_tie)
	
	riskset <- attr(tie_stats, "riskset")
	rd <- apply(riskset, 1, function(x) {
		which(riskset[,1] == as.numeric(x[2]) & riskset[,2] == as.numeric(x[1])
			& riskset[,3] == as.character(x[3]))
	})
	
	expect_equal(tie_stats[,,2], tie_stats[,rd,2])
	expect_equal(tie_stats[,,3], tie_stats[,rd,3])
})

test_that("sp and spUnique", {
	data(history)
	history$weight <- 1
	reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
	
	effects <- ~ spUnique() + sp()
	tie_stats <- tomstats(effects, reh = reh_tie)
	expect_true(all(tie_stats[,,2] < 10))
	expect_true(all(tie_stats[,,3] >= tie_stats[,,2]))
	
	effects <- ~ spUnique(scaling = "std") + sp(scaling = "std")
	tie_stats <- tomstats(effects, reh = reh_tie)
	expect_equal(rowMeans(tie_stats[,,2]), rep(0, nrow(history)))
	expect_equal(rowMeans(tie_stats[,,3]), rep(0, nrow(history)))
	
	colnames(history)[4] <- "type"
	reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
	effects <- ~ spUnique(consider_type = TRUE) + sp(consider_type = TRUE)
	tie_stats <- tomstats(effects, reh = reh_tie)
	expect_true(all(tie_stats[,,2] < 10))
	expect_true(all(tie_stats[,,3] >= tie_stats[,,2]))
})


