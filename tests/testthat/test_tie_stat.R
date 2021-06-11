library(remify)
library(remstats)

test_that("tie effect", {
	skip("skip")
	
	actors <- unique(info$id)
	age <- info[match(actors, info$id), "age"]
	
	bothOld <- sapply(1:length(actors), function(i) {
		sapply(1:length(actors), function(j) {
			ifelse(age[i] & age[j] == 1,  1, 0)
		})
	})
	
	effects <- ~ tie(bothOld, variableName = "both.old")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	dyads <- which(tomres$riskset[,1] %in% actors[age == 1] & 
			tomres$riskset[,2] %in% actors[age == 1])
	nondyads <- (1:nrow(tomres$riskset))[-dyads]
	expect_true(all(tomres$statistics[,dyads,2] == 1))
	expect_true(all(tomres$statistics[,nondyads,2] == 0))
	
	events <- which(aomres$edgelist$actor1 %in% actors[age == 1])
	receivers <- which(age==1)
	expect_true(all(aomres$statistics$choice[events, receivers,]==1))
	
	# Standardized scaling
	effects <- ~ tie(bothOld, variableName = "both.old", scaling = "std")
	tomres <- tomstats(effects, edgelist = history)
	aomres <- aomstats(choiceEffects = effects, edgelist = history)
	
	expect_equal(rowMeans(tomres$statistics[,,2]), rep(0, nrow(tomres$edgelist)))
	expect_equal(rowMeans(aomres$statistics$choice), rep(0, nrow(aomres$edgelist)))
})


