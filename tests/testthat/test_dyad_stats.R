library(remify)
library(remstats)

test_that("average", {
	
	effects <- ~ average("extraversion") 
	
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(choiceEffects = effects, edgelist = history, attributes = info)
	
	expect_true(all(tomres$statistics[,,2] %in% 
			apply(expand.grid(info$extraversion, info$extraversion), 1, mean)))
	expect_true(all(aomres$statistics$choice %in% 
			apply(expand.grid(info$extraversion, info$extraversion), 1, mean)))
})

test_that("same", {
	
	effects <- ~ same("age") 
	
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(choiceEffects = effects, edgelist = history, attributes = info)
	
	temp <- info$age[!duplicated(info$id)]
	count <- sum(temp==0)*(sum(temp==0)-1) + sum(temp==1)*(sum(temp==1)-1)
	
	expect_true(all(rowSums(tomres$statistics[,,2])==count))
	expect_true(all(rowSums(aomres$statistics$choice) %in% 
			c(sum(temp == 0), sum(temp == 1))))
})

test_that("difference", {
	
	effects <- ~ difference("extraversion", absolute = TRUE) 
	
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	aomres <- aomstats(choiceEffects = effects, edgelist = history, attributes = info)
	
	expect_true(all(tomres$statistics[,,2] %in% 
			apply(expand.grid(info$extraversion, info$extraversion), 1, function(x) abs(diff(x)))))
	expect_true(all(aomres$statistics$choice %in% 
			apply(expand.grid(info$extraversion, info$extraversion), 1, function(x) abs(diff(x)))))
})

test_that("maximum", {
	
	effects <- ~ maximum("extraversion") 
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	
	expect_true(all(tomres$statistics[,,2] %in% 
			apply(expand.grid(info$extraversion, info$extraversion), 1, max)))
})

test_that("minimum", {
	
	effects <- ~ minimum("extraversion") 
	tomres <- tomstats(effects, edgelist = history, attributes = info)
	
	expect_true(all(tomres$statistics[,,2] %in% 
			apply(expand.grid(info$extraversion, info$extraversion), 1, min)))
})