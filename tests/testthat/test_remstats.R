context("remstats")

library(remstats)

# Data
data(edgelistD)
data(edgelistU)
data(edgelistUT)

# Covariates
data(covar)

# Effects
effectsD <- c("sender_effect", "receiver_effect", "same", "difference",  
        "mean", "min", "max", "both_equal_to", "event_effect", "inertia", "inertia_weighted", "reciprocity", "reciprocity_weighted", "indegree_sender", "indegree_receiver", "outdegree_sender", "outdegree_receiver", "totaldegree_sender", "totaldegree_receiver", "OTP", "ITP", "OSP", "ISP", "sender_effect*inertia", "same*inertia")

effectsU <- c("same", "difference",  "mean", "min", "max", "both_equal_to", 
    "event_effect", "inertia", "inertia_weighted", "shared_partners", "unique_sp", "same*inertia")

effectsUT <- c("same", "difference",  "mean", "min", "max", "both_equal_to", 
    "event_effect", "type_effect", "inertia", "inertia_weighted", "inertia_type", "inertia_type_weighted", "shared_partners", "unique_sp", "shared_partners_type", "unique_sp_type", "same*inertia")

# Covariates 
covariates <- rep(list(covar[,c(1,2,4)]), 8)
names(covariates) <- c("sender_effect", "receiver_effect", "same", 
    "difference", "mean", "min", "max", "both_equal_to")

# Event effect
event_effect <- sample(c(0,1), nrow(edgelistD), replace = TRUE)

# Weights
equal_weights <- rep(1, nrow(edgelistD))

# Statistics
statsA <- remstats(edgelistD, effects = effectsD, covariates = covariates, 
    event_effect = event_effect, weights = equal_weights, equal_val = 0)
statsB <- remstats(edgelistU, effects = effectsU, directed = FALSE, 
    covariates = covariates, event_effect = event_effect, weights = equal_weights, equal_val = 0)
statsC <- remstats(edgelistUT, effects = effectsUT, directed = FALSE, 
    type = TRUE, covariates = covariates, event_effect = event_effect, weights = equal_weights, equal_val = 0)

single_statA <- remstats(edgelistD, effects = "inertia", 
    weights = equal_weights, equal_val = 0)

# Tests
test_that("dimensions of the output of remstats", {
    expect_output(str(statsA), "List of 5")
    expect_output(str(statsB), "List of 5")
    expect_output(str(statsC), "List of 5")
    expect_output(str(single_statA), "List of 5")
})

test_that("baseline in remstats", {
    expect_equal(statsA$statistics[,,"baseline"], 
        matrix(1, nrow(statsA$edgelist), nrow(statsA$riskset)))
    expect_equal(statsB$statistics[,,"baseline"], 
        matrix(1, nrow(statsB$edgelist), nrow(statsB$riskset)))
    expect_equal(statsC$statistics[,,"baseline"], 
        matrix(1, nrow(statsC$edgelist), nrow(statsC$riskset)))

    expect_equal(single_statA$statistics[,,"baseline"], 
        matrix(1, nrow(single_statA$edgelist), nrow(single_statA$riskset)))
})

test_that("actor effects in remstats", {
    covar$id <- statsA$actors$id[match(covar$id, statsA$actors$name)]
    covar <- as.matrix(covar)

    expect_equal(statsA$statistics[,,"sender_effect"], 
        actorstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsA$edgelist, riskset = statsA$riskset))
    expect_equal(statsA$statistics[,,"receiver_effect"], 
        actorstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsA$edgelist, riskset = statsA$riskset))
})

test_that("dyad effects in remstats", {
    covar$id <- statsA$actors$id[match(covar$id, statsA$actors$name)]
    covar <- as.matrix(covar)

    expect_equal(statsA$statistics[,,"same"], 
        dyadstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsA$edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"difference"], 
        dyadstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsA$edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"mean"], 
        dyadstat(values = covar[,c(1,2,4)], type = 3, edgelist = statsA$edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"min"], 
        dyadstat(values = covar[,c(1,2,4)], type = 4, edgelist = statsA$edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"max"], 
        dyadstat(values = covar[,c(1,2,4)], type = 5, edgelist = statsA$edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"both_equal_to"], 
        dyadstat(values = covar[,c(1,2,4)], type = 6, edgelist = statsA$edgelist, riskset = statsA$riskset, equal_val = 0))
    
    expect_equal(statsB$statistics[,,"same"], 
        dyadstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsB$edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"difference"], 
        dyadstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsB$edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"mean"], 
        dyadstat(values = covar[,c(1,2,4)], type = 3, edgelist = statsB$edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"min"], 
        dyadstat(values = covar[,c(1,2,4)], type = 4, edgelist = statsB$edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"max"], 
        dyadstat(values = covar[,c(1,2,4)], type = 5, edgelist = statsB$edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"both_equal_to"], 
        dyadstat(values = covar[,c(1,2,4)], type = 6, edgelist = statsB$edgelist, riskset = statsB$riskset, equal_val = 0))

    expect_equal(statsC$statistics[,,"same"], 
        dyadstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsC$edgelist, riskset = statsC$riskset, equal_val = 0))
    expect_equal(statsC$statistics[,,"difference"], 
        dyadstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsC$edgelist, riskset = statsC$riskset, equal_val = 0))
    expect_equal(statsC$statistics[,,"mean"], 
        dyadstat(values = covar[,c(1,2,4)], type = 3, edgelist = statsC$edgelist, riskset = statsC$riskset, equal_val = 0))
    expect_equal(statsC$statistics[,,"min"], 
        dyadstat(values = covar[,c(1,2,4)], type = 4, edgelist = statsC$edgelist, riskset = statsC$riskset, equal_val = 0))
    expect_equal(statsC$statistics[,,"max"], 
        dyadstat(values = covar[,c(1,2,4)], type = 5, edgelist = statsC$edgelist, riskset = statsC$riskset, equal_val = 0))
    expect_equal(statsC$statistics[,,"both_equal_to"], 
        dyadstat(values = covar[,c(1,2,4)], type = 6, edgelist = statsC$edgelist, riskset = statsC$riskset, equal_val = 0))
})

test_that("event effect in remstats", {
    expect_equal(statsA$statistics[,1,"event_effect"], 
        statsA$statistics[,2,"event_effect"])
    expect_equal(statsA$statistics[,1,"event_effect"], 
        event_effect)
    
    expect_equal(statsB$statistics[,1,"event_effect"], 
        statsB$statistics[,2,"event_effect"])
    expect_equal(statsB$statistics[,1,"event_effect"], 
        event_effect)
    
    expect_equal(statsC$statistics[,1,"event_effect"], 
        statsC$statistics[,2,"event_effect"])
    expect_equal(statsC$statistics[,1,"event_effect"], 
        event_effect)
})

test_that("type effect in remstats", {
    expect_equal(statsC$statistics[,,"type_effect_2"],
        typestat(statsC$edgelist, statsC$riskset, 2))
    expect_equal(statsC$statistics[,,"type_effect_3"],
        typestat(statsC$edgelist, statsC$riskset, 3))
})

test_that("inertia effects in remstats", {
    expect_equal(statsA$statistics[,,"inertia"], 
        inertia(statsA$edgelist, statsA$riskset, equal_weights, FALSE))
    expect_equal(statsB$statistics[,,"inertia"], 
        inertia(statsB$edgelist, statsB$riskset, equal_weights, FALSE))
    expect_equal(statsC$statistics[,,"inertia"], 
        inertia(statsC$edgelist, statsC$riskset, equal_weights, FALSE))  
    expect_equal(single_statA$statistics[,,"inertia"], 
        inertia(single_statA$edgelist, single_statA$riskset, equal_weights, 
        FALSE))

    expect_equal(statsA$statistics[,,"inertia_weighted"], 
        inertia(statsA$edgelist, statsA$riskset, equal_weights, FALSE))
    expect_equal(statsB$statistics[,,"inertia_weighted"], 
        inertia(statsB$edgelist, statsB$riskset, equal_weights, FALSE))
    expect_equal(statsC$statistics[,,"inertia_weighted"], 
        inertia(statsC$edgelist, statsC$riskset, equal_weights, FALSE))  

    expect_equal(statsC$statistics[,,"inertia_type"], 
        inertia_type(statsC$edgelist, statsC$riskset, equal_weights, FALSE))   
})

test_that("reciprocity effects in remstats", {
    expect_equal(statsA$statistics[,,"reciprocity"], 
        reciprocity(statsA$edgelist, statsA$riskset, equal_weights, FALSE))
})

test_that("degree effects in remstats", {
    expect_equal(statsA$statistics[,,"indegree_sender"], 
        degree(edgelist = statsA$edgelist, riskset = statsA$riskset, type = 1, 
        FALSE))
    expect_equal(statsA$statistics[,,"indegree_receiver"], 
        degree(edgelist = statsA$edgelist, riskset = statsA$riskset, type = 2, 
        FALSE))
    expect_equal(statsA$statistics[,,"outdegree_sender"], 
        degree(edgelist = statsA$edgelist, riskset = statsA$riskset, type = 3, 
        FALSE))
    expect_equal(statsA$statistics[,,"outdegree_receiver"], 
        degree(edgelist = statsA$edgelist, riskset = statsA$riskset, type = 4, 
        FALSE))
    expect_equal(statsA$statistics[,,"totaldegree_sender"], 
        degree(edgelist = statsA$edgelist, riskset = statsA$riskset, type = 5, 
        FALSE))
    expect_equal(statsA$statistics[,,"totaldegree_receiver"], 
        degree(edgelist = statsA$edgelist, riskset = statsA$riskset, type = 6, 
        FALSE))
})

test_that("triad effects in remstats", {
    expect_equal(statsA$statistics[,,"OTP"], 
        triad(actors = statsA$actors[,1], edgelist = statsA$edgelist, riskset = 
        statsA$riskset, type = 1, FALSE))
    expect_equal(statsA$statistics[,,"ITP"], 
        triad(actors = statsA$actors[,1], edgelist = statsA$edgelist, riskset = 
        statsA$riskset, type = 2, FALSE))
    expect_equal(statsA$statistics[,,"OSP"], 
        triad(actors = statsA$actors[,1], edgelist = statsA$edgelist, riskset = 
        statsA$riskset, type = 3, FALSE))
    expect_equal(statsA$statistics[,,"ISP"], 
        triad(actors = statsA$actors[,1], edgelist = statsA$edgelist, riskset = 
        statsA$riskset, type = 4, FALSE))
    
    expect_equal(statsB$statistics[,,"shared_partners"], 
        triadU(actors = statsB$actors[,1], edgelist = statsB$edgelist, 
        riskset = statsB$riskset, unique_sp = FALSE, FALSE))
    expect_equal(statsB$statistics[,,"unique_sp"], 
        triadU(actors = statsB$actors[,1], edgelist = statsB$edgelist, 
        riskset = statsB$riskset, unique_sp = TRUE, FALSE))

    expect_equal(statsC$statistics[,,"shared_partners"], 
        triadU(actors = statsC$actors[,1], edgelist = statsC$edgelist, 
        riskset = statsC$riskset, unique_sp = FALSE, FALSE))
    expect_equal(statsC$statistics[,,"unique_sp"], 
        triadU(actors = statsC$actors[,1], edgelist = statsC$edgelist, 
        riskset = statsC$riskset, unique_sp = TRUE, FALSE))
    
    expect_equal(statsC$statistics[,,"shared_partners_type"], 
        triadU_type(actors = statsC$actors[,1], edgelist = statsC$edgelist, 
        riskset = statsC$riskset, unique_sp = FALSE, FALSE))
    expect_equal(statsC$statistics[,,"unique_sp_type"], 
        triadU_type(actors = statsC$actors[,1], edgelist = statsC$edgelist, 
        riskset = statsC$riskset, unique_sp = TRUE, FALSE))
})

test_that("interaction effects in remstats", {
    expect_equal(statsA$statistics[,,"sender_effect*inertia"],
    statsA$statistics[,,"sender_effect"]*statsA$statistics[,,"inertia"])
    expect_equal(statsA$statistics[,,"same*inertia"],
    statsA$statistics[,,"same"]*statsA$statistics[,,"inertia"])

    expect_equal(statsB$statistics[,,"same*inertia"],
    statsB$statistics[,,"same"]*statsB$statistics[,,"inertia"])

    expect_equal(statsC$statistics[,,"same*inertia"],
    statsC$statistics[,,"same"]*statsC$statistics[,,"inertia"])
})

