context("remstatsMW")

library(remstats)

# Data
data(edgelistD)
data(edgelistUT)

# Covariates
data(covar)

# Prepare windows
windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))

edgelistDw2 <- edgelistD[edgelistD$time > windows$start[2] & 
    edgelistD$time <= windows$end[2],]

edgelistUTw2 <- edgelistUT[edgelistUT$time > windows$start[2] & 
    edgelistUT$time <= windows$end[2],]

# Effects
effectsD <- c("sender_effect", "receiver_effect", "same", "difference",  
        "mean", "min", "max", "both_equal_to", "event_effect", "inertia", "inertia_weighted", "sender_effect*inertia", "same*inertia")

effectsUT <- c("same", "difference",  "mean", "min", "max", "both_equal_to", 
    "event_effect", "type_effect", "inertia", "inertia_weighted", "inertia_type", "inertia_type_weighted", "shared_partners", "unique_sp", "shared_partners_type", "unique_sp_type", "same*inertia")

# Covariates 
covariates <- rep(list(covar[,c(1,2,4)]), 8)
names(covariates) <- c("sender_effect", "receiver_effect", "same", 
    "difference", "mean", "min", "max", "both_equal_to")

# Event effect
event_effect <- sample(c(0,1), nrow(edgelistDw2), replace = TRUE)

# Weights
equal_weights <- rep(1, nrow(edgelistD))

# Statistics
statsA <- remstatsMW(full_edgelist = edgelistD, window_edgelist = edgelistDw2, 
    effects = effectsD, window_length = 100, covariates = covariates, 
    event_effect = event_effect, full_weights = equal_weights, equal_val = 0)
statsB <- remstatsMW(full_edgelist = edgelistUT, 
    window_edgelist = edgelistUTw2, effects = effectsUT, window_length = 100, 
    directed = FALSE, type = TRUE, covariates = covariates, 
    event_effect = event_effect, full_weights = equal_weights, equal_val = 0)

# Tests
test_that("dimensions of the output of remstatsMW", {
    expect_output(str(statsA), "List of 7")
    expect_output(str(statsB), "List of 7")
})

test_that("baseline in remstatsMW", {
    expect_equal(statsA$statistics[,,"baseline"], 
        matrix(1, nrow(statsA$window_evls), nrow(statsA$riskset)))
    expect_equal(statsB$statistics[,,"baseline"], 
        matrix(1, nrow(statsB$window_evls), nrow(statsB$riskset)))
})

test_that("actor effects in remstatsMW", {
    covar$id <- statsA$actors$id[match(covar$id, statsA$actors$name)]
    covar <- as.matrix(covar)

    expect_equal(statsA$statistics[,,"sender_effect_x2"], 
        actorstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsA$window_edgelist, riskset = statsA$riskset))
    expect_equal(statsA$statistics[,,"receiver_effect_x2"], 
        actorstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsA$window_edgelist, riskset = statsA$riskset))
})

test_that("dyad effects in remstatsMW", {
    covar$id <- statsA$actors$id[match(covar$id, statsA$actors$name)]
    covar <- as.matrix(covar)

    expect_equal(statsA$statistics[,,"same_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsA$window_edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"difference_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsA$window_edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"mean_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 3, edgelist = statsA$window_edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"min_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 4, edgelist = statsA$window_edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"max_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 5, edgelist = statsA$window_edgelist, riskset = statsA$riskset, equal_val = 0))
    expect_equal(statsA$statistics[,,"both_equal_to_x20"], 
        dyadstat(values = covar[,c(1,2,4)], type = 6, edgelist = statsA$window_edgelist, riskset = statsA$riskset, equal_val = 0))
    
    expect_equal(statsB$statistics[,,"same_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 1, edgelist = statsB$window_edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"difference_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 2, edgelist = statsB$window_edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"mean_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 3, edgelist = statsB$window_edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"min_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 4, edgelist = statsB$window_edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"max_x2"], 
        dyadstat(values = covar[,c(1,2,4)], type = 5, edgelist = statsB$window_edgelist, riskset = statsB$riskset, equal_val = 0))
    expect_equal(statsB$statistics[,,"both_equal_to_x20"], 
        dyadstat(values = covar[,c(1,2,4)], type = 6, edgelist = statsB$window_edgelist, riskset = statsB$riskset, equal_val = 0))
})

test_that("event effect in remstatsMW", {
    expect_equal(statsA$statistics[,1,"event_effect1"], 
        statsA$statistics[,2,"event_effect1"])
    expect_equal(statsA$statistics[,1,"event_effect1"], 
        event_effect)
    
    expect_equal(statsB$statistics[,1,"event_effect1"], 
        statsB$statistics[,2,"event_effect1"])
    expect_equal(statsB$statistics[,1,"event_effect1"], 
        event_effect)
})

test_that("type effect in remstatsMW", {
    expect_equal(statsB$statistics[,,"type_effect_2"],
        typestat(statsB$window_edgelist, statsB$riskset, 2))
    expect_equal(statsB$statistics[,,"type_effect_3"],
        typestat(statsB$window_edgelist, statsB$riskset, 3))
})

test_that("inertia effects in remstatsMW", {
    expect_equal(statsA$statistics[,,"inertia"], 
        inertiaMW(statsA$full_edgelist, statsA$window_edgelist, 100, statsA$riskset, equal_weights, FALSE))
    expect_equal(statsA$statistics[,,"inertia_weighted"], 
        inertiaMW(statsA$full_edgelist, statsA$window_edgelist, 100,  statsA$riskset, equal_weights, FALSE))
    
    expect_equal(statsB$statistics[,,"inertia"], 
        inertiaMW(statsB$full_edgelist, statsB$window_edgelist, 100, statsB$riskset, equal_weights, FALSE))
    expect_equal(statsB$statistics[,,"inertia_weighted"], 
        inertiaMW(statsB$full_edgelist, statsB$window_edgelist, 100,  statsB$riskset, equal_weights, FALSE))
    expect_equal(statsB$statistics[,,"inertia_type"], 
        inertia_typeMW(statsB$full_edgelist, statsB$window_edgelist, 100, statsB$riskset, equal_weights, FALSE))   
})

test_that("triad effects in remstats", {    
    expect_equal(statsB$statistics[,,"shared_partners"], 
        triadUMW(actors = statsB$actors[,1], 
        full_edgelist = statsB$full_edgelist, 
        window_edgelist = statsB$window_edgelist, window_length = 100, 
        riskset = statsB$riskset, unique_sp = FALSE, FALSE))
    expect_equal(statsB$statistics[,,"unique_sp"], 
        triadUMW(actors = statsB$actors[,1], 
        full_edgelist = statsB$full_edgelist, 
        window_edgelist = statsB$window_edgelist, window_length = 100, 
        riskset = statsB$riskset, unique_sp = TRUE, FALSE))
    expect_equal(statsB$statistics[,,"shared_partners_type"], 
        triadU_typeMW(actors = statsB$actors[,1], 
        full_edgelist = statsB$full_edgelist, 
        window_edgelist = statsB$window_edgelist, window_length = 100, 
        riskset = statsB$riskset, unique_sp = FALSE, FALSE))
    expect_equal(statsB$statistics[,,"unique_sp_type"], 
        triadU_typeMW(actors = statsB$actors[,1], 
        full_edgelist = statsB$full_edgelist, 
        window_edgelist = statsB$window_edgelist, window_length = 100, 
        riskset = statsB$riskset, unique_sp = TRUE, FALSE))
})

test_that("interaction effects in remstats", {
    expect_equal(statsA$statistics[,,"sender_effect_x2*inertia"],
    statsA$statistics[,,"sender_effect_x2"]*statsA$statistics[,,"inertia"])
    expect_equal(statsA$statistics[,,"same_x2*inertia"],
    statsA$statistics[,,"same_x2"]*statsA$statistics[,,"inertia"])

    expect_equal(statsB$statistics[,,"same_x2*inertia"],
    statsB$statistics[,,"same_x2"]*statsB$statistics[,,"inertia"])
})
