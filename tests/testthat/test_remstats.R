context("remstats")

require(remstats)

test_that("Output remstats for dyadic directed relational events", {
    # Test for directed relational events
    data("edgelistD")
    data(covar)

    effects <- c("sender_effect", "receiver_effect", "same", "difference", 
        "mean", "min", "max", "both_equal_to", "inertia", "inertia_weighted", 
        "reciprocity", "indegree_sender", "indegree_receiver", 
        "outdegree_sender", "outdegree_receiver", "totaldegree_sender", 
        "totaldegree_receiver", "OTP", "ITP", "OSP", "ISP", "sender_effect2*inertia", "max*reciprocity")
    
    covariates <- list(sender_effect = covar, 
        receiver_effect = covar[,c(1:3)], 
		same = covar[,c(1:2, 4)],
        difference = covar,
        mean = covar[,c(1:3)],
        min = covar[,c(1:3)],
        max = covar[,c(1:3)],
        both_equal_to = covar[,c(1:2, 4)])

    out <- remstats(edgelist = edgelistD, effects = effects, 
        covariates = covariates, equal_val = 0)

    # General output
    expect_output(str(out), "List of 5")

    # Statistics
    expect_output(str(out$statistics), 
        "num[1:nrow(edgelistD), 1:nrow(out$riskset), 1:length(effects)]")

    expect_true(all(out$statistics[,,"baseline"]==1))
    expect_equal(out$statistics[,,"inertia"], 
        inertia(out$evls, out$riskset, rep(1, nrow(edgelistD)), 
        standardize = FALSE))
    expect_equal(out$statistics[,,"inertia_weighted"], 
        out$statistics[,,"inertia"])
    expect_equal(out$statistics[,,"reciprocity"], 
        reciprocity(out$edgelist, out$riskset, FALSE))
    expect_equal(out$statistics[,,"indegree_sender"], 
        degree(out$edgelist, out$riskset, 1, FALSE))
    expect_equal(out$statistics[,,"indegree_receiver"], 
        degree(out$edgelist, out$riskset, 2, FALSE))
    expect_equal(out$statistics[,,"outdegree_sender"], 
        degree(out$edgelist, out$riskset, 3, FALSE))
    expect_equal(out$statistics[,,"outdegree_receiver"], 
        degree(out$edgelist, out$riskset, 4, FALSE))
    expect_equal(out$statistics[,,"totaldegree_sender"], 
        degree(out$edgelist, out$riskset, 5, FALSE))
    expect_equal(out$statistics[,,"totaldegree_receiver"], 
        degree(out$edgelist, out$riskset, 6, FALSE))
    expect_equal(out$statistics[,,"OTP"], 
        triad(out$actors[,1], out$edgelist, out$riskset, 1, FALSE))
    expect_equal(out$statistics[,,"ITP"], 
        triad(out$actors[,1], out$edgelist, out$riskset, 2, FALSE))
    expect_equal(out$statistics[,,"OSP"], 
        triad(out$actors[,1], out$edgelist, out$riskset, 3, FALSE))
    expect_equal(out$statistics[,,"ISP"], 
        triad(out$actors[,1], out$edgelist, out$riskset, 4, FALSE))   
    expect_equal(out$statistics[,,"sender_effect2*inertia"],
        out$statistics[,,3]*out$statistics[,,"inertia"])
    expect_equal(out$statistics[,,"max*reciprocity"],
        out$statistics[,,"max"]*out$statistics[,,"reciprocity"])

    # Edgelist 
    expect_output(str(out$edgelist), "num[1:nrow(edgelistD), 1:3]")
    expect_equal(out$edgelist, prepER(edgelistD)$edgelist)

    # Riskset
    expect_output(str(out$riskset), 
        "int[1:(length(out$actors)*(length(out$actors)-1)), 1:2]")
    expect_equal(out$riskset, prepER(edgelistD)$riskset)
    
    # Evls
    expect_output(str(out$evls), "num[1:nrow(edgelistD), 1:2]")
    expect_equal(out$evls, prepEvls(out$edgelist, out$riskset))
})

test_that("Output remstats for dyadic undirected relational events", {
    # Test for undirected relational events
    data("edgelistU")

    effects = c("inertia", "shared_partners", "inertia_weighted", "unique_sp")
    out <- remstats(edgelist = edgelistU, effects = effects, directed = FALSE)

    # General output
    expect_output(str(out), "List of 5")

    # Statistics
    expect_output(str(out$statistics), 
        "num[1:nrow(edgelistD), 1:nrow(out$riskset), 1:length(effects)]")
    expect_equal(dimnames(out$statistics)[[3]], c("baseline", effects))

    expect_true(all(out$statistics[,,"baseline"]==1))
    expect_equal(out$statistics[,,"inertia"], 
        inertia(out$evls, out$riskset, rep(1, nrow(edgelistD)), 
        standardize = FALSE))
    expect_equal(out$statistics[,,"shared_partners"], 
        triadU(out$actors[,1], out$edgelist, out$riskset, FALSE, FALSE))  
    expect_equal(out$statistics[,,"inertia_weighted"], 
        out$statistics[,,"inertia"])
     expect_equal(out$statistics[,,"unique_sp"], 
        triadU(out$actors[,1], out$edgelist, out$riskset, TRUE, FALSE))

    # Edgelist 
    expect_output(str(out$edgelist), "num[1:nrow(edgelistD), 1:3]")
    expect_equal(out$edgelist, prepER(edgelistU, directed = FALSE)$edgelist)

    # Riskset
    expect_output(str(out$riskset), 
        "int[1:(length(out$actors)*(length(out$actors)-1)), 1:2]")
    expect_equal(out$riskset, prepER(edgelistU, directed = FALSE)$riskset)
    
    # Evls
    expect_output(str(out$evls), "num[1:nrow(edgelistD), 1:2]")
    expect_equal(out$evls, prepEvls(out$edgelist, out$riskset))
})