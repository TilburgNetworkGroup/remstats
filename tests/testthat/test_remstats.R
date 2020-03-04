context("remStats")

require(remstats)

test_that("Output remstats for dyadic directed relational events", {
    # Test for directed relational events
    data("edgelistD")

    effects = c("inertia", "reciprocity", "indegree_sender",
    "indegree_receiver", "outdegree_sender", "outdegree_receiver", 
    "totaldegree_sender", "totaldegree_receiver", "OTP", "ITP", "OSP", "ISP", 
    "inertia_weighted")

    out <- remStats(edgelistD, effects)

    # General output
    expect_output(str(out), "List of 5")

    # Statistics
    expect_output(str(out$statistics), 
        "num[1:nrow(edgelistD), 1:nrow(out$riskset), 1:length(effects)]")
    expect_equal(dimnames(out$statistics)[[3]], c("baseline", effects))

    expect_true(all(out$statistics[,,1]==1))
    expect_equal(out$statistics[,,2], 
        inertia(out$evls, out$riskset, rep(1, nrow(edgelistD))))
    expect_equal(out$statistics[,,3], reciprocity(out$edgelist, out$riskset))
    expect_equal(out$statistics[,,4], degree(out$edgelist, out$riskset, 1))
    expect_equal(out$statistics[,,5], degree(out$edgelist, out$riskset, 2))
    expect_equal(out$statistics[,,6], degree(out$edgelist, out$riskset, 3))
    expect_equal(out$statistics[,,7], degree(out$edgelist, out$riskset, 4))
    expect_equal(out$statistics[,,8], degree(out$edgelist, out$riskset, 5))
    expect_equal(out$statistics[,,9], degree(out$edgelist, out$riskset, 6))
    expect_equal(out$statistics[,,10], 
        triad(out$actors, out$edgelist, out$riskset, 1))
    expect_equal(out$statistics[,,11], 
        triad(out$actors, out$edgelist, out$riskset, 2))
    expect_equal(out$statistics[,,12], 
        triad(out$actors, out$edgelist, out$riskset, 3))
    expect_equal(out$statistics[,,13], 
        triad(out$actors, out$edgelist, out$riskset, 4))
    expect_equal(out$statistics[,,14], out$statistics[,,2])

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
    out <- remStats(edgelistU, effects, directed = FALSE)

    # General output
    expect_output(str(out), "List of 5")

    # Statistics
    expect_output(str(out$statistics), 
        "num[1:nrow(edgelistD), 1:nrow(out$riskset), 1:length(effects)]")
    expect_equal(dimnames(out$statistics)[[3]], c("baseline", effects))

    expect_true(all(out$statistics[,,1]==1))
    expect_equal(out$statistics[,,2], 
        inertia(out$evls, out$riskset, rep(1, nrow(edgelistD))))
    expect_equal(out$statistics[,,3], 
        triadU(out$actors, out$edgelist, out$riskset, FALSE))  
    expect_equal(out$statistics[,,4], out$statistics[,,2])
     expect_equal(out$statistics[,,5], 
        triadU(out$actors, out$edgelist, out$riskset, TRUE  ))

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