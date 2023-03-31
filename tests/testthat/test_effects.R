library(remstats)

test_that("tie_effects", {

    # Standard
    out <- tie_effects()
    effects <- c(
        "send",
        "receive",
        "tie",
        "same",
        "difference",
        "average",
        "minimum",
        "maximum",
        "event",
        "indegreeSender",
        "indegreeReceiver",
        "outdegreeSender",
        "outdegreeReceiver",
        "totaldegreeSender",
        "totaldegreeReceiver",
        "totaldegreeDyad",
        "degreeDiff",
        "degreeMin",
        "degreeMax",
        "inertia",
        "reciprocity",
        "otp",
        "itp",
        "osp",
        "isp",
        "sp",
        "spUnique",
        #"ccp",
        "psABBA",
        "psABBY",
        "psABXA",
        "psABXB",
        "psABXY",
        "psABAY",
        "psABAB",
        "rrankSend",
        "rrankReceive",
        "recencySendSender",
        "recencySendReceiver",
        "recencyReceiveSender",
        "recencyReceiveReceiver",
        "recencyContinue",
        "FEtype",
        "userStat"
    )

    expect_equal(out, effects)

    out <- tie_effects(directed = TRUE)
    directed_effects <- effects[!(effects %in% c(
                "sp", "spUnique", "degreeMin", "degreeMax", "ccp", "degreeDiff"
            ))]

    expect_equal(out, directed_effects)

    out <- tie_effects(directed = FALSE)
    undirected_effects <- effects[!(effects %in% c(
                "send", "receive", "reciprocity", "indegreeSender",
                "indegreeReceiver", "outdegreeSender", "outdegreeReceiver",
                "totaldegreeSender", "totaldegreeReceiver", "otp", "itp",
                "osp", "isp", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY",
                "rrankSend", "rrankReceive", "recencySendSender",
                "recencySendReceiver", "recencyReceiveSender",
                "recencyReceiveReceiver"
            ))]
    expect_equal(out, undirected_effects)
})

test_that("actor_effects", {
    out <- actor_effects()
    effects <- list(
        sender = c(
            "baseline",
            "send",
            "indegreeSender",
            "outdegreeSender",
            "totaldegreeSender",
            "recencySendSender",
            "recencyReceiveSender"
        ),
        receiver = c(
            "receive",
            "same",
            "difference",
            "average",
            "tie",
            "inertia",
            "reciprocity",
            "indegreeReceiver",
            "outdegreeReceiver",
            "totaldegreeReceiver",
            "otp",
            "itp",
            "osp",
            "isp",
            "rrankSend",
            "rrankReceive",
            "recencySendReceiver",
            "recencyReceiveReceiver",
            "recencyContinue"
        )
    )
    expect_equal(out, effects)

    out <- actor_effects(step = "sender")
    expect_equal(out, effects$sender)

    out <- actor_effects(step = "receiver")
    expect_equal(out, effects$receiver)
})