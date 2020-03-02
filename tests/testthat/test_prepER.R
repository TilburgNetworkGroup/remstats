context("prepER output")

require(remstats) 

test_that("prepER output, edgelist input contains factor variables for sender and receivers", {
    data(edgelistF)
    out <- prepER(edgelistF, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistF), 1:3]", "int[1:nrow(rs), 1:2]")
    # Test the time information
    expect_equal(el[,1], edgelistF[,1])
    # Test the senders and receivers
    expect_equal(unique(c(el[,2], el[,3])), unique(c(edgelistF[,2], edgelistF[,3])))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    expect_equal(nrow(rs), nAc*(nAc-1))
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
})

test_that("prepER output, edgelist input contains numeric variables for sender and receivers", {
    data(edgelistN)
    out <- prepER(edgelistN, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistN), 1:3]", "int[1:nrow(rs), 1:2]")
    # Test the time information
    expect_equal(el[,1], edgelistF[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistN[,2], edgelistN[,3]))))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    expect_equal(nrow(rs), nAc*(nAc-1))
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
})

test_that("prepER output, edgelist input contains integer variables for sender and receivers", {
    data(edgelistI)
    out <- prepER(edgelistI, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistI), 1:3]", "int[1:nrow(rs), 1:2]")
    # Test the time information
    expect_equal(el[,1], edgelistF[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistI[,2], edgelistI[,3]))))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    expect_equal(nrow(rs), nAc*(nAc-1))
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
})

test_that("prepER output, edgelist input contains character variables for sender and receivers", {
    data(edgelistC)
    out <- prepER(edgelistC, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistC), 1:3]", "int[1:nrow(rs), 1:2]")
    # Test the time information
    expect_equal(el[,1], edgelistF[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistC[,2], edgelistC[,3]))))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    expect_equal(nrow(rs), nAc*(nAc-1))
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
})

test_that("prepER output, edgelist input contains directed relational events", {
    data(edgelistD)
    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistD), 1:3]", "int[1:nrow(rs), 1:2]")
    # Test the time information
    expect_equal(el[,1], edgelistD[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistD[,2], edgelistD[,3]))))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    expect_equal(nrow(rs), nAc*(nAc-1))
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
})

test_that("prepER output, edgelist input contains undirected relational events", {
    data(edgelistU)
    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistU), 1:3]", "int[1:nrow(rs), 1:2]")
    # Test the time information
    expect_equal(el[,1], edgelistU[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistU[,2], edgelistU[,3]))))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    expect_equal(nrow(rs), choose(nAc, 2))
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
})

test_that("prepER output, edgelist input contains directed relational events that consider event type", {
    data(edgelistDT)
    out <- prepER(edgelistDT, directed = TRUE, type = TRUE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistDT), 1:4]", "int[1:nrow(rs), 1:3]")
    # Test the time information
    expect_equal(el[,1], edgelistDT[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistDT[,2], edgelistDT[,3]))))
    # Test the event types
    expect_equal(length(unique(el[,4])), length(unique(edgelistDT[,4])))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    nTy <- length(unique(rs[,3]))
    expect_equal(nrow(rs), nAc*(nAc-1)*nTy)
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
    # Test the match between event types in the edgelist and riskest
    expect_true(all(unique(el[,4]) %in% unique(rs[,3])))
})

test_that("prepER output, edgelist input contains undirected relational events that consider event type", {
    data(edgelistUT)
    out <- prepER(edgelistUT, directed = FALSE, type = TRUE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    # Test the output classes
    expect_output(str(out), "num[1:nrow(edgelistUT), 1:4]", "int[1:nrow(rs), 1:3]")
    # Test the time information
    expect_equal(el[,1], edgelistUT[,1])
    # Test the senders and receivers
    expect_equal(length(unique(c(el[,2], el[,3]))), length(unique(c(edgelistUT[,2], edgelistUT[,3]))))
    # Test the event types
    expect_equal(length(unique(el[,4])), length(unique(edgelistUT[,4])))
    # Test the length of the riskset
    nAc <- length(unique(c(rs[,1], rs[,2])))
    nTy <- length(unique(rs[,3]))
    expect_equal(nrow(rs), choose(nAc,2)*nTy)
    # Test the match between senders and receivers in the edgelist and riskset
    expect_true(all(unique(c(el[,2], el[,3])) %in% unique(c(rs[,1], rs[,2]))))
    # Test the match between event types in the edgelist and riskest
    expect_true(all(unique(el[,4]) %in% unique(rs[,3])))
})

test_that("prepER output; all events in the edgelist occur in the riskset", {
    data(edgelistD) 
    out <- prepER(edgelistD, directed = TRUE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    expect_true(all(apply(el, 1, function(x) {
	    length(which(rs[,1] == x[2] & rs[,2] == x[3])) > 0
    })))

    data(edgelistU) 
    out <- prepER(edgelistU, directed = FALSE, type = FALSE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    expect_true(all(apply(el, 1, function(x) {
	    length(which(rs[,1] == x[2] & rs[,2] == x[3])) > 0
    })))

    data(edgelistDT) 
    out <- prepER(edgelistDT, directed = TRUE, type = TRUE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    expect_true(all(apply(el, 1, function(x) {
	    length(which(rs[,1] == x[2] & rs[,2] == x[3] & rs[,3] == x[4])) > 0
    })))

    data(edgelistUT) 
    out <- prepER(edgelistUT, directed = FALSE, type = TRUE, riskset = NULL, 
        actors = NULL)
    el <- out$edgelist
    rs <- out$riskset

    expect_true(all(apply(el, 1, function(x) {
	    length(which(rs[,1] == x[2] & rs[,2] == x[3] & rs[,3] == x[4])) > 0
    })))

})