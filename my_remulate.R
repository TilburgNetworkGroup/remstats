#' @export
my_remulate <- function(TEMPparam, effects, actors, time, events = NULL, 
    start_time = 0, seed = NULL, damping = 0, attributes = NULL, 
    memory = c("full", "window", "decay", "interval"), memory_value = NA) {

    # To do: allow for initial edgelist! 
    # To do: allow for user risk set

    # Specify waiting time 
    waiting_time <- "exp"

    # Set seed
    if (!is.null(seed)) {
        set.seed(seed)
    }

    # if(is.data.frame(initial)) {
    #     reh <- initial
    #     colnames(reh) <- c("time", "actor1", "actor2")
    # } else {
    #     reh <- data.frame(time = 0, actor1 = actors[1], actor2 = actors[2])
    # }

    # Create a dummy reh (note: assumes at least two actors)    
    reh <- data.frame(time = 0, actor1 = actors[1], actor2 = actors[2])
    reh <- remify::remify(reh, actors = actors, model = "tie")

    # Prepare all required objects to compute statistics
    object_list <- remstats::prepare_tomstats(effects = effects, reh = reh, 
        attributes = attributes, memory = memory, memory_value = memory_value, 
        start = 1, stop = 1)

    form <- object_list$form
    all_effects <- object_list$all_effects
    effects <- object_list$effects
    effectsN <- object_list$effectsN
    edgelist <- object_list$edgelist
    actors <- object_list$actor
    types <- object_list$types
    riskset <- object_list$prepR
    memory <- object_list$memory
    memory_value <- object_list$memory_value
    scaling <- object_list$scaling
    covar <- object_list$covar
    interactions <- object_list$interactions

    # initialize start time as t=0 if simulating cold-start else set t as time 
    # of last event in initial edgelist
    # if (is.data.frame(initial)) {
    #     t <- initial[nrow(initial), 1]
    #     if (t > time) {
    #     stop("Last event of initial data.frame is after 'time' argument")
    #     }
    # } else {
    #     # in case is.numeric(initial) OR intial == NULL
    #     t <- start_time
    # }
    t <- start_time

    # initialize params
    P <- length(TEMPparam)
    beta <- vector(length = P)
    for (i in 1:P) {
        if (class(TEMPparam[[i]]) == "function") {
        # function must be defined at t=0
        beta[i] <- TEMPparam[[i]](t)
        } else {
        beta[i] <- TEMPparam[[i]]
        }
    }

    # Initialize output objects
    statistics <- list() # list of matrices
    statistics[[1]] <- array(0, dim = c(nrow(riskset), P))
    if (any(effectsN == 1)) { # fill in baseline for first time point
        statistics[[1]][, which(effectsN == 1)] <- 
            array(1, dim = c(nrow(riskset), 1))
    }
    edgelist <- array(0, dim = c(1, 3))
    evls <- array(0, dim = c(1, 3)) # @m changed! 
    probs <- array(0, dim = c(1, nrow(riskset)))

    # Initialize the adjacency matrix
    adjmat <- matrix()

    i <- 1
    while(t <= time) {
        # updating event rate / lambda
        if (P == 1) {
        lambda <- exp(statistics[[i]] * beta)
        } else {
        lambda <- exp(statistics[[i]] %*% beta)
        }

        # sampling waiting time dt
        if (waiting_time == "exp") {
        dt <- rexp(1, rate = sum(lambda))
        t <- t + dt
        } else if (waiting_time == "weibull") {
        # TODO: add checks on time params
        dt <- rweibull(1, shape = time_param, scale = sum(lambda))
        t <- t + dt
        } else if (waiting_time == "gompertz") {
        dt <- rgompertz(1, scale = sum(lambda), shape = time_param)
        t <- t + dt
        }

        if (t > time) {
        cat(i - 1, "events generated \n")
        break
        }

        # sampling dyad for next event
        # R sampling slightly faster than arma sampling (due to hashing)
        if (runif(1) < damping) {
        dyad <- sample(1:nrow(riskset), 1)
        } else {
        dyad <- sample(1:nrow(riskset), 1, prob = lambda / sum(lambda))
        }

        edgelist[i, ] <- c(t, riskset[dyad, 1], riskset[dyad, 2])
        evls[i, ] <- c(t, dyad, 1)  # @m changed! 
        probs[i, ] <- lambda

        # stop if max number of events reached
        if (!is.null(events) && i - 1 >= events) {
        cat(paste0("Stopping: maximum number of events (", i - 1, ") sampled \n"))
        break
        }

        # Dummy evls (because stats are computed for the past)
        dummy_evls <- rbind(evls, c(t + 1, 1, 1))
        dummy_evls[,2] <- dummy_evls[,2] - 1

        # Update adjmat
        if (any(effectsN %in% c(10:23, 40:45, 52:59, 67:70, 72, 76:77))) {
            adjmat <- compute_adjmat(
                dummy_evls, nrow(actors), reh$D, attr(reh, "directed"), 
                memory, memory_value, i, i
            )
        } 

        # Compute statistics
        statistics_array <- compute_stats_tie(
            effectsN, dummy_evls, adjmat, actors[, 2], types[, 2], riskset, 
            scaling, covar, interactions, i, i, attr(reh, "directed")
        )
        statistics[[i + 1]] <- aperm(statistics_array, c(2,3,1))[,,1]

        # add row for next iteration
        edgelist <- rbind(edgelist, array(0, dim = c(1, 3)))
        evls <- rbind(evls, array(0, dim = c(1, 3)))
        probs <- rbind(probs, array(0, dim = c(1, nrow(riskset))))

        # update beta
        for (j in 1:P) {
        if (class(TEMPparam[[j]]) == "function") {
            beta[j] <- TEMPparam[[j]](t)
        } else {
            beta[j] <- TEMPparam[[j]]
        }
        }
        i <- i + 1
    }

    # combine stats from list to 3d array
    statistics <- array(
        data = do.call(rbind, lapply(statistics, as.vector)),
        dim = c(length(statistics), dim(statistics[[1]]))
    )

    statistics <- statistics[-dim(statistics)[1], , ]
    edgelist <- edgelist[-dim(edgelist)[1], ]
    evls <- evls[-dim(evls)[1], c(2,1)]

    edgelist <- as.data.frame(edgelist)
    colnames(edgelist) <- c("time", "sender", "receiver")

    # change actor ids to names in edgelist
    edgelist["sender"] <- lapply(edgelist["sender"], function(x) {
        actors$actorName[x + 1]
    })
    edgelist["receiver"] <- lapply(edgelist["receiver"], function(x) {
        actors$actorName[x + 1]
    })

    # Add variable names to the statistics dimnames
    statistics <- remstats::add_variable_names(statistics, all_effects, 
        effectsN, effects, interactions)

    names(TEMPparam) <- dimnames(statistics)[[3]]

    return(
        list(
        edgelist = edgelist,
        evls = evls,
        statistics = statistics,
        params = TEMPparam,
        riskset = riskset,
        actors = actors,
        #density = get.density(evls, actors),
        probs = probs
        )
    )
}