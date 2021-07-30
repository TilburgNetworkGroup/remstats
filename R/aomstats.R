#' aomstats
#' 
#' Computes statistics for the sender activity rate step and receiver choice 
#' step in actor-oriented relational event models (e.g., see Stadtfeld & Block, 
#' 2017). 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be 
#' supplied to the \code{sender_effects} and/or \code{receiver_effects} 
#' arguments in the form \code{~ effects}. The terms are separated by + 
#' operators. For example: \code{receiver_effects = ~ inertia() + otp()}.
#' Interactions between two effects can be included with * or : 
#' operators. For example: \code{receivereffects = ~ inertia():otp()}. A list 
#' of available effects and their corresponding statistics follows at the 
#' bottom. 
#' 
#' For the computation of the \emph{exogenous} statistics an attributes object 
#' with the exogenous covariate information has to be supplied to the 
#' \code{attributes} argument in either \code{aomstats()} or in the separate 
#' effect functions supplied to the \code{sender_effects} or 
#' \code{receiver_effects} argument (e.g., see \code{\link{send}}). This 
#' \code{attributes} object should be constructed as follows: A dataframe with 
#' rows refering to the attribute value of actor \emph{i} at timepoint 
#' \emph{t}. An `id` column is required that contains the actor id 
#' (corresponding to the actor id's in the edgelist). A `time` column is 
#' required that contains the time when attributes change (set to zero if none 
#' of the attributes vary over time). Subsequent columns contain the attributes 
#' that are called in the specifications of exogenous statistics (column name 
#' corresponding to the string supplied to the \code{variable} argument in the 
#' effect function). Note that the procedure for the exogenous effect `tie' 
#' deviates from this, here the exogenous covariate information has to be 
#' specified in a different way, see \code{\link{tie}}. 
#' 
#' The majority of the statistics can be scaled in some way, see 
#' the documentation of the \code{scaling} argument in the separate effect 
#' functions for more information on this. 
#' 
#' Two more elements can affect the computation of the 
#' \emph{endogenous} statistics: the settings of the \code{memory} and 
#' \code{memoryValue} arguments in \code{aomstats} and the events weights in 
#' the supplied \code{edgelist} object. First, the memory settings affect the 
#' way past events are included in the computation of the endogenous 
#' statistics. Options are one of "full" (all past events are considered), 
#' "window" (only past events within a given time interval are considered) or 
#' "Brandes" (the weight of events depends on the elapsed time through an 
#' exponential decay with a half-life parameter). Second, the weight of the 
#' events affect the way past events are summed in the computation of the 
#' endogenous statistics, namely based on their weight. Note that if the 
#' edgelist contains a column that is named ``weight'', it is assumed that 
#' these affect the endogenous statistics. These settings are defined globally 
#' in the \code{aomstats} function and affect the computation of all endogenous 
#' statistics with the following exceptions (that follow logically from their 
#' definition). Since the recency statistics (recencyContinue, 
#' recencySendSender, recencySendReceiver, recencyReceiveSender, 
#' recencyReceiveReceiver) depend on the time past, the computation of these 
#' statistics do not depend on event weights and are therefore affected by 
#' "window" memory but not by "Brandes" memory or supplied event weights. The 
#' recency-rank statistics (rrankSend, rrankReceive) are (for now) only 
#' available with the "full" memory, and are, per definition, not affected by 
#' supplied event weights.  
#' 
#' Optionally, statistics can be computed for a slice of the edgelist - but 
#' based on the entire history. This is achieved by setting the start and 
#' stop values equal to the index of the first and last event for which 
#' statistics are requested. For example, start = 5 and stop = 5 computes the 
#' statistics for only the 5th event in the edgelist, based on the history that 
#' consists of events 1-4. 
#' 
#' Optionally, a previously computed adjacency matrix can be supplied. Note 
#' that the endogenous statistics will be computed based on this adjacency 
#' matrix. Hence, supplying a previously computed adjacency matrix can reduce 
#' computation time but the user should be absolutely sure the adjacency matrix 
#' is accurate. 
#' 
#' A list of available effects and their corresponding statistics for the 
#' \emph{sender activity rate} step: 
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{send}()}
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{totaldegreeSender}()}
#'  \item \code{\link{recencySendSender}()}
#'  \item \code{\link{recencyReceiveSender}()}
#' }
#' 
#' A list of available effects and their corresponding statistics for the 
#' \emph{receiver choice} step: 
#' \itemize{
#'  \item \code{\link{receive}()}
#'  \item \code{\link{tie}()}
#'  \item \code{\link{same}()}
#'  \item \code{\link{difference}()}
#'  \item \code{\link{average}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeReceiver}()}
#'  \item \code{\link{inertia}()}
#'  \item \code{\link{reciprocity}()}
#'  \item \code{\link{otp}()}
#'  \item \code{\link{itp}()}
#'  \item \code{\link{osp}()}
#'  \item \code{\link{isp}()}
#'  \item \code{\link{rrankSend}()}
#'  \item \code{\link{rrankReceive}()}
#'  \item \code{\link{recencySendReceiver}()}
#'  \item \code{\link{recencyReceiveReceiver}()}
#'  \item \code{\link{recencyContinue}()}
#' }
#' 
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history 
#' sorted by time with columns `time`, `actor1`, `actor2` and optionally `type` 
#' and `weight`. Alternatively, an object of class \code{"\link[remify]{reh}"}.
#' @param sender_effects an object of class \code{"\link[stats]{formula}"} (or 
#' one that can be coerced to that class): a symbolic description of the 
#' effects in the sender activity rate step of the actor-oriented model for 
#' which statistics are computed, see `Details'
#' @param receiver_effects an object of class \code{"\link[stats]{formula}"} 
#' (or one that can be coerced to that class): a symbolic description of the 
#' effects in the receiver choice step of model for which statistics are 
#' computed, see `Details'
#' @param attributes optionally, an object of class 
#' \code{"\link[base]{data.frame}"} that contains the exogenous attributes (see 
#' Details).
#' @inheritParams remify::reh
#' @param memory indicates the type of memory effect, i.e., how past events 
#' influence future events. One of "full" (all past events are considered), 
#' "window" (only past events within a given time interval are considered) or 
#' "Brandes" (the weight of events depends on the elapsed time through an 
#' exponential decay with a half-life parameter)
#' @param memory_value numeric value indicating the memory parameter, i.e., the 
#' window width if memory is "full", and the half-life time if memory is 
#' "Brandes"
#' @param start integer value, refers to the index in the edgelist of the first 
#' event for which statistics are requested (see Details)
#' @param stop integer value, refers to the index in the edgelist of the last 
#' event for which statistics are requested (see Details)
#' @param adjmat optionally, an adjacency matrix with on the rows the 
#' timepoints and on the columns the riskset entries
#' 
#' @return \code{edgelist } Dataframe with the edgelist
#' @return \code{statistics  } List with in the first element the statistics 
#' for the sender activity rate step and in the second element the statistics 
#' for the receiver choice step
#' @return \code{adjmat } Matrix with the adjacency matrix, rows refer to 
#' timepoints and columns to riskset entries
#' 
#' @examples 
#' library(remstats)
#' seff <- ~ send("extraversion")
#' reff <- ~ receive("agreeableness") + inertia() + otp()
#' aomstats(edgelist = history, sender_effects = seff, receiver_effects = reff, 
#'  attributes = info)
#' 
#' @references Stadtfeld, C., & Block, P. (2017). Interactions, actors, and 
#' time: Dynamic network actor models for relational events. Sociological 
#' Science, 4, 318–352. \url{https://doi.org/10.15195/v4.a14}
#' 
#' 
#' @export 
aomstats <- function(edgelist, sender_effects = NULL, receiver_effects = NULL,  
    attributes = NULL, actors = NULL, types = NULL, ordinal = FALSE, 
    origin = NULL, omit_dyad = NULL, memory = "full", 
    memory_value = Inf, start = 1, stop = Inf, adjmat = NULL, verbose = FALSE) {

    # Prepare the edgelist 
    if(!("reh" %in% class(edgelist))) {
        prep <- remify::reh(edgelist = edgelist, actors = actors, 
            types = types, directed = TRUE, ordinal = ordinal, origin = origin, 
            omit_dyad = omit_dyad)
    } else {
        prep <- edgelist 
    }

    # Extract relevant elements from the prepared remify::reh object 
    prepE <- as.matrix(prep$edgelist)
    prepE[,1] <- as.numeric(prep$edgelist$time)
    prepE <- t(apply(prepE, 1, as.numeric))
    prepR <- prep$risksetMatrix
    prepRC <- prep$risksetCube #(not given in new version remify)
    actors <- attr(prep, "dictionary")$actors
    types <- attr(prep, "dictionary")$types

    # Check for event types
    if(nrow(types)>1) {
        stop("Multiple event types are not (yet) defined for the actor-oriented model.")
    }

    # Edgelist in new version of remify
    rp <- apply(prepE, 1, function(x) {
        prepRC[as.numeric(x[2])+1, as.numeric(x[3])+1, as.numeric(x[4])+1] 
    })

    newE <- cbind(prepE[,1], rp, prepE[,5])
    colnames(newE) <- c("time", "event", "weight")

    # Match memory
    memory <- match(memory, c("full", "window", "Brandes"))
    if(memory %in% c(2,3) & memory_value == Inf) {
        stop("A memory_value should be supplied when memory is `window' or `Brandes'.")
    }
    
    # Convert R start and stop indices to C++ (indexing starts at 0)
    start <- start - 1
    if(stop == Inf) {stop <- nrow(prepE)}
    stop <- stop - 1

    # Initialize stats
    rateStats <- NULL
    choiceStats <- NULL

    # sender_effects
    if(!is.null(sender_effects)) {
        # Prepare main sender_effects
        rateFormula <- sender_effects
        sender_effects <- parse_formula(rateFormula, "rateEffects")
        all_sender_effects <- c("baseline", "send", # 1,2
            "indegreeSender", "outdegreeSender", "totaldegreeSender", # 3,4,5
            "recencySendSender", "recencyReceiveSender", #6, 7, 
            "interact") #99
        sender_effectsN  <- match(sapply(sender_effects, function(x) x$effect), 
            all_sender_effects)
        
        # Prepare interaction sender_effects
        sender_effects_int <- parse_int(rateFormula, "rateEffects", 
            sender_effects)
        sender_effectsN <- append(sender_effectsN, 
            rep(99, length(sender_effects_int)), 
            length(sender_effectsN))
        rate_interactions <- list()
        rate_interactions[which(sender_effectsN==99)] <- sender_effects_int

        # Prepare sender_effects covariate information
        rateCovar <- lapply(sender_effects, function(x) {
            if(x$effect == "send") {
                if(is.null(x$x)) {
                    dat <- data.frame(
                        id = attributes$id, 
                        time = attributes$time,
                        x = attributes[, x$variable]
                    )
                    dat$id <- attr(prep, "dictionary")$actors[match(dat$id, attr(prep, "dictionary")$actors[,1]),2]
                    colnames(dat)[3] <- x$variable
                    as.matrix(dat)
                } else {
                    dat <- x$x
                    dat$id <- attr(prep, "dictionary")$actors[match(dat$id, attr(prep, "dictionary")$actors[,1]),2]
                    as.matrix(dat)
                }
            } else {
                matrix()
            }
        })

        # Prepare sender_effects scaling 
        rateScaling <- as.numeric(
            sapply(sender_effects, function(x) x$scaling))

        # Compute the adjacency matrix 
        if(any(sender_effectsN %in% c(3,4,5))) {
            if(is.null(adjmat)) {
                adjmat <- compute_adjmat(newE, nrow(actors), nrow(prepR), 
                    TRUE, memory, memory_value, start, stop)
            }
        } else {
            adjmat <- matrix()
        }   

        # Compute the rate statistics 
        rateStats <- compute_stats_rate(sender_effectsN, newE, prepR, adjmat, 
            actors[,2], rateScaling, rateCovar, rate_interactions, start, 
            stop)    

        # Reset the adjacency matrix to null 
        if(all(dim(adjmat) == c(1,1))) {
            adjmat <- NULL
        }

        # Dimnames rateStats
        dimnames(rateStats) <- 
            list(NULL, NULL, unlist(c(
                # Main effects
                all_sender_effects[sender_effectsN[which(sender_effectsN!=99)]],
                # Interaction effects
                sapply(sender_effects_int, function(x) {
                    names(x) <- c(
                        strsplit(names(x[1]), "[()]")[[1]][1], 
                        strsplit(names(x[2]), "[()]")[[1]][1])
                    y <- all_sender_effects[match(names(x), all_sender_effects)]
                    paste0(y[1], ".x.", y[2])
                })
            )))

        # Add variable name to exogenous rateStats 
        dimnames(rateStats)[[3]][which(sender_effectsN==2)] <- 
            sapply(sender_effects[which(sender_effectsN==2)], function(x) {
                paste0(x$effect, ".", x$variable)
            })
    }

    # receiver_effects
    if(!is.null(receiver_effects)) {
        # Prepare main receiver_effects
        choiceFormula <- receiver_effects
        receiver_effects <- parse_formula(choiceFormula, "choiceEffects")
        all_receiver_effects <- c(
            "receive", "same", "difference", "average", #1, 2, 3, 4
            "tie", #5
            "inertia", "reciprocity", #6, 7
            "indegreeReceiver", "outdegreeReceiver", #8, 9
            "totaldegreeReceiver", #10
            "otp", "itp", "osp", "isp", #11, 12, 13, 14
            "rrankSend", "rrankReceive", #15, 16
            "recencySendReceiver", "recencyReceiveReceiver", #17 #18
            "recencyContinue", #19
            "interact") #99
        receiver_effectsN  <- match(sapply(receiver_effects, function(x) x$effect), 
            all_receiver_effects)

        # Prepare interaction receiver_effects
        receiver_effects_int <- parse_int(choiceFormula, "choiceEffects", 
            receiver_effects)
        receiver_effectsN <- append(receiver_effectsN, 
            rep(99, length(receiver_effects_int)), length(receiver_effectsN))
        choice_interactions <- list()
        choice_interactions[which(receiver_effectsN==99)] <- receiver_effects_int

        # Prepare receiver_effects covariate information
        choiceCovar <- lapply(receiver_effects, function(x) {
            if(x$effect %in% c("receive", "same", "difference", "average")) {
                if(is.null(x$x)) {
                    dat <- data.frame(
                        id = attributes$id, 
                        time = attributes$time,
                        x = attributes[, x$variable]
                    )
                    dat$id <- attr(prep, "dictionary")$actors[match(dat$id, attr(prep, "dictionary")$actors[,1]),2]
                    colnames(dat)[3] <- x$variable
                    as.matrix(dat)
                } else {
                    dat <- x$x
                    dat$id <- attr(prep, "dictionary")$actors[match(dat$id, attr(prep, "dictionary")$actors[,1]),2]
                    as.matrix(dat)
                }
            } else if(x$effect == "tie") {
                parse_tie(x, prep)
            } else {
                matrix()
            }
        })

        # Prepare receiver_effects scaling 
        choiceScaling <- as.numeric(
            sapply(receiver_effects, function(x) x$scaling))

        # Compute the adjacency matrix 
        if(any(receiver_effectsN %in% 6:14)) {
            if(is.null(adjmat)) {
                adjmat <- compute_adjmat(newE, nrow(actors), nrow(prepR), 
                    TRUE, memory, memory_value, start, stop)
            }
        } else {
            if(is.null(adjmat)) {
                adjmat <- matrix()
            }
        }   

        # Compute the choice statistics 
        choiceStats <- compute_stats_choice(receiver_effectsN, newE, adjmat, 
            actors[,2], prepR, choiceScaling, choiceCovar, choice_interactions, 
            start, stop)   

        # Reset the adjacency matrix to null 
        if(all(dim(adjmat) == c(1,1))) {
            adjmat <- NULL
        }

        # Dimnames choiceStats
        dimnames(choiceStats) <- 
            list(NULL, NULL, unlist(c(
                # Main effects
                all_receiver_effects[receiver_effectsN[which(receiver_effectsN!=99)]],
                # Interaction effects
                sapply(receiver_effects_int, function(x) {
                    names(x) <- c(
                        strsplit(names(x[1]), "[()]")[[1]][1], 
                        strsplit(names(x[2]), "[()]")[[1]][1])
                    y <- all_receiver_effects[match(names(x), all_receiver_effects)]
                    paste0(y[1], ".x.", y[2])
                })
            )))

        # Add variable name to exogenous choiceStats 
        dimnames(choiceStats)[[3]][which(receiver_effectsN %in% c(1:5))] <- 
            sapply(receiver_effects[which(receiver_effectsN %in% c(1:5))], 
            function(x) {
                if(!is.null(x$variable)) {
                    paste0(x$effect, ".", x$variable)
                } else {
                    x$effect
                }          
            }) 
    }

    # Edgelist output
    edgelist <- prep$edgelist
    edgelist$actor1 <- sapply(edgelist$actor1, function(a) {
        remify::actorName(prep, a)
    })
    edgelist$actor2 <- sapply(edgelist$actor2, function(a) {
        remify::actorName(prep, a)
    })
    edgelist$type <- sapply(edgelist$type, function(a) {
        remify::typeName(prep, a)
    })
    edgelist <- as.data.frame(edgelist)

    # Riskset output
    riskset <- prep$risksetMatrix
    riskset <- as.data.frame(riskset)
    colnames(riskset) <- c("actor1", "actor2", "type", "id")
    riskset$actor1 <- sapply(riskset$actor1, function(a) {
        remify::actorName(prep, a)
    })
    riskset$actor2 <- sapply(riskset$actor2, function(a) {
        remify::actorName(prep, a)
    })
    riskset$type <- sapply(riskset$type, function(a) {
        remify::typeName(prep, a)
    })
    riskset$id <- riskset$id + 1
    riskset <- as.data.frame(riskset)

    # Output
    out <- list(statistics = 
        list(sender_stats = rateStats, receiver_stats = choiceStats),
        edgelist = edgelist, riskset = riskset, actors = actors[,1], 
        adjmat = adjmat)
    class(out) <- "aomstats"
    out
}
