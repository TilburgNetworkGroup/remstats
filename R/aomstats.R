#' aomstats
#' 
#' Computes statistics for the rate step and choice step in actor-oriented 
#' relational event models (e.g., see Stadtfeld & Block, 2017). 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be 
#' supplied to the \code{rateEffects} and/or \code{choiceEffects} arguments in 
#' the form \code{~ effects}. The terms are separated by + operators. 
#' Interactions between two effects can be included with * or : operators. 
#' 
#' A list of available effects and their corresponding statistics for the 
#' \emph{rate} step: 
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
#' \emph{choice} step: 
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
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a 
#' dataframe with attributes can be defined in the separate effect functions 
#' supplied to the \code{rateEffects} or \code{choiceEffects} argument.
#' 
#' Optionally, statistics can be computed for a slice of the edgelist - but 
#' based on the entire history. This is achieved by setting the start and 
#' stop values equal to the index of the first and last event for which 
#' statistics are requested. For example, start = 5 and stop = 5 computes the 
#' statistics for only the 5th event in the edgelist, based on the history that 
#' consists of events 1-4. 
#' 
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history 
#' sorted by time with columns `time`, `actor1`, `actor2` and optionally `type` 
#' and `weight`. Alternatively, an object of class \code{"\link[remify]{reh}"}.
#' @param rateEffects an object of class \code{"\link[stats]{formula}"} (or one 
#' that can be coerced to that class): a symbolic description of the effects in 
#' the rate model
#' @param choiceEffects an object of class \code{"\link[stats]{formula}"} (or 
#' one that can be coerced to that class): a symbolic description of the 
#' effects in the choice model
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
#' @return \code{edgelist } An object class \code{"\link[remify]{reh}"}, i.e., 
#' the processed edgelist used for the computation of the statistics
#' @return \code{statistics  } List with in the first element the statistics 
#' for the rate model and in the second element the statistics for the choice 
#' model
#' @return \code{adjmat } Matrix with the adjacency matrix, rows refer to 
#' timepoints and columns to riskset entries
#' 
#' @examples 
#' library(remstats)
#' rateEffects <- ~ send("extraversion")
#' choiceEffects <- ~ receive("agreeableness") + inertia() + otp()
#' aomstats(edgelist = history, rateEffects, choiceEffects, 
#'  attributes = info)
#' 
#' 
#' @export 
aomstats <- function(edgelist, rateEffects = NULL, choiceEffects = NULL,  
    attributes = NULL, actors = NULL, types = NULL, ordinal = FALSE, 
    origin = NULL, omit_dyad = NULL, memory = "full", 
    memory_value = Inf, start = 1, stop = Inf, adjmat = NULL) {

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

    # rateEffects
    if(!is.null(rateEffects)) {
        # Prepare main rateEffects
        rateFormula <- rateEffects
        rateEffects <- parse_formula(rateFormula, "rateEffects")
        all_rateEffects <- c("baseline", "send", # 1,2
            "indegreeSender", "outdegreeSender", "totaldegreeSender", # 3,4,5
            "recencySendSender", "recencyReceiveSender", #6, 7, 
            "interact") #99
        rateEffectsN  <- match(sapply(rateEffects, function(x) x$effect), 
            all_rateEffects)
        
        # Prepare interaction rateEffects
        rateEffects_int <- parse_int(rateFormula, "rateEffects", rateEffects)
        rateEffectsN <- append(rateEffectsN, rep(99, length(rateEffects_int)), 
            length(rateEffectsN))
        rate_interactions <- list()
        rate_interactions[which(rateEffectsN==99)] <- rateEffects_int

        # Prepare rateEffects covariate information
        rateCovar <- lapply(rateEffects, function(x) {
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

        # Prepare rateEffects scaling 
        rateScaling <- as.numeric(
            sapply(rateEffects, function(x) x$scaling))

        # Compute the adjacency matrix 
        if(any(rateEffectsN %in% c(3,4,5))) {
            if(is.null(adjmat)) {
                adjmat <- compute_adjmat(newE, nrow(actors), nrow(prepR), 
                    TRUE, memory, memory_value, start, stop)
            }
        } else {
            adjmat <- matrix()
        }   

        # Compute the rate statistics 
        rateStats <- compute_stats_rate(rateEffectsN, newE, prepR, adjmat, 
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
                all_rateEffects[rateEffectsN[which(rateEffectsN!=99)]],
                # Interaction effects
                sapply(rateEffects_int, function(x) {
                    names(x) <- c(
                        strsplit(names(x[1]), "[()]")[[1]][1], 
                        strsplit(names(x[2]), "[()]")[[1]][1])
                    y <- all_rateEffects[match(names(x), all_rateEffects)]
                    paste0(y[1], ".x.", y[2])
                })
            )))

        # Add variable name to exogenous rateStats 
        dimnames(rateStats)[[3]][which(rateEffectsN==2)] <- 
            sapply(rateEffects[which(rateEffectsN==2)], function(x) {
                paste0(x$effect, ".", x$variable)
            })
    }

    # choiceEffects
    if(!is.null(choiceEffects)) {
        # Prepare main choiceEffects
        choiceFormula <- choiceEffects
        choiceEffects <- parse_formula(choiceFormula, "choiceEffects")
        all_choiceEffects <- c(
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
        choiceEffectsN  <- match(sapply(choiceEffects, function(x) x$effect), 
            all_choiceEffects)

        # Prepare interaction choiceEffects
        choiceEffects_int <- parse_int(choiceFormula, "choiceEffects", 
            choiceEffects)
        choiceEffectsN <- append(choiceEffectsN, 
            rep(99, length(choiceEffects_int)), length(choiceEffectsN))
        choice_interactions <- list()
        choice_interactions[which(choiceEffectsN==99)] <- choiceEffects_int

        # Prepare choiceEffects covariate information
        choiceCovar <- lapply(choiceEffects, function(x) {
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

        # Prepare choiceEffects scaling 
        choiceScaling <- as.numeric(
            sapply(choiceEffects, function(x) x$scaling))

        # Compute the adjacency matrix 
        if(any(choiceEffectsN %in% 6:14)) {
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
        choiceStats <- compute_stats_choice(choiceEffectsN, newE, adjmat, 
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
                all_choiceEffects[choiceEffectsN[which(choiceEffectsN!=99)]],
                # Interaction effects
                sapply(choiceEffects_int, function(x) {
                    names(x) <- c(
                        strsplit(names(x[1]), "[()]")[[1]][1], 
                        strsplit(names(x[2]), "[()]")[[1]][1])
                    y <- all_choiceEffects[match(names(x), all_choiceEffects)]
                    paste0(y[1], ".x.", y[2])
                })
            )))

        # Add variable name to exogenous choiceStats 
        dimnames(choiceStats)[[3]][which(choiceEffectsN %in% c(1:5))] <- 
            sapply(choiceEffects[which(choiceEffectsN %in% c(1:5))], 
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
    out <- list(statistics = list(rate = rateStats, choice = choiceStats),
        edgelist = edgelist, riskset = riskset, adjmat = adjmat)
    class(out) <- "aomstats"
    out
}
