#' tomstats
#' 
#' Computes statistics for modeling relational event history data with Butts' 
#' (2008) relational event model. 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be 
#' supplied to the \code{effects} argument in the form \code{~ effects}. The 
#' terms are separated by + operators. For example: 
#' \code{effects = ~ inertia() + otp()}. Interactions between two effects 
#' can be included with * or : operators. For example: 
#' \code{effects = ~ inertia():otp()}. A list of available effects and their 
#' corresponding statistics follows at the bottom. 
#' 
#' For the computation of the \emph{exogenous} statistics an attributes object 
#' with the exogenous covariate information has to be supplied to the 
#' \code{attributes} argument in either \code{tomstats()} or in the separate 
#' effect functions supplied to the \code{effects} argument (e.g., see 
#' \code{\link{send}}). This \code{attributes} object should be constructed as 
#' follows: A dataframe with rows refering to the attribute value of actor 
#' \emph{i} at timepoint \emph{t}. An `id` column is required that contains the 
#' actor id (corresponding to the actor id's in the edgelist). A `time` column 
#' is required that contains the time when attributes change (set to zero if 
#' none of the attributes vary over time). Subsequent columns contain the 
#' attributes that are called in the specifications of exogenous statistics 
#' (column name corresponding to the string supplied to the \code{variable} 
#' argument in the effect function). Note that the procedure for the exogenous 
#' effects `tie' and `event' deviates from this, here the exogenous covariate 
#' information has to be specified in a different way, see \code{\link{tie}} 
#' and \code{\link{event}}. 
#' 
#' The majority of the statistics can be scaled in some way, see 
#' the documentation of the \code{scaling} argument in the separate effect 
#' functions for more information on this. 
#' 
#' The majority of the statistics can account for the event type 
#' included as a dependent variable, see the documentation of the 
#' \code{consider_type} argument in the separate effect functions for more 
#' information on this. 
#' 
#' Note that events in the edgelist can be directed or undirected. Some 
#' statistics are only defined for either directed or undirected events (see 
#' the documentation of the statistics). 
#' 
#' Two more elements can affect the computation of the 
#' \emph{endogenous} statistics: the settings of the \code{memory} and 
#' \code{memoryValue} arguments in \code{tomstats} and the events weights in 
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
#' in the \code{tomstats} function and affect the computation of all endogenous 
#' statistics with the following exceptions (that follow logically from their 
#' definition). Since spUnique is a count of the number of unique interaction 
#' partners, and the recency statistics (recencyContinue, 
#' recencySendSender, recencySendReceiver, recencyReceiveSender, 
#' recencyReceiveReceiver) depend on the time past, the computation of these 
#' statistics do not depend on event weights and are therefore affected by 
#' "window" memory but not by "Brandes" memory or supplied event weights. Since 
#' the baseline statistic is always one, the FEtype statistic is binary and 
#' does not depend on past events, and the p-shifts (PSAB-BA, PSAB-BY, PSAB-XA, 
#' PSAB-XB, PSAB-XY and PSAB-AY) are binary and only dependent on the previous 
#' event, these statistics are not affected by the memory settings or the 
#' supplied event weights. The recency-rank statistics (rrankSend, 
#' rrankReceive) are (for now) only available with the "full" memory, and are, 
#' per definition, not affected by supplied event weights.  
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
#' Exogenous statistics:
#' \itemize{
#'  \item \code{\link{send}()}
#'  \item \code{\link{receive}()}
#'  \item \code{\link{tie}()}
#'  \item \code{\link{same}()}
#'  \item \code{\link{difference}()}
#'  \item \code{\link{average}()}
#'  \item \code{\link{minimum}()}
#'  \item \code{\link{maximum}()}
#'  \item \code{\link{event}()}
#' }
#' 
#' Endogenous statistics:
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{FEtype}()}
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeSender}()}
#'  \item \code{\link{totaldegreeReceiver}()}
#'  \item \code{\link{inertia}()}
#'  \item \code{\link{reciprocity}()}
#'  \item \code{\link{otp}()}
#'  \item \code{\link{itp}()}
#'  \item \code{\link{osp}()}
#'  \item \code{\link{isp}()}
#'  \item \code{\link{sp}()}
#'  \item \code{\link{spUnique}()}
#'  \item \code{\link{psABBA}()}
#'  \item \code{\link{psABBY}()}
#'  \item \code{\link{psABXA}()}
#'  \item \code{\link{psABXB}()}
#'  \item \code{\link{psABXY}()}
#'  \item \code{\link{psABAY}()}
#'  \item \code{\link{rrankSend}()}
#'  \item \code{\link{rrankReceive}()}
#'  \item \code{\link{recencySendSender}()}
#'  \item \code{\link{recencySendReceiver}()}
#'  \item \code{\link{recencyReceiveSender}()}
#'  \item \code{\link{recencyReceiveReceiver}()}
#'  \item \code{\link{recencyContinue}()}
#' }
#' 
#' @param effects an object of class \code{"\link[stats]{formula}"} (or one 
#' that can be coerced to that class): a symbolic description of the effects in 
#' the model for which statistics are computed, see 'Details' for the available 
#' effects and their corresponding statistics
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history 
#' sorted by time with columns `time`, `actor1`, `actor2` and optionally `type` 
#' and `weight`. Alternatively, an object of class \code{"\link[remify]{reh}"} 
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
#' event for which statistics are requested (see 'Details')
#' @param stop integer value, refers to the index in the edgelist of the last 
#' event for which statistics are requested (see 'Details')
#' @param adjmat optionally, a previously computed adjacency matrix with on the 
#' rows the timepoints and on the columns the riskset entries 
#' 
#' @return \code{statistics } Array with the computed statistics, where rows 
#' refer to time points, columns refer to potential relational event (i.e., 
#' potential edges) in the risk set and slices refer to statistics 
#' @return \code{evls } Matrix with the edgelist, processed such that it can be 
#' used to estimate a relational event model with \code{"\link[relevent]{rem}"} 
#' @return \code{edgelist } Dataframe with the edgelist
#' @return \code{adjmat } Matrix with the adjacency matrix, rows refer to 
#' timepoints and columns to riskset entries
#' 
#' @examples 
#' library(remstats)
#' effects <- ~ inertia():send("extraversion") + otp()
#' tomstats(effects, edgelist = history, attributes = info)
#' 
#' @references Butts, C. T. (2008). A relational event framework for social 
#' action. Sociological Methodology, 38(1), 155–200. 
#' \url{https://doi.org/10.1111/j.1467-9531.2008.00203.x}
#' 
#' @export 
tomstats <- function(effects, edgelist, attributes = NULL, actors = NULL, 
    types = NULL, directed = TRUE, ordinal = FALSE, origin = NULL, 
    omit_dyad = NULL, memory = "full", memory_value = Inf, start = 1, 
    stop = Inf, adjmat = NULL, verbose = FALSE) {

    # Prepare the edgelist 
    if(!("reh" %in% class(edgelist))) {
        prep <- remify::reh(edgelist = edgelist, actors = actors, 
            types = types, directed = directed, ordinal = ordinal, 
            origin = origin, omit_dyad = omit_dyad)
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
    if(start < 1) {stop("start should be set to 1 or larger.")}
    if(stop < start) {stop("stop cannot be smaller than start.")}
    start <- start - 1
    if(stop == Inf) {stop <- nrow(prepE)}
    stop <- stop - 1  
    
    # Prepare main effects
    form <- effects
    effects <- parse_formula(form, "rem", ordinal)
    all_effects <- c(
        "baseline", #1
        "send", "receive", #2 #3
        "same", "difference", "average", #4 #5 #6
        "minimum", "maximum", "removed", #7 #8 #9 
        "inertia", "reciprocity", #10 #11
        "indegreeSender", "indegreeReceiver", #12 #13
        "outdegreeSender", "outdegreeReceiver", #14 #15
        "totaldegreeSender", "totaldegreeReceiver", #16, #17
        "otp", "itp", "osp", "isp", #18 #19 #20 #21
        "sp", "spUnique", #22, #23
        "psABBA", "psABBY", "psABXA",  #24 #25 #26 
        "psABXB", "psABXY", "psABAY",  #27 #28 #29
        "rrankSend", "rrankReceive",  #30 #31
        "FEtype", "event", #32 #33 
        "recencyContinue", #34
        "recencySendSender","recencySendReceiver", #35 #36
        "recencyReceiveSender","recencyReceiveReceiver", #37 #38
        "tie",  #39
        
        "indegreeSender.type", "indegreeReceiver.type", #40 #41
        "outdegreeSender.type", "outdegreeReceiver.type", #42 #43
        "totaldegreeSender.type", "totaldegreeReceiver.type", #44 #45
        "psABBA.type", "psABBY.type", "psABXA.type",  #46 #47 #48 
        "psABXB.type", "psABXY.type", "psABAY.type",  #49 #50 #51
        "inertia.type", "reciprocity.type", #52 #53
        "otp.type", "itp.type", "osp.type", "isp.type", #54 #55 #56 #57
        "sp.type", "spUnique.type", #58, #59
        "rrankSend.type", "rrankReceive.type",  #60 #61
        "recencyContinue.type", #62
        "recencySendSender.type","recencySendReceiver.type", #63 #64
        "recencyReceiveSender.type","recencyReceiveReceiver.type", #65 #66

        "interact") #99
    effectsN <- match(sapply(effects, function(x) x$effect), all_effects)
   
    # Check correct specification effects
    if(!directed) {
        if(any(effectsN %in% 
            c(2, 3, 11:21, 24:31, 35:38, 40:51, 53:57, 60:61, 63:66))) {
            
            stop(paste("Attempting to request effects that are not defined for undirected events"))
        }
    }

    if(directed) {
        if(any(effectsN %in% c(22:23, 58:59))) {
            stop(paste("Attemping to request effects that are not defined for directed events"))
        }
    }

    # Prepare fixed effects 
    if(any(sapply(effects, function(x) x$effect == "FEtype"))) {
        C <- nrow(types)
        FEeffects <- lapply(2:C, function(c) {
            x <- list()
            x$effect <- "FEtype"
            x$scaling <- 1
            x$typeName <- types$typeName[c]
            x$typeID <- types$typeID[c]
            x
        })
        
        pos <- which(sapply(effects, function(x) x$effect == "FEtype"))
        effects <- append(effects[-pos], FEeffects, pos-1)
        effectsN <- match(sapply(effects, function(x) x$effect), all_effects)
    }

    # Prepare interaction effects
    effects_int <- parse_int(form, "rem", effects, ordinal)
    effectsN <- append(effectsN, rep(99, length(effects_int)), length(effectsN))
    interactions <- list()
    interactions[which(effectsN==99)] <- effects_int

    # Prepare covariate information
    covar <- lapply(effects, function(x) {
        if(x$effect %in% c("send", "receive", "same", "difference", "average", 
            "minimum", "maximum")) {
            if(is.null(x$x)) {
                dat <- data.frame(
                    id = attributes$id, 
                    time = attributes$time,
                    x = attributes[, x$variable]
                )
                dat$id <- actors[match(dat$id, actors[,1]),2]
                colnames(dat)[3] <- x$variable
                as.matrix(dat)
            } else {
                dat <- x$x
                dat$id <- actors[match(dat$id, actors[,1]),2]
                as.matrix(dat)
            }
        } else if(x$effect == "tie") {
            parse_tie(x, prep)
        } else if(x$effect == "event") {
            dat <- edgelist[,x$variable]
            as.matrix(dat)
        } else if(x$effect == "FEtype") {
            dat <- x$typeID
            as.matrix(dat)
        } else {
            matrix()
        }
    })

    # Prepare scaling info (vector length p)
    scaling <- as.numeric(sapply(effects, function(x) x$scaling))

    # Compute the adjacency matrix 
    if(any(effectsN %in% c(10:23, 40:45, 52:59))) {
        if(is.null(adjmat)) {
            adjmat <- compute_adjmat(newE, nrow(actors), nrow(prepR), 
                directed, memory, memory_value, start, stop)
        }
    } else {
        if(is.null(adjmat)) {
            adjmat <- matrix()
        }
    }   

    # Compute statistics
    statistics <- compute_stats_tie(effectsN, newE, adjmat, actors[,2], 
        types[,2], prepR, scaling, covar, interactions, start, stop, directed, 
        verbose)

    # Dimnames statistics
    dimnames(statistics) <- 
        list(NULL, NULL, unlist(c(
            # Main effects
            all_effects[effectsN[which(effectsN!=99)]],
            # Interaction effects
            sapply(effects_int, function(x) {
                names(x) <- c(
                    strsplit(names(x[1]), "[()]")[[1]][1], 
                    strsplit(names(x[2]), "[()]")[[1]][1])
                y <- all_effects[match(names(x), all_effects)]
                paste0(y[1], ".x.", y[2])
            })
        )))

    # Add variable name to exogenous statistics 
    dimnames(statistics)[[3]][which(effectsN %in% c(2:8, 33, 39))] <- 
        sapply(effects[which(effectsN %in% c(2:8, 33, 39))], 
        function(x) {
           if(!is.null(x$variable)) {
                paste0(x$effect, ".", x$variable)
            } else {
                x$effect
            }          
        }) 
    
    # Transform edgelist to evls
    # Get riskset position
    rp <- apply(prepE, 1, function(x) {
        prepRC[as.numeric(x[2])+1, as.numeric(x[3])+1, as.numeric(x[4])+1] + 1
    })

    evls <- cbind(rp, cumsum(prep$intereventTime))
    colnames(evls) <- c("event", "time")

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
    out <- list(
        statistics = statistics, 
        edgelist = edgelist,
        riskset = riskset, 
        actors = actors[,1],
        evls = evls[(start+1):(stop+1),],
        adjmat = adjmat
    )
    class(out) <- "tomstats"
    out
}
