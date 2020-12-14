#' remstats
#' 
#' Computes statistics for relational event history data. 
#' 
#' The statistics to be computed are defined symbolically and should be in the 
#' form \code{~ statistics}. The statistics are separated by + operators. 
#' Interactions between two statistics can be included with * or : operators. 
#' 
#' A list of available statistics follows: 
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{send}()}
#'  \item \code{\link{receive}()}
#'  \item \code{\link{same}()}
#'  \item \code{\link{difference}()}
#'  \item \code{\link{average}()}
#'  \item \code{\link{minimum}()}
#'  \item \code{\link{maximum}()}
#'  \item \code{\link{equate}()}
#'  \item \code{\link{event}()}
#'  \item \code{\link{inertia}()}
#'  \item \code{\link{reciprocity}()}
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeSender}()}
#'  \item \code{\link{totaldegreeReceiver}()}
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
#'  \item \code{\link{recencySender}()}
#'  \item \code{\link{recencyReceiver}()}
#'  \item \code{\link{recencyContinue}()}
#' }
#' 
#' @param formula an object of class \code{"\link[stats]{formula}"} (or one 
#' that can be coerced to that class): a symbolic description of statistics to 
#' be computed. The details of the specification of the statistics and an 
#' overview of the available statistics are given under 'Details'. 
#' @param edgelist an object of class \code{"\link[base]{matrix}"} or 
#' \code{"\link[base]{data.frame}"} that contains the relational event history. 
#' Each row in the edgelist should refer to one event. The first column should 
#' refer to the timepoint or order of the event, the second column to the 
#' sender (or first actor) and the third column to the receiver (or second 
#' actor). The fourth column may refer to the type of the event, this column is 
#' only used when \code{with_type = TRUE}.
#' @param directed Logical value. Indicates whether events in the edgelist are 
#' directed (\code{directed = TRUE}, default) or undirected 
#' (\code{directed = FALSE}).
#' @param with_type Logical value. Indicates whether event types are considered 
#' in the dependent variable (\code{with_type = TRUE}) or not 
#' (\code{with_type = FALSE}, default).
#' @param riskset an object of class \code{"\link[base]{matrix}"} or 
#' \code{"\link[base]{data.frame}"} that contains the riskset. The first column 
#' should refer to the sender/actor1, the second column to the receiver/actor2 
#' and a third column may refer to the event type. Can be supplied to indicate 
#' a non-standard riskset. 
#' @param actors Vector with actor id's. Should be supplied when not all actors 
#' that can interact are observed in the edgelist.
#' @param types Vector with event type id's. Should be supplied when not all 
#' types that can occur are observed in the edgelist.
#' @param start Integer value. Indicates the first row in the edgelist for 
#' which statistics need to be computed. Can be used to compute statistics for 
#' a subpart of the relational event history but based on the whole relational 
#' event history. If nothing is indicated, statistics will be computed starting 
#' from the first row of the edgelist. 
#' @param stop Integer value. Indicates the last row in the edgelist for 
#' which statistics need to be computed. If nothing is indicated, statistics 
#' will be computed stopping at the last row of the edgelist. 
#' 
#' @examples 
#' data(history)
#' data(info)
#' form <- ~ baseline() + inertia():send("extraversion", info)
#' remstats(form, edgelist = history)
#' 
#' @export 
remstats <- function(formula, edgelist, directed = TRUE, with_type = FALSE, 
    riskset = NULL, actors = NULL, types = NULL, start = NULL, stop = NULL) {

    # Get effects information
    ft <- stats::terms(formula)
    
    var <- attr(ft, "variables")
    var <- as.list(var)[-1]

    effects <- lapply(var, eval)
    effects <- unlist(effects, recursive = FALSE)

    # Set start and stop
    if(is.null(start)) {
        start <- 1
    } 
    if(start < 1) {
        stop("start should be set to 1 or larger.")
    }
    if(is.null(stop)) {
        stop <- nrow(edgelist)
    } 
    if(stop < start) {
        stop("stop cannot be smaller than start.")
    }

    # Prepare REH input data
    dat <- prepEdgelist(edgelist, directed, with_type, riskset, 
        actors, types)

    actors <- dat$actors
    actorsUser <- dat$actorsUser
    edgelist <- dat$edgelist
    edgelistUser <- dat$edgelistUser
    riskset <- dat$riskset
    risksetUser <- dat$risksetUser
    evls <- dat$evls

    # Prepare the effects
    all_effects <- c(
        "baseline", #1
        "send", "receive", #2 #3
        "same", "difference", "average", #4 #5 #6
        "minimum", "maximum", "equate", #7 #8 #9  
        "inertia", "reciprocity", #10 #11
        "indegreeSender", "indegreeReceiver", #12 #13
        "outdegreeSender", "outdegreeReceiver", #14 #15
        "totaldegreeSender", "totaldegreeReceiver", #16, #17
        "otp", "itp", "osp", "isp", #18 #19 #20 #21
        "sp", "spUnique", #22, #23
        "psABBA", "psABBY", "psABXA",  #24 #25 #26 
        "psABXB", "psABXY", "psABAY",  #27 #28 #29
        "rrankSend", "rrankReceive",  #30 #31
        "baselineType", "interact", "event", #32 #33 #34
        "recencySender","recencyReceiver","recencyContinue", #35 #36 #37
        "tie" #38
        ) 
    eff <- match(names(effects), all_effects)

    # Check correct specification effects
    if(!directed) {
        if(any(names(effects) %in% c("send", "receive", "reciprocity", 
            "indegreeSender", "indegreeReceiver", "outdegreeSender", 
            "outdegreeReceiver", "totaldegreeSender", "totaldegreeReceiver", 
            "otp", "itp", "osp", "isp", "psABBA", "psABBY", "psABXA", "psABXB", 
            "psABXY", "psABAY", "rrankSend", "rrankReceive"))) {
            stop(paste("Attempting to request effects that are not defined when `directed = FALSE`."))
        }
    }

    if(directed) {
        if(any(names(effects) %in% c("sp", "spUnique"))) {
            stop(paste("Attemping to request effects that are not defined when `directed = TRUE`"))
        }
    }

    # Prepare scaling info (vector length p)
    scaling <- sapply(effects, function(x) {
        sc <- x$scaling
        if(is.null(sc)) {sc <- 0}
        sc
    })

    # Prepare memory_value info (vector length p)
    memory_value <- sapply(effects, function(x) {
        mv <- x$memory_value
        if(is.null(mv)) {mv <- Inf}
        mv
    })

    # Prepare with_type Info (vector length p)
    with_typeVar <- sapply(effects, function(x) {
        wt <- x$with_type
        if(is.null(wt)) {wt <- FALSE}
        wt
    })
    if(any(with_typeVar) && !with_type) {
        stop(paste(names(effects)[which(with_typeVar)], "with type requested but no types in riskset: set with_type = TRUE in remstats() or request", names(effects)[which(with_typeVar)], "with_type = FALSE. "))
    }

    # Prepare event weights (m x p matrix)
    event_weights <- lapply(effects, function(x) {
        ew <- x$event_weights
        if(is.null(ew)) {ew <- rep(1, length(edgelist))}
        ew
    })
    event_weights <- do.call(cbind, event_weights)

    # Prepare baselineType effect
    if(any(with_typeVar[which(names(effects) == "baseline")])) {
        typesUser <- unique(sort(risksetUser[,3]))
        typesUser <- typesUser[-1]
        types <- unique(sort(riskset[,3]))
        types <- types[-1]
        
        ind <- which(with_typeVar == TRUE & names(effects) == "baseline")
        
        for(p in seq_along(types)) {
            x <- data.frame(type = types[p])
            attributes(x)$effect <- "baselineType"
            colnames(x) <- typesUser[p]
            
            if(p == 1) {
                effects <- append(effects[-ind], 
                    list(baselineType = list(x = x)),
                    ind - 1)
            } else {
                effects <- append(effects, 
                    list(baselineType = list(x = x)), 
                    ind - 2 + p)
                attr(ft, "factor") <- rbind(
                    attr(ft, "factor")[1:ind,],
                    attr(ft, "factor")[ind:nrow(attr(ft, "factor")),])
            }
        }
        rownames(attr(ft, "factor")) <- names(effects)
        eff <- match(names(effects), all_effects)
    }

    # Prepare interaction effects 
    # Note: interaction effects should always be the last ones in the effects 
    # and eff objects: this makes sure the main effects are previously computed
    for(p in seq_along(attr(ft, "order"))) {
        if(attr(ft, "order")[p] == 2) {
            out <- list(
                interact = list(
                    x = as.matrix(which(attr(ft, "factor")[,p]>0))
                )
            )
            attributes(out$interact$x)$effect <- "interact"
            effects <- append(effects, out)
        }
    }
    eff <- match(names(effects), all_effects)

    # Prepare exogenous information (list with p elements)
    values <- lapply(effects, function(y) {
        val <- y$x
        effect <- attributes(val)$effect
        if(!is.null(val)) {
            if(!(effect %in% c("baselineType", "interact", "event", "tie"))) {
                val$id <- match(val$id, actorsUser)
                if(!all(actors %in% val$id)) {
                    stop(paste0("Make sure that for every actor a ", effect, "Effect value is defined."))
                }
                if(!all(actors %in% val[val$time <= edgelist[start,1],"id"])) {
                    stop(paste0("Make sure that for every actor a ", effect, "Effect starting value is defined."))
                }
            }
            if(effect == "tie") {
                if(is.null(rownames(val)) | is.null(colnames(val))) {
                    if(nrow(val) != ncol(val)) {stop("Expect equal number of rows and columns for matrix `X` in tie.")}
                    if(nrow(val) != length(actors)) {stop("Make sure that the dimensions of the matrix 'X' in tie correspond to the number of unique actors in the riskset.")}
                    rownames(val) <- colnames(val) <- actors
                } else {
                    if(any(rownames(val) %in% actorsUser)) {
                        rownames(val) <- match(rownames(val), actorsUser)
                    }
                    if(any(colnames(val) %in% actorsUser)) {
                        colnames(val) <- match(colnames(val), actorsUser)
                    }
                }
                longVal <- expand.grid(actors, actors)
                longVal[,3] <- apply(longVal, 1, function(y) {
                    val[which(rownames(val) == y[1]), which(rownames(val) == y[2])]
                })
                val <- longVal
                colnames(val) <- c("id1", "id2", "value")
            }
            
            val <- as.matrix(val)
            val <- apply(val, 2, as.numeric)
            val <- as.matrix(val)
        }
        val
    })

    # Prepare equal_val
    equal_val <- sapply(effects, function(y) {
        val <- y$equal_val
        if(is.null(val)) {val <- NA}
        val
    })

    # Compute statistics
    statistics <- compute_stats(eff, edgelist, riskset, start, stop, 
        values, scaling, memory_value, with_typeVar, event_weights, equal_val)

    # Prepare output
    effectnames <- sapply(effects, function(y) {
        if(!is.null(attr(y$x, "effect"))) {
            if(attr(y$x, "effect") == "baselineType") {
                paste0("baselineType_", colnames(y$x)[1])
            } else if(attr(y$x, "effect") == "interact") {
                paste0(all_effects[eff[y$x[1]]], "*", all_effects[eff[y$x[2]]])
            } else if(attr(y$x, "effect") == "event") {
                paste0("event_", colnames(y$x)[1]) 
            } else if(attr(y$x, "effect") == "tie") { 
                "tie"
            } else {
                paste0(attributes(y$x)$effect, "_", colnames(y$x)[3])
            }
        } else {
            NA
        }
    })
    
    effectnames <- unlist(effectnames)
    effectnames <- ifelse(is.na(effectnames), names(effectnames), effectnames)
    names(effectnames) <- NULL
    temp <- attr(ft, "term.labels")[grepl("tie", attr(ft, "term.labels"))]
    temp <- strsplit(temp, "tie(", fixed = TRUE)
    effectnames[which(effectnames == "tie")] <- paste0("tie_", sapply(temp, function(x) {
        unlist(strsplit(x[2], ")", fixed = TRUE))
    }))
    dimnames(statistics) <- list(NULL, NULL, effectnames)
    class(statistics) <- "remstats"

    edgelistUser <- edgelistUser[start:stop,]
    evls <- evls[start:stop,]

    # Output
    list(statistics = statistics, edgelist = edgelistUser, 
        riskset = risksetUser, evls = evls)
}
