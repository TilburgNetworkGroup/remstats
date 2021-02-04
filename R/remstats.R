#' remstats
#' 
#' Computes statistics for relational event history data. 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be in the 
#' form \code{~ statistics}. The statistics are separated by + operators. 
#' Interactions between two statistics can be included with * or : operators. 
#' 
#' A list of available statistics follows: 
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{send}()}
#'  \item \code{\link{receive}()}
#'  \item \code{\link{tie}()}
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
#' that can be coerced to that class): a symbolic description of the requested 
#' statistics, see 'Details'. 
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history 
#' sorted by time with columns `time`, `actor1`, `actor2` and optionally `type` 
#' and `weight`. Alternatively, an object of class \code{"\link[remify]{reh}"}. 
#' @inheritParams remify::reh
#' @param start integer value that indicates the first row in the edgelist for 
#' which statistics need to be computed. Can be used to compute statistics for 
#' a subpart of the relational event history based on the whole past relational 
#' event history. If nothing is indicated, statistics will be computed starting 
#' from the first row of the edgelist. 
#' @param stop integer value that indicates the last row in the edgelist for 
#' which statistics need to be computed. If nothing is indicated, statistics 
#' will be computed stopping at the last row of the edgelist. 
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ baseline() + inertia():send("extraversion", info), 
#'  edgelist = history)
#' 
#' @export 
remstats <- function(formula, edgelist, actors = NULL, types = NULL, 
    directed = TRUE, ordinal = FALSE, origin = NULL, omit_dyad = NULL, start = NULL, stop = NULL) {

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

    # Prepare edgelist, riskset, actors & type
    if(class(edgelist) == "reh") {
        dat <- edgelist 

    } else {
        # Process edgelist
        dat <- remify::reh(edgelist = edgelist, actors = actors, 
            types = types, directed = directed, ordinal = ordinal, 
            origin = origin, omit_dyad = omit_dyad)
    }

    edgelist <- as.matrix(dat$edgelist)
    riskset <- dat$risksetMatrix
    risksetCube <- dat$risksetCube
    actors <- attr(dat, "dictionary")$actors
    types <- attr(dat, "dictionary")$types

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
            stop(paste("Attempting to request effects that are not defined for undirected events"))
        }
    }

    if(directed) {
        if(any(names(effects) %in% c("sp", "spUnique"))) {
            stop(paste("Attemping to request effects that are not defined for directed events"))
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
    
    if(any(with_typeVar) && !attr(dat, "with_type")) {
        stop(paste(names(effects)[which(with_typeVar)], "with type requested but no types in edgelist, make sure edgelist has a 'type' column or request", names(effects)[which(with_typeVar)], "with_type = FALSE. "))
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
        typesUser <- types[,1]
        typesUser <- typesUser[-1]
        types <- types[,2]
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
                val$id <- actors[match(val$id, actors[,1]),2]
                if(!all(actors[,2] %in% val$id)) {
                    stop(paste0("Make sure that for every actor a ", effect, "Effect value is defined."))
                }
                if(!all(actors[,2] %in% val[val$time <= edgelist[start,1],"id"])) {
                    stop(paste0("Make sure that for every actor a ", effect, "Effect starting value is defined."))
                }
            }
            if(effect == "tie") {
                if(is.null(rownames(val)) | is.null(colnames(val))) {
                    if(nrow(val) != ncol(val)) {stop("Expect equal number of rows and columns for matrix `X` in tie.")}
                    if(nrow(val) != nrow(actors)) {stop("Make sure that the dimensions of the matrix 'X' in tie correspond to the number of unique actors in the riskset.")}
                    rownames(val) <- colnames(val) <- actors[,2]
                } else {
                    if(any(rownames(val) %in% actors[,1])) {
                        rownames(val) <- actors[match(rownames(val), actors[,1]),2]
                    }
                    if(any(colnames(val) %in% actors[,1])) {
                        colnames(val) <- actors[match(colnames(val), actors[,1]),2]
                    }
                }
                longVal <- expand.grid(actors[,2], actors[,2])
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
    
    # Transform edgelist to evls
    # Get riskset position
    rp <- apply(edgelist, 1, function(x) {
        risksetCube[x[2]+1, x[3]+1, x[4]+1] + 1
    })

    evls <- cbind(rp, edgelist[,1])
    colnames(evls) <- c("event", "time")

    # Output
    out <- list(
        statistics = statistics, 
        edgelist = edgelist[start:stop,], 
        riskset = riskset, 
        evls = evls[start:stop,]
    )
    class(out) <- "remstats"
    out
}
