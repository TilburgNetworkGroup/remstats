#' tomstats
#'
#' Computes statistics for modeling relational event history data 
#' with Butts' (2008) relational event model.
#' 
#' @param effects an object of class \code{"\link[stats]{formula}"} (or one
#' that can be coerced to that class): a symbolic description of the effects in
#' the model for which statistics are computed, see 'Details' for the available
#' effects and their corresponding statistics
#' @inheritParams remstats
#'
#' @section Effects:
#' The statistics to be computed are defined symbolically and should be
#' supplied to the \code{effects} argument in the form \code{~ effects}. The
#' terms are separated by + operators. For example:
#' \code{effects = ~ inertia() + otp()}. Interactions between two effects
#' can be included with * operators. For example: 
#' \code{effects = ~ inertia()*otp()}. A list of available effects can be 
#' obtained with \code{\link{tie_effects}()}.
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
#' @section Attributes:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attributes} argument in either \code{tomstats()} or in the separate
#' effect functions supplied to the \code{effects} argument (e.g., see
#' \code{\link{send}}). This \code{attributes} object should be constructed as
#' follows: A data.frame with rows referring to the attribute value of actor
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
#' @section Memory:
#' The default `memory` setting is `"full"`, which implies that at each time
#' point $t$ the entire event history before $t$ is included in the computation
#' of the statistics. Alternatively, when `memory` is set to `"window"`, only
#' the past event history within a given time window is considered (see
#' Mulders & Leenders, 2019). This length of this time window is set by the
#' `memory_value` parameter. For example, when `memory_value = 100` and `memory
#' = "window"`, at time point $t$ only the past events that happened at most
#' 100 time units ago are included in the computation of the statistics.
#' A third option is to set `memory` to `"interval"`. In this case, the past 
#' event history within a given time interval is considered. For example, when 
#' `"memory_value" = c(50, 100)` and `memory = "window"`, at time point $t$ 
#' only the past events tha happened between 50 and 100 time units ago are 
#' included in the computation of the statistics. Finally, the fourth option is 
#' to set `memory` to `"decay"`. In this case, the weight of the past event in 
#' the computation of the statistics depend on the elapsed time between $t$ and 
#' the past event. This weight is determined based on an exponential decay 
#' function with half-life parameter `memory_value` (see Brandes et al., 2009).
#'
#' @section Event weights:
#' Note that if the  edgelist contains a column that is named ``weight'', it is
#' assumed that these affect the endogenous statistics. These settings are
#' defined globally in the \code{tomstats} function and affect the computation
#' of all endogenous statistics with the following exceptions (that follow
#' logically from their definition). Since spUnique is a count of the number of
#' unique interaction partners, and the recency statistics (recencyContinue,
#' recencySendSender, recencySendReceiver, recencyReceiveSender,
#' recencyReceiveReceiver) depend on the time past, the computation of these
#' statistics do not depend on event weights. Since the baseline statistic is
#' always one, the FEtype statistic is binary and does not depend on past
#' events, and the p-shifts (PSAB-BA, PSAB-BY, PSAB-XA, PSAB-XB, PSAB-XY and
#' PSAB-AY) are binary and only dependent on the previous event, these
#' statistics are not affected by the memory settings or the supplied event
#' weights. The recency-rank statistics (rrankSend, rrankReceive) are (for now)
#' only available with the "full" memory, and are, per definition, not affected
#' by supplied event weights.
#'
#' @section Subset of the edgelist:
#' Optionally, statistics can be computed for a slice of the edgelist - but
#' based on the entire history. This is achieved by setting the start and
#' stop values equal to the index of the first and last event for which
#' statistics are requested. For example, start = 5 and stop = 5 computes the
#' statistics for only the 5th event in the edgelist, based on the history that
#' consists of events 1-4.
#'
#' @section Adjacency matrix:
#' Optionally, a previously computed adjacency matrix can be supplied. Note
#' that the endogenous statistics will be computed based on this adjacency
#' matrix. Hence, supplying a previously computed adjacency matrix can reduce
#' computation time but the user should be absolutely sure the adjacency matrix
#' is accurate.
#' 
#' @return \code{statistics } array with the computed statistics, where rows
#' refer to time points, columns refer to potential relational event (i.e.,
#' potential edges) in the risk set and slices refer to statistics
#' @return \code{evls } matrix with the edgelist, processed such that it can be
#' used to estimate a relational event model with \code{"\link[relevent]{rem}"}
#' @return \code{edgelist } data.frame with the edgelist
#' @return \code{adjmat } matrix with the adjacency matrix, rows refer to
#' time points and columns to risk set entries. At timepoint t, it gives the
#' cumulative weight until t-1 (i.e., the events that occurred before time
#' point t).
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
                     types = NULL, directed = TRUE, ordinal = FALSE,
                     origin = NULL, omit_dyad = NULL,
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = NA, start = 1, stop = Inf, adjmat = NULL,
                     output = c("all", "stats_only")) {
  # Prepare the edgelist
  if (!("reh" %in% class(edgelist))) {
    prep <- remify::reh(
      edgelist = edgelist, actors = actors,
      types = types, directed = directed, ordinal = ordinal,
      origin = origin, omit_dyad = omit_dyad, model = "tie"
    )
  } else {
    prep <- edgelist
  }

  # Extract relevant elements from the prepared remify::reh object
  edgelist.reh <- prep$edgelist
  actors <- attr(prep, "dictionary")$actors
  types <- attr(prep, "dictionary")$types

  # Riskset
  prepR <- getRisksetMatrix(
    actors$actorID, types$typeID, nrow(actors),
    nrow(types), directed
  )

  # Match output
  output <- match.arg(output)

  # Match memory
  memory <- match.arg(memory)
  if (memory == "full") {
    memory_value <- Inf
  }
  if (memory == "window") {
    if ((!(length(memory_value) == 1)) || (!is.numeric(memory_value))) {
      stop("A 'memory_value' should be supplied when memory is 'window'")
    }
  }
  if (memory == "decay") {
    if ((!(length(memory_value) == 1)) || (!is.numeric(memory_value))) {
      stop("A 'memory_value' should be supplied when memory is 'decay'")
    }
  }
  if (memory == "interval") {
    if ((!(length(memory_value) == 2)) || (!is.numeric(memory_value))) {
      stop("Two 'memory_value' values should be supplied when memory is 'interval'")
    }
    if (memory_value[1] > memory_value[2]) {
      stop("The first memory_value value should be lower than the second")
    }
  }

  # Convert R start and stop indices to C++ (indexing starts at 0)
  if (start < 1) {
    stop("start should be set to 1 or larger.")
  }
  if (stop < start) {
    stop("stop cannot be smaller than start.")
  }
  start <- start - 1
  if (stop == Inf) {
    stop <- nrow(edgelist.reh)
  }
  stop <- stop - 1

  # Prepare main effects
  form <- effects
  effects <- parse_formula(form, "rem", ordinal)
  all_effects <- c(
    "baseline", # 1
    "send", "receive", # 2 #3
    "same", "difference", "average", # 4 #5 #6
    "minimum", "maximum", "removed", # 7 #8 #9
    "inertia", "reciprocity", # 10 #11
    "indegreeSender", "indegreeReceiver", # 12 #13
    "outdegreeSender", "outdegreeReceiver", # 14 #15
    "totaldegreeSender", "totaldegreeReceiver", # 16, #17
    "otp", "itp", "osp", "isp", # 18 #19 #20 #21
    "sp", "spUnique", # 22, #23
    "psABBA", "psABBY", "psABXA", # 24 #25 #26
    "psABXB", "psABXY", "psABAY", # 27 #28 #29
    "rrankSend", "rrankReceive", # 30 #31
    "FEtype", "event", # 32 #33
    "recencyContinue", # 34
    "recencySendSender", "recencySendReceiver", # 35 #36
    "recencyReceiveSender", "recencyReceiveReceiver", # 37 #38
    "tie", # 39

    "indegreeSender.type", "indegreeReceiver.type", # 40 #41
    "outdegreeSender.type", "outdegreeReceiver.type", # 42 #43
    "totaldegreeSender.type", "totaldegreeReceiver.type", # 44 #45
    "psABBA.type", "psABBY.type", "psABXA.type", # 46 #47 #48
    "psABXB.type", "psABXY.type", "psABAY.type", # 49 #50 #51
    "inertia.type", "reciprocity.type", # 52 #53
    "otp.type", "itp.type", "osp.type", "isp.type", # 54 #55 #56 #57
    "sp.type", "spUnique.type", # 58, #59
    "rrankSend.type", "rrankReceive.type", # 60 #61
    "recencyContinue.type", # 62
    "recencySendSender.type", "recencySendReceiver.type", # 63 #64
    "recencyReceiveSender.type", "recencyReceiveReceiver.type", # 65 #66

    "degreeMin", "degreeMax", # 67 #68
    "degreeMin.type", "degreeMax.type", # 69 #70
    "ccp", # 71
    "totaldegreeDyad", # 72
    "userStat", # 73
    "psABAB", "psABAB.type", # 74 #75
    "degreeDiff", "degreeDiff.type", # 76 #77

    "interact"
  ) # 99
  effectsN <- match(sapply(effects, function(x) x$effect), all_effects)

  # Check correct specification effects
  if (!directed) {
    if (any(effectsN %in%
      c(2, 3, 11:21, 24:28, 30:31, 35:38, 40:50, 53:57, 60:61, 63:66))) {
      stop(paste("Attempting to request effects that are not (yet) defined for undirected events"))
    }
  }

  if (directed) {
    if (any(effectsN %in% c(22:23, 58:59, 67:71, 76:77))) {
      stop(paste("Attemping to request effects that are not (yet) defined for directed events"))
    }
  }

  # Prepare fixed effects
  if (any(sapply(effects, function(x) x$effect == "FEtype"))) {
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
    effects <- append(effects[-pos], FEeffects, pos - 1)
    effectsN <- match(sapply(effects, function(x) x$effect), all_effects)
  }

  # Prepare interaction effects
  effects_int <- parse_int(form, "rem", effects, ordinal)
  effectsN <- append(effectsN, rep(99, length(effects_int)), length(effectsN))
  interactions <- list()
  interactions[which(effectsN == 99)] <- effects_int

  # Prepare covariate information
  covar <- lapply(effects, function(x) {
    if (x$effect %in% c(
      "send", "receive", "same", "difference", "average",
      "minimum", "maximum"
    )) {
      if (is.null(x$x)) {
        # Check if the variable name is in the attributes object
        if (!(x$variable %in% colnames(attributes))) {
          stop(paste0("Variable '", x$variable, "' not in attributes object for the '", x$effect, "' effect."))
        }
        # Check if the time variable is available
        if (!("time" %in% colnames(attributes))) {
          stop("time variable is missing in attributes object")
        }
        if (anyNA(attributes$time)) {
          stop("time variable in attributes cannot have missing values")
        }
        # Collect the information in a dataframe
        dat <- data.frame(
          id = attributes$id,
          time = attributes$time,
          x = attributes[, x$variable]
        )
        # Warning for missing values
        if (anyNA(dat)) {
          warning(paste0("Missing values in the attributes object for the '", x$effect, "' effect can cause unexpected behavior."))
        }
        # Check if all actors are in the attributes
        if (!all(actors[, 1] %in% dat$id)) {
          stop("Missing actors in the attributes object.")
        }
        dat$id <- actors[match(dat$id, actors[, 1]), 2]
        colnames(dat)[3] <- x$variable
        dat <- dat[order(as.numeric(dat$id)), ]
        as.matrix(dat)
      } else {
        dat <- x$x
        # Check if all actors are in the attributes
        if (!all(actors[, 1] %in% dat$id)) {
          stop("Missing actors in the attributes object.")
        }
        dat$id <- actors[match(dat$id, actors[, 1]), 2]
        dat <- dat[order(as.numeric(dat$id)), ]
      }
      # Check for actors in the attributes object that are not in the
      # risk set
      if (any(is.na(dat$id))) {
        warning(paste0("Attributes contain actors that are not in the risk set. These are not included in the computation of the statistics."))
        dat <- dat[!is.na(dat$id), ]
      }
      as.matrix(dat)
    } else if (x$effect == "tie") {
      parse_tie(x, prep)
    } else if (x$effect == "event") {
      if (length(x$x) != nrow(edgelist.reh)) {
        stop("Length of vector 'x' in event() does not match number of events in edgelist")
      }
      dat <- x$x
      as.matrix(dat)
    } else if (x$effect == "FEtype") {
      dat <- x$typeID
      as.matrix(dat)
    } else if (x$effect == "ccp") {
      dat <- x$x
      as.matrix(dat)
    } else if (x$effect == "userStat") {
      if (NROW(x$x) != nrow(edgelist.reh)) {
        stop("Number of rows of matrix 'x' in userStat() does not match number of events in edgelist")
      }
      if (NCOL(x$x) != nrow(prepR)) {
        stop("Number of columns of matrix 'x' in userStat() does not match number of dyads in risk set")
      }
      dat <- x$x
      as.matrix(dat)
    } else {
      matrix()
    }
  })

  # Prepare scaling info (vector length p)
  scaling <- as.numeric(sapply(effects, function(x) x$scaling))

  # Compute the adjacency matrix
  if (any(effectsN %in% c(10:23, 40:45, 52:59, 67:70, 72, 76:77))) {
    if (is.null(adjmat)) {
      adjmat <- compute_adjmat(
        edgelist.reh, nrow(actors), prep$D,
        directed, memory, memory_value, start, stop
      )
    }
  } else {
    if (is.null(adjmat)) {
      adjmat <- matrix()
    }
  }

  # Compute statistics
  statistics <- compute_stats_tie(
    effectsN, edgelist.reh, adjmat, actors[, 2],
    types[, 2], prepR, scaling, covar, interactions, start, stop, directed
  )

  # Dimnames statistics
  dimnames(statistics) <-
    list(NULL, NULL, unlist(c(all_effects[effectsN])))

  # Add variable name to exogenous statistics
  dimnames(statistics)[[3]][which(effectsN %in% c(2:8, 73))] <-
    sapply(
      effects[which(effectsN %in% c(2:8, 73))],
      function(x) {
        if (!is.null(x$variable)) {
          paste0(x$effect, "_", x$variable)
        } else {
          x$effect
        }
      }
    )

  # Add variable name to event and tie statistic
  dimnames(statistics)[[3]][which(effectsN %in% c(33, 39))] <-
    sapply(
      effects[which(effectsN %in% c(33, 39))],
      function(x) {
        if (!is.null(x$variable)) {
          paste0(x$variable)
        } else {
          x$effect
        }
      }
    )

  # Add counter to tie name
  tie_effects <- grepl("tie", dimnames(statistics)[[3]])
  if (sum(tie_effects) > 1) {
    dimnames(statistics)[[3]][tie_effects] <-
      paste0("tie", 1:sum(tie_effects))
  }

  # Add counter to event name
  event_effects <- grepl("event", dimnames(statistics)[[3]])
  if (sum(event_effects) > 1) {
    dimnames(statistics)[[3]][event_effects] <-
      paste0("event", 1:sum(event_effects))
  }

  # Add variable name to interaction statistics
  dimnames(statistics)[[3]][which(effectsN == 99)] <-
    sapply(
      interactions[which(effectsN == 99)],
      function(x) {
        paste0(
          dimnames(statistics)[[3]][as.numeric(x[1])],
          ".x.",
          dimnames(statistics)[[3]][as.numeric(x[2])]
        )
      }
    )

  if (output == "all") {
    # Transform edgelist to evls (for estimation with relevent::rem)
    evls <- edgelist.reh[, c(2, 1)]
    if (is.null(nrow(evls))) {
      evls[1] <- evls[1] + 1
      names(evls) <- c("event", "time")
    } else {
      evls[, 1] <- evls[, 1] + 1
      colnames(evls) <- c("event", "time")
      evls[(start + 1):(stop + 1), ]
    }


    # Riskset output
    riskset <- prepR
    riskset <- as.data.frame(riskset)
    if (directed) {
      colnames(riskset) <- c("sender", "receiver", "type", "id")
    } else {
      colnames(riskset) <- c("actor1", "actor2", "type", "id")
    }
    riskset[, 1] <- sapply(riskset[, 1], function(a) {
      remify::actorName(prep, a)
    })
    riskset[, 2] <- sapply(riskset[, 2], function(a) {
      remify::actorName(prep, a)
    })
    riskset[, 3] <- sapply(riskset[, 3], function(a) {
      remify::typeName(prep, a)
    })
    if (!("reh" %in% class(edgelist))) {
      riskset$id <- riskset$id + 1
    } else {
      riskset$stat_column <- riskset$id + 1
    }
    riskset <- as.data.frame(riskset)

    # Edgelist output
    if ("reh" %in% class(edgelist)) {
      edgelist <- prep$edgelist
    }

    # Output
    out <- list(
      statistics = statistics,
      edgelist = edgelist,
      riskset = riskset,
      actors = actors[, 1],
      types = types[, 1],
      evls = evls,
      adjmat = adjmat
    )
  } else {
    # Output
    out <- list(
      statistics = statistics
    )
  }

  class(out) <- c("tomstats", "remstats")
  attr(out, "model") <- "tie"
  attr(out, "formula") <- form
  out
}
