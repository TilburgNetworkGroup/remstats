#' aomstats
#'
#' Computes statistics for the sender activity rate step and receiver choice
#' step in actor-oriented relational event models (e.g., see Stadtfeld & Block,
#' 2017).
#' 
#' @inheritParams remstats
#'
#' @section Effects:
#' The statistics to be computed are defined symbolically and should be
#' supplied to the \code{sender_effects} and/or \code{receiver_effects}
#' arguments in the form \code{~ effects}. The terms are separated by +
#' operators. For example: \code{receiver_effects = ~ inertia() + otp()}.
#' Interactions between two effects can be included with * or :
#' operators. For example: \code{receivereffects = ~ inertia():otp()}.  A list 
#' of available effects can be obtained with \code{\link{actor_effects}()}.
#' 
#' The majority of the statistics can be scaled in some way, see
#' the documentation of the \code{scaling} argument in the separate effect
#' functions for more information on this.
#'
#' @section Attributes:
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
#' Note that if the edgelist contains a column that is named ``weight'', it is
#' assumed that these affect the endogenous statistics. These settings are
#' defined globally in the \code{aomstats} function and affect the computation
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
#' aomstats(
#'   edgelist = history, sender_effects = seff, receiver_effects = reff,
#'   attributes = info
#' )
#'
#' @references Stadtfeld, C., & Block, P. (2017). Interactions, actors, and
#' time: Dynamic network actor models for relational events. Sociological
#' Science, 4, 318–352. \url{https://doi.org/10.15195/v4.a14}
#'
#'
#' @export
aomstats <- function(edgelist, sender_effects = NULL, receiver_effects = NULL,
                     attributes = NULL, actors = NULL, types = NULL,
                     ordinal = FALSE, origin = NULL, omit_dyad = NULL,
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = Inf, start = 1, stop = Inf, adjmat = NULL, display_progress = FALSE) {
  # Prepare the edgelist
  if (!("reh" %in% class(edgelist))) {
    prep <- remify::reh(
      edgelist = edgelist, actors = actors,
      types = types, directed = TRUE, ordinal = ordinal,
      origin = origin, omit_dyad = omit_dyad, model = "actor"
    )
  } else {
    prep <- edgelist
  }

  # Extract relevant elements from the prepared remify::reh object
  edgelist.reh <- prep$edgelist
  actors <- attr(prep, "dictionary")$actors
  types <- attr(prep, "dictionary")$types

  # Check for event types
  if (nrow(types) > 1) {
    stop("Multiple event types are not yet defined for the actor-oriented model.")
  }

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

  # Riskset
  prepR <- getRisksetMatrix(
    actors$actorID, types$typeID, nrow(actors),
    nrow(types), TRUE
  )

  # Initialize stats
  rateStats <- NULL
  choiceStats <- NULL

  # sender_effects
  rateFormula <- sender_effects
  if (!is.null(sender_effects)) {
    # Prepare main sender_effects
    sender_effects <- parse_formula(rateFormula, "rateEffects")
    all_sender_effects <- c(
      "baseline", "send", # 1,2
      "indegreeSender", "outdegreeSender", "totaldegreeSender", # 3,4,5
      "recencySendSender", "recencyReceiveSender", # 6, 7,
      "interact"
    ) # 99
    sender_effectsN <- match(
      sapply(sender_effects, function(x) x$effect),
      all_sender_effects
    )
    # Check correct specification effects
    if (anyNA(sender_effectsN)) {
      stop(paste("Attempting to request effects that are not defined for the sender activity model"))
    }

    # Prepare interaction sender_effects
    sender_effects_int <- parse_int(
      rateFormula, "rateEffects",
      sender_effects
    )
    sender_effectsN <- append(
      sender_effectsN,
      rep(99, length(sender_effects_int)),
      length(sender_effectsN)
    )
    rate_interactions <- list()
    rate_interactions[which(sender_effectsN == 99)] <- sender_effects_int

    # Prepare sender_effects covariate information
    rateCovar <- lapply(sender_effects, function(x) {
      if (x$effect == "send") {
        if (is.null(x$x)) {
          # Check if the variable name is in the attributes object
          if (!(x$variable %in% colnames(attributes))) {
            stop(paste0("Variable '", x$variable, "' not in attributes object for the '", x$effect, "' effect."))
          }
          # Check if the time variable is available
          if (!("time" %in% colnames(attributes))) {
            stop(paste0("time variable is missing in attributes object"))
          }
          if (anyNA(attributes$time)) {
            stop("time variable in attributes cannot have missing values")
          }
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
          dat$id <- attr(prep, "dictionary")$actors[match(dat$id, attr(prep, "dictionary")$actors[, 1]), 2]
          colnames(dat)[3] <- x$variable
          as.matrix(dat)
        } else {
          dat <- x$x
          # Check if all actors are in the attributes
          if (!all(actors[, 1] %in% dat$id)) {
            stop("Missing actors in the attributes object.")
          }
          dat$id <- attr(prep, "dictionary")$actors[match(dat$id, attr(prep, "dictionary")$actors[, 1]), 2]
          as.matrix(dat)
        }
        # Check for actors in the attributes object that are not in the
        # risk set
        if (any(is.na(dat$id))) {
          warning(paste0("Attributes contain actors that are not in the risk set. These are not included in the computation of the statistics."))
          dat <- dat[!is.na(dat$id), ]
        }
        as.matrix(dat)
      } else {
        matrix()
      }
    })

    # Prepare sender_effects scaling
    rateScaling <- as.numeric(
      sapply(sender_effects, function(x) x$scaling)
    )

    # Compute the adjacency matrix
    adjmat <- matrix()

    # Compute the rate statistics
    rateStats <- compute_stats_rate(
      sender_effectsN, edgelist.reh, prepR, adjmat, actors[, 2], rateScaling, 
      rateCovar, rate_interactions, memory, memory_value, start, stop, display_progress
    )

    # Reset the adjacency matrix to null
    if (all(dim(adjmat) == c(1, 1))) {
      adjmat <- NULL
    }

    # Dimnames statistics
    dimnames(rateStats) <-
      list(NULL, NULL, unlist(c(all_sender_effects[sender_effectsN])))

    # Add variable name to exogenous rateStats
    dimnames(rateStats)[[3]][which(sender_effectsN == 2)] <-
      sapply(sender_effects[which(sender_effectsN == 2)], function(x) {
        paste0(x$effect, "_", x$variable)
      })

    # Add variable name to interaction statistics
    dimnames(rateStats)[[3]][which(sender_effectsN == 99)] <-
      sapply(
        rate_interactions[which(sender_effectsN == 99)],
        function(x) {
          paste0(
            dimnames(rateStats)[[3]][as.numeric(x[1])],
            ".x.",
            dimnames(rateStats)[[3]][as.numeric(x[2])]
          )
        }
      )
  }

  # receiver_effects
  choiceFormula <- receiver_effects
  if (!is.null(receiver_effects)) {
    # Prepare main receiver_effects
    receiver_effects <- parse_formula(choiceFormula, "choiceEffects")
    all_receiver_effects <- c(
      "receive", "same", "difference", "average", # 1, 2, 3, 4
      "tie", # 5
      "inertia", "reciprocity", # 6, 7
      "indegreeReceiver", "outdegreeReceiver", # 8, 9
      "totaldegreeReceiver", # 10
      "otp", "itp", "osp", "isp", # 11, 12, 13, 14
      "rrankSend", "rrankReceive", # 15, 16
      "recencySendReceiver", "recencyReceiveReceiver", # 17 #18
      "recencyContinue", # 19
      "interact"
    ) # 99

    receiver_effectsN <- match(sapply(
      receiver_effects,
      function(x) x$effect
    ), all_receiver_effects)

    # Check correct specification effects
    if (anyNA(receiver_effectsN)) {
      stop(paste("Attempting to request effects that are not defined for the receiver choice model"))
    }

    # Prepare interaction receiver_effects
    receiver_effects_int <- parse_int(
      choiceFormula, "choiceEffects",
      receiver_effects
    )
    receiver_effectsN <- append(
      receiver_effectsN,
      rep(99, length(receiver_effects_int)), length(receiver_effectsN)
    )
    choice_interactions <- list()
    choice_interactions[which(receiver_effectsN == 99)] <-
      receiver_effects_int

    # Prepare receiver_effects covariate information
    choiceCovar <- lapply(receiver_effects, function(x) {
      if (x$effect %in% c("receive", "same", "difference", "average")) {
        if (is.null(x$x)) {
          # Check if the variable name is in the attributes object
          if (!(x$variable %in% colnames(attributes))) {
            stop(paste0("Variable '", x$variable, "' not in attributes object for the '", x$effect, "' effect."))
          }
          # Check if the time variable is available
          if (!("time" %in% colnames(attributes))) {
            stop(paste0("time variable is missing in attributes object"))
          }
          if (anyNA(attributes$time)) {
            stop("time variable in attributes cannot have missing values")
          }
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
          dat$id <- attr(prep, "dictionary")$actors[match(
            dat$id,
            attr(prep, "dictionary")$actors[, 1]
          ), 2]
          colnames(dat)[3] <- x$variable
          as.matrix(dat)
        } else {
          dat <- x$x
          # Check if all actors are in the attributes
          if (!all(actors[, 1] %in% dat$id)) {
            stop("Missing actors in the attributes object.")
          }
          dat$id <- attr(prep, "dictionary")$actors[match(
            dat$id,
            attr(prep, "dictionary")$actors[, 1]
          ), 2]
          as.matrix(dat)
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
      } else {
        matrix()
      }
    })

    # Prepare receiver_effects scaling
    choiceScaling <- as.numeric(
      sapply(receiver_effects, function(x) x$scaling)
    )

    # Compute the adjacency matrix
    if (any(receiver_effectsN %in% c(6:7, 11:14))) {
      if (is.null(adjmat)) {
        adjmat <- compute_adjmat(
          edgelist.reh, nrow(actors), prep$D,
          TRUE, memory, memory_value, start, stop
        )
      }
    } else {
      if (is.null(adjmat)) {
        adjmat <- matrix()
      }
    }

    # Compute the choice statistics
    choiceStats <- compute_stats_choice(
      receiver_effectsN, edgelist.reh, adjmat, actors[, 2], prepR, 
      choiceScaling, choiceCovar, choice_interactions, memory, memory_value, 
      start, stop, display_progress
    )

    # Dimnames statistics
    dimnames(choiceStats) <-
      list(NULL, NULL, unlist(c(all_receiver_effects[receiver_effectsN])))

    # Add variable name to exogenous choiceStats
    dimnames(choiceStats)[[3]][which(receiver_effectsN %in% c(1:5))] <-
      sapply(
        receiver_effects[which(receiver_effectsN %in% c(1:5))],
        function(x) {
          if (!is.null(x$variable)) {
            paste0(x$effect, "_", x$variable)
          } else {
            x$effect
          }
        }
      )

    # Add variable name to tie statistic
    dimnames(choiceStats)[[3]][which(receiver_effectsN == 5)] <-
      sapply(
        receiver_effects[which(receiver_effectsN == 5)],
        function(x) {
          if (!is.null(x$variable)) {
            paste0(x$variable)
          } else {
            x$effect
          }
        }
      )

    # Add counter to tie name
    tie_effects <- grepl("tie", dimnames(choiceStats)[[3]])
    if (sum(tie_effects) > 1) {
      dimnames(choiceStats)[[3]][tie_effects] <-
        paste0("tie", 1:sum(tie_effects))
    }


    # Add variable name to interaction statistics
    dimnames(choiceStats)[[3]][which(receiver_effectsN == 99)] <-
      sapply(
        choice_interactions[which(receiver_effectsN == 99)],
        function(x) {
          paste0(
            dimnames(choiceStats)[[3]][as.numeric(x[1])],
            ".x.",
            dimnames(choiceStats)[[3]][as.numeric(x[2])]
          )
        }
      )
  }

  # Riskset output 
  riskset <- prepR
  riskset <- as.data.frame(riskset)
  colnames(riskset) <- c("sender", "receiver", "type", "id")
  riskset$sender <- actors$actorName[match(riskset$sender, actors$actorID)]
  riskset$receiver <- actors$actorName[match(riskset$receiver, actors$actorID)]
  riskset$type <- types$typeName[match(riskset$type, types$typeID)]
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
    statistics = list(
      sender_stats = rateStats, receiver_stats = choiceStats
    ),
    edgelist = edgelist,
    riskset = riskset,
    actors = actors[, 1],
    adjmat = adjmat
  )
  class(out) <- c("aomstats", "remstats")
  attr(out, "model") <- "actor"
  attr(out, "formula") <- list(rate = rateFormula, choice = choiceFormula)
  out
}
