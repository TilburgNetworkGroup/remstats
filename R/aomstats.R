#' aomstats
#'
#' Computes statistics for the sender activity rate step and receiver choice
#' step in actor-oriented relational event models (e.g., see Stadtfeld & Block,
#' 2017).
#'
#' @inheritParams remstats
#' @param display_progress should a progress bar for the computation of the
#' endogenous statistics be shown (TRUE) or not (FALSE)?
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
#' @section attr_data:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attr_data} argument in either \code{remstats()} or in the separate
#' effect functions supplied to the \code{..._effects} arguments (e.g., see
#' \code{\link{send}}). This \code{attr_data} object should be constructed as
#' follows: A dataframe with rows refering to the attribute value of actor
#' \emph{i} at timepoint \emph{t}. A `name` column is required that contains the
#' actor name (corresponding to the actor names in the relational event
#' history). A `time` column is required that contains the time when attributes
#' change (set to zero if none of the attributes vary over time). Subsequent
#' columns contain the attributes that are called in the specifications of
#' exogenous statistics (column name corresponding to the string supplied to
#' the \code{variable} argument in the effect function). Note that the
#' procedure for the exogenous effects `tie' and `event' deviates from this,
#' here the exogenous covariate information has to be specified in a different
#' way, see \code{\link{tie}} and \code{\link{event}}.
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
#' Note that if the relational event history contains a column that is named
#' ``weight'', it is assumed that these affect the endogenous statistics. These
#' affect the computation of all endogenous statistics with a few exceptions
#' that follow logically from their definition (e.g., the recenyContinue
#' statistic does depend on time since the event and not on event weights).
#'
#' @section Subset of the relational event history:
#' Optionally, statistics can be computed for a slice of the relational event
#' sequence - but based on the entire history. This is achieved by setting the
#' start and stop values equal to the index of the first and last event for
#' which statistics are requested. For example, start = 5 and stop = 5 computes
#' the statistics for only the 5th event in the relational event sequence,
#' based on the history that consists of events 1-4.
#'
#' @return An object of class 'aomstats'. List with in the first element the statistics for the sender activity rate step and in the second element the statistics for the receiver choice step. The 'aomstats' object has the 
#' following attributes: 
#'   \describe{
#'     \item{\code{model}}{Type of model that is estimated.}
#'     \item{\code{formula}}{Model formula(s), obtained from the formula(s) inputted to 'sender_effects' and/or 'receiver_effects'.}
#'     \item{\code{actors}}{The set of actors used to construct the statistics, obtained from the remify object inputted to 'reh'.}
#'   }
#'
#' @examples
#' library(remstats)
#' seff <- ~ send("extraversion")
#' reh <- remify::remify(edgelist = history, model = "actor")
#' reff <- ~ receive("agreeableness") + inertia() + otp()
#' aomstats(
#'   reh = reh, sender_effects = seff, receiver_effects = reff,
#'   attr_data = info
#' )
#'
#' @references Stadtfeld, C., & Block, P. (2017). Interactions, actors, and
#' time: Dynamic network actor models for relational events. Sociological
#' Science, 4, 318–352. \url{https://doi.org/10.15195/v4.a14}
#'
#'
#' @export
aomstats <- function(reh,
                     sender_effects = NULL,
                     receiver_effects = NULL,
                     attr_data = NULL,
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = Inf,
                     start = 1,
                     stop = Inf,
                     display_progress = FALSE,
                     attributes, edgelist) {

   # Check if the deprecated argument "attributes" is used
  if (!missing(attributes)) {
        warning("use 'attr_data' instead of 'attributes'")
        attr_data <- attributes
  }

  # Check if the deprecated "id" column is used in attr_data
    if (!is.null(attr_data)) {
        if (("id" %in% colnames(attr_data)) & !("name" %in% colnames(attr_data))) {
        warning("use 'name' instead of 'id' in 'attr_data'")
        colnames(attr_data)[which(colnames(attr_data) == "id")] <- "name"
        }
    }

  # Check if the deprecated argument "edgelist" is used
  if (!missing(edgelist)) {
        warning("use 'reh' instead of 'edgelist'")
        reh <- edgelist
  }

  # Check the reh
  if (!("remify" %in% class(reh))) {
    stop("Expected a reh object of class remify")
  }
  if (attr(reh, "model") != "actor") {
    stop("The reh object should be prepared with the model argument set to `actor' if effects for the actor-oriented model are computed")
  }

  # Extract relevant elements from the prepared remify::remify object
  edgelist <- reh$edgelist
  actors <- attr(reh, "dictionary")$actors
  types <- attr(reh, "dictionary")$types
  edgelist[,1] <- cumsum(reh$intereventTime)

  # Transform to cpp indexing!
  mat_edges <- as.matrix(edgelist)
  mat_edges[, 2] <- mat_edges[, 2] - 1
  mat_edges[, 3] <- mat_edges[, 3] - 1

  actors$actorID <- actors$actorID - 1

  # Check for event weights
  if (!("weight" %in% colnames(edgelist))) {
    weights <- rep(1, nrow(edgelist))
  } else {
    weights <- edgelist$weight
  }

  # Check for event types
  if (!is.null(types)) {
    stop("Multiple event types are not yet defined for the actor-oriented model.")
  }

  # Match memory
  memory <- match.arg(memory)
  memory_value <- validate_memory(memory, memory_value)

  # Convert R start and stop indices to C++ (indexing starts at 0)
  if (start < 1) {
    stop("start should be set to 1 or larger.")
  }
  if (stop < start) {
    stop("stop cannot be smaller than start.")
  }
  start <- start - 1
  if (stop == Inf) {
    stop <- nrow(edgelist)
  }
  stop <- stop - 1

  # Initialize stats
  rateStats <- NULL
  choiceStats <- NULL

  # sender_effects
  rateFormula <- sender_effects
  if (!is.null(sender_effects)) {
    # Prepare main sender_effects
    check_formula(rateFormula)
    sender_effects <- parse_formula(rateFormula, "rateEffects")
    sender_effectsNames <- sapply(sender_effects, function(x) x$effect)

    # Check correct specification effects
    if (!all(sender_effectsNames %in% actor_effects(step = "sender"))) {
      stop(paste("Attempting to request effects that are not defined for the sender activity model"))
    }

    # Prepare interaction sender_effects
    sender_effects_int <- parse_int(
      rateFormula, "rateEffects",
      sender_effects
    )
    sender_effectsNames <- append(sender_effectsNames, rep("interact", length(sender_effects_int)), length(sender_effectsNames))
    rate_interactions <- list()
    rate_interactions[which(sender_effectsNames == "interact")] <- sender_effects_int

    # Prepare sender_effects covariate information
    rateCovar <- lapply(sender_effects, function(x) {
      if (x$effect == "send") {
        if (is.null(x$x)) {
          # Check if the variable name is in the attr_data object
          if (!(x$variable %in% colnames(attr_data))) {
            stop(paste0("Variable '", x$variable, "' not in attr_data object for the '", x$effect, "' effect."))
          }
          # Check if the time variable is available
          if (!("time" %in% colnames(attr_data))) {
            stop(paste0("time variable is missing in attr_data object"))
          }
          if (anyNA(attr_data$time)) {
            stop("time variable in attr_data cannot have missing values")
          }
          dat <- data.frame(
            name = attr_data$name,
            time = attr_data$time,
            x = attr_data[, x$variable]
          )
          # Warning for missing values
          if (anyNA(dat)) {
            warning(paste0("Missing values in the attr_data object for the '", x$effect, "' effect can cause unexpected behavior."))
          }
          # Check if all actors are in the attr_data
          if (!all(actors[, 1] %in% dat$name)) {
            stop("Missing actors in the attr_data object.")
          }
          dat$name <- actors[match(dat$name, actors[, 1]), 2]
          colnames(dat)[3] <- x$variable
          as.matrix(dat)
        } else {
          dat <- x$x
          # Check if all actors are in the attr_data
          if (!all(actors[, 1] %in% dat$name)) {
            stop("Missing actors in the attr_data object.")
          }
          dat$name <- actors[match(dat$name, actors[, 1]), 2]
          as.matrix(dat)
        }
        # Check for actors in the attr_data object that are not in the
        # risk set
        if (any(is.na(dat$name))) {
          warning(paste0("attr_data contain actors that are not in the risk set. These are not included in the computation of the statistics."))
          dat <- dat[!is.na(dat$name), ]
        }
        as.matrix(dat)
      } else if(x$effect == "userStat") {
        if (NROW(x$x) != nrow(edgelist)) {
          stop("Number of rows of matrix 'x' in userStat() does not match number of events in edgelist")
        }

        if (NCOL(x$x) != nrow(actors)) {
          stop("Number of columns of matrix 'x' in userStat() does not match number of actors")
        }

        as.matrix(x$x)
      } else {
        matrix()
      }
    })

    # Prepare sender_effects scaling
    rateScaling <- sapply(sender_effects, function(x) {
	    ifelse("scaling" %in% names(x), x$scaling, "none")
    })
    rateScaling <- append(rateScaling, rep("none", length(sender_effects_int)), length(rateScaling))

    # Compute the rate statistics
    rateStats <- compute_stats_rate(
      sender_effectsNames, mat_edges, actors[, 2], weights, rateCovar,
      rate_interactions, memory, memory_value, rateScaling, start, stop,
      display_progress
    )

    # Add variable names to the statistics dimnames
    rateStats <- add_variable_names(rateStats, sender_effectsNames, 
      sender_effects, rate_interactions)
  }

  # receiver_effects
  choiceFormula <- receiver_effects
  if (!is.null(receiver_effects)) {
    # Prepare main receiver_effects
    check_formula(choiceFormula)
    receiver_effects <- parse_formula(choiceFormula, "choiceEffects")
    receiver_effectsNames <- sapply(receiver_effects, function(x) x$effect)

    # Check correct specification effects
    if (!all(receiver_effectsNames %in% actor_effects(step = "receiver"))) {
      stop(paste("Attempting to request effects that are not defined for the receiver choice model"))
    }

    # Prepare interaction receiver_effects
    receiver_effects_int <- parse_int(
      choiceFormula, "choiceEffects",
      receiver_effects
    )
    receiver_effectsNames <- append(receiver_effectsNames, rep("interact", length(receiver_effects_int)), length(receiver_effectsNames))
    choice_interactions <- list()
    choice_interactions[which(receiver_effectsNames == "interact")] <- receiver_effects_int

    # Prepare receiver_effects covariate information
    choiceCovar <- lapply(receiver_effects, function(x) {
      if (x$effect %in% c("receive", "same", "difference", "average")) {
        if (is.null(x$x)) {
          # Check if the variable name is in the attr_data object
          if (!(x$variable %in% colnames(attr_data))) {
            stop(paste0("Variable '", x$variable, "' not in attr_data object for the '", x$effect, "' effect."))
          }
          # Check if the time variable is available
          if (!("time" %in% colnames(attr_data))) {
            stop(paste0("time variable is missing in attr_data object"))
          }
          if (anyNA(attr_data$time)) {
            stop("time variable in attr_data cannot have missing values")
          }
          dat <- data.frame(
            name = attr_data$name,
            time = attr_data$time,
            x = attr_data[, x$variable]
          )
          # Warning for missing values
          if (anyNA(dat)) {
            warning(paste0("Missing values in the attr_data object for the '", x$effect, "' effect can cause unexpected behavior."))
          }
          # Check if all actors are in the attr_data
          if (!all(actors[, 1] %in% dat$name)) {
            stop("Missing actors in the attr_data object.")
          }
          dat$name <- actors[match(dat$name, actors[, 1]), 2]
          colnames(dat)[3] <- x$variable
          as.matrix(dat)
        } else {
          dat <- x$x
          # Check if all actors are in the attr_data
          if (!all(actors[, 1] %in% dat$name)) {
            stop("Missing actors in the attr_data object.")
          }
          dat$name <- actors[match(dat$name, actors[, 1]), 2]
          as.matrix(dat)
        }
        # Check for actors in the attr_data object that are not in the
        # risk set
        if (any(is.na(dat$name))) {
          warning(paste0("attr_data contain actors that are not in the risk set. These are not included in the computation of the statistics."))
          dat <- dat[!is.na(dat$name), ]
        }
        as.matrix(dat)
      } else if (x$effect == "tie") {
        parse_tie(x, reh)
      } else if(x$effect == "userStat") {
        if (NROW(x$x) != nrow(edgelist)) {
          stop("Number of rows of matrix 'x' in userStat() does not match number of events in edgelist")
        }

        if (NCOL(x$x) != nrow(actors)) {
          stop("Number of columns of matrix 'x' in userStat() does not match number of actors")
        }

        as.matrix(x$x)
      } else {
        matrix()
      }
    })

    # Prepare receiver_effects scaling
    choiceScaling <- sapply(receiver_effects, function(x) {
	    ifelse("scaling" %in% names(x), x$scaling, "none")
    })
    choiceScaling <- append(choiceScaling, rep("none", length(receiver_effects_int)), length(choiceScaling))

    # Compute the choice statistics
    choiceStats <- compute_stats_choice(
      receiver_effectsNames, mat_edges, actors[, 2], weights, choiceCovar,
      choice_interactions, memory, memory_value, choiceScaling,
      start, stop, display_progress
    )

    # Add variable names to the statistics dimnames
    choiceStats <- add_variable_names(choiceStats, 
      receiver_effectsNames, receiver_effects, choice_interactions)
  }

  # Output
  out <- list(sender_stats = rateStats, receiver_stats = choiceStats)
  class(out) <- c("aomstats", "remstats")
  attr(out, "model") <- "actor"
  attr(out, "formula") <- list(rate = rateFormula, choice = choiceFormula)
  attr(out, "actors") <- actors
  out
}
