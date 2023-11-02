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
#' @section attr_actors:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attr_actors} argument in either \code{remstats()} or in the separate
#' effect functions supplied to the \code{..._effects} arguments (e.g., see
#' \code{\link{send}}). This \code{attr_actors} object should be constructed as
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
#' @section attr_dyads:  
#' For the computation of the \emph{dyad exogenous} statistics with \code{tie()}, an attributes object with the exogenous covariates information per dyad has to be supplied. This is a \code{data.frame} or \code{matrix} containing attribute information for dyads. If \code{attr_dyads} is a \code{data.frame}, the first two columns should represent "actor1" and "actor2" (for directed events, "actor1" corresponds to the sender, and "actor2" corresponds to the receiver). Additional columns can represent dyads' exogenous attributes. If attributes vary over time, include a column named "time". If \code{attr_dyads} is a \code{matrix}, the rows correspond to "actor1", columns to "actor2", and cells contain dyads' exogenous attributes.
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
#'   attr_actors = info
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
                     attr_actors = NULL,
                     attr_dyads = NULL, 
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = Inf,
                     start = 1,
                     stop = Inf,
                     display_progress = FALSE,
                     attr_data, attributes, edgelist) {

  # Check if the deprecated argument "attributes" is used
	if (!missing(attributes)) {
    warning("Deprecated argument: Use 'attr_actors' instead of 'attributes'")
    attr_actors <- attributes
  }
  if (!missing(attr_data)) {
    warning("Deprecated argument: Use 'attr_actors' instead of 'attr_data'")
    attr_actors <- attr_data
  }
  # Check if the deprecated argument "edgelist" is used
  if (!missing(edgelist)) {
    warning("Deprecated argument: Use 'reh' instead of 'edgelist'")
    reh <- edgelist
  }

  # Validate remaining aomstats arguments
	attr_actors <- validate_aomstats_arguments(attr_actors, reh)	

  # Prepare the edgelist
  edgelist <- prepare_aomstats_edgelist(reh) 

  # Prepare the network actors 
  actors <- prepare_aomstats_actors(reh)

  # Prepare event weights
  weights <- prepare_aomstats_event_weights(edgelist)

  # Validate the memory argument 
  memory <- match.arg(memory)
  memory_value <- validate_memory(memory, memory_value)

  # Prepare subset arguments
  subset <- prepare_subset(start, stop, edgelist, method = "pe")
  start <- subset$start
  stop <- subset$stop 

  # Initialize stats
  sender_stats <- NULL
  receiver_stats <- NULL

  # Sender model ------------------------------------------------------------
  sender_formula <- sender_effects
  if (!is.null(sender_formula)) {
    # Prepare sender_effects
    temp <- prepare_sender_effects(sender_formula)
    sender_effects <- temp$sender_effects
    sender_effects_names <- temp$sender_effects_names
    sender_interactions <- temp[[3]]

    # Prepare sender covariate information
    sender_covar <- prepare_sender_covariates(sender_effects, attr_actors, 
      actors, edgelist)

    # Prepare sender_effects scaling
    sender_scaling <- prepare_aomstats_scaling(sender_effects, 
      sender_interactions)

    # Compute the sender statistics
    sender_stats <- compute_stats_rate(
      sender_effects_names, edgelist, actors[, 2], weights, sender_covar,
      sender_interactions, memory, memory_value, sender_scaling, start, stop,
      display_progress
    )

    # Add variable names to the statistics dimnames
    sender_stats <- add_variable_names(sender_stats, sender_effects_names, 
      sender_effects, sender_interactions)
  }

  # Receiver model ----------------------------------------------------------
  receiver_formula <- receiver_effects
  if (!is.null(receiver_effects)) {
    # Prepare receiver_effects
    temp <- prepare_receiver_effects(receiver_formula)
    receiver_effects <- temp$receiver_effects
    receiver_effects_names <- temp$receiver_effects_names
    receiver_interactions <- temp$receiver_interactions

    # Prepare receiver covariate information
    receiver_covar <- prepare_receiver_covariates(receiver_effects, 
      attr_actors, attr_dyads, actors, edgelist, reh)

    # Prepare receiver_effects scaling
    receiver_scaling <- prepare_aomstats_scaling(receiver_effects, 
      receiver_interactions)

    # Compute the choice statistics
    receiver_stats <- compute_stats_choice(
      receiver_effects_names, edgelist, actors[, 2], weights, receiver_covar,
      receiver_interactions, memory, memory_value, receiver_scaling,
      start, stop, display_progress
    )

    # Add variable names to the statistics dimnames
    receiver_stats <- add_variable_names(receiver_stats, 
      receiver_effects_names, receiver_effects, receiver_interactions)
  }

  # Output
  out <- list(sender_stats = sender_stats, receiver_stats = receiver_stats)
  class(out) <- c("aomstats", "remstats")
  attr(out, "model") <- "actor"
  attr(out, "formula") <- list(rate = sender_effects, choice = receiver_effects)
  attr(out, "actors") <- actors
  out
}
