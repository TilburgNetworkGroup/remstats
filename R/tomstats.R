#' tomstats
#'
#' Computes statistics for modeling relational event history data
#' with the tie-oriented relational event model.
#'
#' @param effects an object of class \code{"\link[stats]{formula}"} (or one
#' that can be coerced to that class): a symbolic description of the effects in
#' the model for which statistics are computed, see 'Details' for the available
#' effects and their corresponding statistics
#' @param adjmat optionally, a previously computed adjacency matrix with on the
#' rows the time points and on the columns the risk set entries
#' @param get_adjmat whether the adjmat computed by tomstats should be
#' outputted as an attribute of the statistics.
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
#' Note that events in the relational event history can be directed or
#' undirected. Some statistics are only defined for either directed or
#' undirected events (see the documentation of the statistics). Note that
#' undirected events are only available for the tie-oriented model.
#'
#' @section attr_actors:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attr_actors} argument in either \code{remstats()} or in the separate
#' effect functions supplied to the \code{..._effects} arguments (e.g., see
#' \code{\link{send}}). This \code{attr_actors} object should be constructed as
#' follows: A dataframe with rows referring to the attribute value of actor
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
#' only the past events that happened between 50 and 100 time units ago are
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
#' @section Subset the event history using 'start' and 'stop':
#' It is possible to compute statistics for a segment of the relational event
#' sequence, based on the entire event history. This is done by specifying the
#' 'start' and 'stop' values as the indices for the first and last event times
#' for which statistics are needed. For instance, setting 'start = 5' and 'stop
#' = 5' calculates statistics for the 5th event in the relational event
#' sequence, considering events 1-4 in the history. Note that in cases of
#' simultaneous events with the 'method' set to 'pt' (per timepoint), 'start'
#' and 'stop' should correspond to the indices of the first and last
#' \emph{unique} event timepoints for which statistics are needed. For example,
#' if 'start = 5' and 'stop = 5', statistics are computed for the 5th unique
#' timepoint in the relational event sequence, considering all events occurring
#' at unique timepoints 1-4.
#'
#' @section Adjacency matrix:
#' Optionally, a previously computed adjacency matrix can be supplied. Note
#' that the endogenous statistics will be computed based on this adjacency
#' matrix. Hence, supplying a previously computed adjacency matrix can reduce
#' computation time but the user should be absolutely sure the adjacency matrix
#' is accurate.
#'
#' @return An object of class 'tomstats'. Array with the computed statistics,
#' where rows refer to time points, columns refer to potential relational event
#' (i.e., potential edges) in the risk set and slices refer to statistics. The
#' 'tomstats' object has the following attributes:
#'   \describe{
#'     \item{\code{model}}{Type of model that is estimated.}
#'     \item{\code{formula}}{Model formula, obtained from the formula inputted to 'tie_effects'.}
#'     \item{\code{riskset}}{The risk set used to construct the statistics.}
#'     \item{\code{adjmat}}{[Optional], if "get_adjmat = TRUE", the matrix with the accumulated event weights for each time point (on the rows) and each dyad (in the columns).}
#'   }
#'
#' @examples
#' library(remstats)
#'
#' # Load data
#' data(history)
#' data(info)
#'
#' # Prepare data
#' reh <- remify::remify(edgelist = history, model = "tie")
#'
#' # Compute effects
#' effects <- ~ inertia():send("extraversion") + otp()
#' tomstats(effects, reh = reh, attr_actors = info)
#'
#' @references Butts, C. T. (2008). A relational event framework for social
#' action. Sociological Methodology, 38(1), 155–200.
#' \doi{10.1111/j.1467-9531.2008.00203.x}
#'
#' @export
tomstats <- function(effects, reh, attr_actors = NULL, attr_dyads = NULL,
                     method = c("pt", "pe"), 
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = NA, start = 1, stop = Inf,
                     display_progress = FALSE,
                     adjmat = NULL, get_adjmat = FALSE,
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

  # Check if the deprecated "id" column is used in attr_actors
  if (!is.null(attr_actors)) {
    if (("id" %in% colnames(attr_actors)) & !("name" %in% colnames(attr_actors))) {
      warning("use 'name' instead of 'id' in 'attr_actors'")
      colnames(attr_actors)[which(colnames(attr_actors) == "id")] <- "name"
    }
  }

  # Check if the deprecated argument "edgelist" is used
  if (!missing(edgelist)) {
    warning("Deprecated argument: Use 'reh' instead of 'edgelist'")
    reh <- edgelist
  }

  # Prepare all required inputs
  inputs <- prepare_tomstats(
    effects = effects, reh = reh,
    attr_actors = attr_actors, attr_dyads = attr_dyads,
    memory = memory, memory_value = memory_value,
    start = start, stop = stop, method = method
  )

  form <- inputs$form
  effects <- inputs$effects
  effectNames <- inputs$effectNames
  edgelist <- inputs$edgelist
  weights <- inputs$weights
  actors <- inputs$actor
  types <- inputs$types
  riskset <- inputs$riskset
  risksetMatrix <- inputs$risksetMatrix
  memory <- inputs$memory
  memory_value <- inputs$memory_value
  scaling <- inputs$scaling
  consider_type <- inputs$consider_type
  covar <- inputs$covar
  interactions <- inputs$interactions
  start <- inputs$start
  stop <- inputs$stop
  method <- inputs$method

  # Compute the inertia building block
  if (is.null(adjmat)) {
    if (any(grepl("degree", effectNames)) | any(effectNames %in% c("inertia", "reciprocity", "otp", "itp", "osp", "isp", "sp"))) {
      inertia <- calculate_inertia(
        edgelist, weights, risksetMatrix, memory,
        memory_value, start, stop, display_progress, method
      )
    } else {
      inertia <- matrix()
    }
  }

  # Compute statistics
  statistics <- compute_stats_tie(
    effectNames, edgelist, riskset,
    risksetMatrix, inertia, covar, interactions, memory, memory_value, scaling,
    consider_type, start, stop, attr(reh, "directed"), display_progress, method
  )

  # Add variable names to the statistics dimnames
  statistics <- add_variable_names(
    statistics, effectNames, effects,
    interactions
  )

  # Modify riskset output
  riskset <- modify_riskset(riskset, reh, actors, types)

  # Format output
  class(statistics) <- c("tomstats", "remstats")
  attr(statistics, "model") <- "tie"
  attr(statistics, "formula") <- form
  attr(statistics, "riskset") <- riskset
  attr(statistics, "subset") <- data.frame(start = start + 1, stop = stop + 1)
  attr(statistics, "method") <- method
  if (get_adjmat) {
    attr(statistics, "adjmat") <- adjmat
  }

  # Output
  statistics
}
