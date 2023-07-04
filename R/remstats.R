#' remstats
#'
#' Computes statistics for modeling relational events with a tie-oriented
#' approach (see Butts, 2008) or actor-oriented approach (see Stadtfeld &
#' Block, 2017).
#'
#' @section Effects:
#' The statistics to be computed are defined symbolically and should be
#' supplied to the \code{tie_effects} (for the tie-oriented model), or
#' \code{sender_effects} and/or \code{receiver_effects} (for the actor-oriented
#' model) argument in the form \code{~ effects}. The terms are separated by +
#' operators. For example: \code{effects = ~ inertia() + otp()}. Interactions
#' between two effects can be included with * or : operators. For example:
#' \code{effects = ~ inertia():otp()}. A list of available effects
#' can be obtained with \code{\link{tie_effects}()} and
#' \code{\link{actor_effects}()}.
#'
#' The majority of the statistics can be scaled in some way, see
#' the documentation of the \code{scaling} argument in the separate effect
#' functions for more information on this.
#'
#' The majority of the statistics can account for the event type
#' included as a dependent variable, see the documentation of the
#' \code{consider_type} argument in the separate effect functions for more
#' information on this. Note that this option is only available for the
#' tie-oriented model.
#'
#' Note that events in the relational event history can be directed or
#' undirected. Some statistics are only defined for either directed or
#' undirected events (see the documentation of the statistics). Note that
#' undirected events are only available for the tie-oriented model.
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
#' @section Adjacency matrix:
#' Optionally, a previously computed adjacency matrix can be supplied. Note
#' that the endogenous statistics will be computed based on this adjacency
#' matrix. Hence, supplying a previously computed adjacency matrix can reduce
#' computation time but the user should be absolutely sure the adjacency matrix
#' is accurate.
#'
#' @param tie_effects an object of class \code{"\link[stats]{formula}"} (or one
#' that can be coerced to that class): a symbolic description of the effects in
#' the tie-oriented model for which statistics are computed, see 'Details' for
#' the available effects and their corresponding statistics
#' @param sender_effects an object of class \code{"\link[stats]{formula}"} (or
#' one that can be coerced to that class): a symbolic description of the
#' effects in the sender activity rate step of the actor-oriented model for
#' which statistics are computed, see `Details'
#' @param receiver_effects an object of class \code{"\link[stats]{formula}"}
#' (or one that can be coerced to that class): a symbolic description of the
#' effects in the receiver choice step of model for which statistics are
#' computed, see `Details'
#' @param reh an object of class \code{"\link[remify]{remify}"} characterizing 
#' the relational event history.
#' @param attr_data optionally, an object of class
#' \code{"\link[base]{data.frame}"} that contains the exogenous attributes (see
#' Details).
#' @param memory The memory to be used. See `Details'.
#' @param memory_value Numeric value indicating the memory parameter. See
#' `Details'.
#' @param start an optional integer value, specifying the index of the first
#' event in the relational event history for which statistics must be computed
#' (see 'Details')
#' @param stop an optional integer value, specifying the index of the last
#' event in the relational event history for which statistics muts be computed
#' (see 'Details')
#' @param adjmat optionally, for a tie-oriented model a previously computed 
#' adjacency matrix with on the rows the time points and on the columns the 
#' risk set entries
#' @param get_adjmat for a tie-oriented model, whether the adjmat computed by 
#' remstats should be outputted as an attribute of the statistics.
#' @param attributes deprecated, please use "attr_data" instead
#' @param edgelist deprecated, please use "reh" instead
#'
#' @return \code{statistics } In case of the tie-oriented model, an array with
#' the computed statistics, where rows refer to time points, columns refer to
#' potential relational event (i.e., potential edges) in the risk set and
#' slices refer to statistics. In case of the actor-oriented model, list with
#' in the first element the statistics for the sender activity rate step and in
#' the second element the statistics for the receiver choice step, where rows
#' refer to time points, columns refer to potential senders or receivers,
#' respectively.
#'
#' @examples
#' library(remstats)
#'
#' eff <- ~ inertia():send("extraversion") + otp()
#' reh_tie <- remify::remify(edgelist = history, model = "tie")
#' remstats(reh = reh_tie, tie_effects = eff, attr_data = info)
#'
#' seff <- ~ send("extraversion")
#' reff <- ~ receive("agreeableness") + inertia() + otp()
#' reh_actor <- remify::remify(edgelist = history, model = "actor")
#' remstats(
#'     reh = reh_actor, sender_effects = seff, receiver_effects = reff,
#'     attr_data = info
#' )
#'
#' @references Butts, C. T. (2008). A relational event framework for social
#' action. Sociological Methodology, 38(1), 155–200.
#' \url{https://doi.org/10.1111/j.1467-9531.2008.00203.x},
#' Stadtfeld, C., & Block, P. (2017). Interactions, actors, and
#' time: Dynamic network actor models for relational events. Sociological
#' Science, 4, 318–352. \url{https://doi.org/10.15195/v4.a14}
#'
#' @export
remstats <- function(reh, tie_effects = NULL, sender_effects = NULL,
                     receiver_effects = NULL, attr_data = NULL, 
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = NA, start = 1, stop = Inf,
                     adjmat = NULL, get_adjmat = FALSE,
                     attributes, edgelist) {
    if (!is.null(tie_effects) &
        (!is.null(sender_effects) | !is.null(receiver_effects))) {
        stop("Either provide effects for the tie-oriented model using `tie_effects` or effects for the actor-oriented model using `sender_effects` or `receiver_effects`, but not both.")
    }

    if (is.null(tie_effects) &
        (is.null(sender_effects) & is.null(receiver_effects))) {
        stop("Either provide effects for the tie-oriented model using `tie_effects` or effects for the actor-oriented model using `sender_effects` or `receiver_effects`.")
    }

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

    if (!is.null(tie_effects)) {
        out <- tomstats(
            effects = tie_effects, reh = reh,
            attr_data = attr_data, memory = memory,
            memory_value = memory_value, start = start,
            stop = stop, adjmat = adjmat, get_adjmat = get_adjmat
        )
    }

    if (!is.null(sender_effects) | !is.null(receiver_effects)) {
        if (!attr(reh, "directed")) {
            stop("Undirected events are not defined for the actor-oriented model.")
        }
        out <- aomstats(
            reh = reh, sender_effects = sender_effects,
            receiver_effects = receiver_effects,
            attr_data = attr_data, memory = memory,
            memory_value = memory_value, start = start,
            stop = stop
        )
    }

    out
}
