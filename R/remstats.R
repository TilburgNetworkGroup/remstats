#' remstats
#'
#' Computes statistics for the tie-oriented model (see Butts, 2008) or
#' actor-oriented model (see Stadtfeld & Block, 2017).
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
#' Note that events in the edgelist can be directed or undirected. Some
#' statistics are only defined for either directed or undirected events (see
#' the documentation of the statistics). Note that undirected events are only
#' available for the tie-oriented model.
#'
#' @section Attributes:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attributes} argument in either \code{remstats()} or in the separate
#' effect functions supplied to the \code{..._effects} arguments (e.g., see
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
#' defined globally in the \code{remstats} function and affect the computation
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
#' @param edgelist an object of class \code{"\link[remify]{reh}"}
#' characterizing the relational event history sorted by time with columns
#' `time`, `dyad`, `weight`. Alternatively, an object of class
#' \code{"\link[base]{data.frame}"} or \code{"\link[base]{matrix}"} sorted by
#' time with columns `time`, `actor1`, `actor2` and optionally `type` and
#' `weight`.
#' @param attributes optionally, an object of class
#' \code{"\link[base]{data.frame}"} that contains the exogenous attributes (see
#' Details).
#' @inheritParams remify::reh
#' @param memory The memory to be used. See `Details'.
#' @param memory_value Numeric value indicating the memory parameter. See
#' `Details'.
#' @param start integer value, refers to the index in the edgelist of the first
#' event for which statistics are requested (see 'Details')
#' @param stop integer value, refers to the index in the edgelist of the last
#' event for which statistics are requested (see 'Details')
#' @param adjmat optionally, a previously computed adjacency matrix with on the
#' rows the time points and on the columns the risk set entries
#' @param output indicates which output objects need to be provided, i.e.,
#' either only the statistics matrix ("stats_only", faster!) or all the below
#' defined information objects ("all", default).
#'
#' @return \code{statistics } In case of the tie-oriented model, an array with
#' the computed statistics, where rows refer to time points, columns refer to
#' potential relational event (i.e., potential edges) in the risk set and
#' slices refer to statistics. In case of the actor-oriented model, list with
#' in the first element the statistics for the sender activity rate step and in
#' the second element the statistics for the receiver choice step, where rows
#' refer to time points, columns refer to potential senders or recievers,
#' respectively.
#' @return \code{evls } In case of the tie-oriented model, matrix with the
#' edgelist, processed such that it can be used to estimate a relational event
#' model with \code{"\link[relevent]{rem}"}
#' @return \code{edgelist } Dataframe with the edgelist
#' @return \code{adjmat } Matrix with the adjacency matrix, rows refer to
#' timepoints and columns to riskset entries
#'
#' @examples
#' library(remstats)
#'
#' eff <- ~ inertia():send("extraversion") + otp()
#' remstats(edgelist = history, tie_effects = eff, attributes = info)
#'
#' seff <- ~ send("extraversion")
#' reff <- ~ receive("agreeableness") + inertia() + otp()
#' remstats(
#'     edgelist = history, sender_effects = seff, receiver_effects = reff,
#'     attributes = info
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
remstats <- function(edgelist, tie_effects = NULL, sender_effects = NULL,
                     receiver_effects = NULL, attributes = NULL, actors = NULL,
                     types = NULL, directed = TRUE, ordinal = FALSE,
                     origin = NULL, omit_dyad = NULL,
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = NA, start = 1, stop = Inf,
                     adjmat = NULL, output = c("all", "stats_only")) {
    if (!is.null(tie_effects) &
        (!is.null(sender_effects) | !is.null(receiver_effects))) {
        stop("Supply effects for the tie-oriented model to tie_effects OR
            effects for the actor-oriented model to sender_effects or
            receiver_effects.")
    }

    if (is.null(tie_effects) &
        (is.null(sender_effects) & is.null(receiver_effects))) {
        stop("Supply effects for the tie-oriented model to tie_effects or
            effects for the actor-oriented model to sender_effects or
            receiver_effects.")
    }

    if (!is.null(tie_effects)) {
        out <- tomstats(
            effects = tie_effects, edgelist = edgelist,
            attributes = attributes, actors = actors, types = types,
            directed = directed, ordinal = ordinal, origin = origin,
            omit_dyad = omit_dyad, memory = memory,
            memory_value = memory_value, start = start,
            stop = stop, adjmat = adjmat, output = output
        )
    }

    if (!is.null(sender_effects) | !is.null(receiver_effects)) {
        if (!directed) {
            stop("Undirected events are not defined for the actor-oriented model.")
        }
        out <- aomstats(
            edgelist = edgelist, sender_effects = sender_effects,
            receiver_effects = receiver_effects,
            attributes = attributes, actors = actors, types = types,
            ordinal = ordinal, origin = origin,
            omit_dyad = omit_dyad, memory = memory,
            memory_value = memory_value, start = start,
            stop = stop
        )
    }

    out
}
