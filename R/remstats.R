#' remstats
#' 
#' Computes statistics for the tie-oriented model (see Butts, 2008) or 
#' actor-oriented model (see Stadtfeld & Block, 2017). 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be 
#' supplied to the \code{tie_effects} (for the tie-oriented model), or 
#' \code{sender_effects} and/or \code{receiver_effects} (for the actor-oriented 
#' model) argument in the form \code{~ effects}. The terms are separated by + 
#' operators. For example: \code{effects = ~ inertia() + otp()}. Interactions 
#' between two effects can be included with * or : operators. For example: 
#' \code{effects = ~ inertia():otp()}. A list of available effects and their 
#' corresponding statistics follows at the bottom. 
#' 
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
#' Two more elements can affect the computation of the 
#' \emph{endogenous} statistics: the settings of the \code{memory} and 
#' \code{memoryValue} arguments in \code{remstats} and the events weights in 
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
#' in the \code{remstats} function and affect the computation of all endogenous 
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
#' @inheritParams tomstats
#' @inheritParams aomstats
#' @param tie_effects an object of class \code{"\link[stats]{formula}"} (or one 
#' that can be coerced to that class): a symbolic description of the effects in 
#' the tie-oriented model for which statistics are computed, see 'Details' for 
#' the available effects and their corresponding statistics
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
#' remstats(edgelist = history, sender_effects = seff, receiver_effects = reff, 
#'  attributes = info)
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
    receiver_effects = NULL, attributes = NULL, actors = NULL, types = NULL, 
    directed = TRUE, ordinal = FALSE, origin = NULL, omit_dyad = NULL, 
    memory = "full", memory_value = Inf, start = 1, stop = Inf, adjmat = NULL,
    verbose = FALSE) {

    if(!is.null(tie_effects) & 
        (!is.null(sender_effects) | !is.null(receiver_effects))) {
            stop("Supply effects for the tie-oriented model to tie_effects OR effects for the actor-oriented model to sender_effects or receiver_effects.")
        }

    if(is.null(tie_effects) & 
        (is.null(sender_effects) & is.null(receiver_effects))) {
            stop("Supply effects for the tie-oriented model to tie_effects or effects for the actor-oriented model to sender_effects or receiver_effects.")
        }    

    if(!is.null(tie_effects)) {
        out <- tomstats(effects = tie_effects, edgelist = edgelist, 
            attributes = attributes, actors = actors, types = types, 
            directed = directed, ordinal = ordinal, origin = origin, 
            omit_dyad = omit_dyad, memory = memory, 
            memory_value = memory_value, start = start, 
            stop = stop, adjmat = adjmat)
    } 

    if(!is.null(sender_effects) | !is.null(receiver_effects)) {
        out <- aomstats(edgelist = edgelist, sender_effects = sender_effects, 
            receiver_effects = receiver_effects,
            attributes = attributes, actors = actors, types = types, 
            ordinal = ordinal, origin = origin, 
            omit_dyad = omit_dyad, memory = memory, 
            memory_value = memory_value, start = start, 
            stop = stop, adjmat = adjmat)
    }

    out
}