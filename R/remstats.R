#' remstats
#' 
#' Computes statistics for the tie-oriented model (see Butts, 2008) or 
#' actor-oriented model (see Stadtfeld & Block, 2017). 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be 
#' supplied to the \code{tie_effects}, \code{sender_effects} and/or 
#' \code{receiver_effects} argument in the form \code{~ effects}. The 
#' terms are separated by + operators. Interactions between two effects can be 
#' included with * or : operators. 
#' 
#' A list of available effects and their corresponding statistics follows: 
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
#'  \item \code{\link{event}()}
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
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a 
#' dataframe with attributes can be defined in the separate effect functions 
#' supplied to the \code{effects} argument.
#' 
#' Optionally, statistics can be computed for a slice of the edgelist - but 
#' based on the entire history. This is achieved by setting the start and 
#' stop values equal to the index of the first and last event for which 
#' statistics are requested. For example, start = 5 and stop = 5 computes the 
#' statistics for only the 5th event in the edgelist, based on the history that 
#' consists of events 1-4. 
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
#' @export
remstats <- function(edgelist, tie_effects = NULL, sender_effects = NULL,   
    receiver_effects = NULL, attributes = NULL, actors = NULL, types = NULL, 
    directed = TRUE, ordinal = FALSE, origin = NULL, omit_dyad = NULL, 
    memory = "full", memory_value = Inf, start = 1, stop = Inf, adjmat = NULL) {

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