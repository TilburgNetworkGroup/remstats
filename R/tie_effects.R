#' tie_effects
#'
#' Overview of statistics in the tie-oriented model, see Details.
#'
#' @param directed outputs all statistics in the tie-oriented model for
#' directed events if true, or all statistics in the tie-oriented model for
#' undirected events if false.
#' @param endogenous outputs all endogenous statistics in the tie-oriented
#' model if true, or all exogenous statistics if false
#'
#' @details
#' Overview of statistics in the tie-oriented model.
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
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeSender}()}
#'  \item \code{\link{totaldegreeReceiver}()}
#'  \item \code{\link{totaldegreeDyad}()}
#'  \item \code{\link{degreeMin}()}
#'  \item \code{\link{degreeMax}()}
#'  \item \code{\link{degreeDiff}()}
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
#'  \item \code{\link{psABAB}()}
#'  \item \code{\link{rrankSend}()}
#'  \item \code{\link{rrankReceive}()}
#'  \item \code{\link{recencySendSender}()}
#'  \item \code{\link{recencySendReceiver}()}
#'  \item \code{\link{recencyReceiveSender}()}
#'  \item \code{\link{recencyReceiveReceiver}()}
#'  \item \code{\link{recencyContinue}()}
#'  \item \code{\link{FEtype}()}
#'  \item \code{\link{userStat}()}
#' }
#'
#' @export
tie_effects <- function(directed = NULL, endogenous = NULL) {
  # Name all effects
  effects <- sort(all_tie_effects())

  # Filter out 'interact'
  effects <- effects[effects != "interact"]

  if (!is.null(directed)) {
    if (directed) {
      # Filter out effects that are not defined for directed events
      effects <- effects[!(effects %in% c(
        "sp", "spUnique", "degreeMin", "degreeMax", "ccp", "degreeDiff"
      ))]
    } else if (!directed) {
      # Filter out effects that are not defined for undirected events
      effects <- effects[!(effects %in% c(
        "send", "receive", "reciprocity", "indegreeSender",
        "indegreeReceiver", "outdegreeSender", "outdegreeReceiver",
        "totaldegreeSender", "totaldegreeReceiver", "otp", "itp",
        "osp", "isp", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY",
        "rrankSend", "rrankReceive", "recencySendSender",
        "recencySendReceiver", "recencyReceiveSender",
        "recencyReceiveReceiver"
      ))]
    }
  }

  if (!is.null(endogenous)) {
    if (endogenous) {
      # Filter out exogenous effects
      effects <- effects[!(effects %in% c(
        "send", "receive", "tie", "same", "difference", "average",
        "minimum", "maximum", "event", "FEtype", "userStat"
      ))]
    } else if (!endogenous) {
      # Filter out endogenous effects
      effects <- effects[!(effects %in% c(
        "indegreeSender", "indegreeReceiver", "outdegreeSender",
        "outdegreeReceiver", "totaldegreeSender", "totaldegreeReceiver", "ccp",
        "totaldegreeDyad", "degreeDiff", "degreeMin", "degreeMax",
        "inertia", "reciprocity", "otp", "itp", "osp", "isp", "sp",
        "spUnique", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY",
        "psABAY", "psABAB", "rrankSend", "rrankReceive",
        "recencySendSender", "recencySendReceiver", "recencyReceiveSender",
        "recencyReceiveReceiver", "recencyContinue"
      ))]
    }
  }


  effects
}
