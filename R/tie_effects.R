#' tie_effects
#' 
#' Overview of statistics in the tie-oriented model, see Details. 
#' 
#' @param directed outputs all statistics in the tie-oriented model for 
#' directed events if true, or all statistics in the tie-oriented model for 
#' undirected events if false.
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
tie_effects <- function(directed = NULL) {
    # Name all effects
    effects <- c(
        "send",
        "receive",
        "tie",
        "same",
        "difference",
        "average",
        "minimum",
        "maximum",
        "event",
        "indegreeSender",
        "indegreeReceiver",
        "outdegreeSender",
        "outdegreeReceiver",
        "totaldegreeSender",
        "totaldegreeReceiver",
        "totaldegreeDyad",
        "degreeDiff",
        "degreeMin",
        "degreeMax",
        "inertia",
        "reciprocity",
        "otp",
        "itp",
        "osp",
        "isp",
        "sp",
        "spUnique",
        #"ccp",
        "psABBA",
        "psABBY",
        "psABXA",
        "psABXB",
        "psABXY",
        "psABAY",
        "psABAB",
        "rrankSend",
        "rrankReceive",
        "recencySendSender",
        "recencySendReceiver",
        "recencyReceiveSender",
        "recencyReceiveReceiver",
        "recencyContinue",
        "FEtype",
        "userStat"
    )

    if (!is.null(directed)) {
        if (directed) {
            # Filter out effects that are not defined for directed events
            directed_effects <- effects[!(effects %in% c(
                "sp", "spUnique", "degreeMin", "degreeMax", "ccp", "degreeDiff"
            ))]
            return(directed_effects)
        } else if (!directed) {
            # Filter out effects that are not defined for directed events
            undirected_effects <- effects[!(effects %in% c(
                "send", "receive", "reciprocity", "indegreeSender",
                "indegreeReceiver", "outdegreeSender", "outdegreeReceiver",
                "totaldegreeSender", "totaldegreeReceiver", "otp", "itp",
                "osp", "isp", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY",
                "rrankSend", "rrankReceive", "recencySendSender",
                "recencySendReceiver", "recencyReceiveSender",
                "recencyReceiveReceiver"
            ))]
            return(undirected_effects)
        }
    }

    effects
}
