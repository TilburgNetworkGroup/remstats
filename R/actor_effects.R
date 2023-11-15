#' actor_effects
#'
#' Overview of statistics in the actor-oriented model, see Details.
#'
#' @param step outputs all statistics in the sender activity step (if `step =
#' sender`) or receiver choice step (if `step = receiver`).
#'
#' @details
#' Overview of statistics in the actor-oriented model.
#'
#' A list of available effects and their corresponding statistics for the
#' \emph{sender activity rate} step:
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{send}()}
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{totaldegreeSender}()}
#'  \item \code{\link{recencySendSender}()}
#'  \item \code{\link{recencyReceiveSender}()}
#'  \item \code{\link{psABA}()}
#'  \item \code{\link{psABB}()}
#'  \item \code{\link{psABX}()}
#' }
#'
#' A list of available effects and their corresponding statistics for the
#' \emph{receiver choice} step:
#' \itemize{
#'  \item \code{\link{receive}()}
#'  \item \code{\link{tie}()}
#'  \item \code{\link{same}()}
#'  \item \code{\link{difference}()}
#'  \item \code{\link{average}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeReceiver}()}
#'  \item \code{\link{inertia}()}
#'  \item \code{\link{reciprocity}()}
#'  \item \code{\link{otp}()}
#'  \item \code{\link{itp}()}
#'  \item \code{\link{osp}()}
#'  \item \code{\link{isp}()}
#'  \item \code{\link{rrankSend}()}
#'  \item \code{\link{rrankReceive}()}
#'  \item \code{\link{recencySendReceiver}()}
#'  \item \code{\link{recencyReceiveReceiver}()}
#'  \item \code{\link{recencyContinue}()}
#'  \item \code{\link{psABAB}()}
#'  \item \code{\link{psABBA}()}
#'  \item \code{\link{psABXA}()}
#'  \item \code{\link{psABXB}()}
#'  \item \code{\link{psABAY}()}
#'  \item \code{\link{psABBY}()}
#'  \item \code{\link{psABXY}()}
#' }
#' 
#' @examples 
#' # List of available effects for both the sender and receiver step
#' actor_effects()
#' 
#' # List of available effects for the sender step
#' actor_effects(step = "sender")
#' 
#' # List of available effects for the receiver step
#' actor_effects(step = "receiver")
#' 
#' @return
#' Returns a list of available effects and their corresponding statistics based 
#' on the specified `step` (sender or receiver).
#'
#' @export
actor_effects <- function(step = NULL) {
    # Name all effects
    effects <- list(
        sender = c(
            "baseline",
            "send",
            "indegreeSender",
            "outdegreeSender",
            "totaldegreeSender",
            "recencySendSender",
            "recencyReceiveSender",
            "psABA",
            "psABB",
            "psABX",
            "userStat"
        ),
        receiver = c(
            "receive",
            "same",
            "difference",
            "average",
            "tie",
            "inertia",
            "reciprocity",
            "indegreeReceiver",
            "outdegreeReceiver",
            "totaldegreeReceiver",
            "otp",
            "itp",
            "osp",
            "isp",
            "rrankSend",
            "rrankReceive",
            "recencySendReceiver",
            "recencyReceiveReceiver",
            "recencyContinue",
            "psABBA",
            "psABAB",
            "psABXB",
            "psABXA",
            "psABAY",
            "psABBY",
            "psABXY",
            "userStat"
        )
    )

    if (!is.null(step)) {
        if (step == "sender") {
            return(effects$sender)
        } else if (step == "receiver") {
            return(effects$receiver)
        }
    }

    effects
}
