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
#' }
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
            "recencyReceiveSender"
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
            "recencyContinue"
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
