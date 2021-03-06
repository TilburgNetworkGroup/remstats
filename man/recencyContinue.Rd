% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effects.R
\name{recencyContinue}
\alias{recencyContinue}
\alias{recency}
\title{recencyContinue}
\usage{
recencyContinue(consider_type = FALSE)
}
\arguments{
\item{consider_type}{logical, indicates whether to compute the recency
separately for each event type (TRUE) or regardless of event types (FALSE, 
default).}
}
\description{
Specifies the statistic for a recency continue effect in the 
\code{effects} argument of \code{\link{tomstats}} or the 
\code{choiceEffects} argument of \code{\link{aomstats}}.
}
\details{
The recencyContinue effect refers to a recency statistic similar to what is 
described in Vu et al. (2017) and Mulder and Leenders (2019). For each 
timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time 
that has past since the dyad was last active + 1).
}
\examples{
effects <- ~ recencyContinue()
tomstats(effects, edgelist = history)
aomstats(choiceEffects = effects, edgelist = history)

}
\seealso{
\code{\link{rrankSend}}, \code{\link{rrankReceive}}, 
\code{\link{recencySendSender}}, \code{\link{recencyReceiveSender}}, 
\code{\link{recencyReceiveSender}} and \code{\link{recencyReceiveReceiver}} 
for other type of recency effects
}
