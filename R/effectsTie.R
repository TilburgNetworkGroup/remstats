#' Effects for the tie-oriented REM 
#' 
#' This page lists all the effects that can be specified in remstats for a 
#' tie-oriented relational event model (Butts, 2008). This page gives a global 
#' description of the effects, for more specific information see the help pages 
#' of the respective effects. 
#' 
#' @usage 
#' # Specification of an endogenous effect
#' endo(scaling = c("as.is", "std", "prop"), consider_type = FALSE, param = NULL)
#' 
#' # Specification of an exogenous effect
#' exo(variable, attributes = NULL, scaling = c("as.is", "std"), param = NULL)
#'
#' @param variable for exogenous effects, string with the name of the column in 
#' the \code{attributes} data.frame for which the statistic has to be computed.
#' @param attributes for exogenous effects, optionally, an object of class 
#' \code{"\link[base]{data.frame}"} that contains the exogenous attributes, see 
#' 'Details.'
#' @param scaling the method for scaling the statistic. Default is to not scale 
#' the statistic but keep it "as.is". Alternatively, for the majority of the 
#' effects, standardization of the  statistic per time point can be requested 
#' with "std". A third option for some of the endogenous effects is to scale 
#' the statistic relative to some other statistic of the network with "prop", 
#' see the help pages of the respective effects for more information. 
#' @param consider_type an option for some of the endogenous effects for the 
#' case when event type is considered in the dependent variable. Logical, 
#' indicates whether to count the number of past events separately for each 
#' event type (TRUE) or sum across different event types (FALSE, default).
#' @param param numeric value or function with time parameter to be used for 
#' generation of relational event histories with \code{\link{remulate}}. 
#' 
#' @details 
#' The statistics to be computed are defined symbolically and should be 
#' supplied to the \code{tie_effects} argument of the \code{remstats} function 
#' in the form \code{~ effects}. The terms are separated by + operators. For 
#' example: \code{tie_effects = ~ inertia() + otp()}. Interactions between two 
#' effects can be included with * or : operators. For example: 
#' \code{effects = ~ inertia():otp()}. A list of available effects and their 
#' corresponding statistics follows at the bottom. 
#' 
#' Note that events in the edgelist can be directed or undirected. Some 
#' statistics are only defined for either directed or undirected events (see 
#' the documentation of the statistics). 
#' 
#' For the computation of the \emph{exogenous} statistics, an attributes object 
#' with the exogenous covariate information has to be supplied to the 
#' \code{attributes} argument in either \code{remstats()} or in the separate 
#' effect functions supplied to the \code{tie_effects} arguments. This 
#' \code{attributes} object should be constructed as follows: A dataframe with 
#' rows refering to the attribute value of actor \emph{i} at 
#' timepoint \emph{t}. An `id` column is required that contains the actor id 
#' (corresponding to the actor id's in the edgelist). A `time` column is 
#' required that contains the time when attributes change (set to zero if none 
#' of the attributes vary over time). Subsequent columns contain the attributes 
#' that are called in the specifications of exogenous statistics (column name 
#' corresponding to the string supplied to the \code{variable} argument in the 
#' effect function). 
#' 
#' The majority of the statistics can account for the event type 
#' included as a dependent variable, see the documentation of the 
#' \code{consider_type} argument in the separate effect functions for more 
#' information on this. 
#' 
#' Note that the default `memory` setting in \code{remstats} is `"full"`, which 
#' implies that at each time point $t$ the entire event history before $t$ is 
#' included in the computation of the statistics. Alternatively, when `memory` 
#' is set to `"window"`, only the past event history within a given time 
#' interval is considered (see Mulders & Leenders, 2019). This length of this 
#' time interval is set by the `memory_value` parameter. For example, when 
#' `memory_value = 100` and `memory = "window"`, at time point $t$ only the 
#' past events that happened at most 100 time units ago are included in the 
#' computation of the statistics. A third option is to set `memory` to 
#' `Brandes`. In this case, the weight of the past event in the computation of 
#' the statistics depend on the elapsed time between $t$ and the past event. 
#' This weight is determined based on an exponential decay function with 
#' half-life parameter `memory_value` (see Brandes et al., 2009). 
#' 
#' Note that if the  edgelist contains a column that is named ``weight'', it is 
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
#' \describe{
#'  \item{\code{\link{baseline}}}{Refers to the baseline tendency for interaction.}
#'  \item{\code{\link{send}()}}{Refers to an exogenous actor attribute that affects actor i's rate of sending events.}
#'  \item{\code{\link{receive}()}}{Refers to an exogenous actor attribute that affects actor i's rate of receiving events.}
#'  \item{\code{\link{same}()}}{Effect on actor pair (i,j)'s' rate of interacting dependent on whether actor i and j have the same value on an exogenous actor attribute.}
#'  \item{\code{\link{difference}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the (absolute) difference between actor i's and actor j's value on an exogenous actor attribute.}
#'  \item{\code{\link{average}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the average of actor i's and actor j's value on an exogenous actor attribute.}
#'  \item{\code{\link{minimum}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the minimum of actor i's and actor j's value on an exogenous actor attribute.}
#'  \item{\code{\link{maximum}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the maximum of actor i's and actor j's value on an exogenous actor attribute.}
#'  \item{\code{\link{tie}()}}{Refers to an exogenous actor pair attribute that affects actor pair (i,j)'s' rate of interacting.}
#'  \item{\code{\link{event}()}}{Refers to an exogenous event attribute that describes actor pair (i,j)'s rate of interacting in an event of this type.'}
#'  \item{\code{\link{FEtype}()}}{Fixed effects for the event type, capture the variation in event rates for different event types in the dependent variable.}
#'  \item{\code{\link{indegreeSender}()}}{Refers to the effect of actor i's previously received events on actor i's rate of sending a new event.}
#'  \item{\code{\link{indegreeReceiver}()}}{Refers to the effect of actor i's previously received events on actor i's rate of receiving a new event.}
#'  \item{\code{\link{outdegreeSender}()}}{Refers to the effect of actor i's previously sent events on actor i's rate of sending a new event.}
#'  \item{\code{\link{outdegreeReceiver}()}}{Refers to the effect of actor i's previously sent events on actor i's rate of receiving a new event.}
#'  \item{\code{\link{totaldegreeSender}()}}{Refers to the effect of actor i's previously sent and received events on actor i's rate of sending a new event.}
#'  \item{\code{\link{totaldegreeReceiver}()}}{Refers to the effect of actor i's previously sent and received events on actor i's rate of receiving a new event.}
#'  \item{\code{\link{totaldegreeDyad}()}}{Refers to the effect of actor pair (i,j)'s previously sent and received events on actor pair (i,j)'s rate of interacting again.}
#'  \item{\code{\link{inertia}()}}{Refers to the effect of actor i's previously sent events to actor j on actor pair (i,j)'s rate of interacting again.}
#'  \item{\code{\link{reciprocity}()}}{Refers to the effect of actor i's previously received events from actor j on actor pair (i,j)'s rate of interacting again.}
#'  \item{\code{\link{otp}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the previous events sent from actor i to actors h who sent events in the past to actor j.}
#'  \item{\code{\link{itp}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the previous events received from actor i from actors h who received events in the past from actor j.}
#'  \item{\code{\link{osp}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the previous events sent from actor i to actors h who received events in the past from actor j.}
#'  \item{\code{\link{isp}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the previous events received from actor i from actors h who sent events in the past to actor j.}
#'  \item{\code{\link{sp}()}}{Effect on actor pair (i,j)'s rate for undirected interactions dependent on the previous events between actor i and actors h who also interacted in the past with actor j.}
#'  \item{\code{\link{spUnique}()}}{Effect on actor pair (i,j)'s rate for undirected interactions dependent on the unique actors with whom both actor i and j interacted in the past.}
#'  \item{\code{\link{psABBA}()}}{Effect on actor pair (i,j)'s rate for interacting if the previous event was from actor j to actor i (i.e., immediate reciprocity).}
#'  \item{\code{\link{psABBY}()}}{Effect on actor pair (i,j)'s rate for interacting if the previous event was from any actor other than i or j to i (i.e., turn receiving).}
#'  \item{\code{\link{psABXA}()}}{Effect on actor pair (i,j)'s rate for interacting if the previous event was to any actor other than i or j from j (i.e., turn usurping).}
#'  \item{\code{\link{psABXB}()}}{Effect on actor pair (i,j)'s rate for interacting if the previous event was from any actor other than i or j to j (i.e., turn usurping).}
#'  \item{\code{\link{psABXY}()}}{Effect on actor pair (i,j)'s rate for interacting if the previous event was from any actor other than i or j to any actor other than i or j (i.e., turn usurping).}
#'  \item{\code{\link{psABAY}()}}{Effect on actor pair (i,j)'s rate for interacting if the previous event was from actor i to any other actor than i or j (i.e., turn continuing).}
#'  \item{\code{\link{rrankSend}()}}{Effect on actor pair (i,j)'s rate for interacting dependent on the rank of actor j among the actors to whom actor i has most recently send events.'}
#'  \item{\code{\link{rrankReceive}()}}{Effect on actor pair (i,j)'s rate for interacting dependent on the rank of actor j among the actors from whom actor j has most recently received events.'}
#'  \item{\code{\link{recencySendSender}()}}{Refers to the effect of the time since actor i has send an event on actor i's rate of sending a new event.'}
#'  \item{\code{\link{recencySendReceiver}()}}{Refers to the effect of the time since actor i has send an event on actor i's rate of receiving a new event.}
#'  \item{\code{\link{recencyReceiveSender}()}}{Refers to the effect of the time since actor i has received an event on actor i's rate of sending a new event.}
#'  \item{\code{\link{recencyReceiveReceiver}()}}{Refers to the effect of the time since actor i has received an event on actor i's rate of receiving a new event.}
#'  \item{\code{\link{recencyContinue}()}}{Effect on actor pair (i,j)'s rate of interacting dependent on the time since the pair last interacted.}
#' }
#' 
#' @name effectsTie
NULL 
