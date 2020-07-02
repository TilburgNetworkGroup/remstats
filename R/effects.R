#' baseline
#' 
#' Specifies the statistic for a baseline effect in the \code{formula} argument 
#' of \code{\link{remstats}}.
#' 
#' @details
#' The baseline effect refers to the baseline tendency to interact: the 
#' log-inverse of the estimated parameter translates to the average number of 
#' observed events per time unit per dyad. The statistic is equal to one for 
#' all dyads in the riskset at all timepoints. 
#' 
#' The baseline effect with type refers to the baseline tendency to interact 
#' for an event type, compared to the reference category. The statistic is 
#' equal to one for all dyads in the riskset with the given event type at all 
#' timepoints and zero for all dyads in the riskset with other event types. 
#' 
#' @param with_type logical value. If set to true, creates dummy variables for 
#' the event types that refer to the baseline tendency to interact within the 
#' respective event type compared to the reference category (first of sorted 
#' event types). Note: with_type can be FALSE even when a distinction 
#' between different event types is made in the risk set but cannot be TRUE if 
#' a distinction between different event type is not made in the riskset. 
#' 
#' @examples
#' data(history)
#' remstats(~ baseline(), edgelist = history)
#'
#' @export 
baseline <- function(with_type = FALSE) {
    list(baseline = list(with_type = with_type))
}

#' send
#' 
#' Specifies the statistic for a send effect in the \code{formula} argument 
#' of \code{\link{remstats}}.
#' 
#' @details
#' A send effect refers to an exogenous actor attribute that affects actor 
#' i's rate of sending events. The statistic at timepoint \emph{t} is equal to 
#' the value of the exogenous attribute for actor \emph{i} at time \emph{t} for 
#' all dyads in the riskset that have actor \emph{i} as sender. A send effect 
#' is only defined for directed relational events. 
#' 
#' @param variables character vector: names of one or more columns in the 
#' \code{covariates} object for which the statistic has to be computed. 
#' @param covariates an object of class \code{"\link[base]{data.frame}"} that 
#' contains the exogenous covariates. Each row should refer to one actor. The 
#' first column should contain the actor id (corresponding to the actor id's in 
#' the edgelist). The second column refers to the time when covariates change 
#' (set to zero if none of the covariates vary over time). Subsequent columns 
#' contain the covariates that are called in the specifications of exogenous 
#' statistics. 
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ send(c("extraversion", "agreeableness"), info), 
#'  edgelist = history)
#' 
#' @export 
send <- function(variables, covariates) {
    out <- prepExoVar("send", variables, covariates)
    out
}

#' receive
#' 
#' Specifies the statistic for a receive effect in the \code{formula} argument 
#' of \code{\link{remstats}}.
#' 
#' @details
#' A receive effect refers to an exogenous actor attribute that affects actor 
#' i's rate of receiving events. The statistic at timepoint \emph{t} is equal 
#' to the value of the exogenous attribute for actor \emph{i} at time \emph{t} 
#' for all dyads in the riskset that have actor \emph{i} as receiver. A 
#' receive effect is only defined for directed relational events.
#' 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ receive(c("extraversion", "agreeableness"), info), 
#'  edgelist = history)
#' 
#' @export 
receive <- function(variables, covariates) {
    out <- prepExoVar("receive", variables, covariates)
    out
}

#' same
#' 
#' Specifies the statistic for a same effect in the \code{formula} argument 
#' of \code{\link{remstats}}.
#' 
#' @details 
#' A same effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on whether they have the same value 
#' (or not) on this attribute. The statistic at timepoint \emph{t} is equal to 
#' one for dyads \emph{(i,j)} that have the same value on the attribute at 
#' timepoint \emph{t} and equal to 0 for dyads that do not have the same value. 
#' 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ same(c("age", "sex"), info), edgelist = history)
#' 
#' @export 
same <- function(variables, covariates) {
    out <- prepExoVar("same", variables, covariates)
    out
}

#' difference
#' 
#' Specifies the statistic for a difference effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details 
#' A difference effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the difference between their 
#' values on this attribute. The statistic at timepoint \emph{t} for dyad 
#' \emph{(i,j)} is equal to the absolute difference between the values of actor 
#' \emph{i} and \emph{j} on the attribute at timepoint \emph{t}.  
#' 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ difference(c("extraversion", "agreeableness"), info), 
#'  edgelist = history)
#' 
#' @export 
difference <- function(variables, covariates) {
    out <- prepExoVar("difference", variables, covariates)
    out
}

#' average
#' 
#' Specifies the statistic for an average effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details 
#' An average effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the average of their values on 
#' this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the average of the values of actor \emph{i} and \emph{j} on the 
#' attribute at timepoint \emph{t}.  
#' 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ average(c("extraversion", "agreeableness"), info), 
#'  edgelist = history)
#' 
#' @export 
average <- function(variables, covariates) {
    out <- prepExoVar("average", variables, covariates)
    out
}

#' minimum
#' 
#' Specifies the statistic for a minimum effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details 
#' A minimum effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the minimum of their values on 
#' this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the minimum of the values of actor \emph{i} and \emph{j} on the 
#' attribute at timepoint \emph{t}.   
#' 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ minimum(c("extraversion", "agreeableness"), info), 
#'  edgelist = history)
#' 
#' @export 
minimum <- function(variables, covariates) {
    out <- prepExoVar("minimum", variables, covariates)
    out
}

#' maximum
#' 
#' Specifies the statistic for a maximum effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details 
#' A maximum effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the maximum of their values on 
#' this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the maximum of the values of actor \emph{i} and \emph{j} on the 
#' attribute at timepoint \emph{t}.  
#' 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ maximum(c("extraversion", "agreeableness"), info), 
#'  edgelist = history)
#' 
#' @export 
maximum <- function(variables, covariates) {
    out <- prepExoVar("maximum", variables, covariates)
    out
}

#' equate
#' 
#' Specifies the statistic for an `equate_to`-effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details 
#' An `equate_to`-effect refers to an exogenous actor attribute that affects 
#' dyad \emph{(i,j)}'s rate of interacting based on whether both their values 
#' are equal to a specific value on this attribute. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to one if the values of actor 
#' \emph{i} and \emph{j} on the attribute at timepoint \emph{t} are both equal 
#' to the specified value and equal to zero if not. 
#' 
#' @param equal_val numeric vector. 
#' @inheritParams send
#' 
#' @examples 
#' data(history)
#' data(info)
#' remstats(~ equate(c("age", "age"), c(0,1), info), 
#'  edgelist = history)
#' 
#' @export 
equate <- function(variables, equal_val, covariates) {
    out <- prepExoVar("equate", variables, covariates)
    out <- lapply(X = 1:length(out), function(X) {
	    list(x = out[[X]]$x, equal_val = equal_val[X])
    })
    names(out) <- rep("equate", length(out))
    out
}

#' event
#' 
#' Specifies the statistic for an event effect in the \code{formula} argument 
#' of \code{\link{remstats}}.
#' 
#' @details 
#' An event effect refers to an exogenous event attribute that affects the rate 
#' of interacting in events higher or lower on this attribute. The statistic at 
#' timepoint \emph{t} is for all dyads in the riskset equal to the attribute of 
#' the event at timepoint \emph{t}. 
#' 
#' @param x numeric vector with the attribute of the events. The length of this 
#' vector should be equal to the number of events in the edgelist
#' 
#' @examples 
#' data(history)
#' remstats(~ event(history$setting), edgelist = history)
#' 
#' @export 
event <- function(x) {

    # Preprocess the information 
    var <- as.matrix(x)

    if(class(var[1])=="character") {
			var <- factor(var, levels = var, labels = match(var, unique(var)))
			var <- as.numeric(var)
			var <- match(var, unique(var))-1
		}

    # Collect the information in a dataframe
    dat <- data.frame(x = var)

		# Set the column name equal to the variable name
		if(!is.null(names(x))) {
        colnames(dat) <- names(x)
    } 

    # Give the dataframe an effect attribute with the effect type
		attributes(dat)$effect <- "event"

    # Output
    list(event = list(
        x = dat
    ))
}

#' inertia
#' 
#' Specifies the statistic for an inertia effect in the \code{formula} argument 
#' of \code{\link{remstats}}.
#' 
#' @details 
#' An inertia effect refers to the tendency for actors to repeat past 
#' interactions. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the number of \emph{(i,j)} events before timepoint \emph{t}. 
#' Optionally, a scaling method can be set with \code{scaling}, events that 
#' happened a certain number of time units ago can be disregarded in the count 
#' by setting \code{memory_value}, events of different types can be counted 
#' separately by setting \code{with_type = TRUE} or past events can be weighted 
#' by setting \code{event_weights}. 
#' 
#' @param scaling the method for scaling the inertia statistic. Options are one 
#' of \code{"counts"} (default option, gives the raw counts of past (i,j) 
#' events at time t), \code{"outdegreeSender"} (in which raw counts are 
#' divided by the outdegree of the sender at time t, only available for 
#' directed events), and \code{"standardize"} (in which raw counts are 
#' standardized per time point).
#' @param memory_value numeric value. Specifies the time after which events are 
#' no longer included in the statistic count (default: all past events are 
#' considered). Note: make sure memory_value is specified in the same time unit 
#' as the time for the events in the edgelist. 
#' @param with_type logical value. If TRUE, past (i,j) events of different 
#' types c are counted separately for every (i,j,c) event in the risk set. If 
#' FALSE (default), the type of the events is not considered in the counting of 
#' past (i,j) events. Note: with_type can be FALSE even when a distinction 
#' between different event types is made in the risk set but cannot be TRUE if 
#' a distinction between different event type is not made in the riskset. 
#' @param event_weights vector with numeric values that indicate the intensity 
#' of the events in the edgelist. If event weights are supplied, the inertia 
#' statistic is equal to the sum of the intensity of past (i,j) events.
#' 
#' @examples 
#' data(history)
#' remstats(~ inertia(), edgelist = history)
#'
#' @export 
inertia <- function(scaling = c("counts", "outdegreeSender", "standardize"), 
    memory_value = Inf, with_type = FALSE, event_weights = NULL) {

    out <- prepEndoVar("inertia", scaling, memory_value, with_type, 
        event_weights)
    out$inertia$scaling <- match(out$inertia$scaling, 
        c("counts", "outdegreeSender", "standardize"))
    out
}

#' reciprocity
#' 
#' Specifies the statistic for a reciprocity effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details 
#' A reciprocity effect refers to the tendency for actors to reciprocate past 
#' interactions. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the number of \emph{(j,i)} events before timepoint \emph{t}. 
#' Optionally, a scaling method can be set with \code{scaling}, events that 
#' happened a certain number of time units ago can be disregarded in the count 
#' by setting \code{memory_value}, events of different types can be counted 
#' separately by setting \code{with_type = TRUE} or past events can be weighted 
#' by setting \code{event_weights}.
#' 
#' @param scaling the method for scaling the reciprocity statistic. Options are 
#' one of \code{"counts"} (default option, gives the raw counts of past (j,i) 
#' events at time t), \code{"indegreeSender"} (in which raw counts are 
#' divided by the indegree of the sender at time t), and \code{"standardize"} 
#' (in which raw counts are standardized per time point).
#' @param with_type logical value. If TRUE, past (j,i) events of different 
#' types c are counted separately for every (i,j,c) event in the risk set. If 
#' FALSE (default), the type of the events is not considered in the counting of 
#' past (j,i) events. Note: with_type can be FALSE even when a distinction 
#' between different event types is made in the risk set. 
#' @param event_weights vector with numeric values that indicate the intensity 
#' of the events in the edgelist. If event weights are supplied, the 
#' reciprocity statistic is equal to the sum of the intensity of past (j,i)
#' events.
#' @inheritParams inertia
#' 
#' @examples 
#' data(history)
#' remstats(~ reciprocity(), edgelist = history)
#'
#' @export 
reciprocity <- function(scaling = c("counts", "indegreeSender", 
    "standardize"), memory_value = Inf, with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("reciprocity", scaling, memory_value, with_type, event_weights)
    out$reciprocity$scaling <- match(out$reciprocity$scaling, 
        c("counts", "indegreeSender", "standardize"))
    out
}

#' indegreeSender
#' 
#' Specifies the statistic for an indegree of the sender effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details 
#' An indegree of the sender effect refers to the tendency for actors to send 
#' events if they have received more past events. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to the number of events received by 
#' actor \emph{i} before timepoint \emph{t}. Optionally, a scaling method can 
#' be set with \code{scaling}, events that happened a certain number of time 
#' units ago can be disregarded in the count by setting \code{memory_value}, 
#' events of different types can be counted separately by setting 
#' \code{with_type = TRUE} or past events can be weighted by setting 
#' \code{event_weights}. 
#' 
#' @param scaling the method for scaling the degree statistic. Options are one 
#' of \code{"counts"} (default option, gives the raw degree counts at time t), 
#' \code{"total"} (in which raw degree counts are divided by the number of past 
#' events at time t), and \code{"standardize"} (in which raw degree counts are 
#' standardized per time point).
#' @param memory_value numeric value. Specifies the time after which events are 
#' no longer included in the degree count (default: all past events are 
#' considered). Note: make sure memory_value is specified in the same time unit 
#' as the time for the events in the edgelist. 
#' @param with_type logical value. If TRUE, past events of different types c 
#' are counted separately for every (i,j,c) event in the risk set. If FALSE 
#' (default), the type of the events is not considered in the degree counts. 
#' Note: with_type can be FALSE even when a distinction between different event 
#' types is made in the risk set. 
#' @param event_weights vector with numeric values that indicate the intensity 
#' of the events in the edgelist. If event weights are supplied, the degree 
#' statistic is equal to the sum of the intensity of past events instead of the 
#' number of past events. 
#' 
#' @aliases degree indegree
#' @seealso \code{\link{indegreeReceiver}}, \code{\link{outdegreeSender}}, 
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' data(history)
#' remstats(~ indegreeSender(), edgelist = history)
#'
#' @export
indegreeSender <- function(scaling = c("counts", "total", 
    "standardize"), memory_value = Inf, with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("indegreeSender", scaling, memory_value, with_type, 
        event_weights)
    out$indegreeSender$scaling <- match(out$indegreeSender$scaling, 
        c("counts", "total", "standardize"))
    out
}

#' indegreeReceiver
#' 
#' Specifies the statistic for an indegree of the receiver effect in the 
#' \code{formula} argument of \code{\link{remstats}}. 
#' 
#' @details 
#' An indegree of the receiver effect refers to the tendency for actors to 
#' receive events if they have received more past events. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of events 
#' received by actor \emph{j} before timepoint \emph{t}. Optionally, a scaling 
#' method can be set with \code{scaling}, events that happened a certain number 
#' of time units ago can be disregarded in the count by setting 
#' \code{memory_value}, events of different types can be counted separately by 
#' setting \code{with_type = TRUE} or past events can be weighted by setting 
#' \code{event_weights}. 
#' 
#' @inheritParams indegreeSender
#' 
#' @seealso \code{\link{indegreeSender}}, \code{\link{outdegreeSender}}, 
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' data(history)
#' remstats(~ indegreeReceiver(), edgelist = history)
#'
#' @export
indegreeReceiver <- function(scaling = c("counts", "total", 
    "standardize"), memory_value = Inf, with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("indegreeReceiver", scaling, memory_value, with_type, 
        event_weights)
    out$indegreeReceiver$scaling <- match(out$indegreeReceiver$scaling, 
        c("counts", "total", "standardize"))
    out
}

#' outdegreeSender
#' 
#' Specifies the statistic for an outdegree of the sender effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details 
#' An outdegree of the sender effect refers to the tendency for actors to send 
#' events if they have send more past events. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to the number of events send by 
#' actor \emph{i} before timepoint \emph{t}. Optionally, a scaling method can 
#' be set with \code{scaling}, events that happened a certain number of time 
#' units ago can be disregarded in the count by setting \code{memory_value}, 
#' events of different types can be counted separately by setting 
#' \code{with_type = TRUE} or past events can be weighted by setting 
#' \code{event_weights}. 
#' 
#' @inheritParams indegreeSender
#' 
#' @aliases outdegree
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}}, 
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' data(history)
#' remstats(~ outdegreeSender(), edgelist = history)
#'
#' @export
outdegreeSender <- function(scaling = c("counts", "total", 
    "standardize"), memory_value = Inf, with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("outdegreeSender", scaling, memory_value, with_type, 
        event_weights)
    out$outdegreeSender$scaling <- match(out$outdegreeSender$scaling, 
        c("counts", "total", "standardize"))
    out
}

#' outdegreeReceiver
#' 
#' Specifies the statistic for an outdegree of the receiver effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details 
#' An outdegree of the sender effect refers to the tendency for actors to 
#' receive events if they have send more past events. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of events 
#' send by actor \emph{j} before timepoint \emph{t}. Optionally, a scaling 
#' method can be set with \code{scaling}, events that happened a certain number 
#' of time units ago can be disregarded in the count by setting 
#' \code{memory_value}, events of different types can be counted separately by 
#' setting \code{with_type = TRUE} or past events can be weighted by setting 
#' \code{event_weights}. 
#' 
#' @inheritParams indegreeSender
#' 
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}}, 
#' \code{\link{outdegreeSender}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' data(history)
#' remstats(~ outdegreeReceiver(), edgelist = history)
#'
#' @export
outdegreeReceiver <- function(scaling = c("counts", "total", 
    "standardize"), memory_value = Inf,with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("outdegreeReceiver", scaling, memory_value, with_type, 
        event_weights)
    out$outdegreeReceiver$scaling <- match(out$outdegreeReceiver$scaling, 
        c("counts", "total", "standardize"))
    out
}

#' totaldegreeSender
#' 
#' Specifies the statistic for a total degree of the sender effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details 
#' A total degree of the sender effect refers to the tendency for actors to 
#' send events if they have send and received more past events. The statistic 
#' at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of events 
#' send and received by actor \emph{i} before timepoint \emph{t}. Optionally, a 
#' scaling method can be set with \code{scaling}, events that happened a 
#' certain number of time units ago can be disregarded in the count by setting 
#' \code{memory_value}, events of different types can be counted separately by 
#' setting \code{with_type = TRUE} or past events can be weighted by setting 
#' \code{event_weights}. 
#' 
#' @param scaling the method for scaling the degree statistic. Options are one 
#' of \code{"counts"} (default option, gives the raw degree counts at time t), 
#' \code{"total"} (in which raw degree counts are divided by the number of past 
#' events at time t times two), and \code{"standardize"} (in which raw degree 
#' counts are standardized per time point).
#' @inheritParams indegreeSender
#' 
#' @aliases totaldegree
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}}, 
#' \code{\link{outdegreeSender}}, \code{\link{outdegreeReceiver}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' data(history)
#' remstats(~ totaldegreeSender(), edgelist = history)
#'
#' @export
totaldegreeSender <- function(scaling = c("counts", "total", 
    "standardize"), memory_value = Inf, with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("totaldegreeSender", scaling, memory_value, with_type, 
        event_weights)
    out$totaldegreeSender$scaling <- match(out$totaldegreeSender$scaling, 
        c("counts", "total", "standardize"))
    out
}

#' totaldegreeReceiver
#' 
#' Specifies the statistic for a total degree of the receiver effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details 
#' A total degree of the receiver effect refers to the tendency for actors to 
#' receive events if they have send and received more past events. The 
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number 
#' of events send and received by actor \emph{j} before timepoint \emph{t}. 
#' Optionally, a scaling method can be set with \code{scaling}, events that 
#' happened a certain number of time units ago can be disregarded in the count 
#' by setting \code{memory_value}, events of different types can be counted 
#' separately by setting \code{with_type = TRUE} or past events can be weighted 
#' by setting \code{event_weights}. 
#' 
#' @inheritParams totaldegreeSender
#' 
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}}, 
#' \code{\link{outdegreeSender}}, \code{\link{outdegreeReceiver}}, or
#' \code{\link{totaldegreeSender}} for other types of degree effects.
#' 
#' @examples 
#' data(history)
#' remstats(~ totaldegreeReceiver(), edgelist = history)
#'
#' @export
totaldegreeReceiver <- function(scaling = c("counts", "total", 
    "standardize"), memory_value = Inf, with_type = FALSE, 
    event_weights = NULL) {

    out <- prepEndoVar("totaldegreeReceiver", scaling, memory_value, with_type, 
        event_weights)
    out$totaldegreeReceiver$scaling <- match(out$totaldegreeReceiver$scaling, 
        c("counts", "total", "standardize"))
    out
}

#' otp
#' 
#' Specifies the statistic for an outgoing two-path effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details 
#' An outgoing two-path effect refers to the tendency of dyads to interact if 
#' they have more past outgoing two-paths between them. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the minimum of past 
#' \emph{(i,h)}, \emph{(h,j)} events, summed over all actors \emph{h}. 
#' Optionally, a scaling method can be set with \code{scaling}, events that 
#' happened a certain number of time units ago can be disregarded in the count 
#' by setting \code{memory_value}, events of different types can be counted 
#' separately by setting \code{with_type = TRUE} or past events can be weighted 
#' by setting \code{event_weights}.
#' 
#' @param scaling the method for scaling the triad statistic. Options are one 
#' of \code{"counts"} (default option, gives the raw triad counts at time t), 
#' or \code{"standardize"} (in which raw triad counts are standardized per time 
#' point).
#' @inheritParams indegreeSender
#' 
#' @aliases triad 
#' @seealso \code{\link{itp}}, \code{\link{osp}}, or \code{\link{isp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' data(history)
#' remstats(~ otp(), edgelist = history)
#' 
#' @export  
otp <- function(scaling = c("counts", "standardize"), 
    memory_value = Inf, with_type = FALSE, event_weights = NULL) {

    out <- prepEndoVar("otp", scaling, memory_value, with_type, event_weights)
    out$otp$scaling <- match(out$otp$scaling, c("counts", "standardize"))
    out
}

#' itp
#' 
#' Specifies the statistic for an incoming two-path effect in the 
#' \code{formula} argument of \code{\link{remstats}}. 
#' 
#' @details
#' An incomping two-path effect refers to the tendency of dyads to interact if 
#' they have more past incoming two-paths between them. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the minimum of past 
#' \emph{(j,h)}, \emph{(h,i)} events, summed over all actors \emph{h}. 
#' Optionally, a scaling method can be set with \code{scaling}, events that 
#' happened a certain number of time units ago can be disregarded in the count 
#' by setting \code{memory_value}, events of different types can be counted 
#' separately by setting \code{with_type = TRUE} or past events can be weighted 
#' by setting \code{event_weights}.
#' 
#' @inheritParams otp
#' 
#' @seealso \code{\link{otp}}, \code{\link{osp}}, or \code{\link{isp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' data(history)
#' remstats(~ itp(), edgelist = history)
#' 
#' @export  
itp <- function(scaling = c("counts", "standardize"), memory_value = Inf, 
    with_type = FALSE, event_weights = NULL) {

    out <- prepEndoVar("itp", scaling, memory_value, with_type, event_weights)
    out$itp$scaling <- match(out$itp$scaling, c("counts", "standardize"))
    out
}

#' osp
#' 
#' Specifies the statistic for an outgoing shared partners effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details
#' An outgoing shared partners effect refers to the tendency of dyads to 
#' interact if they have more past outgoing shared partners between them. The 
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the 
#' minimum of past \emph{(i,h)}, \emph{(j,h)} events, summed over all actors 
#' \emph{h}. Optionally, a scaling method can be set with \code{scaling}, 
#' events that happened a certain number of time units ago can be disregarded 
#' in the count by setting \code{memory_value}, events of different types can 
#' be counted separately by setting \code{with_type = TRUE} or past events can 
#' be weighted by setting \code{event_weights}.
#' 
#' @inheritParams otp
#' 
#' @seealso \code{\link{otp}}, \code{\link{itp}}, or \code{\link{isp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' data(history)
#' remstats(~ osp(), edgelist = history)
#' 
#' @export  
osp <- function(scaling = c("counts", "standardize"), memory_value = Inf, 
    with_type = FALSE, event_weights = NULL) {

    out <- prepEndoVar("osp", scaling, memory_value, with_type, event_weights)
    out$osp$scaling <- match(out$osp$scaling, c("counts", "standardize"))
    out
}

#' isp
#' 
#' Specifies the statistic for an incoming shared partners effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details
#' An incoming shared partners effect refers to the tendency of dyads to 
#' interact if they have more past incoming shared partners between them. The 
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the 
#' minimum of past \emph{(h,i)}, \emph{(h,j)} events, summed over all actors 
#' \emph{h}. Optionally, a scaling method can be set with \code{scaling}, 
#' events that happened a certain number of time units ago can be disregarded 
#' in the count by setting \code{memory_value}, events of different types can 
#' be counted separately by setting \code{with_type = TRUE} or past events can 
#' be weighted by setting \code{event_weights}.
#' 
#' @inheritParams otp
#' 
#' @seealso \code{\link{otp}}, \code{\link{itp}}, or \code{\link{osp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' data(history)
#' remstats(~ isp(), edgelist = history)
#' 
#' @export  
isp <- function(scaling = c("counts", "standardize"), memory_value = Inf, 
    with_type = FALSE, event_weights = NULL) {

    out <- prepEndoVar("isp", scaling, memory_value, with_type, event_weights)
    out$isp$scaling <- match(out$isp$scaling, c("counts", "standardize"))
    out
}

#' sp
#' 
#' Specifies the statistic for a shared partners effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' A shared partners effect refers to the tendency of dyads to interact if they 
#' have more past shared partners between them. The statistic is defined for 
#' undirected relational events. The statistic at timepoint \emph{t} for dyad 
#' \emph{(i,j)} is equal to the minimum of past undirected \emph{(i,h)}, 
#' \emph{(j,h)} events, summed over all actors \emph{h}. Optionally, a scaling 
#' method can be set with \code{scaling}, events that happened a certain number 
#' of time units ago can be disregarded in the count by setting 
#' \code{memory_value}, events of different types can be counted separately by 
#' setting \code{with_type = TRUE} or past events can be weighted by setting 
#' \code{event_weights}.
#' 
#' @inheritParams otp
#' 
#' @seealso \code{\link{spUnique}} for another type of triadic effect for 
#' undirected relational events and \code{\link{otp}}, \code{\link{itp}}, 
#' \code{\link{osp}}, or \code{\link{isp}} for triadic effects for directed 
#' relational events.
#' 
#' @examples 
#' data(history)
#' remstats(~ sp(), edgelist = history, directed = FALSE)
#' 
#' @export  
sp <- function(scaling = c("counts", "standardize"), memory_value = Inf, 
    with_type = FALSE, event_weights = NULL) {

    out <- prepEndoVar("sp", scaling, memory_value, with_type, event_weights)
    out$sp$scaling <- match(out$sp$scaling, c("counts", "standardize"))
    out
}

#' spUnique
#' 
#' Specifies the statistic for a unique shared partners effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' @details
#' A unique shared partners effect refers to the tendency of dyads to interact 
#' if they have more past unique shared partners between them. The statistic is 
#' defined for undirected relational events. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to the number of unique actors 
#' \emph{h} that both actors \emph{i} and \emph{j} interacted with in the past. 
#' Optionally, a scaling method can be set with \code{scaling}, events that 
#' happened a certain number of time units ago can be disregarded in the count 
#' by setting \code{memory_value} or events of different types can be counted 
#' separately by setting \code{with_type = TRUE}.
#' 
#' @inheritParams otp
#' 
#' @seealso \code{\link{sp}} for another type of triadic effect for 
#' undirected relational events and \code{\link{otp}}, \code{\link{itp}}, 
#' \code{\link{osp}}, or \code{\link{isp}} for triadic effects for directed 
#' relational events.
#' 
#' @examples 
#' data(history)
#' remstats(~ spUnique(), edgelist = history, directed = FALSE)
#' 
#' @export  
spUnique <- function(scaling = c("counts", "standardize"), memory_value = Inf, 
    with_type = FALSE) {

    out <- prepEndoVar("spUnique", scaling, memory_value, with_type, NULL)
    out$spUnique$scaling <- match(out$spUnique$scaling, 
        c("counts", "standardize"))
    out
}

#' psABBA
#' 
#' Specifies the statistic for a pshift AB-BA effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' The AB-BA pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-BA pshift refers to the tendency for immediate 
#' reciprocation (the next sender is the current receiver and the next receiver 
#' is the current sender). For each timepoint t, the psABBA statistic is equal 
#' to one for the dyad that will create the participation shift if it would 
#' occur in the edgelist at time t and equal to zero for the dyads that will 
#' not create this participation shift.
#' 
#' @param with_type logical value. If TRUE, the pshift is set only for those
#' dyads in the riskset with the same event type as the last event. If FALSE 
#' (default), pshifts are set regardless of the event types of the dyads in 
#' the riskset. 
#' 
#' @aliases pshift 
#' @seealso \code{\link{psABBY}}, \code{\link{psABXA}}, \code{\link{psABXB}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' data(history)
#' remstats(~ psABBA(), edgelist = history)
#' 
#' @export
psABBA <- function(with_type = FALSE) {

    list(
        "psABBA" = list(with_type = with_type)
    )
}

#' psABBY
#' 
#' Specifies the statistic for a pshift AB-BY effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' The AB-BY pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-BY pshift refers to a tendency for turn 
#' receiving (here, the next sender is the current receiver and the next 
#' receiver is not in the current event). For each timepoint t, the psABBY 
#' statistic is equal to one for the dyads that will create the participation 
#' shift if they would occur in the edgelist at time t and equal to zero for 
#' the dyads that will not create this participation shift.
#' 
#' @inheritParams psABBA
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABXA}}, \code{\link{psABXB}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' data(history)
#' remstats(~ psABBY(), edgelist = history)
#' 
#' @export
psABBY <- function(with_type = FALSE) {

    list(
        "psABBY" = list(with_type = with_type)
    )
}

#' psABXA
#' 
#' Specifies the statistic for a pshift AB-XA effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' The AB-XA pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-XA pshift refers to a tendency for turn 
#' usurping (here, the next sender is not in the current event and the next 
#' receiver is the current sender). For each timepoint t, the psABXA statistic 
#' is equal to one for the dyads that will create the participation shift if 
#' they would occur in the edgelist at time t and equal to zero for the dyads 
#' that will not create this participation shift.
#' 
#' @inheritParams psABBA
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXB}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' data(history)
#' remstats(~ psABBA(), edgelist = history)
#' 
#' @export
psABXA <- function(with_type = FALSE) {

    list(
        "psABXA" = list(with_type = with_type)
    )
}

#' psABXB
#' 
#' Specifies the statistic for a pshift AB-XB effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' The AB-XB pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-XB pshift refers to a tendency for turn 
#' usurping (here, the next sender is not in the current event and the next 
#' receiver is the current receiver). For each timepoint t, the psABXB 
#' statistic is equal to one for the dyads that will create the participation 
#' shift if they would occur in the edgelist at time t and equal to zero for 
#' the dyads that will not create this participation shift.
#' 
#' @inheritParams psABBA
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' data(history)
#' remstats(~ psABXB(), edgelist = history)
#' 
#' @export
psABXB <- function(with_type = FALSE) {

    list(
        "psABXB" = list(with_type = with_type)
    )
}

#' psABXY
#' 
#' Specifies the statistic for a pshift AB-XY effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' The AB-XY pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-XY pshift refers to a tendency for turn 
#' usurping (here, the next sender and the next receiver are not in the current 
#' event). For each timepoint t, the psABXY statistic is equal to one for the 
#' dyads that will create the participation shift if they would occur in the 
#' edgelist at time t and equal to zero for the dyads that will not create this 
#' participation shift.
#' 
#' @inheritParams psABBA
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}}, 
#' \code{\link{psABXB}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' data(history)
#' remstats(~ psABXY(), edgelist = history)
#' 
#' @export
psABXY <- function(with_type = FALSE) {

    list(
        "psABXY" = list(with_type = with_type)
    )
}

#' psABAY
#' 
#' Specifies the statistic for a pshift AB-AY effect in the \code{formula} 
#' argument of \code{\link{remstats}}.
#' 
#' @details
#' The AB-AY pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-AY pshift refers to a tendency for turn 
#' continuing (here, the next sender is the current sender and the next 
#' receiver is not in the current event). For each timepoint t, the psABAY 
#' statistic is equal to one for the dyads that will create the participation 
#' shift if they would occur in the edgelist at time t and equal to zero for 
#' the dyads that will not create this participation shift.
#' 
#' @inheritParams psABBA
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}}, 
#' \code{\link{psABXB}} or \code{\link{psABXY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' data(history)
#' remstats(~ psABAY(), edgelist = history)
#' 
#' @export
psABAY <- function(with_type = FALSE) {

    list(
        "psABAY" = list(with_type = with_type)
    )
}

#' rrankSend
#' 
#' Specifies the statistic for a recency rank send effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' The rrankSend effect refers to a rank-based recency effect, as in section 2.
#' 2.5 of Butts (2008). For each timepoint t, for directed dyad (i,j) the 
#' statistic is equal to the inverse of the rank of receiver j among the actors 
#' to which sender i has most recently send past events.
#' 
#' @param with_type logical value. If TRUE, ranks are created taking event 
#' types into account. If FALSE (default), ranks are created regardless of the 
#' event types of the dyads in the riskset.
#' 
#' @aliases recency recencyRank rrank
#' @seealso \code{\link{rrankReceive}}
#' 
#' @examples 
#' data(history)
#' remstats(~ rrankSend(), edgelist = history)
#'
#' @export 
rrankSend <- function(with_type = FALSE) {

    list(
        rrankSend = list(with_type = with_type)
    )
}

#' rrankReceive
#' 
#' Specifies the statistic for a recency rank receive effect in the 
#' \code{formula} argument of \code{\link{remstats}}.
#' 
#' The rrankSend effect refers to a rank-based recency effect, as in section 2.
#' 2.5 of Butts (2008). For each timepoint t, for directed dyad (i,j) the 
#' statistic is equal to the inverse of the rank of receiver j among the actors 
#' from which sender i has most recently received past events
#' 
#' @inheritParams rrankSend
#' 
#' @seealso \code{\link{rrankSend}}
#' 
#' @examples
#' data(history)
#' remstats(~ rrankReceive(), edgelist = history)
#'
#' @export 
rrankReceive <- function(with_type = FALSE) {

    list(
        rrankReceive = list(with_type = with_type)
    )
}


#' recenySender
#' 
#' Description
#' 
#' Details
#' 
#' @param memory_value numeric value. Specifies the time after which events are 
#' no longer included in the statistic count (default: all past events are 
#' considered). Note: make sure memory_value is specified in the same time unit 
#' as the time for the events in the edgelist. 
#' 
#' seealso
#' 
#' examples
#' 
#' @export 
recenySender <- function(memory_value = NULL) {
	
	list(
		recenySender = list(memory_value = memory_value)
	)
	
}



#' recenyReceiver
#' 
#' Description
#' 
#' Details
#' 
#' @param memory_value numeric value. Specifies the time after which events are 
#' no longer included in the statistic count (default: all past events are 
#' considered). Note: make sure memory_value is specified in the same time unit 
#' as the time for the events in the edgelist. 
#' 
#' seealso
#' 
#' examples
#' 
#' @export 
recenyReceiver <- function(memory_value = NULL) {
	
	list(
		recenyReceiver = list(memory_value = memory_value)
	)
	
}


#' recenyContinue
#' 
#' Description
#' 
#' Details
#' 
#' @param memory_value numeric value. Specifies the time after which events are 
#' no longer included in the statistic count (default: all past events are 
#' considered). Note: make sure memory_value is specified in the same time unit 
#' as the time for the events in the edgelist. 
#' 
#' seealso
#' 
#' examples
#' 
#' @export 
recenyContinue <- function(memory_value = NULL) {
	
	list(
		recenyContinue = list(memory_value = memory_value)
	)
	
}





