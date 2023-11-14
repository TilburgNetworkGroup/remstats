#' baseline
#'
#' Specifies an intercept for the tie-oriented model or the sender activity
#' rate step of the actor-oriented model in the same manner as in
#' \code{\link[stats]{lm}} (see Details).
#'
#' @details
#' A baseline effect is automatically specified for the tie-oriented model and
#' the sender activity rate step of the actor-oriented model when the
#' \code{ordinal} argument in \code{\link{remstats}}, \code{\link{tomstats}},
#' \code{\link{aomstats}} is set to FALSE (default) and automatically removed
#' when this argument is set to TRUE. Alternatively, a baseline effect can be
#' explicitly specified by adding '1' to the equation or explicitly removed by
#' adding '-1' to the equation.
#'
#' The baseline effect refers to the baseline tendency to interact. In the
#' tie-oriented model, the log-inverse of the estimated parameter translates to
#' the average number of observed events per time unit per dyad. In the
#' actor-oriented model, the log-inverse of the estimated parameter translates
#' to the average number of observed events per time unit per actor. The
#' statistic is equal to one for all dyads resp. actors in the riskset at all
#' timepoints.
#'
#' @name baseline
#' @aliases intercept
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(reh = reh_tie, tie_effects = ~1)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = ~1)
NULL

#' send
#'
#' Specifies the statistic for a "send" effect in the tie-oriented model or the
#' actor activity rate step of the actor-oriented model. A "send" effect refers
#' to an exogenous actor attribute that affects actor \emph{i}'s rate of
#' sending events.
#'
#' @param variable string with the name of the column in the
#' \code{attr_actors} object for which the statistic has to be computed.
#' @param attr_actors optionally, an object of class
#' \code{\link[base]{data.frame}} that contains the attribute, see
#' 'Details.'
#' @param scaling the method for scaling the statistic. Default is to not scale
#' the statistic. Alternatively, standardization of the
#' statistic per time point can be requested with "std".
#' @param attr_data Deprecated argument. Please use 'attr_actors' instead.
#'
#' @details
#' The statistic at timepoint \emph{t} is equal to the value of the exogenous
#' attribute for actor \emph{i} at time \emph{t} for all dyads in the risk set
#' that have actor \emph{i} as sender. Note that a "send" effect is only
#' defined for directed relational events.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "send" effect (i.e., the column name should correspond
#' to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{send()} and, instead, supply it in the call of \code{remstats()} for
#' multiple exogenous effects.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ send("extraversion")
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = effects, attr_actors = info)
#'
#' @export
send <- function(variable, attr_actors = NULL, scaling = c("none", "std"), 
  attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  # Prep
  prep_exo("send", variable, attr_actors, scaling)
}

#' receive
#'
#' Specifies the statistic for a "receive" effect in the tie-oriented model or
#' the receiver choice step of the actor-oriented model. A "receive" effect
#' refers to an exogenous actor attribute that affects actor \emph{i}'s rate of
#' receiving events.
#'
#' @details
#' The statistic at timepoint \emph{t} is equal to the value of the exogenous
#' attribute for actor \emph{i} at time \emph{t} for all dyads in the riskset
#' that have actor \emph{i} as receiver. Note that a "receive" effect is only
#' defined for directed relational events.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "receive" effect (i.e., the column name should
#' correspond to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{receive()} and, instead, supply it in the call of \code{remstats()}
#' for multiple exogenous effects.
#'
#' @inheritParams send
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ receive("extraversion")
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects, attr_actors = info)
#'
#' @export
receive <- function(variable, attr_actors = NULL, scaling = c("none", "std"), attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  # Prep
  prep_exo("receive", variable, attr_actors, scaling)
}

#' tie
#'
#' Specifies the statistic for a "tie" (or, "dyad") effect.
#'
#' @details
#' The "tie" effect or "dyad" effect refers to an exogenous dyad attribute that influences dyad \emph{(i,j)}'s interaction rate (in tie-oriented models) or the probability of actor \emph{j} being chosen as a receiver for the event sent by the active sender \emph{i} (in actor-oriented models). The statistic represents the value of the exogenous attribute for dyad \emph{(i,j)} in the \code{attr_dyads} data.
#'
#' @param variable A string specifying the attribute to compute the statistic. If \code{attr_dyads} is a \code{data.frame}, this refers to the column name in \code{attr_actors}. If \code{attr_dyads} is a \code{matrix}, this corresponds to the name of the exogenous attribute, used to label the statistic in the resulting \code{remstats} object.
#' @param attr_dyads A \code{data.frame} or \code{matrix} containing attribute information for dyads. If \code{attr_dyads} is a \code{data.frame}, the first two columns should represent "actor1" and "actor2" (for directed events, "actor1" corresponds to the sender, and "actor2" corresponds to the receiver). Additional columns can represent dyads' exogenous attributes. If attributes vary over time, include a column named "time". If \code{attr_dyads} is a \code{matrix}, the rows correspond to "actor1", columns to "actor2", and cells contain dyads' exogenous attributes.
#' @param scaling The method for scaling the statistic. The default is no scaling. Alternatively, standardization of the statistic per time point can be requested with "std".
#' @param x Deprecated argument. Please use 'attr_dyads' instead.
#' @param variableName Deprecated argument. Please use 'variable' instead.
#'
#' @aliases dyad
#'
#' @examples
#' data(history)
#' data(both_male_long)
#' effect <- ~ tie(variable = "both_male", attr_dyads = both_male_long)
#' reh <- remify::remify(history, model = "tie")
#' remstats(reh = reh, tie_effects = effect)
#'
#' data(both_male_wide)
#' effect <- ~ tie(variable = "both_male", attr_dyads = both_male_wide)
#' reh <- remify::remify(history, model = "tie")
#' remstats(reh = reh, tie_effects = effect)
#'
#' @export
tie <- function(variable, attr_dyads = NULL, scaling = c("none", "std"), x, variableName) {
  # Deal with old function set-up
  if (!missing(x)) {
    warning("The 'x' argument in 'tie()' is deprecated. Please use 'attr_dyads' instead.")
    attr_dyads <- x
  }
  if (!missing(variableName)) {
    warning("The 'variableName' argument in 'tie()' is deprecated. Please use 'variable' instead.")
    variable <- variableName
  }
  if(!is.character(variable)) {
    stop("The 'variable' argument should be of type character.")
  }
  if(!is.null(attr_dyads) & !is.matrix(attr_dyads) & !is.data.frame(attr_dyads)) {
    stop("The 'attr_dyads' argument should be of type matrix or data.frame.")
  }

  # Match scaling
  if ("as.is" %in% scaling) {
    warning("The 'scaling' value 'as.is' is deprecated. Use 'none' instead.")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  # Prep
  prep_tie(variable, attr_dyads, scaling)
}

#' @export
dyad <- tie

#' same
#'
#' Specifies the statistic for a "same" effect in the tie-oriented model or the
#' receiver choice step of the actor-oriented model. A "same" effect refers to
#' an exogenous actor attribute that affects dyad \emph{(i,j)}'s rate of
#' interacting (tie-oriented model) or actor \emph{j}'s probability of being
#' chosen as a receiver for the event send by the active sender \emph{i} at
#' time \emph{t} (actor-oriented model) based on whether actors \emph{i} and
#' \emph{j} have the same value (or not) on this attribute.
#'
#' @details
#' The statistic at timepoint \emph{t} is equal to one for dyads \emph{(i,j)}
#' that have the same value on the attribute at timepoint \emph{t}
#' (tie-oriented model) or one for receivers \emph{j} that have the same value
#' on the attribute as the active sender \emph{i} at timepoint \emph{t}
#' (actor-oriented model) and equal to 0 for dyads and receivers that do not
#' have the same value.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "same" effect (i.e., the column name should correspond
#' to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{same()} and, instead, supply it in the call of \code{remstats()} for
#' multiple exogenous effects.
#'
#' @inheritParams send
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ same("age")
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects, attr_actors = info)
#'
#' @export
same <- function(variable, attr_actors = NULL, attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }
  # Prep
  prep_exo("same", variable, attr_actors, "none")
}

#' difference
#'
#' Specifies the statistic for a "difference" effect in the tie-oriented model
#' or the receiver choice step of the actor-oriented model. A difference effect
#' refers to an exogenous actor attribute that affects dyad \emph{(i,j)}'s rate
#' of interacting (tie-oriented model) or actor \emph{j}'s probability of being
#' chosen as a receiver for the event send by the active sender \emph{i} at
#' time \emph{t} (actor-oriented model) based on the difference between the
#' values of actors \emph{i} and \emph{j} on this attribute.
#'
#' @details
#' The statistic at timepoint \emph{t} is equal to the (absolute) difference
#' between the values of actor \emph{i} and \emph{j} on the attribute at
#' timepoint \emph{t}.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "difference" effect (i.e., the column name should
#' correspond to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{difference()} and, instead, supply it in the call of \code{remstats()}
#' for multiple exogenous effects.
#'
#' @inheritParams send
#' @param absolute logical value indicating whether the difference values
#' should be converted to the absolute difference (default is TRUE).
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ difference("extraversion", absolute = TRUE)
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects, attr_actors = info)
#'
#' @export
difference <- function(variable,
                       attr_actors = NULL,
                       scaling = c("none", "std"),
                       absolute = TRUE,
                       attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }

  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  scaling <- ifelse(absolute, paste0(scaling, "_abs"), scaling)
  # Prep
  prep_exo("difference", variable, attr_actors, scaling)
}

#' average
#'
#' Specifies the statistic for an "average" effect in the tie-oriented model or
#' the receiver choice step of the actor-oriented model. An "average" effect
#' refers to an exogenous actor attribute that affects dyad \emph{(i,j)}'s rate
#' of interacting (tie-oriented model) or actor \emph{j}'s probability of being
#' chosen as a receiver for the event send by the active sender \emph{i} at
#' time \emph{t} (actor-oriented model) based on the average of the values of
#' actors \emph{i} and \emph{j} on this attribute.
#'
#' @details
#' The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the
#' average of the values of actor \emph{i} and \emph{j} on the attribute at
#' timepoint \emph{t}.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "average" effect (i.e., the column name should
#' correspond to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{average()} and, instead, supply it in the call of \code{remstats()} for
#' multiple exogenous effects.
#'
#' @inheritParams send
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ average("extraversion")
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects, attr_actors = info)
#'
#' @export
average <- function(variable, attr_actors = NULL, scaling = c("none", "std"), attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }

  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  # Prep
  prep_exo(effect = "average", variable = variable, attr_actors = attr_actors, scaling = scaling)
}

#' minimum
#'
#' Specifies the statistic for a "minimum" effect in the tie-oriented model. A
#' "minimum" effect refers to an exogenous actor attribute that affects dyad
#' \emph{(i,j)}'s rate of interacting based on the minimum of the values of
#' actors \emph{i} and \emph{j} on this attribute.
#'
#' @details
#' The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the
#' minimum of the values of actor \emph{i} and \emph{j} on the attribute at
#' timepoint \emph{t}.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "minimum" effect (i.e., the column name should
#' correspond to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{minimum()} and, instead, supply it in the call of \code{remstats()} for
#' multiple exogenous effects.
#'
#' @inheritParams send
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ minimum("extraversion")
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' @export
minimum <- function(variable, attr_actors = NULL, scaling = c("none", "std"), attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }

  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  # Prep
  prep_exo("minimum", variable, attr_actors, scaling)
}

#' maximum
#'
#' Specifies the statistic for a "maximum" effect in the tie-oriented model. A
#' "maximum" effect refers to an exogenous actor attribute that affects dyad
#' \emph{(i,j)}'s rate of interacting based on the maximum of the values of
#' actors \emph{i} and \emph{j} on this attribute.
#'
#' @details
#' The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the
#' maximum of the values of actor \emph{i} and \emph{j} on the attribute at
#' timepoint \emph{t}.
#'
#' The \code{attr_actors} object should be constructed as a
#' \code{\link[base]{data.frame}} where each row refers to the attribute value
#' of actor \emph{i} at timepoint \emph{t}:
#' \itemize{
#'  \item{\code{name}: the actors' name}
#'  \item{\code{time}: the time when the attribute values change (set to a
#' column with only zero's if the attribute does not vary over time).}
#'  \item{}{the third column contains the attribute that is called in the
#' specification of the "maximum" effect (i.e., the column name should
#' correspond to the string that is supplied to the \code{variable} argument)}
#' }
#' Note that it is possible to omit the attr_actors object in the call of
#' \code{maximum()} and, instead, supply it in the call of \code{remstats()} for
#' multiple exogenous effects.
#'
#' @inheritParams send
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ maximum("extraversion")
#' remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#'
#' @export
maximum <- function(variable, attr_actors = NULL, scaling = c("none", "std"), attr_data) {
  # Deal with old function set-up
  if (!missing(attr_data)) {
    warning("The 'attr_data' argument in 'send()' is deprecated. Please use 'attr_actors' instead.")
    attr_actors <- attr_data
  }

  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }
  # Prep
  prep_exo("maximum", variable, attr_actors, scaling)
}

#' event
#'
#' Specifies the statistic for an "event" effect in the tie-oriented model. An
#' "event" effect refers to an exogenous event attribute that affects the
#' waiting time between events.
#'
#' @details
#' The statistic at timepoint \emph{t} is for all dyads in the risk set equal
#' to the attribute of the event at timepoint \emph{t}.
#'
#' @param x vector with the event attribute
#' @param variableName optionally, a string indicating the variable name, used
#' for the dimnames of the output statistics object
#'
#' @seealso \code{\link{FEtype}}
#'
#' @examples
#' \dontrun{
#'   reh_tie <- remify::remify(history, model = "tie")
#'   data(history, package = "remstats")
#'   history$work <- ifelse(history$setting == "work", 1, 0)
#'   effects <- ~ event(x = history$work, variableName = "setting_is_work")
#'   remstats(reh = reh_tie, tie_effects = effects)
#' }
#'
#' @export
event <- function(x, variableName = NULL) {
  # Missing values
  if (anyNA(x)) {
    stop("Vector 'x' in event() contains missing values.")
  }

  # Output
  list(
    effect = "event",
    x = x,
    variable = variableName
  )
}

#' FEtype
#'
#' Specifies the statistic for fixed effects for event types in the
#' tie-oriented model.
#'
#' @details
#' Fixed effects for event types capture the variation in event rate across
#' different event types (e.g., see Butts, 2008). The specification of FEtype
#' results in the creation of C-1 statistics, were C is the number of different
#' event types in the riskset. Let one of the event types, e.g. \emph{c = 1},
#' represent the reference category. Than, for every event type
#' \emph{c = 2, ..., C}, a statistic is created that at timepoint \emph{t} for
#' dyad \emph{(i,j,c)} is equal to 1 if \emph{c} is equal to the respective
#' event type and equal to 0 otherwise (i.e., dummy variables are created).
#' Note that specifying fixed effects for event types is only available when
#' event types are modeled in the dependent variable.
#'
#' @seealso \code{\link{event}}
#'
#' @examples
#' history$type <- history$setting
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ FEtype()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
FEtype <- function() {
  call_args <- as.list(formals(FEtype))

  # Add effect
  call_args$effect <- "FEtype"

  return(call_args)
}

#' inertia
#'
#' Specifies the statistic for an inertia effect in the tie-oriented model or
#' the receiver choice step of the actor-oriented model.
#'
#' @details
#' An inertia effect refers to the tendency for dyads to repeatedly interact
#' with each other (tie-oriented model) or for actors to repeatedly choose the
#' same actor as receiver of their events (actor-oriented model). The statistic
#' at timepoint \emph{t} for dyad \emph{(i,j)} resp. receiver \emph{j} is
#' equal to the number of \emph{(i,j)} events before timepoint \emph{t}.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' inertia count by the outdegree of the sender ("prop"), the statistic refers
#' to the fraction of messages send by actor i that were send to actor j. If
#' actor i hasn't send any messages yet it can be assumed that every actor is
#' equally likely to receive a message from i and the statistic is set equal to
#' 1/(n-1), where n refers to the number of actors. The resulting statistic is
#' similar to the "FrPSndSnd" statistic in the R package relevent, or the
#' persistence statistic in Section 2.2.2 of Butts (2008). Note that this
#' scaling method is only defined for directed events.
#'
#' @param scaling the method for scaling the inertia statistic. Default is to
#' not scale the statistic (scaling = "none"). Alternatively, the
#' statistics can be scaled by specifying 'prop', in which raw counts are
#' divided by the outdegree of the sender at time t (see 'details') or
#' standardization of the raw counts per time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the number of past
#' events separately for each event type (TRUE, default) or sum across
#' different event types (FALSE).
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ inertia()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
inertia <- function(scaling = c("none", "prop", "std"),
                    consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(inertia))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "inertia"

  return(call_args)
}

#' reciprocity
#'
#' Specifies the statistic for a reciprocity effect in the tie-oriented model
#' or the receiver choice step of the actor-oriented model.
#'
#' @details
#' A reciprocity effect refers to the tendency for actors to reciprocate past
#' interactions. The statistic at timepoint \emph{t} for dyad \emph{(i,j)}
#' (tie-oriented model) or receiver \emph{j} (actor-oriented model) is equal to
#' the number of \emph{(j,i)} events before timepoint \emph{t}. Note that a
#' reciprocity effect is only defined for directed events.
#'
#' Optionally, a scaling method can be set with \code{scaling}.  By scaling the
#' reciprocity count by the indegree of the sender, the statistic refers to the
#' fraction of messages received by actor i that were received from actor j. If
#' actor i hasn't received any messages yet it can be assumed that actor i is
#' equally likely to receive a message from every actor and the statistic is
#' set equal to 1/(n-1), where n refers to the number of actors. The resulting
#' statistic is similar to the "FrRecSnd" statistic in the R package relevent.
#'
#' @param scaling the method for scaling the reciprocity statistic. Default is
#' to not scale the statistic but keep the raw 'counts'. Alternatively, the
#' statistics can be scaled by 'prop', in which raw counts are
#' divided by the indegree of the sender at time t (see 'details') or
#' standardization of the raw counts per time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the number of past
#' reciprocal events separately for each event type (TRUE, default) or sum
#' across different event types (FALSE).
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ reciprocity()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
reciprocity <- function(scaling = c("none", "prop", "std"),
                        consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(reciprocity))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "reciprocity"

  return(call_args)
}

#' indegreeSender
#'
#' Specifies the statistic for an `indegreeSender` effect in the tie-oriented
#' model or the sender activity rate step of the actor-oriented model.
#'
#' @details
#' An indegree of the sender effect refers to the tendency for actors to send
#' events if they have received more past events. The statistic at timepoint
#' \emph{t} for dyad \emph{(i,j)} (tie-oriented model) or sender \emph{i}
#' (actor-oriented model) is equal to the number of events received by
#' actor \emph{i} before timepoint \emph{t}. Note that the 'indegreeSender'
#' effect is only defined for directed events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events, the statistic refers to the
#' fraction of past events that were received by actor i. At the first time
#' point, when no events did previously occur, it is assumed that every actor
#' is equally likely to send a message and the statistic is set equal to 1/n,
#' where n refers to the number of actors.
#'
#' @param scaling the method for scaling the degree statistic. Default is
#' to not scale the statistic (scaling = "none"). Alternatively,
#' scaling of the raw degree counts by the number of past events at time t can
#' be requested with 'prop' or standardization of the raw degree counts per
#' time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the degrees
#' separately for each event type (TRUE, default) or sum degrees across
#' different event types (FALSE).
#'
#' @aliases degree indegree
#' @seealso \code{\link{indegreeReceiver}}, \code{\link{outdegreeSender}},
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ indegreeSender()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = effects)
#'
#' @export
indegreeSender <- function(scaling = c("none", "prop", "std"),
                           consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(indegreeSender))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "indegreeSender"

  return(call_args)
}

#' indegreeReceiver
#'
#' Specifies the statistic for an `indegreeReceiver` effect in the tie-oriented
#' model or the receiver choice step of the actor-oriented model.
#'
#' @details
#' An indegree of the receiver effect refers to the tendency for actors to
#' receive events if they have received more past events. The statistic at
#' timepoint \emph{t} for dyad \emph{(i,j)} (tie-oriented model) or
#' receiver \emph{j} (actor-oriented model) is equal to the number of events
#' received by actor \emph{j} before timepoint \emph{t}. Note that the
#' 'indegreeReceiver' effect is only defined for directed events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events, the statistic refers to the
#' fraction of past events that were received by actor j. At the first time
#' point, when no events did previously occur, it is assumed that every actor
#' is equally likely to receive a message and the statistic is set equal to 1/
#' n, where n refers to the number of actors.
#'
#' @inheritParams indegreeSender
#'
#' @seealso \code{\link{indegreeSender}}, \code{\link{outdegreeSender}},
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ indegreeReceiver()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
indegreeReceiver <- function(scaling = c("none", "prop", "std"),
                             consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(indegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "indegreeReceiver"

  return(call_args)
}

#' outdegreeSender
#'
#' Specifies the statistic for an `outdegreeSender` effect in the tie-oriented
#' model or the sender activity rate step of the actor-oriented model.
#'
#' @details
#' An outdegree of the sender effect refers to the tendency for actors to send
#' events if they have send more past events. The statistic at timepoint
#' \emph{t} for dyad \emph{(i,j)} (tie-oriented model) or sender \emph{i}
#' (actor-oriented model) is equal to the number of events send by
#' actor \emph{i} before timepoint \emph{t}. Note that the 'outdegreeSender'
#' effect is only defined for directed events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events, the statistic refers to the
#' fraction of past events that were send by actor i. At the first time
#' point, when no events did previously occur, it is assumed that every actor
#' is equally likely to send a message and the statistic is set equal to 1/n,
#' where n refers to the number of actors.
#'
#' @inheritParams indegreeSender
#'
#' @aliases outdegree
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}},
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ outdegreeSender()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = effects)
#'
#' @export
outdegreeSender <- function(scaling = c("none", "prop", "std"),
                            consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(outdegreeSender))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "outdegreeSender"

  return(call_args)
}

#' outdegreeReceiver
#'
#' Specifies the statistic for an `outdegreeReceiver` effect in the
#' tie-oriented model or the receiver choice step of the actor-oriented model.
#'
#' @details
#' An outdegree of the receiver effect refers to the tendency for actors to
#' receive events if they have send more past events. The statistic at
#' timepoint \emph{t} for dyad \emph{(i,j)} (tie-oriented model) or
#' receiver \emph{j} (actor-oriented model) is equal to the number of events
#' send by actor \emph{j} before timepoint \emph{t}. Note that the
#' 'outdegreeReceiver' effect is only defined for directed events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events, the statistic refers to the
#' fraction of past events that were send by actor j. At the first time
#' point, when no events did previously occur, it is assumed that every actor
#' is equally likely to receive a message and the statistic is set equal to
#' 1/n, where n refers to the number of actors.
#'
#' @inheritParams indegreeSender
#'
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}},
#' \code{\link{outdegreeSender}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ outdegreeReceiver()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
outdegreeReceiver <- function(scaling = c("none", "prop", "std"),
                              consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(outdegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "outdegreeReceiver"

  return(call_args)
}

#' totaldegreeSender
#'
#' Specifies the statistic for an `totaldegreeSender` effect in the
#' tie-oriented model or the sender activity rate step of the actor-oriented
#' model.
#'
#' @details
#' A total degree of the sender effect refers to the tendency for actors to
#' send events if they have send and received more past events. The statistic
#' at timepoint \emph{t} for dyad \emph{(i,j)} (tie-oriented model) or sender
#' \emph{i} (actor-oriented model) is equal to the number of events
#' send and received by actor \emph{i} before timepoint \emph{t}. Note that the
#' 'totaldegreeSender' effect is only defined for directed events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events times two, the statistic
#' refers to the fraction of past events times two that involved actor i. At
#' the first time point, when no events did previously occur, it is assumed
#' that every actor is equally likely to send a message and the statistic is
#' set equal to 1/n, where n refers to the number of actors.
#'
#' @param scaling the method for scaling the degree statistic. Default is
#' to not scale the statistic (scaling = "none"). Alternatively,
#' scaling of the raw degree counts by two times the number of past events at
#' time t can be requested with 'prop' or standardization of the raw degree
#' counts per time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the degrees
#' separately for each event type (TRUE, default) or sum degrees across
#' different event types (FALSE).
#'
#' @aliases totaldegree
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}},
#' \code{\link{outdegreeSender}}, \code{\link{outdegreeReceiver}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#'
#' @examples
#' effects <- ~ totaldegreeSender()
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = effects)
#'
#' @export
totaldegreeSender <- function(scaling = c("none", "prop", "std"),
                              consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeSender))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "totaldegreeSender"

  return(call_args)
}

#' totaldegreeReceiver
#'
#' Specifies the statistic for an `totaldegreeReceiver` effect in the
#' tie-oriented model or the receiver choice step of the actor-oriented model.
#'
#' @details
#' A total degree of the receiver effect refers to the tendency for actors to
#' receive events if they have send and received more past events. The
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} (tie-oriented model)
#' or receiver \emph{j} (actor-oriented model) is equal to the number
#' of events send and received by actor \emph{j} before timepoint \emph{t}.
#' Note that the 'totaldegreeReceiver' effect is only defined for directed
#' events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events times two, the statistic
#' refers to the fraction of past events times two that involved actor j. At
#' the first time point, when no events did previously occur, it is assumed
#' that every actor is equally likely to receive a message and the statistic is
#' set equal to 1/n, where n refers to the number of actors.
#'
#' @inheritParams totaldegreeSender
#'
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}},
#' \code{\link{outdegreeSender}}, \code{\link{outdegreeReceiver}}, or
#' \code{\link{totaldegreeSender}} for other types of degree effects.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ totaldegreeReceiver()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
totaldegreeReceiver <- function(scaling = c("none", "prop", "std"),
                                consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "totaldegreeReceiver"

  return(call_args)
}

#' totaldegreeDyad
#'
#' Specifies the statistic for a 'totaldegreeDyad' effect.
#'
#' @inheritParams totaldegreeSender
#'
#' @details
#' The 'totaldegreeDyad' effect refers to the tendency of pairs of actors
#' (dyads) to increase their interaction rate as the total degree (number of
#' interactions) of both actors in the pair goes up. To calculate this effect
#' for a specific pair (i,j) at a given timepoint (t), we sum the degrees of
#' the two actors in the dyad (i,j).
#'
#' Additionally, there is an optional scaling method, which can be chosen using
#' the 'scaling' method. When the 'prop' scaling method is applied, the degree
#' count is divided by two times the total number of past events. This scaling
#' converts the statistic into a fraction, representing the proportion of past
#' events in which at least one actor in the dyad was involved. For the first
#' timepoint, where no events have previously occurred, it is assumed that each
#' actor is equally likely to be involved in an event. In this case, the
#' statistic is set to 1 divided by the total number of actors (N).
#'
#' The totaldegreeDyad effect is defined for the tie-oriented model and is
#' applicable to both directed and undirected events.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ totaldegreeDyad()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
totaldegreeDyad <- function(scaling = c("none", "prop", "std"),
                            consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeDyad))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "totaldegreeDyad"

  return(call_args)
}

#' degreeDiff
#'
#' Specifies the statistic for a `degreeDiff` effect in the tie-oriented
#' model.
#'
#' @param scaling the method for scaling the degree statistic. Default is to
#' not scale the statistic (scaling = "none"). Alternatively, standardization
#' of the degree difference per time point can be requested with `std`.
#' @inheritParams totaldegreeSender
#'
#' @details
#' A degreeDiff effect refers to the tendency for dyads to increase their
#' interaction rate if the absolute difference in degree for the two actors in
#' the pair increases. The statistic at timepoint \emph{t} for dyad
#' \emph{(i,j)} is equal to the difference between the following two values:
#' the number of events before timepoint \emph{t} that involved actor \emph{i}
#' and actor \emph{j}, respectively. The degreeDiff effect is only defined for
#' undirected events.
#'
#' @seealso \code{\link{degreeMin}}, \code{\link{degreeMax}} or
#' \code{\link{totaldegreeDyad}} for other types of degree effects for
#' undirected events.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ degreeDiff()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
degreeDiff <- function(scaling = c("none", "std"), consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(degreeDiff))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "degreeDiff"

  return(call_args)
}

#' degreeMin
#'
#' Specifies the statistic for an `degreeMin` effect in the tie-oriented
#' model with undirected events.
#'
#' @details
#' An degreeMin effect refers to the tendency for dyads to increase their
#' interaction rate if the total degree of the least active actor in the pair
#' increases. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is
#' equal to the minimum of the following two values: the number of events
#' before timepoint \emph{t} that involved actor \emph{i} and actor \emph{j},
#' respectively. Note that the degreeMin effect is only defined for undirected
#' events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events, the statistic refers to the
#' fraction of past events that the least active actor was involved in. At the
#' first time point, when no events did previously occur, it is assumed that
#' every actor is equally likely to be involved in an event and the statistic
#' is set equal to 1/n, where n refers to the number of actors.
#'
#' @inheritParams totaldegreeSender
#'
#' @seealso \code{\link{degreeDiff}}, \code{\link{degreeMax}} or
#' \code{\link{totaldegreeDyad}} for other types of degree effects for
#' undirected events.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ degreeMin()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
degreeMin <- function(scaling = c("none", "prop", "std"),
                      consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(degreeMin))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "degreeMin"

  return(call_args)
}

#' degreeMax
#'
#' Specifies the statistic for an `degreeMax` effect in the tie-oriented
#' model with undirected events.
#'
#' @details
#' An degreeMax effect refers to the tendency for dyads to increase their
#' interaction rate if the total degree of the most active actor in the pair
#' increases. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is
#' equal to the maximum of the following two values: the number of events
#' before timepoint \emph{t} that involved actor \emph{i} and actor \emph{j},
#' respectively. Note that the degreeMax effect is only defined for undirected
#' events.
#'
#' Optionally, a scaling method can be set with \code{scaling}. By scaling the
#' degree count by the total number of past events, the statistic refers to the
#' fraction of past events that the most active actor was involved in. At the
#' first time point, when no events did previously occur, it is assumed that
#' every actor is equally likely to be involved in an event and the statistic
#' is set equal to 1/n, where n refers to the number of actors.
#'
#' @inheritParams totaldegreeSender
#'
#' @seealso \code{\link{degreeDiff}}, \code{\link{degreeMin}} or
#' \code{\link{totaldegreeDyad}} for other types of degree effects for
#' undirected events.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ degreeMax()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
degreeMax <- function(scaling = c("none", "prop", "std"),
                      consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(degreeMax))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- scaling

  # Add effect
  call_args$effect <- "degreeMax"

  return(call_args)
}

#' otp
#'
#' Specifies the statistic for an outgoing two-path effect.
#'
#' @details
#' The outgoing two-path effect describes the propensity of dyads to interact
#' based on the number of past outgoing two-paths between them. By default, the
#' statistic at timepoint t for the dyad (i,j) is computed as the sum of the
#' minimum occurrences of past (i,h) and (h,j) events across all actors h.
#'
#' When the unique parameter is set to TRUE, a different approach is taken.
#' In this case, the statistic counts the number of actors h that contribute to
#' the creation of a new, distinct two-path between actors i and j.
#'
#' Additionally, it is possible to specify a scaling method using the scaling
#' parameter.
#'
#' Please note that the outgoing two-path effect, 'otp', is exclusively defined
#' for directed events.
#'
#' @param unique A logical value indicating whether to sum the minimum of
#' events with third actors (FALSE, default) or the number of third actors that
#' create a new, unique two-path (TRUE). See details for more information.
#' @param scaling The method for scaling the triad statistic. The default value
#' is "none", which means the statistic is not scaled. Alternatively, you can
#' set it to "std" to request standardization of the raw counts per time point.
#' @param consider_type A logical value indicating whether to count the
#' two-paths separately for each event type (TRUE, default) or sum across
#' different event types (FALSE).
#'
#' @aliases triad
#' @seealso \code{\link{itp}}, \code{\link{osp}}, or \code{\link{isp}} for
#' other types of triadic effects for directed relational events and
#' \code{\link{sp}} for triadic effects for undirected relational events.
#'
#' @references Butts, C. (2008). A relational event framework for social
#' action. Sociological Methodology.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ otp()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
otp <- function(unique = FALSE, scaling = c("none", "std"),
                consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- ifelse(unique, paste0(scaling, "_unique"), scaling)

  # Add effect
  call_args$effect <- "otp"

  return(call_args)
}

#' itp
#'
#' Specifies the statistic for an incoming two-path effect.
#'
#' @details
#' The incoming two-path effect describes the propensity of dyads to interact
#' based on the number of past incoming two-paths between them. By default, the
#' statistic at timepoint t for the dyad (i,j) is computed as the sum of the
#' minimum occurrences of past (j,h) and (h,i) events across all actors h.
#'
#' When the unique parameter is set to TRUE, a different approach is taken.
#' In this case, the statistic counts the number of actors h that contribute to
#' the creation of a new, distinct two-path between actors i and j.
#'
#' Additionally, it is possible to specify a scaling method using the scaling
#' parameter.
#'
#' Please note that the incoming two-path effect, 'itp', is exclusively defined
#' for directed events.
#'
#' @inheritParams otp
#'
#' @seealso \code{\link{otp}}, \code{\link{osp}}, or \code{\link{isp}} for
#' other types of triadic effects for directed relational events and
#' \code{\link{sp}} for triadic effects for undirected relational events.
#'
#' @references Butts, C. (2008). A relational event framework for social
#' action. Sociological Methodology.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ itp()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
itp <- function(unique = FALSE, scaling = c("none", "std"),
                consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- ifelse(unique, paste0(scaling, "_unique"), scaling)

  # Add effect
  call_args$effect <- "itp"

  return(call_args)
}

#' osp
#'
#' Specifies the statistic for an outgoing shared partners effect.
#'
#' @details
#' The outgoing shared partners effect describes the propensity of dyads to
#' interact based on the number of past outgoing shared partners between them.
#' By default, the statistic at timepoint t for the dyad (i,j) is computed as
#' the sum of the minimum occurrences of past (i,h) and (j,h) events across all
#' actors h.
#'
#' When the unique parameter is set to TRUE, a different approach is taken.
#' In this case, the statistic counts the number of actors h that contribute to
#' the creation of a new, distinct shared partner between actors i and j.
#'
#' Additionally, it is possible to specify a scaling method using the scaling
#' parameter.
#'
#' Please note that the outgoing shared partners effect, 'osp', is exclusively
#' defined for directed events.
#'
#' @param unique A logical value indicating whether to sum the minimum of
#' events with third actors (FALSE, default) or the number of third actors that
#' create a new, unique shared partner (TRUE). See details for more information.
#' @param scaling the method for scaling the triad statistic. Default is to not
#' scale the statistic but keep the raw 'counts'. Alternatively,
#' standardization of the raw counts per time point can be requested
#' with 'std'.
#' @param consider_type logical, indicates whether to count the shared partners
#' separately for each event type (TRUE, default) or sum across different event
#' types (FALSE).
#'
#' @seealso \code{\link{otp}}, \code{\link{itp}}, or \code{\link{isp}} for
#' other types of triadic effects for directed relational events and
#' \code{\link{sp}} for triadic effects for undirected relational events.
#'
#' @references Butts, C. (2008). A relational event framework for social
#' action. Sociological Methodology.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ osp()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
osp <- function(unique = FALSE, scaling = c("none", "std"), consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- ifelse(unique, paste0(scaling, "_unique"), scaling)

  # Add effect
  call_args$effect <- "osp"

  return(call_args)
}

#' isp
#'
#' Specifies the statistic for an incoming shared partners effect.
#'
#' @details
#' The incoming shared partners effect describes the propensity of dyads to
#' interact based on the number of past incoming shared partners between them.
#' By default, the statistic at timepoint t for the dyad (i,j) is computed as
#' the sum of the minimum occurrences of past (h,i) and (h,j) events across all
#' actors h.
#'
#' When the unique parameter is set to TRUE, a different approach is taken.
#' In this case, the statistic counts the number of actors h that contribute to
#' the creation of a new, distinct shared partner between actors i and j.
#'
#' Additionally, it is possible to specify a scaling method using the scaling
#' parameter.
#'
#' Please note that the incoming shared partners effect, 'isp', is exclusively
#' defined for directed events.
#'
#' @inheritParams osp
#'
#' @seealso \code{\link{otp}}, \code{\link{itp}}, or \code{\link{osp}} for
#' other types of triadic effects for directed relational events and
#' \code{\link{sp}} for triadic effects for undirected relational events.
#'
#' @references Butts, C. (2008). A relational event framework for social
#' action. Sociological Methodology.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ isp()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, receiver_effects = effects)
#'
#' @export
isp <- function(
    unique = FALSE, scaling = c("none", "std"),
    consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- ifelse(unique, paste0(scaling, "_unique"), scaling)

  # Add effect
  call_args$effect <- "isp"

  return(call_args)
}

#' sp
#'
#' Specifies the statistic for a shared partners effect for undirected events.
#'
#' @details
#' The shared partners effect describes the propensity of dyads to interact
#' based on the number of past shared partners between them. By default, the
#' statistic at timepoint t for the undirected dyad (i,j) is computed as
#' the sum of the minimum occurrences of past undirected (i,h) and undirected
#' (j,h) events across all actors h.
#'
#' When the unique parameter is set to TRUE, a different approach is taken.
#' In this case, the statistic counts the number of actors h that contribute to
#' the creation of a new, distinct shared partner between actors i and j.
#'
#' Additionally, it is possible to specify a scaling method using the scaling
#' parameter.
#'
#' Please note that the shared partners effect, 'sp', is exclusively defined
#' for undirected events.
#'
#' @inheritParams osp
#'
#' @seealso \code{\link{otp}}, \code{\link{itp}}, \code{\link{osp}}, or
#' \code{\link{isp}} for triadic effects for directed relational events.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ sp()
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' @export
sp <- function(
    unique = FALSE, scaling = c("none", "std"),
    consider_type = TRUE) {
  # Match scaling
  if ("as.is" %in% scaling) {
    warning("use 'scaling' is 'none' instead of 'as.is'")
    scaling <- "none"
  } else {
    scaling <- match.arg(scaling)
  }

  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(totaldegreeReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Match scaling
  call_args$scaling <- ifelse(unique, paste0(scaling, "_unique"), scaling)

  # Add effect
  call_args$effect <- "sp"

  return(call_args)
}

#' spUnique
#'
#' Deprecated. Use \code{\link{sp}}.
#'
#' @export
spUnique <- function() {
  .Deprecated("sp")
}

#' psABBA
#'
#' Specifies the statistic for a participation shift AB-BA.
#'
#' @param consider_type logical, indicates whether to consider the event type
#' in determining which dyads create a pshift (TRUE, default) or not (FALSE).
#'
#' @details
#' The AB-BA pshift effect refers to one of Gibson's (2003) dyadic participation shifts. The AB-BA pshift refers to the tendency for immediate reciprocation (the next sender is the previous receiver and the next receiver is the previous sender). For each timepoint t, the psABBA statistic is equal to one for the dyad that will create the participation shift if it would occur in the edgelist at time t and equal to zero for the dyads that will not create this participation shift. If consider_type is set to TRUE, the type of the AB event and the type of the BA event have to be equal. If it is set to FALSE, the participation shift is set to one for every BA event, regardless of the event type. If multiple events in the edgelist occur at the same time point, the order of these events determines whether the p-shift is observed. Note that the AB-BA pshift is only defined for directed events.
#'
#' @aliases pshift
#' @seealso \code{\link{psABBY}}, \code{\link{psABXA}}, \code{\link{psABXB}},
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation
#' shifts.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ psABBA()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
psABBA <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABBA))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABBA"

  return(call_args)
}

#' psABB
#'
#' Specifies the statistic for a participation shift AB-B in the sender step of the actor-oriented model.
#'
#' @details
#' The AB-B participation shift refers to the tendency for immediate reciprocation (the next sender is the previous receiver). For each timepoint t, the psABBA statistic is equal to one for the actor (i.e, the previous event receiver) that will create the participation shift if it would occur as sender in the edgelist at time t and equal to zero for the actors that will not create this participation shift. If multiple events in the edgelist occur at the same time point, the order of these events determines whether the p-shift is observed.
#'
#' @seealso \code{\link{psABA}} or \code{\link{psABX}} for exploring alternative participation shifts in the sender step of the actor-oriented model.
#'
#' @examples
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = ~ psABB())
#'
#' @export
psABB <- function() {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABB))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABB"

  return(call_args)
}

#' psABBY
#'
#' Specifies the statistic for a participation shift AB-BY.
#'
#' @inheritParams psABBA
#'
#' @details
#' The AB-BY participation shift refers to one of Gibson's (2003) dyadic participation shifts. The AB-BY pshift refers to a tendency for turn receiving (here, the next sender is the previous receiver and the next receiver is not in the current previous). For each timepoint t, the psABBY statistic is equal to one for the dyads that will create the participation shift if they would occur in the edgelist at time t and equal to zero for the dyads that will not create this participation shift. If consider_type is set to TRUE, the type of the AB event and the type of the BY events have to be equal. If it is set to FALSE, the participation shift is set to one for every BY event, regardless of the event type. If multiple events in the edgelist occur at the same time point, the order of these events determines whether the p-shift is observed. Note that the AB-BY pshift is only defined for directed events.
#'
#' @seealso \code{\link{psABBA}}, \code{\link{psABXA}}, \code{\link{psABXB}},
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation
#' shifts.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ psABBY()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
psABBY <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABBY))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABBY"

  return(call_args)
}

#' psABXA
#'
#' Specifies the statistic for a participation shift AB-XA.
#'
#' @inheritParams psABBA
#'
#' @details
#' The AB-XA participation shift refers to one of Gibson's (2003) dyadic participation shifts. The AB-XA pshift refers to a tendency for turn usurping (here, the next sender is not in the previous event and the next receiver is the previous sender). For each timepoint t, the psABXA statistic is equal to one for the dyads that will create the participation shift if they would occur in the edgelist at time t and equal to zero for the dyads that will not create this participation shift. If consider_type is set to TRUE, the type of the AB event and the type of the XA events have to be equal. If it is set to FALSE, the participation shift is set to one for every XA event, regardless of the event type. If multiple events in the edgelist occur at the same time point, the order of these events determines whether the pshift is observed. Note that the AB-XA pshift is only defined for directed events.
#'
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXB}}, \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation shifts.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ psABXA()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
psABXA <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABXA))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABXA"

  return(call_args)
}

#' psABX
#'
#' Specifies the statistic for a participation shift AB-X in the sender step of
#' the actor-oriented model.
#'
#' @details
#' The AB-X participation shift refers to a tendency for turn usurping (here,
#' the next sender is not in the previous event). For each timepoint t, the
#' psABX statistic is equal to one for the actors that will create the
#' participation shift if they would occur as the sender in the edgelist at
#' time t and equal to zero for the actors that will not create this
#' participation shift. If multiple events in the edgelist occur at the same
#' time point, the order of these events determines whether the p-shift is
#' observed.
#'
#' @seealso \code{\link{psABA}} or \code{\link{psABB}} for exploring
#' alternative participation shifts in the sender step of the actor-oriented
#' model.
#'
#' @examples
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(reh = reh_actor, sender_effects = ~ psABX())
#'
#' @export
psABX <- function() {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABX))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABX"

  return(call_args)
}

#' psABXB
#'
#' Specifies the statistic for a participation shift AB-XB.
#'
#' @inheritParams psABBA
#'
#' @details
#' The AB-XB participation shift refers to one of Gibson's (2003) dyadic participation shifts. The AB-XB pshift refers to a tendency for turn usurping (here, the next sender is not in the previous event and the next receiver is the previous receiver). For each timepoint t, the psABXB statistic is equal to one for the dyads that will create the participation shift if they would occur in the edgelist at time t and equal to zero for the dyads that will not create this participation shift. If consider_type is set to TRUE, the type of the AB event and the type of the XB events have to be equal. If it is set to FALSE, the participation shift is set to one for every XB event, regardless of the event type. If multiple events in the edgelist occur at the same time point, the order of these events determines whether the p-shift is observed. Note that the AB-XB pshift is only defined for directed events.
#'
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}},
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation
#' shifts.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ psABXB()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
psABXB <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABXB))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABXB"

  return(call_args)
}

#' psABXY
#'
#' Specifies the statistic for a participation shift AB-XY.
#'
#' @inheritParams psABBA
#'
#' @details
#' The AB-XY participation shift refers to one of Gibson's (2003) dyadic participation shifts. The AB-XY pshift refers to a tendency for turn usurping (here, the next sender and the next receiver are not in the previous event). For each timepoint t, the psABXY statistic is equal to one for the dyads that will create the participation shift if they would occur in the edgelist at time t and equal to zero for the dyads that will not create this participation shift. If consider_type is set to TRUE, the type of the AB event and the type of the XY events have to be equal. If it is set to FALSE, the participation shift is set to one for every XY event, regardless of the event type. If multiple events in the edgelist occur at the same time point, the order of these events determines whether the p-shift is observed. Note that the AB-XY pshift is only defined for directed events.
#'
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}},
#' \code{\link{psABXB}} or \code{\link{psABAY}} for other dyadic participation
#' shifts.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ psABXY()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' @export
psABXY <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABXY))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABXY"

  return(call_args)
}

#' psABAY
#'
#' Specifies the statistic for a participation shift AB-AY.
#'
#' @inheritParams psABBA
#'
#' @details
#' One of Gibson's (2003) dyadic participation shifts. The AB-AY participation
#' shift refers to a tendency for \emph{turn continuing}. For directed events,
#' the sender (A) in the current event is the same as the sender in the
#' previous event (A), and the receiver (Y) is different from the previous
#' receiver (B). In undirected events, one of the current actors (A) matches
#' one of the actors in the previous events (A or B), while the other actor (Y)
#' is different.
#'
#' To identify these shifts, a statistic 'psABAY' is calculated for each pair
#' of actors at a given timepoint (t). If the pair's interaction follows the
#' AB-AY pattern, the statistic is set equal to one; otherwise, it's set to
#' zero.
#'
#' Additionaly, the types of the AB and AY events can be taken into account. If
#' 'consider_type' is 'TRUE', the type of the AB event and the type of the AY
#' event must match for the shift to occur. If 'consider_type' is 'FALSE', the
#' shift happens for every AY event, regardless of the event type.
#'
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}},
#' \code{\link{psABXB}}, \code{\link{psABXY}} or \code{\link{psABAB}} for other
#' dyadic participation shifts.
#'
#' @examples
#' reh <- remify::remify(history, model = "tie")
#' effects <- ~ psABAY()
#' remstats(reh = reh, tie_effects = effects)
#'
#' @export
psABAY <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABAY))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABAY"

  return(call_args)
}

#' psABAB
#'
#' Specifies the statistic for a pshift AB-AB effect.
#'
#' @inheritParams psABBA
#'
#' @details
#'
#' Refers to the tendency for the same dyads to keep interacting. For directed
#' events, the next sender and receiver are equal to the previous sender and
#' receiver. For undirected events, the next actor pair is equal to the current
#' actor pair. For each timepoint t, the psABAB statistic is equal to one for
#' the dyads that will create the participation shift if they would occur in
#' the edgelist at time t and equal to zero for the dyads that will not create
#' this participation shift. If consider_type is set to TRUE, the type of the
#' two subsequent AB events have to be equal. If it is set to FALSE, the
#' participation shift is set to one for every AB event, regardless of the
#' event type. If multiple events in the edgelist occur at the same time point,
#' the order of these events determines whether the p-shift is observed.
#'
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}},
#' \code{\link{psABXB}}, \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation shifts.
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
#' effects <- ~ psABAB()
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' @export
psABAB <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABAB))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABAB"

  return(call_args)
}

#' psABA
#'
#' Specifies the statistic for a participation shift AB-A in the sender step of
#' the actor-oriented model.
#'
#' @details
#'
#' Refers to the tendency for the same actor to keep initiating events: The
#' next sender is equal to the previous sender. For each timepoint t, the psABA
#' statistic is equal to one for the actor that will create the participation
#' shift if they would occur in the edgelist as the sender at time t and equal
#' to zero for the actors that will not create this participation shift. If
#' multiple events in the edgelist occur at the same time point, the order of
#' these events determines whether the p-shift is observed.
#'
#' @seealso \code{\link{psABB}} or \code{\link{psABX}} for exploring
#' alternative participation shifts in the sender step of the actor-oriented
#' model.
#'
#' @examples
#' reh_actor <- remify::remify(history, model = "actor", directed = FALSE)
#' remstats(sender_effects = ~ psABA(), reh = reh_actor)
#'
#' @export
psABA <- function() {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(psABA))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "psABA"

  return(call_args)
}


#' rrankSend
#'
#' Specifies the statistic for a recency rank send effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the
#' \code{receiver_effects} argument of \code{\link{aomstats}}.
#'
#' The rrankSend effect refers to a rank-based recency effect, as described in
#' section 2.2.5 of Butts (2008). For each timepoint t, for directed dyad (i,j)
#' the statistic is equal to the inverse of the rank of receiver j among the
#' actors to which sender i has most recently send past events. Note that the
#' 'rrankSend' effect is only defined for directed events.
#'
#' @param consider_type logical, indicates whether to discriminate between
#' event types in determining the event rank (TRUE, default) or not (FALSE).
#'
#' @aliases recencyRank rrank
#' @seealso \code{\link{rrankReceive}}, \code{\link{recencySendSender}},
#' \code{\link{recencySendReceiver}}, \code{\link{recencyReceiveSender}},
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for
#' other type of recency effects
#'
#' @examples
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ rrankSend()
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(receiver_effects = effects, reh = reh_actor)
#'
#' @export
rrankSend <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(rrankSend))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "rrankSend"

  return(call_args)
}

#' rrankReceive
#'
#' Specifies the statistic for a recency rank receive effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the
#' \code{receiver_effects} argument of \code{\link{aomstats}}.
#'
#' The rrankSend effect refers to a rank-based recency effect, as described in
#' section 2.2.5 of Butts (2008). For each timepoint t, for directed dyad (i,j)
#' the statistic is equal to the inverse of the rank of receiver j among the
#' actors from which sender i has most recently received past events. Note that
#' the 'rrankReceive' effect is only defined for directed events.
#'
#' @inheritParams rrankSend
#'
#' @seealso \code{\link{rrankSend}}, \code{\link{recencySendSender}},
#' \code{\link{recencySendReceiver}}, \code{\link{recencyReceiveSender}},
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for
#' other type of recency effects
#'
#' @examples
#'
#' reh_tie <- remify::remify(history, model = "tie")
#' effects <- ~ rrankReceive()
#' remstats(reh = reh_tie, tie_effects = effects)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(receiver_effects = effects, reh = reh_actor)
#'
#' @export
rrankReceive <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(rrankReceive))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "rrankReceive"

  return(call_args)
}


#' recencySendSender
#'
#' Specifies the statistic for a recency send of sender effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the \code{sender_effects}
#' argument of \code{\link{aomstats}}.
#'
#' The recencySendSender effect refers to a recency statistic similar to what
#' is described in Vu et al. (2017) and Mulder and Leenders (2019). For each
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
#' that has past since sender i was last active as sender + 1). Note that the
#' 'recencySendSender' effect is only defined for directed events.
#'
#' @param consider_type logical, indicates whether to compute the recency
#' separately for each event type (TRUE, default) or regardless of event types
#' (FALSE).
#'
#' @seealso \code{\link{rrankSend}}, \code{\link{rrankReceive}},
#' \code{\link{recencySendReceiver}}, \code{\link{recencyReceiveSender}},
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for
#' other type of recency effects
#'
#' @examples
#' effects <- ~ recencySendSender()
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(sender_effects = effects, reh = reh_actor)
#'
#' @export
recencySendSender <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(recencySendSender))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "recencySendSender"

  return(call_args)
}

#' recencySendReceiver
#'
#' Specifies the statistic for a recency send of receiver effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the
#' \code{receiver_effects} argument of \code{\link{aomstats}}.
#'
#' The recencySendReceiver effect refers to a recency statistic similar to what
#' is described in Vu et al. (2017) and Mulder and Leenders (2019). For each
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
#' that has past since receiver j was last active as sender + 1). Note that the
#' 'recencySendReceiver' effect is only defined for directed events.
#'
#' @inheritParams recencySendSender
#'
#' @seealso \code{\link{rrankSend}}, \code{\link{rrankReceive}},
#' \code{\link{recencySendSender}}, \code{\link{recencyReceiveSender}},
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for
#' other type of recency effects
#'
#' @examples
#' effects <- ~ recencySendReceiver()
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(receiver_effects = effects, reh = reh_actor)
#'
#' @export
recencySendReceiver <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(recencySendReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "recencySendReceiver"

  return(call_args)
}

#' recencyReceiveSender
#'
#' Specifies the statistic for a recency receive of sender effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the
#' \code{sender_effects} argument of \code{\link{aomstats}}.
#'
#' The recencyReceiveSender effect refers to a recency statistic similar to
#' what is described in Vu et al. (2017) and Mulder and Leenders (2019). For
#' each timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the
#' time that has past since sender i was last active as receiver + 1). Note
#' that the 'recencyReceiveSender' effect is only defined for directed events.
#'
#' @inheritParams recencySendSender
#'
#' @seealso \code{\link{rrankSend}}, \code{\link{rrankReceive}},
#' \code{\link{recencySendSender}}, \code{\link{recencySendReceiver}},
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for
#' other type of recency effects
#'
#' @examples
#' effects <- ~ recencyReceiveSender()
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(sender_effects = effects, reh = reh_actor)
#'
#' @export
recencyReceiveSender <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(recencyReceiveSender))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "recencyReceiveSender"

  return(call_args)
}

#' recencyReceiveReceiver
#'
#' Specifies the statistic for a recency receive of receiver effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the
#' \code{receiver_effects} argument of \code{\link{aomstats}}.
#'
#' The recencyReceiveReceiver effect refers to a recency statistic similar to
#' what is described in Vu et al. (2017) and Mulder and Leenders (2019). For
#' each timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the
#' time that has past since receiver j was last active as receiver + 1). Note
#' that the 'recencyReceiveReceiver' effect is only defined for directed
#' events.
#'
#' @inheritParams recencySendSender
#'
#' @seealso \code{\link{rrankSend}}, \code{\link{rrankReceive}},
#' \code{\link{recencySendSender}}, \code{\link{recencyReceiveSender}},
#' \code{\link{recencyReceiveSender}} and \code{\link{recencyContinue}} for
#' other type of recency effects
#'
#' @examples
#' effects <- ~ recencyReceiveReceiver()
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(receiver_effects = effects, reh = reh_actor)
#'
#' @export
recencyReceiveReceiver <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(recencyReceiveReceiver))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "recencyReceiveReceiver"

  return(call_args)
}

#' recencyContinue
#'
#' Specifies the statistic for a recency continue effect in the
#' \code{effects} argument of \code{\link{tomstats}} or the
#' \code{receiver_effects} argument of \code{\link{aomstats}}.
#'
#' The recencyContinue effect refers to a recency statistic similar to what is
#' described in Vu et al. (2017) and Mulder and Leenders (2019). For each
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
#' that has past since the dyad was last active + 1).
#'
#' @inheritParams recencySendSender
#'
#' @aliases recency
#'
#' @seealso \code{\link{rrankSend}}, \code{\link{rrankReceive}},
#' \code{\link{recencySendSender}}, \code{\link{recencyReceiveSender}},
#' \code{\link{recencyReceiveSender}} and \code{\link{recencyReceiveReceiver}}
#' for other type of recency effects
#'
#' @examples
#' effects <- ~ recencyContinue()
#' reh_tie <- remify::remify(history, model = "tie")
#' remstats(tie_effects = effects, reh = reh_tie)
#'
#' reh_actor <- remify::remify(history, model = "actor")
#' remstats(receiver_effects = effects, reh = reh_actor)
#'
#' @export
recencyContinue <- function(consider_type = TRUE) {
  call_args <- as.list(match.call()[-1])
  defaults <- as.list(formals(recencyContinue))

  # Update call_args with default values
  for (arg_name in names(defaults)) {
    if (!(arg_name %in% names(call_args))) {
      call_args[[arg_name]] <- defaults[[arg_name]]
    }
  }

  # Add effect
  call_args$effect <- "recencyContinue"

  return(call_args)
}

#' userStat
#'
#' Allows the user to add its own pre-computed statistic to the statistics
#' object and, optionally, interact this statistic with other statistics in the
#' formula.
#'
#' @param x Matrix with number of rows equal to the number of events and number
#' of columns equal to the number of dyads in the network (tie-oriented model) 
#' or the number of actors in the network (actor-oriented model)
#' @param variableName Optionally, a string with the name of the statistic. 
#'
#' @examples
#' \dontrun{
#'  reh <- remify::remify(history, model = "tie")
#'  actor101Events <- which(history$actor1 == "101" | history$actor2 == "101")
#'  actor101_stat <- t(sapply(seq_len(nrow(history)), function(i) {
#'    rep(i %in% actor101Events, reh$D)
#'  }))
#'  
#'  # Main effects only
#'  effects <- ~ userStat(x = actor101_stat, variableName = "actor101event")
#'  remstats(reh = reh, tie_effects = effects)
#'  
#'  # Model with interaction effects
#'  interaction_effects <- ~ inertia() *
#'    userStat(x = actor101_stat, variableName = "actor101event")
#'  remstats(reh = reh, tie_effects = interaction_effects)
#' }
#' @export
userStat <- function(x, variableName = NULL) {
  # Missing values
  if (anyNA(x)) {
    warning("Matrix 'x' in userStat() contains missing values.")
  }

  # Output
  list(
    effect = "userStat",
    x = x,
    variable = variableName,
    scaling = "none"
  )
}
