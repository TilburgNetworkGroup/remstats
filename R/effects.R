#' baseline
#' 
#' A baseline effect in the \code{effects} argument of \code{\link{tomstats}} 
#' is automatically specified when \code{ordinal} in \code{\link{tomstats}} is 
#' set to FALSE (default) and automatically removed when this argument is set 
#' to TRUE. Alternatively, a baseline effect can be explicitly specified by 
#' adding '1' to the equation or explicitly removed by adding '-1' to the 
#' equation. This also holds for the \code{rateEffects} in 
#' \code{\link{aomstats}}. 
#' 
#' @details
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
#' tomstats(~ 1, edgelist = history)
#' aomstats(rateEffects = ~ 1, edgelist = history)
NULL

#' send
#' 
#' Specifies the statistic for a `send` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{rateEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details
#' A send effect refers to an exogenous actor attribute that affects actor 
#' i's rate of sending events. The statistic at timepoint \emph{t} is equal to 
#' the value of the exogenous attribute for actor \emph{i} at time \emph{t} for 
#' all dyads in the riskset that have actor \emph{i} as sender. Note that a 
#' send effect is only defined for directed relational events. 
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}} or \code{\link{aomstats}}. 
#' 
#' @param variable string with the name of the column in the 
#' \code{attributes} object for which the statistic has to be computed. 
#' @param attributes optionally, an object of class 
#' \code{"\link[base]{data.frame}"} that contains the exogenous attributes (see 
#' details). 
#' @param scaling the method for scaling the statistic. Default is to not scale 
#' the statistic but keep it "as.is". Alternatively, standardization of the 
#' statistic per time point can be requested with "std". 
#' 
#' @examples 
#' effects <- ~ send("extraversion")
#' tomstats(effects, edgelist = history, attributes = info)
#' aomstats(rateEffects = effects, edgelist = history, attributes = info)
#' 
#' @export 
send <- function(variable, attributes = NULL, scaling = c("as.is", "std")) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "send", 
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'send' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'send' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "send", 
            variable = variable,
            x = dat, 
            scaling = scaling)
    }
}

#' receive
#' 
#' Specifies the statistic for a `receive` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details
#' A receive effect refers to an exogenous actor attribute that affects actor 
#' i's rate of receiving events. The statistic at timepoint \emph{t} is equal 
#' to the value of the exogenous attribute for actor \emph{i} at time \emph{t} 
#' for all dyads in the riskset that have actor \emph{i} as receiver. Note that 
#' a receive effect is only defined for directed relational events.
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}} or \code{\link{aomstats}}. 
#' 
#' @inheritParams send
#' 
#' @examples 
#' effects <- ~ receive("extraversion") 
#' tomstats(effects, edgelist = history, attributes = info)
#' aomstats(choiceEffects = effects, edgelist = history, attributes = info)
#' 
#' @export 
receive <- function(variable, attributes = NULL, scaling = c("as.is", "std")) {
    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "receive", 
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'receive' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'receive' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "receive", 
            variable = variable,
            x = dat, 
            scaling = scaling)
    }
}

# TO DO: timevarying tie effect
#' tie
#' 
#' Specifies the statistic for a `tie` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details 
#' A tie effect refers to an exogenous dyad attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting. 
#' 
#' @param x a matrix with attribute information, rows and columns should refer 
#' to actors in the edgelist
#' @param variableName optionally, a string indicating the variable name, used 
#' for the dimnames of the output statistics object 
#' @param scaling the method for scaling the statistic. Default is to not scale 
#' the statistic but keep it "as.is". Alternatively, standardization of the 
#' statistic per time point can be requested with "std". 
#' 
#' @examples 
#' data(info)
#' actors <- unique(info$id)
#' age <- info[match(actors, info$id), "age"]
#' bothOld <- sapply(1:length(actors), function(i) {
#'  sapply(1:length(actors), function(j) {
#'      ifelse(age[i] & age[j] == 1 & i != j, 1, 0)
#'  })})
#' effects <- ~ tie(bothOld, variableName = "both.old")
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export
tie <- function(x, variableName = NULL, scaling = c("as.is", "std")) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    list(
        effect = "tie", 
        x = x,
        variable = variableName,
        scaling = scaling
    )

}

#' same
#' 
#' Specifies the statistic for a `same` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details
#' A same effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on whether they have the same value 
#' (or not) on this attribute. The statistic at timepoint \emph{t} is equal to 
#' one for dyads \emph{(i,j)} that have the same value on the attribute at 
#' timepoint \emph{t} and equal to 0 for dyads that do not have the same value. 
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}} or \code{\link{aomstats}}. 
#' 
#' @inheritParams send
#' 
#' @examples 
#' effects <- ~ same("age") 
#' tomstats(effects, edgelist = history, attributes = info)
#' aomstats(choiceEffects = effects, edgelist = history, attributes = info)
#' 
#' @export 
same <- function(variable, attributes = NULL) {
   
    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "same", 
            variable = variable,
            x = NULL,
            scaling = 1
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'same' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'same' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "same", 
            variable = variable,
            x = dat, 
            scaling = 1)
    }
}

#' difference
#' 
#' Specifies the statistic for a `difference` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details 
#' A difference effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the difference between their 
#' values on this attribute. The statistic at timepoint \emph{t} for dyad 
#' \emph{(i,j)} is equal to the absolute difference between the values of actor 
#' \emph{i} and \emph{j} on the attribute at timepoint \emph{t}.  
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}} or \code{\link{aomstats}}. 
#' 
#' @inheritParams send
#' @param absolute logical value indicating whether the difference values 
#' should be converted to the absolute difference
#' 
#' @examples 
#' effects <- ~ difference("extraversion", absolute = TRUE) 
#' tomstats(effects, edgelist = history, attributes = info)
#' aomstats(choiceEffects = effects, edgelist = history, attributes = info)
#' 
#' @export 
difference <- function(variable, attributes = NULL, 
    scaling = c("as.is", "std"), absolute = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    temp <- scaling
    if(temp == "as.is" & !absolute) {scaling <- 1}
    if(temp == "as.is" & absolute) {scaling <- 2}  # absolute values
    if(temp == "std" & !absolute) {scaling <- 3}  # standardized values
    if(temp == "std" & absolute) {scaling <- 4}  # std absolute values

    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "difference", 
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'difference' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'difference' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "difference", 
            variable = variable,
            x = dat, 
            scaling = scaling)
    }
}

#' average
#' 
#' Specifies the statistic for an `average` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#'  
#' @details 
#' An average effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the average of their values on 
#' this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the average of the values of actor \emph{i} and \emph{j} on the 
#' attribute at timepoint \emph{t}.  
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}} or \code{\link{aomstats}}. 
#' 
#' @inheritParams send
#' 
#' @examples 
#' effects <- ~ average("extraversion") 
#' tomstats(effects, edgelist = history, attributes = info)
#' aomstats(choiceEffects = effects, edgelist = history, attributes = info)
#' 
#' @export 
average <- function(variable, attributes = NULL, scaling = c("as.is", "std")) {
    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "average", 
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'average' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'average' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "average", 
            variable = variable,
            x = dat, 
            scaling = scaling)
    }
}

#' minimum
#' 
#' Specifies the statistic for a `minimum` effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @details 
#' A minimum effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the minimum of their values on 
#' this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the minimum of the values of actor \emph{i} and \emph{j} on the 
#' attribute at timepoint \emph{t}.   
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}}. 
#' 
#' @inheritParams send
#' 
#' @examples 
#' effects <- ~ minimum("extraversion") 
#' tomstats(effects, edgelist = history, attributes = info)
#' 
#' @export 
minimum <- function(variable, attributes = NULL, scaling = c("as.is", "std")) {
    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "minimum", 
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'minimum' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'minimum' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "minimum", 
            variable = variable,
            x = dat, 
            scaling = scaling)
    }
}

#' maximum
#' 
#' Specifies the statistic for a `maximum` effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @details 
#' A maximum effect refers to an exogenous actor attribute that affects dyad 
#' \emph{(i,j)}'s rate of interacting based on the maximum of their values on 
#' this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the maximum of the values of actor \emph{i} and \emph{j} on the 
#' attribute at timepoint \emph{t}.  
#' 
#' The \code{attributes} object should be constructed as follows: Each row 
#' refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
#' `id` column is required that contains the actor id (corresponding to the 
#' actor id's in the edgelist). A `time` column is required that contains the 
#' time when attributes change (set to zero if none of the attributes vary over 
#' time). Subsequent columns contain the attributes that are called in the 
#' specifications of exogenous statistics. Alternatively, a dataframe with 
#' attributes for all exogenous effects can be defined in the \code{attributes} 
#' argument of \code{\link{tomstats}}. 
#' 
#' @inheritParams send
#' 
#' @examples 
#' effects <- ~ maximum("extraversion") 
#' tomstats(effects, edgelist = history, attributes = info)
#' 
#' @export 
maximum <- function(variable, attributes = NULL, scaling = c("as.is", "std")) {
    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Prepare effect 
    if(is.null(attributes)) {
        list(
            effect = "maximum", 
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if(!(variable%in%colnames(attributes))) {
        stop(
            paste("Variable", variable, "not in attributes object for the 'maximum' effect."))
        }

        # Warning for missing values
        if(anyNA(attributes[,variable])) {
            warning("Missing values in the attributes object for the 'maximum' effect can cause unexpected behavior.")
        }

        # Collect the information in a dataframe
        dat <- data.frame(
            id = attributes$id,
            time = attributes$time,
            x = attributes[,variable]
        )
        
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variable

        # Output
        list(
            effect = "maximum", 
            variable = variable,
            x = dat, 
            scaling = scaling)
    }
}

#' event
#' 
#' Specifies the statistic for an event effect in the \code{effects} argument 
#' of \code{\link{tomstats}}.
#' 
#' @details 
#' An event effect refers to an exogenous event attribute that affects the 
#' waiting time between events. The statistic at timepoint \emph{t} is for all 
#' dyads in the riskset equal to the attribute of the event at timepoint 
#' \emph{t}. 
#' 
#' @param variable string with the name of the column in the 
#' \code{edgelist} object supplied to \code{\link{tomstats}} with the 
#' event attribute. 
#' 
#' @seealso \code{\link{FEtype}}
#' 
#' @examples 
#' history$work <- ifelse(history$setting == "work", 1, 0)
#' effects <- ~ event("work")
#' tomstats(effects, edgelist = history)
#' 
#' @export 
event <- function(variable) {
    # Output
    list(
        effect = "event",
        variable = variable,
        scaling = 1
    )
}

#' FEtype
#' 
#' Specifies the statistic for fixed effects for event types in the 
#' \code{effects} argument of \code{\link{tomstats}}.
#' 
#' @details 
#' Fixed effects for event types capture the variation in event rate across 
#' different event types (e.g., see Butts, 2008). The specification of FEtype 
#' in the \code{effects} argument of \code{\link{tomstats}} results in the 
#' specification of C-1 statistics, were C is the number of different event 
#' types in the riskset. Let one of the event types, e.g. \emph{c = 1}, 
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
#' effects <- ~ FEtype()
#' tomstats(effects, edgelist = history)
#' 
#' @export 
FEtype <- function() {
    # Output
    list(
        effect = "FEtype",
        scaling = 1
    )
}

#' inertia
#' 
#' Specifies the statistic for an inertia effect in the \code{effects} argument 
#' of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details
#' An inertia effect refers to the tendency for actors to repeat past 
#' interactions. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
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
#' not scale the statistic but keep the raw counts 'as.is'. Alternatively, the 
#' statistics can be scaled by specifying 'prop', in which raw counts are 
#' divided by the outdegree of the sender at time t (see 'details') or 
#' standardization of the raw counts per time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the number of past 
#' events separately for each event type (TRUE) or sum across different event 
#' types (FALSE, default).
#' 
#' @examples 
#' effects <- ~ inertia()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export 
inertia <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "inertia",
            scaling = scaling
        )
    } else {
        list(
            effect = "inertia.type",
            scaling = scaling
        )
    }
}

#' reciprocity
#' 
#' Specifies the statistic for a reciprocity effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{choiceEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details 
#' A reciprocity effect refers to the tendency for actors to reciprocate past 
#' interactions. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
#' equal to the number of \emph{(j,i)} events before timepoint \emph{t}. Note 
#' that a reciprocity effect is only defined for directed events. 
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
#' reciprocal events separately for each event type (TRUE) or sum across 
#' different event types (FALSE, default).
#' 
#' @examples 
#' effects <- ~ reciprocity()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export 
reciprocity <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "reciprocity",
            scaling = scaling
        )
    } else {
        list(
            effect = "reciprocity.type",
            scaling = scaling
        )
    }
}

#' indegreeSender
#' 
#' Specifies the statistic for an `indegreeSender` effect in the \code{effects} 
#' argument of \code{\link{tomstats}} or the \code{rateEffects} argument of 
#' \code{\link{aomstats}}.
#' 
#' @details 
#' An indegree of the sender effect refers to the tendency for actors to send 
#' events if they have received more past events. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to the number of events received by 
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
#' to not scale the statistic but keep the raw counts 'as.is'. Alternatively, 
#' scaling of the raw degree counts by the number of past events at time t can 
#' be requested with 'prop' or standardization of the raw degree counts per 
#' time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the degrees 
#' separately for each event type (TRUE) or sum degrees across different event 
#' types (FALSE, default).
#' 
#' @aliases degree indegree
#' @seealso \code{\link{indegreeReceiver}}, \code{\link{outdegreeSender}}, 
#' \code{\link{outdegreeReceiver}}, \code{\link{totaldegreeSender}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' effects <- ~ indegreeSender()
#' tomstats(effects, edgelist = history)
#' aomstats(rateEffects = effects, edgelist = history)
#'
#' @export
indegreeSender <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "indegreeSender",
            scaling = scaling
        )
    } else {
        list(
            effect = "indegreeSender.type",
            scaling = scaling
        )
    }
}

#' indegreeReceiver
#' 
#' Specifies the statistic for an `indegreeReceiver` effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' @details 
#' An indegree of the receiver effect refers to the tendency for actors to 
#' receive events if they have received more past events. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of events 
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
#' effects <- ~ indegreeReceiver()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export
indegreeReceiver <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "indegreeReceiver",
            scaling = scaling
        )
    } else {
        list(
            effect = "indegreeReceiver.type",
            scaling = scaling
        )
    }
}

#' outdegreeSender
#' 
#' Specifies the statistic for an `outdegreeSender` effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the \code{rateEffects} 
#' argument of \code{\link{aomstats}}.
#' 
#' @details 
#' An outdegree of the sender effect refers to the tendency for actors to send 
#' events if they have send more past events. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to the number of events send by 
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
#' effects <- ~ outdegreeSender()
#' tomstats(effects, edgelist = history)
#' aomstats(rateEffects = effects, edgelist = history)
#'
#' @export
outdegreeSender <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "outdegreeSender",
            scaling = scaling
        )
    } else {
        list(
            effect = "outdegreeSender.type",
            scaling = scaling
        )
    }
}

#' outdegreeReceiver
#' 
#' Specifies the statistic for an `outdegreeReceiver` effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' @details 
#' An outdegree of the receiver effect refers to the tendency for actors to 
#' receive events if they have send more past events. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of events 
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
#' effects <- ~ outdegreeReceiver()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export
outdegreeReceiver <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "outdegreeReceiver",
            scaling = scaling
        )
    } else {
        list(
            effect = "outdegreeReceiver.type",
            scaling = scaling
        )
    }
}

#' totaldegreeSender
#' 
#' Specifies the statistic for an `totaldegreeSender` effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the \code{rateEffects} 
#' argument of \code{\link{aomstats}}.
#' 
#' @details 
#' A total degree of the sender effect refers to the tendency for actors to 
#' send events if they have send and received more past events. The statistic 
#' at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of events 
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
#' to not scale the statistic but keep the raw counts 'as.is'. Alternatively, 
#' scaling of the raw degree counts by two times the number of past events at 
#' time t can be requested with 'prop' or standardization of the raw degree 
#' counts per time point can be requested with 'std'.
#' @param consider_type logical, indicates whether to count the degrees 
#' separately for each event type (TRUE) or sum degrees across different event 
#' types (FALSE, default).
#' 
#' @aliases totaldegree
#' @seealso \code{\link{indegreeSender}}, \code{\link{indegreeReceiver}}, 
#' \code{\link{outdegreeSender}}, \code{\link{outdegreeReceiver}}, or
#' \code{\link{totaldegreeReceiver}} for other types of degree effects.
#' 
#' @examples 
#' effects <- ~ totaldegreeSender()
#' tomstats(effects, edgelist = history)
#' aomstats(rateEffects = effects, edgelist = history)
#'
#' @export
totaldegreeSender <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "totaldegreeSender",
            scaling = scaling
        )
    } else {
        list(
            effect = "totaldegreeSender.type",
            scaling = scaling
        )
    }
}

#' totaldegreeReceiver
#' 
#' Specifies the statistic for an `totaldegreeReceiver` effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' @details 
#' A total degree of the receiver effect refers to the tendency for actors to 
#' receive events if they have send and received more past events. The 
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number 
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
#' effects <- ~ totaldegreeReceiver()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export
totaldegreeReceiver <- function(scaling = c("as.is", "prop", "std"), 
    consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "prop", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "totaldegreeReceiver",
            scaling = scaling
        )
    } else {
        list(
            effect = "totaldegreeReceiver.type",
            scaling = scaling
        )
    }
}

#' otp
#' 
#' Specifies the statistic for an outgoing two-path effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' @details 
#' An outgoing two-path effect refers to the tendency of dyads to interact if 
#' they have more past outgoing two-paths between them. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the minimum of past 
#' \emph{(i,h)}, \emph{(h,j)} events, summed over all actors \emph{h}. 
#' Optionally, a scaling method can be set with \code{scaling}. Note that an 
#' 'otp' effect is only defined for directed events. 
#' 
#' @param scaling the method for scaling the triad statistic. Default is to not 
#' scale the statistic but keep the raw counts 'as.is'. Alternatively, 
#' standardization of the raw counts per time point can be requested 
#' with 'std'. 
#' @param consider_type logical, indicates whether to count the two-paths 
#' separately for each event type (TRUE) or sum across different event 
#' types (FALSE, default).
#'
#' @aliases triad 
#' @seealso \code{\link{itp}}, \code{\link{osp}}, or \code{\link{isp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' effects <- ~ otp()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export  
otp <- function(scaling = c("as.is", "std"), consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "otp",
            scaling = scaling
        )
    } else {
        list(
            effect = "otp.type",
            scaling = scaling
        )
    }    
}

#' itp
#' 
#' Specifies the statistic for an incoming two-path effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}. 
#' 
#' @details
#' An incomping two-path effect refers to the tendency of dyads to interact if 
#' they have more past incoming two-paths between them. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the minimum of past 
#' \emph{(j,h)}, \emph{(h,i)} events, summed over all actors \emph{h}. 
#' Optionally, a scaling method can be set with \code{scaling}. Note that an 
#' 'itp' effect is only defined for directed events. 
#' 
#' @inheritParams otp
#' 
#' @seealso \code{\link{otp}}, \code{\link{osp}}, or \code{\link{isp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' effects <- ~ itp()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export  
itp <- function(scaling = c("as.is", "std"), consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "itp",
            scaling = scaling
        )
    } else {
        list(
            effect = "itp.type",
            scaling = scaling
        )
    }    
}

#' osp
#' 
#' Specifies the statistic for an outgoing shared partners effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' @details
#' An outgoing shared partners effect refers to the tendency of dyads to 
#' interact if they have more past outgoing shared partners between them. The 
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the 
#' minimum of past \emph{(i,h)}, \emph{(j,h)} events, summed over all actors 
#' \emph{h}. Optionally, a scaling method can be set with \code{scaling}. Note 
#' that an 'osp' effect is only defined for directed events. 
#' 
#' @param scaling the method for scaling the triad statistic. Default is to not 
#' scale the statistic but keep the raw 'counts'. Alternatively, 
#' standardization of the raw counts per time point can be requested 
#' with 'std'. 
#' @param consider_type logical, indicates whether to count the shared partners 
#' separately for each event type (TRUE) or sum across different event 
#' types (FALSE, default).
#' 
#' @seealso \code{\link{otp}}, \code{\link{itp}}, or \code{\link{isp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' effects <- ~ osp()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export  
osp <- function(scaling = c("as.is", "std"), consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "osp",
            scaling = scaling
        )
    } else {
        list(
            effect = "osp.type",
            scaling = scaling
        )
    }    
}

#' isp
#' 
#' Specifies the statistic for an incoming shared partners effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' @details
#' An incoming shared partners effect refers to the tendency of dyads to 
#' interact if they have more past incoming shared partners between them. The 
#' statistic at timepoint \emph{t} for dyad \emph{(i,j)} is equal to the 
#' minimum of past \emph{(h,i)}, \emph{(h,j)} events, summed over all actors 
#' \emph{h}. Optionally, a scaling method can be set with \code{scaling}. Note 
#' that an 'isp' effect is only defined for directed events. 
#' 
#' @inheritParams osp
#' 
#' @seealso \code{\link{otp}}, \code{\link{itp}}, or \code{\link{osp}} for 
#' other types of triadic effects for directed relational events and 
#' \code{\link{sp}} or \code{\link{spUnique}} for triadic effects for 
#' undirected relational events.
#' 
#' @examples 
#' effects <- ~ isp()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export  
isp <- function(scaling = c("as.is", "std"), consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "isp",
            scaling = scaling
        )
    } else {
        list(
            effect = "isp.type",
            scaling = scaling
        )
    }    
}

#' sp
#' 
#' Specifies the statistic for a shared partners effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @details
#' A shared partners effect refers to the tendency of dyads to interact if they 
#' have more past shared partners between them. The statistic at timepoint 
#' \emph{t} for dyad \emph{(i,j)} is equal to the minimum of past undirected 
#' \emph{(i,h)}, \emph{(j,h)} events, summed over all actors \emph{h}. 
#' Optionally, a scaling method can be set with \code{scaling}. Note that the 
#' `shared partners' effect is only defined for undirected events. 
#' 
#' @inheritParams osp
#' 
#' @seealso \code{\link{spUnique}} for another type of triadic effect for 
#' undirected relational events and \code{\link{otp}}, \code{\link{itp}}, 
#' \code{\link{osp}}, or \code{\link{isp}} for triadic effects for directed 
#' relational events.
#' 
#' @examples 
#' effects <- ~ sp()
#' tomstats(effects, edgelist = history, directed = FALSE)
#' 
#' @export  
sp <- function(scaling = c("as.is", "std"), consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "sp",
            scaling = scaling
        )
    } else {
        list(
            effect = "sp.type",
            scaling = scaling
        )
    }    
}

#' spUnique
#' 
#' Specifies the statistic for a unique shared partners effect in the 
#' \code{effects} argument of \code{\link{tomstats}}.
#' 
#' @details
#' A unique shared partners effect refers to the tendency of dyads to interact 
#' if they have more past unique shared partners between them. The statistic at 
#' timepoint \emph{t} for dyad \emph{(i,j)} is equal to the number of unique 
#' actors \emph{h} that both actors \emph{i} and \emph{j} interacted with in 
#' the past. Optionally, a scaling method can be set with \code{scaling}. Note 
#' that the `unique shared partners' effect is only defined for undirected 
#' events. 
#' 
#' @inheritParams osp
#' 
#' @seealso \code{\link{sp}} for another type of triadic effect for 
#' undirected relational events and \code{\link{otp}}, \code{\link{itp}}, 
#' \code{\link{osp}}, or \code{\link{isp}} for triadic effects for directed 
#' relational events.
#' 
#' @examples 
#' effects <- ~ spUnique()
#' tomstats(effects, edgelist = history, directed = FALSE)
#' 
#' @export  
spUnique <- function(scaling = c("as.is", "std"), consider_type = FALSE) {

    # Match scaling
    if(length(scaling) > 1) {scaling <- scaling[1]}
    scaling <- match(scaling, c("as.is", "std"))

    # Output
    if(!consider_type) {
        list(
            effect = "spUnique",
            scaling = scaling
        )
    } else {
        list(
            effect = "spUnique.type",
            scaling = scaling
        )
    }    
}

#' psABBA
#' 
#' Specifies the statistic for a pshift AB-BA effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @param consider_type logical, indicates whether to consider the event type 
#' in determining which dyads create a pshift (TRUE) or not (FALSE, default).
#' 
#' @details
#' The AB-BA pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-BA pshift refers to the tendency for immediate 
#' reciprocation (the next sender is the current receiver and the next receiver 
#' is the current sender). For each timepoint t, the psABBA statistic is equal 
#' to one for the dyad that will create the participation shift if it would 
#' occur in the edgelist at time t and equal to zero for the dyads that will 
#' not create this participation shift. If consider_type is set to TRUE, the 
#' type of the AB event and the type of the BA event have to be equal. If it is 
#' set to FALSE, the participation shift is set to one for every BA event, 
#' regardless of the event type. If multiple events in the edgelist occur at 
#' the same time point, the order of these events determines whether the 
#' p-shift is observed. Note that the AB-BA pshift is only defined for directed 
#' events. 
#' 
#' @aliases pshift 
#' @seealso \code{\link{psABBY}}, \code{\link{psABXA}}, \code{\link{psABXB}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' effects <- ~ psABBA()
#' tomstats(effects, edgelist = history)
#' 
#' @export
psABBA <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "psABBA",
            scaling = 1
        )   
    } else {
        list(
            effect = "psABBA.type",
            scaling = 1
        )
    }
}

#' psABBY
#' 
#' Specifies the statistic for a pshift AB-BY effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @inheritParams psABBA
#' 
#' @details
#' The AB-BY pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-BY pshift refers to a tendency for turn 
#' receiving (here, the next sender is the current receiver and the next 
#' receiver is not in the current event). For each timepoint t, the psABBY 
#' statistic is equal to one for the dyads that will create the participation 
#' shift if they would occur in the edgelist at time t and equal to zero for 
#' the dyads that will not create this participation shift. If consider_type is 
#' set to TRUE, the type of the AB event and the type of the BY events have to 
#' be equal. If it is set to FALSE, the participation shift is set to one for 
#' every BY event, regardless of the event type. If multiple events in the 
#' edgelist occur at the same time point, the order of these events determines 
#' whether the p-shift is observed. Note that the AB-BY pshift is only defined 
#' for directed events.  
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABXA}}, \code{\link{psABXB}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' effects <- ~ psABBY()
#' tomstats(effects, edgelist = history)
#' 
#' @export
psABBY <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "psABBY",
            scaling = 1
        )   
    } else {
        list(
            effect = "psABBY.type",
            scaling = 1
        )
    }
}

#' psABXA
#' 
#' Specifies the statistic for a pshift AB-XA effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @inheritParams psABBA
#' 
#' @details
#' The AB-XA pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-XA pshift refers to a tendency for turn 
#' usurping (here, the next sender is not in the current event and the next 
#' receiver is the current sender). For each timepoint t, the psABXA statistic 
#' is equal to one for the dyads that will create the participation shift if 
#' they would occur in the edgelist at time t and equal to zero for the dyads 
#' that will not create this participation shift. If consider_type is set to 
#' TRUE, the type of the AB event and the type of the XA events have to be 
#' equal. If it is set to FALSE, the participation shift is set to one for 
#' every XA event, regardless of the event type. If multiple events in the 
#' edgelist occur at the same time point, the order of these events determines 
#' whether the p-shift is observed. Note that the AB-XA pshift is only defined 
#' for directed events.  
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXB}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' effects <- ~ psABXA()
#' tomstats(effects, edgelist = history)
#' 
#' @export
psABXA <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "psABXA",
            scaling = 1
        )   
    } else {
        list(
            effect = "psABXA.type",
            scaling = 1
        )
    }
}

#' psABXB
#' 
#' Specifies the statistic for a pshift AB-XB effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @inheritParams psABBA
#' 
#' @details
#' The AB-XB pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-XB pshift refers to a tendency for turn 
#' usurping (here, the next sender is not in the current event and the next 
#' receiver is the current receiver). For each timepoint t, the psABXB 
#' statistic is equal to one for the dyads that will create the participation 
#' shift if they would occur in the edgelist at time t and equal to zero for 
#' the dyads that will not create this participation shift. If consider_type is 
#' set to TRUE, the type of the AB event and the type of the XB events have to 
#' be equal. If it is set to FALSE, the participation shift is set to one for 
#' every XB event, regardless of the event type. If multiple events in the 
#' edgelist occur at the same time point, the order of these events determines 
#' whether the p-shift is observed. Note that the AB-XB pshift is only defined 
#' for directed events.  
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}}, 
#' \code{\link{psABXY}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' effects <- ~ psABXB()
#' tomstats(effects, edgelist = history)
#' 
#' @export
psABXB <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "psABXB",
            scaling = 1
        )   
    } else {
        list(
            effect = "psABXB.type",
            scaling = 1
        )
    }
}

#' psABXY
#' 
#' Specifies the statistic for a pshift AB-XY effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @inheritParams psABBA
#' 
#' @details
#' The AB-XY pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-XY pshift refers to a tendency for turn 
#' usurping (here, the next sender and the next receiver are not in the current 
#' event). For each timepoint t, the psABXY statistic is equal to one for the 
#' dyads that will create the participation shift if they would occur in the 
#' edgelist at time t and equal to zero for the dyads that will not create this 
#' participation shift. If consider_type is set to TRUE, the type of the AB 
#' event and the type of the XY events have to be equal. If it is set to FALSE, 
#' the participation shift is set to one for every XY event, regardless of the 
#' event type. If multiple events in the edgelist occur at the same time point, 
#' the order of these events determines whether the p-shift is observed. Note 
#' that the AB-XY pshift is only defined for directed events.  
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}}, 
#' \code{\link{psABXB}} or \code{\link{psABAY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' effects <- ~ psABXY()
#' tomstats(effects, edgelist = history)
#' 
#' @export
psABXY <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "psABXY",
            scaling = 1
        )   
    } else {
        list(
            effect = "psABXY.type",
            scaling = 1
        )
    }
}

#' psABAY
#' 
#' Specifies the statistic for a pshift AB-AY effect in the \code{effects} 
#' argument of \code{\link{tomstats}}.
#' 
#' @inheritParams psABBA
#' 
#' @details
#' The AB-AY pshift effect refers to one of Gibson's (2003) dyadic 
#' participation shifts. The AB-AY pshift refers to a tendency for turn 
#' continuing (here, the next sender is the current sender and the next 
#' receiver is not in the current event). For each timepoint t, the psABAY 
#' statistic is equal to one for the dyads that will create the participation 
#' shift if they would occur in the edgelist at time t and equal to zero for 
#' the dyads that will not create this participation shift. If consider_type is 
#' set to TRUE, the type of the AB event and the type of the AY events have to 
#' be equal. If it is set to FALSE, the participation shift is set to one for 
#' every AY event, regardless of the event type. If multiple events in the 
#' edgelist occur at the same time point, the order of these events determines 
#' whether the p-shift is observed. Note that the AB-AY pshift is only defined 
#' for directed events.  
#' 
#' @seealso \code{\link{psABBA}}, \code{\link{psABBY}}, \code{\link{psABXA}}, 
#' \code{\link{psABXB}} or \code{\link{psABXY}} for other dyadic participation 
#' shifts. 
#' 
#' @examples 
#' effects <- ~ psABAY()
#' tomstats(effects, edgelist = history)
#' 
#' @export
psABAY <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "psABAY",
            scaling = 1
        )   
    } else {
        list(
            effect = "psABAY.type",
            scaling = 1
        )
    }
}

#' rrankSend
#' 
#' Specifies the statistic for a recency rank send effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
#' 
#' The rrankSend effect refers to a rank-based recency effect, as described in 
#' section 2.2.5 of Butts (2008). For each timepoint t, for directed dyad (i,j) 
#' the statistic is equal to the inverse of the rank of receiver j among the 
#' actors to which sender i has most recently send past events. Note that the 
#' 'rrankSend' effect is only defined for directed events. 
#' 
#' @param consider_type logical, indicates whether to determine the rank 
#' separately for each event type (TRUE) or regardless of event types (FALSE, 
#' default).
#' 
#' @aliases recencyRank rrank
#' @seealso \code{\link{rrankReceive}}, \code{\link{recencySendSender}}, 
#' \code{\link{recencySendReceiver}}, \code{\link{recencyReceiveSender}}, 
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for 
#' other type of recency effects
#' 
#' @examples 
#' effects <- ~ rrankSend()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export 
rrankSend <- function(consider_type = FALSE) {
    
    # Output
    if(!consider_type) {
        list(
            effect = "rrankSend",
            scaling = 1
        )
    } else {
        list(
            effect = "rrankSend.type",
            scaling = 1
        )
    }  
}

#' rrankReceive
#' 
#' Specifies the statistic for a recency rank receive effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
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
#' effects <- ~ rrankReceive()
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#'
#' @export 
rrankReceive <- function(consider_type = FALSE) {

     # Output
    if(!consider_type) {
        list(
            effect = "rrankReceive",
            scaling = 1
        )
    } else {
        list(
            effect = "rrankReceive.type",
            scaling = 1
        )
    }  
}


#' recencySendSender
#' 
#' Specifies the statistic for a recency send of sender effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the \code{rateEffects} 
#' argument of \code{\link{aomstats}}.
#' 
#' The recencySendSender effect refers to a recency statistic similar to what 
#' is described in Vu et al. (2017) and Mulder and Leenders (2019). For each 
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time 
#' that has past since sender i was last active as sender + 1). Note that the 
#' 'recencySendSender' effect is only defined for directed events. 
#' 
#' @param consider_type logical, indicates whether to compute the recency  
#' separately for each event type (TRUE) or regardless of event types (FALSE, 
#' default).
#' 
#' @seealso \code{\link{rrankSend}}, \code{\link{rrankReceive}}, 
#' \code{\link{recencySendReceiver}}, \code{\link{recencyReceiveSender}}, 
#' \code{\link{recencyReceiveReceiver}} and \code{\link{recencyContinue}} for 
#' other type of recency effects
#' 
#' @examples
#' effects <- ~ recencySendSender()
#' tomstats(effects, edgelist = history)
#' aomstats(rateEffects = effects, edgelist = history)
#' 
#' @export 
recencySendSender <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "recencySendSender",
            scaling = 1
        )
    } else {
        list(
            effect = "recencySendSender.type",
            scaling = 1
        )
    }  

}

#' recencySendReceiver
#' 
#' Specifies the statistic for a recency send of receiver effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
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
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export 
recencySendReceiver <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "recencySendReceiver",
            scaling = 1
        )
    } else {
        list(
            effect = "recencySendReceiver.type",
            scaling = 1
        )
    }  

}

#' recencyReceiveSender
#' 
#' Specifies the statistic for a recency receive of sender effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{rateEffects} argument of \code{\link{aomstats}}.
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
#' tomstats(effects, edgelist = history)
#' aomstats(rateEffects = effects, edgelist = history)
#' 
#' @export 
recencyReceiveSender <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "recencyReceiveSender",
            scaling = 1
        )
    } else {
        list(
            effect = "recencyReceiveSender.type",
            scaling = 1
        )
    }  

}

#' recencyReceiveReceiver
#' 
#' Specifies the statistic for a recency receive of receiver effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
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
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export 
recencyReceiveReceiver <- function(consider_type = FALSE) {

    # Output
    if(!consider_type) {
        list(
            effect = "recencyReceiveReceiver",
            scaling = 1
        )
    } else {
        list(
            effect = "recencyReceiveReceiver.type",
            scaling = 1
        )
    }  

}

#' recencyContinue
#' 
#' Specifies the statistic for a recency continue effect in the 
#' \code{effects} argument of \code{\link{tomstats}} or the 
#' \code{choiceEffects} argument of \code{\link{aomstats}}.
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
#' tomstats(effects, edgelist = history)
#' aomstats(choiceEffects = effects, edgelist = history)
#' 
#' @export 
recencyContinue <- function(consider_type = FALSE) {
	
	# Output
    if(!consider_type) {
        list(
            effect = "recencyContinue",
            scaling = 1
        )
    } else {
        list(
            effect = "recencyContinue.type",
            scaling = 1
        )
    }  
}