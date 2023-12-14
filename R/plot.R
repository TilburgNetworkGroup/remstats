#' Plotting Relational Event Network Statistics Distributions
#'
#' Generate boxplots for a specified effect in a \code{\link{tomstats}}
#' object.
#'
#' @param x An object of class \code{\link{tomstats}} containing relational
#' event network statistics.
#' @param effect A character string specifying the name of the effect in 'x' or
#' an integer indicating the index of the effect to be plotted.
#' @param by A string indicating whether the statistic is plotted across
#' 'timepoints' (default) or 'dyads'.
#' @param subset An optional vector specifying a subset of timepoints or dyads
#' to be used for plotting. Per default, a maximum of 20 unique timepoints or 
#' dyads are plotted.
#' @param outliers A logical value specifying whether to include outliers in the
#' plot.
#' @param ... Additional arguments passed to bxp().
#'
#' @details
#' This function produces boxplots to visually represent the distribution of a
#' specified effect in a relational event network, as captured by a
#' \code{\link{tomstats}} object. The 'effect' parameter allows the user to
#' choose a specific effect for visualization, either by providing the effect's
#' name or its index within the 'tomstats' object. The 'by' parameter determines
#' whether the boxplots are created across different 'timepoints' or 'dyads'.
#' Additionally, an optional 'subset' parameter allows the user to focus on
#' specific timepoints or dyads. If 'subset' is not specified, a default
#' maximum of 20 unique timepoints or dyads are plotted. The 'outliers'
#' argument, when set to TRUE, includes the representation of outliers in the
#' boxplots. If set to FALSE, outliers are omitted from the visualization.
#'
#' The boxplots are based on the following summary statistics of the data: The
#' box in the middle represents the interquartile range (IQR) between the first
#' (Q1) and third quartile (Q3), and the line inside the box represents the
#' median. The whiskers extend from the box to the minimum and maximum values
#' within 1.5 times the IQR below Q1 or above Q3. Outliers beyond the whiskers
#' are plotted individually.
#'
#' @examples
#' library(remstats)
#' # Load data
#' data(history)
#' # Prepare data
#' reh <- remify::remify(edgelist = history[,1:3], model = "tie")
#' # Compute effects
#' stats <- remstats(reh, tie_effects = ~ inertia())
#' # Plot the 'inertia' distribution for 20 timepoints
#' boxplot(stats, effect = "inertia")
#' # Plot the 'inertia' distribution for 20 dyads
#' boxplot(stats, effect = "inertia", by = "dyads")
#' # Plot the 'inertia' distribution for dyads 2:5
#' boxplot(stats, effect = "inertia", by = "dyads", subset = 2:5)
#' 
#' @return no return value
#' 
#' @method boxplot tomstats
#' @export
boxplot.tomstats <- function(x, effect, by = "timepoints", subset = NULL, outliers = TRUE, ...) {
    # Check if 'x' is a 'tomstats' object
    if (!("tomstats" %in% class(x))) {
        stop("Expected a tie-oriented remstats object")
    }

    # Figure out which statistic to plot
    if (is.character(effect)) {
        if (!(effect %in% dimnames(x)[[3]])) {
            stop(paste0(effect, " not found in 'x'"))
        } else {
            stat <- x[, , effect]
            effect_name <- effect
        }
    } else if (is.numeric(effect)) {
        if (trunc(effect) > dim(x)[3]) {
            stop("'effect' index out of bound")
        } else {
            stat <- x[, , trunc(effect)]
            effect_name <- dimnames(x)[[3]][effect]
        }
    } else {
        stop("Expected 'effect' argument of type character or integer")
    }

    # Figure out whether to plot by time points or dyads
    by <- match.arg(by, c("timepoints", "dyads"))
    if (by == "dyads") {
        stat <- t(stat) # Now, rows represent dyads and cols represent timepoints
    }

    # Figure out the number of time points or dyads to plot
    if (is.null(subset)) {
        subset <- round(seq(1, nrow(stat), length.out = min(nrow(stat), 20)))
    }
    stat <- matrix(stat[subset, ], ncol = ncol(stat))

    # Calculate summary statistics
    my_summary <- function(x) {
        summary_stats <- quantile(x, prob = c(.25, .5, .75))
        lower <- max(min(x), summary_stats[1] - 1.5 * IQR(x))
        upper <- min(max(x), summary_stats[3] + 1.5 * IQR(x))
        summary_stats <- c(lower, summary_stats, upper)
        names(summary_stats)[c(1, 5)] <- c("lower", "upper")
        return(summary_stats)
    }

    summary_stats <- apply(stat, 1, function(x) {
        my_summary(x)
    })

    # Detect outliers
    if (outliers) {
        detect_outliers <- function(x, lower, upper) {
            x[(x < lower) | (x > upper)]
        }

        all_outliers <- lapply(1:ncol(summary_stats), function(i) {
            outliers <- detect_outliers(stat[i, ], summary_stats[1, i], summary_stats[5, i])
            if (length(outliers) > 0) cbind(outliers, i)
        })

        all_outliers <- Filter(function(x) !is.null(x), all_outliers)

        out <- unlist(lapply(all_outliers, function(x) x[, 1]))
        group <- unlist(lapply(all_outliers, function(x) x[, 2]))
        names(out) <- names(group) <- NULL
    } else {
        out <- NULL
        group <- NULL
    }

    # Prepare input for bxp
    data <- list(
        stats = summary_stats,
        n = rep(ncol(stat), ncol(summary_stats)),
        out = out,
        group = group
    )

    # Output
    capitalize <- function(x) {
        paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
    }
    bxp(data,
        main = capitalize(effect_name),
        xlab = capitalize(by), xaxt = "n",
        ylab = "Value", ...
    )
    axis(1, at = seq_along(subset), labels = subset)
}

#' Plotting Relational Event Network Statistics
#'
#' Generate boxplots for a specified effect in a \code{\link{aomstats}} object.
#'
#' @param x An object of class \code{\link{aomstats}} containing relational
#' event network statistics.
#' @param effect A string specifying the name of the effect in 'x' or an integer
#' indicating the index of the effect to be plotted.
#' @param model A string indicating whether the effect is in the 'sender' model
#' or the 'receiver' model.
#' @param by A string indicating whether the statistic is plotted across
#' 'timepoints' (default) or 'actors'.
#' @param subset An optional vector specifying a subset of timepoints or actors
#' to be used for plotting. Per default, a maximum of 20 unique timepoints or 
#' actors are plotted.
#' @param outliers A logical value specifying whether to include outliers in the
#' plot.
#' @param ... additional arguments passed to bxp().
#'
#' @details
#' This function produces boxplots to visually represent the distribution of a
#' specified effect in a relational event network, as captured by a
#' \code{\link{aomstats}} object. The 'effect' parameter allows the user to
#' choose a specific effect for visualization, either by providing the effect's
#' name or its index within the 'aomstats' object. The 'model' parameter
#' indicates whether the respective effect is in the 'sender' model or the
#' 'receiver' model. The 'by' parameter determines whether the boxplots are
#' created across different 'timepoints' or 'actors'. At the moment, by 'actors'
#' is only supported for the sender model. Additionally, an optional 'subset'
#' parameter allows the user to focus on specific timepoints or actors. If
#' 'subset' is not specified, a default maximum of 20 unique timepoints or
#' actors are plotted. The 'outliers' argument, when set to TRUE, includes the
#' representation of outliers in the boxplots. If set to FALSE, outliers are
#' omitted from the visualization.
#'
#' The boxplots are based on the following summary statistics of the data: The
#' box in the middle represents the interquartile range (IQR) between the first
#' (Q1) and third quartile (Q3), and the line inside the box represents the
#' median. The whiskers extend from the box to the minimum and maximum values
#' within 1.5 times the IQR below Q1 or above Q3. Outliers beyond the whiskers
#' are plotted individually.
#'
#' @examples
#' library(remstats)
#' # Load data
#' data(history)
#' # Prepare data
#' reh <- remify::remify(edgelist = history[,1:3], model = "actor")
#' # Compute effects
#' stats <- remstats(reh, sender_effects = ~ outdegreeSender())
#' # Plot the 'outdegreeSender' distribution for 20 timepoints
#' boxplot(stats, effect = "outdegreeSender", model = "sender")
#' # Plot the 'inertia' distribution for all 10 actors
#' boxplot(stats, effect = "outdegreeSender", model = "sender", by = "actors")
#' 
#' @method boxplot aomstats
#' @export
boxplot.aomstats <- function(x, effect, model, by = "timepoints", subset = NULL, outliers = TRUE, ...) {
    # Check if 'x' is a 'tomstats' object
    if (!("aomstats" %in% class(x))) {
        stop("Expected an actor-oriented remstats object")
    }

    # Figure out the model
    model <- match.arg(model, c("sender", "receiver"))

    # Figure out which statistic to plot
    if (is.character(effect)) {
        if (model == "sender") {
            if (!(effect %in% dimnames(x$sender_stats)[[3]])) {
                stop(paste0(effect, " not found in the sender model of 'x'"))
            } else {
                stat <- x$sender_stats[, , effect]
                effect_name <- effect
            }
        }
        if (model == "receiver") {
            if (!(effect %in% dimnames(x$receiver_stats)[[3]])) {
                stop(paste0(effect, " not found in the receiver model of 'x'"))
            } else {
                stat <- x$receiver_stats[, , effect]
                effect_name <- effect
            }
        }
    } else if (is.numeric(effect)) {
        if (model == "sender") {
            if (trunc(effect) > dim(x$sender_stats)[3]) {
                stop("'effect' index out of bound")
            } else {
                stat <- x$sender_stats[, , trunc(effect)]
                effect_name <- dimnames(x$sender_stats)[[3]][effect]
            }
        }
        if (model == "receiver") {
            if (trunc(effect) > dim(x$receiver_stats)[3]) {
                stop("'effect' index out of bound")
            } else {
                stat <- x$receiver_stats[, , trunc(effect)]
                effect_name <- dimnames(x$receiver_stats)[[3]][effect]
            }
        }
    } else {
        stop("Expected 'effect' argument of type character or integer")
    }

    # Figure out whether to plot by time points or actors
    by <- match.arg(by, c("timepoints", "actors"))
    if (model == "receiver" & by == "actors") {
        stop("'by' = 'actors' is only supported for the 'sender' model")
    }
    if (by == "actors") {
        stat <- t(stat) # Now, rows represent actors and cols represent timepoints
    }

    # Figure out the number of time points or actors to plot
    if (is.null(subset)) {
        subset <- round(seq(1, nrow(stat), length.out = min(nrow(stat), 20)))
    }
    stat <- matrix(stat[subset, ], ncol = ncol(stat))

    # Calculate summary statistics
    my_summary <- function(x) {
        summary_stats <- quantile(x, prob = c(.25, .5, .75))
        lower <- max(min(x), summary_stats[1] - 1.5 * IQR(x))
        upper <- min(max(x), summary_stats[3] + 1.5 * IQR(x))
        summary_stats <- c(lower, summary_stats, upper)
        names(summary_stats)[c(1, 5)] <- c("lower", "upper")
        return(summary_stats)
    }

    summary_stats <- apply(stat, 1, function(x) {
        my_summary(x)
    })

    # Detect outliers
    if (outliers) {
        detect_outliers <- function(x, lower, upper) {
            x[(x < lower) | (x > upper)]
        }

        all_outliers <- lapply(1:ncol(summary_stats), function(i) {
            outliers <- detect_outliers(stat[i, ], summary_stats[1, i], summary_stats[5, i])
            if (length(outliers) > 0) cbind(outliers, i)
        })

        all_outliers <- Filter(function(x) !is.null(x), all_outliers)

        out <- unlist(lapply(all_outliers, function(x) x[, 1]))
        group <- unlist(lapply(all_outliers, function(x) x[, 2]))
        names(out) <- names(group) <- NULL
    } else {
        out <- NULL
        group <- NULL
    }

    # Prepare input for bxp
    data <- list(
        stats = summary_stats,
        n = rep(ncol(stat), ncol(summary_stats)),
        out = out,
        group = group
    )

    # Output
    capitalize <- function(x) {
        paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
    }
    bxp(data,
        main = capitalize(effect_name),
        xlab = capitalize(by), xaxt = "n",
        ylab = "Value", ...
    )
    axis(1, at = seq_along(subset), labels = subset)
}

#' Plotting Relational Event Network Statistics Trajectories
#'
#' Generate line plots to visualize the trajectories of a specified effect in 
#' the sender model of a \code{\link{aomstats}} object.
#'
#' @param x An object of class \code{\link{aomstats}} containing relational
#' event network statistics.
#' @param effect A character string specifying the name of the effect in 'x' or
#' an integer indicating the index of the effect to be plotted.
#' @param subset An optional vector specifying a subset of actors to be used for
#' plotting. By default, a maximum of 5 unique actors are used for plotting.
#' @param ... Additional arguments passed to plot().
#'
#' @details
#' This function creates line plots to illustrate the temporal trajectories of a
#' specified effect in a relational event network, as captured in the sender 
#' model by a \code{\link{aomstats}} object. The 'effect' parameter allows users 
#' to choose a specific effect for visualization, either by providing the effect's 
#' name or its index within the 'aomstats' object. An optional 'subset' parameter 
#' enables users to focus on specific actors. If 'subset' is not specified, a 
#' default maximum of 5 unique actors is plotted. These actors are randomly selected 
#' to represent trajectories across the range of different endpoints for the effect
#' (excluding zero).
#' 
#' @examples
#' library(remstats)
#' # Load data
#' data(history)
#' # Prepare data
#' reh <- remify::remify(edgelist = history[,1:3], model = "actor")
#' # Compute effects
#' stats <- remstats(reh, sender_effects = ~ outdegreeSender())
#' # Plot the 'outdegreeSender' trajectories 5 actors
#' plot(stats, effect = "outdegreeSender")
#' # Plot the 'outdegreeSender' trajectory for a specific actor
#' plot(stats, effect = "outdegreeSender", subset = 10)
#' 
#' @method plot aomstats
#' @export
plot.aomstats <- function(x, effect, subset = NULL, ...) {
    # Check if 'x' is a 'aomstats' object
    if (!("aomstats" %in% class(x))) {
        stop("Expected an actor-oriented remstats object")
    }

    # Figure out which statistic to plot
    if (is.character(effect)) {
        if (!(effect %in% dimnames(x$sender_stats)[[3]])) {
            stop(paste0(effect, " not found in the sender model of 'x'"))
        } else {
            stat <- x$sender_stats[, , effect]
            effect_name <- effect
        }
    } else if (is.numeric(effect)) {
        if (trunc(effect) > dim(x$sender_stats)[3]) {
            stop("'effect' index out of bound")
        } else {
            stat <- x$sender_stats[, , trunc(effect)]
            effect_name <- dimnames(x$sender_stats)[[3]][effect]
        }
    } else {
        stop("Expected 'effect' argument of type character or integer")
    }

    # Figure out which actors to plot
    if (is.null(subset)) {
        lastrow <- stat[nrow(stat),]
        ids <- rank(lastrow, ties.method = "random")
        subset <- which(ids %in% round(quantile(ids[lastrow > 0], c(0, .25, .5, .75, 1))))
        subset <- unique(subset)
    }

    stat <- matrix(stat[, subset], ncol = length(subset))

    # Plot one
    capitalize <- function(x) {
        paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
    }
    plot(stat[, 1],
        type = "l", col = rainbow(n = length(subset))[1],
        main = capitalize(effect_name), xlab = "Time", ylab = "Value",
        ylim = c(min(stat) - 0.05 * (max(stat) - min(stat)), max(stat) + 0.05 * (max(stat) - min(stat))),
        lwd = 1.5, ...
    )
    if (ncol(stat) > 1) {
        # Plot the rest
        sapply(2:length(subset), function(i) {
            lines(stat[, i], col = rainbow(n = length(subset))[i], lwd = 1.5)
        })
    }

    # Add legend
    legend("topleft", legend = subset, col = rainbow(length(subset)), lty = 1, title = "Actors")
}

#' Plotting Relational Event Network Statistics Trajectories
#'
#' Generate line plots to visualize the trajectories of a specified effect in a
#' \code{\link{tomstats}} object.
#'
#' @param x An object of class \code{\link{tomstats}} containing relational
#' event network statistics.
#' @param effect A character string specifying the name of the effect in 'x' or
#' an integer indicating the index of the effect to be plotted.
#' @param subset An optional vector specifying a subset of dyads to be used for
#' plotting. By default, a maximum of 5 unique dyads are used for plotting.
#' @param ... Additional arguments passed to plot().
#'
#' @details
#' This function creates line plots to illustrate the temporal trajectories of a
#' specified effect in a relational event network, as captured by a
#' \code{\link{tomstats}} object. The 'effect' parameter allows users to choose a
#' specific effect for visualization, either by providing the effect's name or
#' its index within the 'tomstats' object. An optional 'subset' parameter enables
#' users to focus on specific dyads. If 'subset' is not specified, a default
#' maximum of 5 unique dyads is plotted. These dyads are randomly selected to
#' represent trajectories across the range of different endpoints for the effect
#' (excluding zero).
#' 
#' @examples
#' library(remstats)
#' # Load data
#' data(history)
#' # Prepare data
#' reh <- remify::remify(edgelist = history[,1:3], model = "tie")
#' # Compute effects
#' stats <- remstats(reh, tie_effects = ~ inertia())
#' # Plot the 'inertia' trajectories for 5 dyads
#' plot(stats, effect = "inertia")
#' # Plot the 'inertia' trajectory for a specific dyad
#' plot(stats, effect = "inertia", subset = 60)
#' 
#' @method plot tomstats
#' @export
plot.tomstats <- function(x, effect, subset = NULL, ...) {
    # Check if 'x' is a 'tomstats' object
    if (!("tomstats" %in% class(x))) {
        stop("Expected a tie-oriented remstats object")
    }

    # Figure out which statistic to plot
    if (is.character(effect)) {
        if (!(effect %in% dimnames(x)[[3]])) {
            stop(paste0(effect, " not found in 'x'"))
        } else {
            stat <- x[, , effect]
            effect_name <- effect
        }
    } else if (is.numeric(effect)) {
        if (trunc(effect) > dim(x)[3]) {
            stop("'effect' index out of bound")
        } else {
            stat <- x[, , trunc(effect)]
            effect_name <- dimnames(x)[[3]][effect]
        }
    } else {
        stop("Expected 'effect' argument of type character or integer")
    }

    # Figure out which dyads to plot
    if (is.null(subset)) {
        lastrow <- stat[nrow(stat),]
        ids <- rank(lastrow, ties.method = "random")
        subset <- which(ids %in% round(quantile(ids[lastrow > 0], c(0, .25, .5, .75, 1))))
        subset <- unique(subset)
    }

    stat <- matrix(stat[, subset], ncol = length(subset))

    # Plot one
    capitalize <- function(x) {
        paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
    }
    plot(stat[, 1],
        type = "l", col = rainbow(n = length(subset))[1],
        main = capitalize(effect_name), xlab = "Time", ylab = "Value",
        ylim = c(min(stat) - 0.05 * (max(stat) - min(stat)), max(stat) + 0.05 * (max(stat) - min(stat))), 
        lwd = 1.5, ...
    )
    if (ncol(stat) > 1) {
        # Plot the rest
        sapply(2:length(subset), function(i) {
            lines(stat[, i], col = rainbow(n = length(subset))[i], lwd = 1.5)
        })
    }

    # Add legend
    dyads <- attr(stats, "riskset")[subset,]
    #labels <- paste0(dyads$id, " (", dyads[,1], ", ", dyads[,2], ")")
    labels <- dyads$id
    legend("topleft", legend = labels, col = rainbow(length(subset)), lty = 1, title = "Dyads")
}
