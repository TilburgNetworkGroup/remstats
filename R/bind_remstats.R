#' Combine two or more remstats objects
#'
#' Function to bind any number of remstats objects into one while duplicated
#' statistics in the combined object are removed based on their name.
#'
#' @param ... Any number of \code{\link{remstats}} objects. All the
#' \code{remstats} objects must have matching dimensions, except for the third
#' dimension. Note that duplicated statistics in the combined object are 
#' removed based on their name.
#'
#' @return \code{statistics } array with the combined statistics, where rows
#' refer to time points, columns refer to potential relational event (i.e.,
#' potential edges) in the risk set and slices refer to statistics
#'
#' @examples
#' library(remstats)
#' reh <- remify::remify(edgelist = history, model = "tie")
#' effects1 <- ~ inertia():send("extraversion") + otp()
#' stats1 <- tomstats(effects1, reh = reh, attr_actors = info)
#' effects2 <- ~ reciprocity()
#' stats2 <- tomstats(effects2, reh = reh, attr_actors = info)
#' statsC <- bind_remstats(stats1, stats2)
#'
#' @export
bind_remstats <- function(...) {
    arg.list <- list(...)

    # Check class
    stopifnot(
        "All objects should be of class remstats" =
            all(sapply(arg.list, function(x) any(class(x) == "remstats")))
    )

    firstclass <- class(arg.list[[1]])[1]
    if (!all(sapply(arg.list, function(x) class(x)[1] == firstclass))) {
        stop(paste0("All objects should be of class ", firstclass))
    }

    if (firstclass == "tomstats") {
        # Check risk set
        firstrs <- attr(arg.list[[1]], "riskset")
        stopifnot(
            "Risk sets of the remstats objects should be equal" =
                all(sapply(arg.list, function(x) attr(x, "riskset") == firstrs))
        )

        # Check names
        keep.names <- dimnames(arg.list[[1]])[[3]]
        keep.ids <- vector(mode = "list", length = length(arg.list))
        keep.ids[[1]] <- 1:(dim(arg.list[[1]])[3])
        for(i in 2:length(arg.list)) {
            current.names <- dimnames(arg.list[[i]])[[3]]
            keep.ids[[i]] <- which(!(current.names %in% keep.names))
            keep.names <- union(keep.names, current.names)
        }

        # Deal with empty keep.ids
        ids.length <- sapply(keep.ids, length)
        if (any(ids.length == 0)) {
            remove.arrays <- which(ids.length == 0)
            arg.list <- arg.list[-remove.arrays]
            keep.ids <- keep.ids[-remove.arrays]      
        }

        # Combine statistics
        statistics <- combine_stats(arg.list, keep.ids)

        # Names of the statistics
        # stat_names <- lapply(arg.list, function(x) dimnames(x)[[3]])
        # stat_names <- do.call(c, stat_names)
        # stat_names <- stat_names[!duplicated(stat_names)]
        dimnames(statistics) <- list(NULL, NULL, keep.names)

        # Duplicated stats warning
        n_stats <- sum(sapply(arg.list, function(x) dim(x)[3]))
        if (dim(statistics)[[3]] < n_stats) {
            warning("Duplicate statistics detected. Removing the duplicate slice.")
        }

        # Combine formula
        # form.list <- sapply(arg.list, function(x) attr(x, "form"))
        # processed_formulas <- sapply(form.list, function(formula) {
        #     formula_string <- as.character(formula)
        #     formula_string <- gsub("~", "", formula_string[-1])
        #     formula_string
        # })
        # combined_formula <- paste(processed_formulas, collapse = " + ")
        # final_formula <- paste("~", combined_formula)
        # final_formula <- stats::as.formula(final_formula)
        final_formula <- "Combined remstats object"

        # Format output
        class(statistics) <- class(arg.list[[1]])
        attr(statistics, "model") <- attr(arg.list[[1]], "model")
        attr(statistics, "formula") <- final_formula
        attr(statistics, "riskset") <- attr(arg.list[[1]], "riskset")
        attr(statistics, "adjmat") <- attr(arg.list[[1]], "adjmat")

        # Output
        return(statistics)
    }

    if (firstclass == "aomstats") {
        # Check actors set
        firstactors <- attr(arg.list[[1]], "actors")
        stopifnot(
            "Actors sets of the remstats objects should be equal" =
                all(sapply(arg.list, function(x) {
                    attr(x, "actors") ==
                        firstactors
                }))
        )

        statistics <- list()
        statistics$sender_stats <- NULL
        statistics$receiver_stats <- NULL

        # Combine rate statistics
        rate.list <- lapply(arg.list, function(x) x$sender_stats)
        # Filter empty list elements
        rate.list <- Filter(Negate(is.null), rate.list)
        if (length(rate.list) > 1) {
            # Check names
            keep.names <- dimnames(rate.list[[1]])[[3]]
            keep.ids <- vector(mode = "list", length = length(rate.list))
            keep.ids[[1]] <- 1:(dim(rate.list[[1]])[3])
            for(i in 2:length(rate.list)) {
                current.names <- dimnames(rate.list[[i]])[[3]]
                keep.ids[[i]] <- which(!(current.names %in% keep.names))
                keep.names <- union(keep.names, current.names)
            }

            # Deal with empty keep.ids
            ids.length <- sapply(keep.ids, length)
            if (any(ids.length == 0)) {
                remove.arrays <- which(ids.length == 0)
                rate.list <- rate.list[-remove.arrays]
                keep.ids <- keep.ids[-remove.arrays]      
            }

            # Combine statistics
            rate_statistics <- combine_stats(rate.list, keep.ids)

            # Names of the statistics
            # stat_names <- lapply(rate.list, function(x) dimnames(x)[[3]])
            # stat_names <- do.call(c, stat_names)
            # dimnames(rate_statistics) <- list(NULL, NULL, stat_names)
            dimnames(rate_statistics) <- list(NULL, NULL, keep.names)

            # Duplicated stats warning
            n_stats <- sum(sapply(arg.list, function(x) dim(x$sender_stats)[3]))
            if (dim(rate_statistics)[[3]] < n_stats) {
                warning("Duplicate statistics detected in the sender_stats. Removing the duplicate slice.")
            }

            # Save
            statistics$sender_stats <- rate_statistics

            # Combine formula
            # form.list <- sapply(arg.list, function(x) attr(x, "formula")$rate)
            # processed_formulas <- sapply(form.list, function(formula) {
            #     formula_string <- as.character(formula)
            #     formula_string <- gsub("~", "", formula_string[-1])
            #     formula_string
            # })
            # combined_formula <- paste(processed_formulas, collapse = " + ")
            # final_formula <- paste("~", combined_formula)
            # final_formula <- stats::as.formula(final_formula)
            final_formula <- "Combined remstats object"
            attr(statistics, "formula")$rate <- final_formula

        } else {

            if (length(rate.list) == 1) {
                statistics$sender_stats <- rate.list[[1]]
            }
            attr(statistics, "formula")$rate <- attr(statistics, "formula")$rate

        }

        # Combine choice statistics
        choice.list <- lapply(arg.list, function(x) x$receiver_stats)

        # Filter empty list elements
        choice.list <- Filter(Negate(is.null), choice.list)

        if (length(choice.list) > 1) {
            # Check names
            keep.names <- dimnames(choice.list[[1]])[[3]]
            keep.ids <- vector(mode = "list", length = length(choice.list))
            keep.ids[[1]] <- 1:(dim(choice.list[[1]])[3])
            for(i in 2:length(choice.list)) {
                current.names <- dimnames(choice.list[[i]])[[3]]
                keep.ids[[i]] <- which(!(current.names %in% keep.names))
                keep.names <- union(keep.names, current.names)
            }

            # Deal with empty keep.ids
            ids.length <- sapply(keep.ids, length)
            if (any(ids.length == 0)) {
                remove.arrays <- which(ids.length == 0)
                choice.list <- choice.list[-remove.arrays]
                keep.ids <- keep.ids[-remove.arrays]      
            }

            # Combine statistics
            choice_statistics <- combine_stats(choice.list, keep.ids)

            # Names of the statistics
            # stat_names <- lapply(choice.list, function(x) dimnames(x)[[3]])
            # stat_names <- do.call(c, stat_names)
            # dimnames(choice_statistics) <- list(NULL, NULL, stat_names)
            dimnames(choice_statistics) <- list(NULL, NULL, keep.names)

            # Duplicated stats warning
            n_stats <- sum(sapply(arg.list, function(x) dim(x$receiver_stats)[3]))
            if (dim(choice_statistics)[[3]] < n_stats) {
                warning("Duplicate statistics detected in the receiver_stats. Removing the duplicate slice.")
            }

            # Save
            statistics$receiver_stats <- choice_statistics

            # Combine formula
            # form.list <- sapply(arg.list, function(x) attr(x, "formula")$choice)
            # processed_formulas <- sapply(form.list, function(formula) {
            #     formula_string <- as.character(formula)
            #     formula_string <- gsub("~", "", formula_string[-1])
            #     formula_string
            # })
            # combined_formula <- paste(processed_formulas, collapse = " + ")
            # final_formula <- paste("~", combined_formula)
            # final_formula <- stats::as.formula(final_formula)
            final_formula <- "Combined remstats object"
            attr(statistics, "formula")$choice <- final_formula
        } else {
            if (length(choice.list) == 1) {
                statistics$receiver_stats <- choice.list[[1]]
            }
            attr(statistics, "formula")$choice <-
                attr(statistics, "formula")$choice
        }

        # Format output
        class(statistics) <- class(arg.list[[1]])
        attr(statistics, "model") <- attr(arg.list[[1]], "model")
        attr(statistics, "actors") <- attr(arg.list[[1]], "actors")

        # Output
        return(statistics)
    }
}
