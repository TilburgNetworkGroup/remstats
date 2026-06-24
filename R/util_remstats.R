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
#' 
#' # Load the data
#' data(history)
#' data(info)
#' 
#' # Prepare the data
#' reh <- remify::remify(edgelist = history, model = "actor")
#' 
#' # Obtain two different statistics objects
#' effects1 <- ~ inertia():receive("extraversion") + otp()
#' stats1 <- remstats(receiver_effects = effects1, reh = reh, attr_actors = info)
#' effects2 <- ~ reciprocity()
#' stats2 <- remstats(receiver_effects = effects2, reh = reh, attr_actors = info)
#' 
#' # Bind the two statistics objects
#' statsC <- bind_remstats(stats1, stats2)
#'
#' @export
bind_remstats <- function(...) {
	arg.list <- list(...)
	
	# Check class
	stopifnot(
		"All objects should be of class remstats or remstats_durem" =
			all(sapply(arg.list, function(x)
				any(class(x) == "remstats") || inherits(x, "remstats_durem")))
	)
	
	firstclass <- class(arg.list[[1]])[1]
	if (!all(sapply(arg.list, function(x) class(x)[1] == firstclass))) {
		stop(paste0("All objects should be of class ", firstclass))
	}
	
	# ── remstats_durem ────────────────────────────────────────────────────────
	if (firstclass == "remstats_durem") {
		
		# Helper: combine a list of 3-D arrays along dim 3, dropping duplicates
		.combine_stat_arrays <- function(arrays) {
			arrays <- Filter(Negate(is.null), arrays)
			if (length(arrays) == 0L) return(NULL)
			if (length(arrays) == 1L) return(arrays[[1L]])
			
			keep_names <- dimnames(arrays[[1L]])[[3L]]
			keep_ids   <- vector("list", length(arrays))
			keep_ids[[1L]] <- seq_len(dim(arrays[[1L]])[3L])
			for (i in seq(2L, length(arrays))) {
				cur_names  <- dimnames(arrays[[i]])[[3L]]
				keep_ids[[i]] <- which(!(cur_names %in% keep_names))
				keep_names <- union(keep_names, cur_names)
			}
			
			# Drop arrays where all stats are duplicates
			nonempty   <- sapply(keep_ids, length) > 0L
			arrays     <- arrays[nonempty]
			keep_ids   <- keep_ids[nonempty]
			
			out <- combine_stats(arrays, keep_ids)
			dimnames(out) <- list(NULL, NULL, keep_names)
			
			n_in  <- sum(sapply(arrays, function(x) dim(x)[3L]))
			n_dup <- n_in - dim(out)[3L]
			if (n_dup > 0L) {
				# Identify which names were duplicated
				all_names <- unlist(lapply(arrays, function(x) dimnames(x)[[3L]]))
				dup_names <- unique(all_names[duplicated(all_names)])
				# Baseline deduplication is expected in mixed formulas; only warn
				# if non-baseline stats were duplicated.
				non_baseline_dups <- dup_names[!grepl("^baseline", dup_names)]
				if (length(non_baseline_dups) > 0L)
					warning("Duplicate statistics detected: ",
									paste(non_baseline_dups, collapse = ", "),
									". Removing duplicate slices.", call. = FALSE)
			}
			
			out
		}
		
		combined_start <- .combine_stat_arrays(
			lapply(arg.list, function(x) x$start_stats))
		combined_end   <- .combine_stat_arrays(
			lapply(arg.list, function(x) x$end_stats))
		
		out <- list(
			start_stats = combined_start,
			end_stats   = combined_end,
			psi_start   = arg.list[[1L]]$psi_start,
			psi_end     = arg.list[[1L]]$psi_end
		)
		# Format output
		attr(out, "reh") <- attr(arg.list[[1L]], "reh")
		attr(out, "model") <- attr(arg.list[[1]], "model")
		attr(out, "subset") <- attr(arg.list[[1]], "subset")
		attr(out, "method") <- attr(arg.list[[1]], "method")
		
		class(out) <- c("remstats_durem", "remstats")
		
		return(out)
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
		attr(statistics, "subset") <- attr(arg.list[[1]], "subset")
		attr(statistics, "method") <- attr(arg.list[[1]], "method")
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
		attr(statistics, "subset") <- attr(arg.list[[1]], "subset")
		attr(statistics, "method") <- attr(arg.list[[1]], "method")
		
		# Output
		return(statistics)
	}
}


#' Select a subset of statistics from a remstats object
#'
#' @param object a \code{remstats} object (of class \code{tomstats},
#'   \code{aomstats}, or \code{remstats_durem})
#' @param tie_effects character vector of statistic names to keep (tomstats only)
#' @param sender_effects character vector of statistic names to keep (aomstats only)
#' @param receiver_effects character vector of statistic names to keep (aomstats only)
#' @param start_effects character vector of statistic names to keep (remstats_durem only)
#' @param end_effects character vector of statistic names to keep (remstats_durem only)
#'
#' @return a remstats object of the same class with only the selected statistics
#'
#' @export
select_stats <- function(object,
												 tie_effects      = NULL,
												 sender_effects   = NULL,
												 receiver_effects = NULL,
												 start_effects    = NULL,
												 end_effects      = NULL) {
	
	if (inherits(object, "remstats_durem")) {
		.select_stats_durem(object, start_effects, end_effects)
	} else if (inherits(object, "aomstats")) {
		.select_stats_aom(object, sender_effects, receiver_effects)
	} else if (inherits(object, "tomstats")) {
		.select_stats_tom(object, tie_effects)
	} else {
		stop("'object' must be a 'tomstats', 'aomstats', or 'remstats_durem' object")
	}
}

# Helper: subset a single 3D stats array by stat names
.subset_array <- function(arr, which) {
	if (is.null(which)) return(arr)
	if (is.null(arr)) {
		stop("cannot select statistics from a NULL array")
	}
	nms <- dimnames(arr)[[3]]
	bad <- setdiff(which, nms)
	if (length(bad) > 0L) {
		stop("statistic(s) not found: ", paste(bad, collapse = ", "))
	}
	out <- arr[, , which, drop = FALSE]
	attrs <- attributes(arr)
	attrs$dim      <- dim(out)
	attrs$dimnames <- dimnames(out)
	attrs$formula  <- NULL
	attributes(out) <- attrs
	out
}

.select_stats_tom <- function(object, which) {
	out <- .subset_array(object, which)
	attr(out, "formula") <- NULL
	out
}

.select_stats_aom <- function(object, sender, receiver) {
	object$sender_stats   <- .subset_array(object$sender_stats,   sender)
	object$receiver_stats <- .subset_array(object$receiver_stats, receiver)
	attr(object, "formula") <- NULL
	object
}

.select_stats_durem <- function(object, start, end) {
	st <- object$stacked
	if (!is.null(st)) {
		# Subset the fit-ready stacked design (the raw start/end arrays are
		# dropped at construction). `start`/`end` name the statistics to keep;
		# NULL keeps all for that process. Matching the array-era contract, only
		# the named statistics are retained — to keep the intercept, name
		# "baseline.start" / "baseline.end" explicitly.
		keep_s <- if (is.null(start)) st$stat_names_start else start
		keep_e <- if (is.null(end))   st$stat_names_end   else end
		bad <- c(setdiff(keep_s, st$stat_names_start),
				 setdiff(keep_e, st$stat_names_end))
		if (length(bad) > 0L)
			stop("statistic(s) not found: ", paste(bad, collapse = ", "))
		drop_cols <- c(setdiff(st$stat_names_start, keep_s),
					   setdiff(st$stat_names_end,   keep_e))
		keep_cols <- setdiff(names(st$remstats_stack), drop_cols)
		st$remstats_stack   <- st$remstats_stack[, keep_cols, drop = FALSE]
		st$stat_names_start <- keep_s
		st$stat_names_end   <- keep_e
		st$stat_names       <- c(keep_s, keep_e)
		object$stacked <- st
		attr(object, "formula") <- NULL
		return(object)
	}
	# Legacy fallback: object still carries the raw arrays.
	object$start_stats <- .subset_array(object$start_stats, start)
	object$end_stats   <- .subset_array(object$end_stats,   end)
	attr(object, "formula") <- NULL
	object
}
