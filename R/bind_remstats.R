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