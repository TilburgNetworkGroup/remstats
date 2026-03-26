# Split consider_type=TRUE statistics into C type-specific slices for the
# sampled (CCS) path. Works on [M x S x P] arrays where S = samp_num and
# columns are typed riskset row indices (via sample_map_cpp).
#
# For each effect p where consider_type[p]=TRUE:
#   - produces C slices [M x S], where slice c keeps value at (m,s) only if
#     the sampled dyad's typeID == types$typeID[c], else 0.
# For consider_type=FALSE effects: kept as-is (C++ already aggregated).
# For non-typed effects: kept as-is.
#
# [param] statistics  array [M x S x P] from compute_stats_tie_sampled
# [param] effects     parsed effects list, length P
# [param] consider_type  logical vector length P
# [param] types       data.frame with columns typeName, typeID (0-based)
# [param] riskset     matrix [D x 4], col 3 = 0-based typeID per typed riskset row
# [param] sample_map  integer matrix [M x S], 1-based riskset row indices
#
# [return] array [M x S x P_out] with updated dimnames[[3]]
split_type_slices_sampled <- function(statistics, effects, consider_type,
                                       types, riskset, sample_map,
                                       stats_by_type = NULL) {
	M          <- dim(statistics)[1]
	S          <- dim(statistics)[2]
	C          <- nrow(types)
	stat_names <- dimnames(statistics)[[3]]

	# [M x S] matrix of typeIDs for each sampled dyad (sample_map is 1-based row index)
	type_mat <- matrix(riskset[sample_map, 3L], nrow = M, ncol = S)

	new_slices <- list()

	for (p in seq_len(dim(statistics)[3])) {
		has_consider_type <- p <= length(effects) &&
			"consider_type" %in% names(effects[[p]])

		if (has_consider_type && consider_type[p] > 0L) {
			base_name <- stat_names[p]

			if (!is.null(stats_by_type)) {
				# Multi-pass available: use per-type values from stats_by_type
				if (consider_type[p] == 2L) {
					# "interact": C^2 slices, zero non-matching dyad type positions
					for (ci in seq_len(C)) {
						past_type_name <- types$typeName[ci]
						sbt <- stats_by_type[[ci]]
						p_sbt <- which(dimnames(sbt)[[3]] == base_name)
						if (length(p_sbt) == 0) next
						for (ci2 in seq_len(C)) {
							dyad_type_name <- types$typeName[ci2]
							dyad_type_id   <- types$typeID[ci2]
							slice_name <- paste0(base_name, ".", past_type_name, ".", dyad_type_name)
							slice2d <- sbt[, , p_sbt]
							slice2d[type_mat != dyad_type_id] <- 0
							new_mat <- array(slice2d, dim = c(M, S, 1))
							dimnames(new_mat)[[3]] <- slice_name
							new_slices[[length(new_slices) + 1]] <- new_mat
						}
					}
				} else {
					# "separate": C slices, no zeroing, use type-c filtered values
					for (ci in seq_len(C)) {
						type_name <- types$typeName[ci]
						slice_name <- paste0(base_name, ".", type_name)
						sbt <- stats_by_type[[ci]]
						p_sbt <- which(dimnames(sbt)[[3]] == base_name)
						if (length(p_sbt) == 0) next
						new_mat <- sbt[, , p_sbt, drop = FALSE]
						dimnames(new_mat)[[3]] <- slice_name
						new_slices[[length(new_slices) + 1]] <- new_mat
					}
				}
			} else if (consider_type[p] == 2L) {
				# "interact" without multi-pass: C^2 slices with zeroing
				for (ci in seq_len(C)) {
					past_type_name <- types$typeName[ci]
					for (ci2 in seq_len(C)) {
						dyad_type_name <- types$typeName[ci2]
						dyad_type_id   <- types$typeID[ci2]
						slice_name <- paste0(base_name, ".", past_type_name, ".", dyad_type_name)
						slice2d <- statistics[, , p]
						slice2d[type_mat != dyad_type_id] <- 0
						new_mat <- array(slice2d, dim = c(M, S, 1))
						dimnames(new_mat)[[3]] <- slice_name
						new_slices[[length(new_slices) + 1]] <- new_mat
					}
				}
			} else {
				# "separate" without multi-pass: C slices, no zeroing
				for (ci in seq_len(C)) {
					type_name  <- types$typeName[ci]
					slice_name <- paste0(base_name, ".", type_name)
					slice2d <- statistics[, , p]
					new_mat <- array(slice2d, dim = c(M, S, 1))
					dimnames(new_mat)[[3]] <- slice_name
					new_slices[[length(new_slices) + 1]] <- new_mat
				}
			}
		} else {
			# Keep as-is: consider_type=FALSE and non-typed effects are already
			# correctly computed by C++ (inertia now aggregates internally too).
			sl <- statistics[, , p, drop = FALSE]
			dimnames(sl)[[3]] <- stat_names[p]
			new_slices[[length(new_slices) + 1]] <- sl
		}
	}

	P_out  <- length(new_slices)
	snames <- sapply(new_slices, function(s) dimnames(s)[[3]])
	array(
		unlist(lapply(new_slices, function(s) s[, , 1])),
		dim      = c(M, S, P_out),
		dimnames = list(NULL, NULL, snames)
	)
}

# Assemble type-split statistics from C per-type C++ calls (ext=FALSE path).
#
# Called after running compute_stats_tie_sampled once per type (C calls total).
# stats_by_type[[ci]] is [M x samp_num x P] from the ci-th type's C++ call.
#
# For consider_type=TRUE effects:
#   - produces C named slices (e.g. "inertia.social", "inertia.work")
#   - slice ci gets its value from stats_by_type[[ci]]
#   - zeroed where type ci doesn't exist for that sampled dyad (type_exists)
# For consider_type=FALSE and non-typed effects:
#   - all C calls return the same aggregated value, use the first call
#
# [param] stats_by_type  list length C of arrays [M x samp_num x P]
# [param] effects        parsed effects list length P
# [param] consider_type  logical vector length P
# [param] types          data.frame with typeName, typeID (0-based), C rows
# [param] sample_map     integer matrix [M x samp_num], 0-based untyped indices
# [param] type_exists    list length D_base: logical vector length C
#
# [return] array [M x samp_num x P_out] with type-split dimnames[[3]]
assemble_type_slices_sampled <- function(stats_by_type, effects, consider_type,
                                          types, sample_map, type_exists) {
	M          <- dim(stats_by_type[[1]])[1]
	samp_num   <- dim(stats_by_type[[1]])[2]
	C          <- nrow(types)
	stat_names <- dimnames(stats_by_type[[1]])[[3]]

	new_slices <- list()

	for (p in seq_len(dim(stats_by_type[[1]])[3])) {
		has_consider_type <- p <= length(effects) &&
			"consider_type" %in% names(effects[[p]])

		if (has_consider_type && consider_type[p] > 0L) {
			# For each type ci, take the value from stats_by_type[[ci]].
			# Zero positions where type ci doesn't exist for the sampled dyad.
			base_name <- stat_names[p]
			for (ci in seq_len(C)) {
				type_name  <- types$typeName[ci]
				slice_name <- paste0(base_name, ".", type_name)

				slice2d <- stats_by_type[[ci]][, , p]   # [M x samp_num]
				# Zero positions where this type doesn't exist for the sampled dyad
				for (s in seq_len(samp_num)) {
					d_untyped  <- sample_map[, s] + 1L   # 1-based, length M
					# type_exists[[d]][ci] tells us if type ci exists for dyad d
					# sample_map is the same for all rows m, but d varies per row
					# Actually sample_map[m,s] varies per m too
					exists_vec <- sapply(seq_len(M), function(m)
						type_exists[[sample_map[m, s] + 1L]][ci])
					slice2d[!exists_vec, s] <- 0
				}
				new_mat <- array(slice2d, dim = c(M, samp_num, 1))
				dimnames(new_mat)[[3]] <- slice_name
				new_slices[[length(new_slices) + 1]] <- new_mat
			}
		} else {
			# consider_type=FALSE or non-typed: all C calls return same value, use first.
			sl <- stats_by_type[[1]][, , p, drop = FALSE]
			dimnames(sl)[[3]] <- stat_names[p]
			new_slices[[length(new_slices) + 1]] <- sl
		}
	}

	P_out  <- length(new_slices)
	snames <- sapply(new_slices, function(s) dimnames(s)[[3]])
	array(
		unlist(lapply(new_slices, function(s) s[, , 1])),
		dim      = c(M, samp_num, P_out),
		dimnames = list(NULL, NULL, snames)
	)
}

# Dual-compatibility normalizer for remify objects.
#
# Patches a reh object so that reh$meta and reh$ids are always populated,
# regardless of whether the object was produced by the old remify::remify()
# (which stores everything in attr()) or the new remify2() (which stores
# everything in named list elements). All downstream helpers can then safely
# use reh$meta$X and reh$ids$X without inline ternaries everywhere.
normalize_reh <- function(reh) {
  if (!is.null(reh$meta)) return(reh)  # already new-style, nothing to do

  # Build reh$meta from attributes
  reh$meta <- list(
    model      = attr(reh, "model"),
    directed   = attr(reh, "directed"),
    ordinal    = attr(reh, "ordinal"),
    weighted   = attr(reh, "weighted"),
    with_type  = isTRUE(attr(reh, "with_type")),
    riskset    = attr(reh, "riskset"),
    origin     = attr(reh, "origin"),
    dictionary = attr(reh, "dictionary")
  )

  # Build reh$ids from attributes / existing slots
  if (is.null(reh$ids)) {
    reh$ids <- list(
      dyad        = attr(reh, "dyad"),
      actor1      = attr(reh, "actor1"),
      actor2      = attr(reh, "actor2"),
      dyad_active = attr(reh, "dyad_active")
    )
    # Old remify stores these as columns on the edgelist_id matrix
    if (is.null(reh$ids$dyad) && !is.null(reh$edgelist_id)) {
      reh$ids$dyad   <- reh$edgelist_id[, "dyadID"]
      reh$ids$actor1 <- reh$edgelist_id[, "actor1ID"]
      reh$ids$actor2 <- reh$edgelist_id[, "actor2ID"]
    }
  }

  reh
}

# Prepare tomstats inputs
#
# This function prepares all the necessary information prior to using the cpp
# to compute statistics in tomstats. It processes the 'reh' object, matches
# memory settings, converts start and stop indices, prepares main effects,
# fixed effects, interaction effects, covariate information, and scaling
# information.
#
# [param] effects A formula object specifying the effects to be included in
# the analysis.
# [param] reh The reh object containing the event history data.
# [param] attr_actors (Optional) A data frame containing attribute data for the
# actors.
# [param] memory A character vector specifying the memory type, which can be
# one of "full", "window", "decay", or "interval".
# [param] memory_value (Optional) The value associated with the memory type.
# [param] start The starting index of the event history data to consider.
# [param] stop The ending index of the event history data to consider.
#
# [return] A list containing all the necessary information for input to the
# 'compute_stats_tie' function.
#
# [examples]
# inputs <- prepare_tomstats(
#   effects, reh, attr_actors, memory, memory_value,
#   start, stop
# )
prepare_tomstats <- function(
    effects, reh, attr_actors = NULL,
    attr_dyads = NULL, memory = c("full", "window", "decay", "interval"),
    memory_value = NA, start = 1, stop = Inf, method = c("pt", "pe")) {
  # Check if reh is of class remify
  if (!("remify" %in% class(reh))) {
    stop("Expected a reh object of class remify")
  }
  reh <- normalize_reh(reh)

  # Check if the reh object is prepared with the correct model argument
  if (reh$meta$model != "tie") {
    stop("The reh object should be prepared with the model argument set to `tie' if tie_effects are computed")
  }

  # Extract relevant elements from the prepared remify::remify object
  dyads <- unlist(reh$ids$dyad)
  actors <- reh$meta$dictionary$actors
  types <- reh$meta$dictionary$types

  # Convert actor IDs to cpp indexing
  actors$actorID <- actors$actorID - 1

  # Origin
  if (reh$meta$ordinal) {
    if (length(reh$meta$origin)==0) {
      reh$meta$origin <- reh$edgelist[1,1] - 1
    }
  }

  # Prepare the edgelist for cpp processing
  edgelist <- reh$edgelist[,1:3]
  edgelist$time <- cumsum(as.numeric(diff(c(reh$meta$origin, edgelist$time))))
  edgelist$actor1 <- unlist(reh$ids$actor1) - 1
  edgelist$actor2 <- unlist(reh$ids$actor2) - 1
  if (!reh$meta$directed) {
    edgelist[, c(2, 3)] <- t(apply(edgelist, 1, function(x) sort(c(x[2], x[3]))))
  }

  # Deal with event types
  if (is.null(types)) {
    # Prepare a default types object
    types <- data.frame(typeName = 0, typeID = 0)
  } else {
    # Convert type IDs to cpp indexing
    types$typeID <- types$typeID - 1
    edgelist$type <- reh$edgelist$type
    edgelist$type <- types$typeID[match(edgelist$type, types$typeName)]
  }

  # Prepare the event weights
  if (reh$meta$weighted) {
    weights <- as.numeric(reh$edgelist$weight)
  } else {
    weights <- rep(1, nrow(edgelist))
  }

  # Determine whether a typed riskset is needed.
  # A typed riskset is needed if:
  #   (a) the remify object was built with extend_riskset_by_type=TRUE, OR
  #   (b) C>1 and any effect requests consider_type=TRUE (prescan formula)
  with_type_riskset <- isTRUE(reh$meta$with_type_riskset)
  C <- nrow(types)

  needs_typed_riskset <- with_type_riskset || (C > 1 && {
    # Prescan: parse formula just enough to detect consider_type flags
    .pre_effects <- tryCatch(
      parse_formula(effects, "rem", reh$meta$ordinal),
      error = function(e) list()
    )
    any(sapply(.pre_effects, function(x) {
      !("consider_type" %in% names(x)) || {
        ct <- if ("consider_type" %in% names(x)) x$consider_type else "ignore"
        !(identical(ct, "ignore") || isFALSE(ct) || identical(ct, 0L) || identical(ct, 0))
      }
    }))
  })

  typeID_for_riskset <- if (needs_typed_riskset) types$typeID else 0L

  # Prepare the full risk set
  prepR <- get_riskset(
    actorID = actors$actorID,
    typeID = typeID_for_riskset,
    directed = reh$meta$directed
  )

  # Reduce risk set to "active" dyads only
  if (reh$meta$riskset == "active") {
    act_id <- setNames(actors$actorID, actors$actorName)

    if (isTRUE(reh$meta$riskset_source == "manual")) {
      # For manual risksets: dyad_map_active is unreliable because getDyad2()
      # decodes riskset_idx using a formula that differs from getDyadIndex() in
      # C++, producing wrong actor/type assignments and NA types for some dyads.
      # Derive the active set directly from reh$edgelist using actor/type names.
      el      <- reh$edgelist
      a1_raw  <- act_id[as.character(el$actor1)]
      a2_raw  <- act_id[as.character(el$actor2)]
      # For undirected: ensure canonical order (actor1 < actor2 by actorID)
      if (!isTRUE(reh$meta$directed)) {
        swap  <- !is.na(a1_raw) & !is.na(a2_raw) & a1_raw > a2_raw
        a1_el <- ifelse(swap, a2_raw, a1_raw)
        a2_el <- ifelse(swap, a1_raw, a2_raw)
      } else {
        a1_el <- a1_raw
        a2_el <- a2_raw
      }
      if (needs_typed_riskset) {
        type_id <- setNames(types$typeID, types$typeName)
        t_el    <- type_id[as.character(el$type)]
        obs_key <- paste(a1_el, a2_el, t_el, sep = "|")
        obs_key <- unique(obs_key[!is.na(a1_el) & !is.na(a2_el) & !is.na(t_el)])
        prepR_key    <- paste(prepR[, 1], prepR[, 2], prepR[, 3], sep = "|")
        dyad_map_key <- obs_key
      } else {
        obs_key <- paste(a1_el, a2_el, sep = "|")
        obs_key <- unique(obs_key[!is.na(a1_el) & !is.na(a2_el)])
        prepR_key    <- paste(prepR[, 1], prepR[, 2], sep = "|")
        dyad_map_key <- obs_key
      }
    } else {
      # For active risksets: dyad_map_active is reliable.
      dyad_map <- reh$index$dyad_map_active %||% reh$index$dyad_map
      a1_ids   <- act_id[as.character(dyad_map$actor1)]
      a2_ids   <- act_id[as.character(dyad_map$actor2)]
      if (needs_typed_riskset) {
        type_id      <- setNames(types$typeID, types$typeName)
        t_ids        <- type_id[as.character(dyad_map$type)]
        prepR_key    <- paste(prepR[, 1], prepR[, 2], prepR[, 3], sep = "|")
        dyad_map_key <- paste(a1_ids, a2_ids, t_ids, sep = "|")
      } else {
        # Untyped prepR: deduplicate actor pairs preserving riskset_idx order
        prepR_key    <- paste(prepR[, 1], prepR[, 2], sep = "|")
        pairs        <- paste(a1_ids, a2_ids, sep = "|")
        dyad_map_key <- pairs[!duplicated(pairs)]
      }
    }

    row_idx <- match(dyad_map_key, prepR_key)
    row_idx <- row_idx[!is.na(row_idx)]
    prepR <- prepR[row_idx, ]
    prepR[, 4] <- seq(0, nrow(prepR) - 1, 1)
  }

  # Get the risksetMatrix
  risksetMatrix <- convert_to_risksetMatrix(prepR, nrow(actors), nrow(types))

  # Match memory
  memory <- match.arg(memory)
  memory_value <- validate_memory(memory, memory_value)
  if (memory == "window") {
    # Change memory to interval (window is a special case)
    memory <- "interval"
    memory_value <- c(0, memory_value)
  }

  # Method for managing simultaneous events
  method <- match.arg(method)

  # Convert R start and stop indices to C++ (indexing starts at 0) and
  # check them
  subset <- prepare_subset(start, stop, edgelist, method, model = "tie")
  start <- subset$start
  stop <- subset$stop

  # Prepare main effects
  check_formula(effects)
  form <- effects
  effects <- parse_formula(form, "rem", reh$meta$ordinal)
  effectNames <- sapply(effects, function(x) x$effect)

  # Check correct specification effects
  if (!reh$meta$directed) {
    if (!all(effectNames %in% tie_effects(directed = FALSE))) {
      stop("Attempting to request effects that are not (yet) defined for undirected events")
    }
  }

  if (reh$meta$directed) {
    if (!all(effectNames %in% tie_effects(directed = TRUE))) {
      stop("Attempting to request effects that are not (yet) defined for directed events")
    }
  }

  # Prepare fixed effects
  if (any(effectNames == "FEtype")) {
    C <- nrow(types)
    if (C < 2) {
      stop("FEtype() is not defined when the number of event types is smaller than 2.")
    }
    FEeffects <- lapply(2:C, function(c) {
      x <- list()
      x$effect <- "FEtype"
      x$scaling <- 1
      x$typeName <- types$typeName[c]
      x$typeID <- types$typeID[c]
      x
    })

    pos <- which(effectNames == "FEtype")
    effects <- append(effects[-pos], FEeffects, pos - 1)
    effectNames <- append(effectNames[-pos], rep("FEtype", C - 1), pos - 1)
  }

  # Prepare interaction effects
  effects_int <- parse_int(form, "rem", effects, reh$meta$ordinal)
  effectNames <- append(effectNames, rep("interact", length(effects_int)), length(effectNames))
  interactions <- list()
  interactions[which(effectNames == "interact")] <- effects_int

  # Prepare covariate information
  covar <- process_covariate(effects, attr_actors, attr_dyads, actors, edgelist, reh, prepR)

  # Prepare scaling info (vector length p)
  scaling <- sapply(effects, function(x) {
    ifelse("scaling" %in% names(x), x$scaling, "none")
  })
  scaling <- append(scaling, rep("none", length(effects_int)), length(scaling))

  # Prepare consider_type info
  consider_type <- sapply(effects, function(x) {
    ct <- if ("consider_type" %in% names(x)) x$consider_type else "ignore"
    # Normalize: "ignore"/FALSE/0 -> 0L, "separate"/TRUE/1 -> 1L, "interact"/2 -> 2L
    if (identical(ct, "ignore") || isFALSE(ct) || identical(ct, 0L) || identical(ct, 0)) 0L
    else if (identical(ct, "separate") || isTRUE(ct) || identical(ct, 1L) || identical(ct, 1)) 1L
    else if (identical(ct, "interact") || identical(ct, 2L) || identical(ct, 2)) 2L
    else stop("consider_type must be \"ignore\", \"separate\", or \"interact\"")
  })
  consider_type <- append(consider_type, rep(1L, length(effects_int)), length(consider_type))
  # C++ consider_type logic:
  # "ignore"   (0): FALSE — aggregate all types in C++
  # "separate" (1): FALSE — aggregate all types in C++, R splits without zeroing
  # "interact" (2): TRUE  — compute own-type per dyad in C++, R zeroes non-matching
  # Coerce "interact" to "separate" when ext=FALSE: the C^2 dyad-type x past-type
  # interaction requires a typed riskset (ext=TRUE). With ext=FALSE there is no
  # dyad-type dimension to interact with, so "interact" is meaningless.
  if (!with_type_riskset && any(consider_type == 2L)) {
    warning('"interact" requires extend_riskset_by_type = TRUE; coercing to "separate".')
    consider_type[consider_type == 2L] <- 1L
  }

  consider_type_cpp <- consider_type == 2L

  # Check consider_type info: warn if "separate"/"interact" requested with only one type
  if (nrow(types) == 1 & any(consider_type > 0L)) {
    warning("'consider_type' = \"separate\" or \"interact\" has no effect with only one event type: treating as \"ignore\".")
  }

  # Check correct scaling inertia statistic
  if (!reh$meta$directed) {
    if (any(sapply(effects, function(x) x$effect == "inertia"))) {
      idx <- which(sapply(effects, function(x) x$effect == "inertia"))
      if (any(scaling[idx] == "prop")) {
        stop("Proportional scaling of the inertia effect is not defined for undirected events")
      }
    }
  }

  # Output
  list(
    form = form,
    effects = effects,
    effectNames = effectNames,
    edgelist = as.matrix(edgelist),
    weights = weights,
    actors = actors,
    types = types,
    riskset = prepR,
    risksetMatrix = risksetMatrix,
    memory = memory,
    memory_value = memory_value,
    scaling = scaling,
    consider_type     = consider_type,      # integer 0/1/2
    consider_type_cpp = consider_type_cpp,  # logical for C++
    covar = covar,
    interactions = interactions,
    start = start,
    stop = stop,
    method = method,
    with_type_riskset = with_type_riskset,
    needs_typed_riskset = needs_typed_riskset,
    C = C,
    prepR_untyped = if (needs_typed_riskset && !with_type_riskset)
      get_prepR_untyped(reh, actors) else NULL
  )
}

# Check if Formula Effects are Specified as Functions
#
# This function checks if the effects in a given formula are specified as
# functions, as expected. It throws an error if any effect is not specified as
# a function.
#
# [param] formula A formula object specifying the effects.
#
# [examples]
# formula1 <- ~ inertia()
# formula2 <- ~ inertia
# check_formula(formula1)  # No error
# check_formula(formula2)  # Throws an error
check_formula <- function(formula) {
  formula_str <- as.character(formula)
  effect_names <- gsub("~ ", "", formula_str) # Extract effect names
  effect_names <- setdiff(effect_names, "~") # Remove "~"
  effect_names <- strsplit(effect_names, "\\+\\s*")[[1]] # Split effect names by "+"
  effect_names <- gsub("\\s+", "", effect_names) # Remove spaces from effect names


  # Check if effects are specified as functions
  incorrect_effects <- effect_names[!grepl("\\(.*\\)", effect_names)]
  incorrect_effects <- incorrect_effects[!grepl("1", incorrect_effects)]

  if (length(incorrect_effects) > 0) {
    stop(paste(
      "The following effects are not specified as functions:",
      paste(incorrect_effects, collapse = ", ")
    ))
  }
}

# Validate memory settings
#
# This function validates the memory settings for a specific memory type. It
# checks the validity of the supplied 'memory_value' based on the selected
# 'memory' type.
#
# [param] memory The memory type, which can be one of "full", "window",
# "decay", or "interval".
# [param] memory_value The value associated with the memory type.
#
# [return] The validated memory value based on the memory type.
#
# [examples]
# validated_value <- validate_memory("window", 10)
validate_memory <- function(memory, memory_value) {
  if (memory == "full") {
    memory_value <- Inf
  }
  if (memory == "window") {
    stopifnot(
      "A 'memory_value' of numeric type should be supplied when memory is 'window'" =
        length(memory_value) == 1 && is.numeric(memory_value)
    )
  }
  if (memory == "decay") {
    stopifnot(
      "A 'memory_value' of numeric type should be supplied when memory is 'decay'" =
        length(memory_value) == 1 && is.numeric(memory_value)
    )
    stopifnot(
      "Invalid 'memory_value': must be a positive non-zero value." =
        memory_value[1] > 0
    )
  }
  if (memory == "interval") {
    stopifnot(
      "Two 'memory_value' values of numeric type should be supplied when memory is 'interval'" =
        length(memory_value) == 2 && is.numeric(memory_value)
    )
    stopifnot(
      "The first 'memory_value' value should be lower than the second value" =
        memory_value[1] < memory_value[2]
    )
  }

  return(memory_value)
}

# Get all tie effects
#
# This function returns a vector of all available tie effects that can be used
# in tie-oriented models.
#
# [return] A character vector containing all tie effects.
#
# [examples]
# tie_effects <- all_tie_effects()
all_tie_effects <- function() {
  c(
    "baseline",
    "FEtype",

    # Exogenous stats
    "send",
    "receive",
    "tie",
    "same",
    "difference",
    "average",
    "minimum",
    "maximum",
    "event",

    # Endogenous stats
    "inertia",
    "reciprocity",
    "indegreeSender",
    "indegreeReceiver",
    "outdegreeSender",
    "outdegreeReceiver",
    "totaldegreeSender",
    "totaldegreeReceiver",
    "totaldegreeDyad",
    "degreeMin",
    "degreeMax",
    "degreeDiff",
    "otp",
    "itp",
    "osp",
    "isp",
    "sp",
    "psABBA",
    "psABBY",
    "psABXA",
    "psABXB",
    "psABXY",
    "psABAY",
    "psABAB",
    "rrankSend",
    "rrankReceive",
    "recencyContinue",
    "recencySendSender",
    "recencySendReceiver",
    "recencyReceiveSender",
    "recencyReceiveReceiver",
    "userStat",
    "interact"
  )
}

# Add variable names to statistics
#
# This function adds variable names to the statistics object based on the
# specified effects and interactions. It modifies the dimnames of the
# statistics object to include the variable names for specific effects.
#
# [param] statistics The statistics object to which variable names will be
# added.
# [param] all_effects A vector containing all possible effects.
# [param] effectsN A numeric vector specifying the indices of the effects in
# 'all_effects' that are present in the statistics.
# [param] effects A list containing information about the effects, including
# the effect name and associated variable.
# [param] interactions A list containing information about the interactions,
# including the indices of the interacting effects.
#
# [return] The modified statistics object with variable names added.
#
# [examples]
# statistics <- add_variable_names(
#   statistics, all_effects, effectsN, effects,
#   interactions
# )
add_variable_names <- function(statistics, effectNames, effects, interactions) {
  # Helper function to add variable name to effect
  add_variable_name <- function(effect, variable) {
    if (!is.null(variable)) {
      paste0(effect, "_", variable)
    } else {
      effect
    }
  }

  # Dimnames statistics
  dimnames(statistics) <- list(NULL, NULL, effectNames)

  # Add variable name to exogenous statistics
  exogenous_indices <- which(sapply(effects, function(x) {
    "variable" %in% names(x)
  }))

  dimnames(statistics)[[3]][exogenous_indices] <- sapply(
    effects[exogenous_indices],
    function(x) add_variable_name(x$effect, x$variable)
  )

  # Add counter to event name
  event_effects <- grepl("event", dimnames(statistics)[[3]])
  if (sum(event_effects) > 1) {
    dimnames(statistics)[[3]][event_effects] <- paste0("event", 1:sum(event_effects))
  }

  # Add the type to FEtype name
  FEtype_effects <- grep("FEtype", dimnames(statistics)[[3]])
  if (sum(FEtype_effects) > 1) {
    dimnames(statistics)[[3]][FEtype_effects] <- paste0("FEtype_", unlist(
      sapply(effects, function(x) {
        x$typeName
      })
    ))
  }

  # Add .unique
  unique_effects <- sapply(effects, function(x) {
    ifelse("scaling" %in% names(x), isTRUE(grepl("unique", x$scaling)), FALSE)
  })
  if (any(unique_effects)) {
    dimnames(statistics)[[3]][unique_effects] <- paste0(dimnames(statistics)[[3]][unique_effects], ".unique")
  }

  # "ignore": no suffix (clean output, e.g. "inertia" not "inertia.TypeAgg")
  type_effects <- sapply(effects, function(x) {
    ct <- if ("consider_type" %in% names(x)) x$consider_type else "ignore"
    identical(ct, "ignore") || isFALSE(ct) || identical(ct, 0L) || identical(ct, 0)
  })

  # Add variable name to interaction statistics
  interaction_index <- which(effectNames == "interact")
  dimnames(statistics)[[3]][interaction_index] <- sapply(
    interactions[interaction_index],
    function(x) {
      paste0(
        dimnames(statistics)[[3]][as.numeric(x[1])],
        ":",
        dimnames(statistics)[[3]][as.numeric(x[2])]
      )
    }
  )

  statistics # Return the modified statistics object
}

variable_check <- function(effect, dat) {
  if (effect == "send" | effect == "receive") {
    if (is.logical(dat$x)) {
      # Transform logical to binary
      dat$x <- as.numeric(dat$x)
    } else if (is.factor(dat$x)) {
      x_levels <- levels(dat$x[, , dat$variable])

      if (length(x_levels) != 2) {
        stop(paste0("The variable in '", effect, "' must have two factor levels or must be numeric."))
      } else {
        # Convert to binary variable
        dat$x <- ifelse(dat$x == x_levels[2], 1, 0)
      }
    } else if(!is.numeric(dat$x)) {
      stop(paste0("The variable in '", effect, "' must be numeric."))
    }
  }

  if (effect == "same") {
    # Force x to be numeric
    dat$x <- as.numeric(as.factor(dat$x))
  }

  if (effect == "average") {
    # x has to be numeric
    if (!is.numeric(dat$x)) {
      stop("The variable in 'average' must be numeric.")
    }
  }

  if (effect == "minimum") {
    # x has to be numeric
    if (!is.numeric(dat$x)) {
      stop("The variable in 'minimum' must be numeric.")
    }
  }

  if (effect == "maximum") {
    # x has to be numeric
    if (!is.numeric(dat$x)) {
      stop("The variable in 'maximum' must be numeric.")
    }
  }

  if (effect == "difference") {
    # x has to be numeric
    if (!is.numeric(dat$x)) {
      stop("The variable in 'difference' must be numeric.")
    }
  }

  return(dat)
}

# Prepare exogenous effect
#
# This function prepares an exogenous effect by matching the scaling, checking
# the presence of the variable in the attr_actors object, and collecting the
# necessary information in a data frame.
#
# [param] effect The effect name.
# [param] variable The variable name.
# [param] attr_actors A data frame containing attribute data for the actors.
# [param] scaling The scaling option for the effect, which can be "as.is" or
# "std".
#
# [return] A list containing the prepared exogenous effect information.
#
# [examples]
# exo_effect <- prep_exo(effect, variable, attr_actors, scaling)
prep_exo <- function(effect, variable, attr_actors, scaling) {
  # Check variable
  if (!is.character(variable)) {
    stop("The 'variable' argument should be a string.")
  }

  # Prepare effect
  if (is.null(attr_actors)) {
    list(
      effect = effect,
      variable = variable,
      x = NULL,
      scaling = scaling
    )
  } else {
    # Check if the variable name is in the attr_actors object
    if (!(variable %in% colnames(attr_actors))) {
      stop(paste0("Variable '", variable, "' not in attr_actors object for the '", effect, "' effect."))
    }

    # Check if the time variable is available
    if (!("time" %in% colnames(attr_actors))) {
      if (anyDuplicated(attr_actors$name) > 0) {
        warning("Duplicated actor names detected in 'attr_actors'. Did you mean to add a 'time' variable?")
      } else {
        attr_actors$time <- 0
      }
    }
    if (anyNA(attr_actors$time)) {
      stop("Missing 'time' values detected in 'attr_actors'.")
    }

    # Collect the information in a data.frame
    dat <- data.frame(
      name = attr_actors$name,
      time = attr_actors$time,
      x = attr_actors[, variable]
    )

    # Check the data class of the variable
    dat <- variable_check(effect, dat)

    # Warning for missing values
    if (anyNA(dat)) {
      warning("Missing values in the attr_actors object for the '", effect, "' effect can cause unexpected behavior.")
    }

    # Set the third column name equal to the variable name
    colnames(dat)[3] <- variable

    # Output
    list(
      effect = effect,
      variable = variable,
      x = dat,
      scaling = scaling
    )
  }
}

# Parse formula
#
# This function parses a formula and extracts the effects specified in the
# formula.
#
# [param] formula The formula specifying the effects.
# [param] type The type of model being used, either "rem" (for the tie-oriented
# model) or "rateEffects" or "choiceEffects".
# [param] ordinal Logical indicating whether the event data is ordinal.
#
# [return] A list containing the parsed effects.
#
# [examples]
# parsed_effects <- parse_formula(formula, type, ordinal)
parse_formula <- function(formula, type, ordinal = FALSE) {
  ft <- stats::terms(formula)

  var <- attr(ft, "variables")
  var <- as.list(var)[-1]

  effects <- lapply(var, function(y) {
    eval(y)
  })

  if (type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
    effects <- append(
      effects,
      list(list(effect = "baseline")), 0
    )
  }
  if (type == "rateEffects" & attr(ft, "intercept") == 1) {
    effects <- append(
      effects,
      list(list(effect = "baseline")), 0
    )
  }

  attributes(effects)$model <- type
  effects
}

# Parse interaction terms
#
# This function parses a formula and extracts the interaction terms specified
# in the formula.
#
# [param] formula The formula specifying the interaction terms.
# [param] type The type of model being used, either "rem" or "rateEffects".
# [param] effects The list of effects parsed from the formula.
# [param] ordinal Logical indicating whether the event data is ordinal.
#
# [return] A list containing the parsed interaction terms.
#
# [examples]
# parsed_interactions <- parse_int(formula, type, effects, ordinal)
parse_int <- function(formula, type, effects, ordinal = FALSE) {
  ft <- stats::terms(formula)
  ft.order <- attr(ft, "order")
  ft.factor <- attr(ft, "factors")
  interactions <- which(ft.order > 1)
  interactions <- lapply(interactions, function(i) {
    if (type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
      which(ft.factor[, i] > 0) + 1
    } else if (type == "rateEffects" & attr(ft, "intercept") == 1) {
      which(ft.factor[, i] > 0) + 1
    } else {
      which(ft.factor[, i] > 0)
    }
  })

  interactions <- if (any(sapply(effects, function(x) x$effect == "FEtype")) &
    any(ft.order > 1)) {
    pos <- which(sapply(effects, function(x) x$effect == "FEtype"))
    unlist(lapply(1:length(interactions), function(i) {
      if (any(interactions[[i]] %in% pos)) {
        change <- which(!interactions[[i]] == min(pos))
        x <- interactions[[i]]
        x[change] <- x[change] + length(pos) - 1
        FE <- which(interactions[[i]] == min(pos))
        lapply(pos, function(p) {
          x[FE] <- p
          x
        })
      } else {
        list(interactions[[i]])
      }
    }), recursive = F)
  } else {
    interactions
  }

  attributes(interactions)$model <- type
  interactions
}

# Modify Riskset
#
# This function modifies the riskset object by mapping actor and type IDs to their corresponding names in the event history data.
#
# [param] riskset The riskset object to be modified.
# [param] reh The reh object representing the event history data.
# [param] actors The actors information.
# [param] types The types information.
#
# [return] The modified riskset object.
#
# Split consider_type=TRUE statistics into C type-specific slices.
#
# Called after compute_stats_tie / compute_stats_tie_sampled when C > 1 and
# a typed riskset was used internally (needs_typed_riskset = TRUE).
#
# For each statistic p where consider_type[p] = TRUE:
#   - ext=TRUE  (with_type_riskset): keeps D_typed columns, zeros out
#               the non-matching type columns -> C slices of [M x D_typed]
#   - ext=FALSE (!with_type_riskset): extracts the D_dyad columns for that
#               type block only -> C slices of [M x D_dyad]
# For consider_type=FALSE statistics:
#   - ext=FALSE: strips from D_typed to first D_dyad columns (all blocks hold
#               the same aggregated value after transform_inertia)
#
# [param] statistics  array [M x D x P] returned by compute_stats_tie*
# [param] effects     parsed effects list (length P, before interactions)
# [param] consider_type  logical vector length P
# [param] types       data.frame with columns typeName, typeID (0-based)
# [param] prepR_typed matrix [D_typed x 4] — the typed riskset passed to C++
# [param] with_type_riskset  logical: was ext=TRUE?
#
# [return] array [M x D_out x P_out] with updated dimnames[[3]]
#
split_type_slices <- function(statistics, effects, consider_type,
                               types, prepR_typed, with_type_riskset,
                               prepR_untyped = NULL,
                               stats_by_type = NULL) {
  C       <- nrow(types)
  D_typed <- dim(statistics)[2]
  type_ids <- prepR_typed[, 3]   # 0-based typeID per riskset row

  # For ext=FALSE: build actor-pair key for typed and untyped risksets so we
  # can map typed columns into the D_dyad output space via key-matching.
  if (!with_type_riskset && !is.null(prepR_untyped)) {
    D_dyad      <- nrow(prepR_untyped)
    key_untyped <- paste(prepR_untyped[, 1], prepR_untyped[, 2], sep = "|")
    key_typed   <- paste(prepR_typed[, 1],   prepR_typed[, 2],   sep = "|")
  } else {
    D_dyad <- D_typed %/% C
  }

  stat_names <- dimnames(statistics)[[3]]
  new_slices <- list()

  for (p in seq_len(dim(statistics)[3])) {
    # Only split endogenous effects that explicitly carry consider_type=TRUE.
    # Exogenous effects (baseline, send, receive, tie, same, ...) and
    # interaction terms never have a consider_type field and must not be split.
    has_consider_type <- p <= length(effects) &&
      "consider_type" %in% names(effects[[p]])
    if (has_consider_type && consider_type[p] > 0L) {
      base_name <- stat_names[p]

      if (!is.null(stats_by_type)) {
        # ext=TRUE with multi-pass: use per-type statistics from stats_by_type
        # stat_names in stats_by_type[[ci]] match base_name
        if (consider_type[p] == 2L) {
          # "interact": C^2 slices named stat.pasttype.dyadtype
          for (ci in seq_len(C)) {
            past_type_name <- types$typeName[ci]
            # Find this effect in stats_by_type[[ci]]
            sbt <- stats_by_type[[ci]]
            p_sbt <- which(dimnames(sbt)[[3]] == base_name)
            if (length(p_sbt) == 0) next
            for (ci2 in seq_len(C)) {
              dyad_type_name <- types$typeName[ci2]
              dyad_type_id   <- types$typeID[ci2]
              slice_name <- paste0(base_name, ".", past_type_name, ".", dyad_type_name)
              new_mat <- sbt[, , p_sbt, drop = FALSE]
              new_mat[, which(type_ids != dyad_type_id), ] <- 0
              dimnames(new_mat)[[3]] <- slice_name
              new_slices[[length(new_slices) + 1]] <- new_mat
            }
          }
        } else {
          # "separate": C slices named stat.pasttype
          for (ci in seq_len(C)) {
            type_id   <- types$typeID[ci]
            type_name <- types$typeName[ci]
            slice_name <- paste0(base_name, ".", type_name)
            sbt <- stats_by_type[[ci]]
            p_sbt <- which(dimnames(sbt)[[3]] == base_name)
            if (length(p_sbt) == 0) next
            if (with_type_riskset) {
              # ext=TRUE: use D_typed output directly
              new_mat <- sbt[, , p_sbt, drop = FALSE]
            } else {
              # ext=FALSE: map type-c rows of typed riskset to D_dyad output
              type_rows <- which(type_ids == type_id)
              new_mat   <- array(0, dim = c(dim(sbt)[1], D_dyad, 1))
              if (!is.null(prepR_untyped)) {
                dst <- match(key_typed[type_rows], key_untyped)
                valid <- !is.na(dst)
                new_mat[, dst[valid], 1] <- sbt[, type_rows[valid], p_sbt]
              } else {
                new_mat[, seq_along(type_rows), 1] <- sbt[, type_rows, p_sbt]
              }
            }
            dimnames(new_mat)[[3]] <- slice_name
            new_slices[[length(new_slices) + 1]] <- new_mat
          }
        }
        next
      }

      if (with_type_riskset && consider_type[p] == 2L) {
        # "interact" + ext=TRUE (no multi-pass fallback): C^2 slices
        for (ci in seq_len(C)) {
          past_type_name <- types$typeName[ci]
          for (ci2 in seq_len(C)) {
            dyad_type_name <- types$typeName[ci2]
            dyad_type_id   <- types$typeID[ci2]
            slice_name <- paste0(base_name, ".", past_type_name, ".", dyad_type_name)
            new_mat <- statistics[, , p, drop = FALSE]
            new_mat[, which(type_ids != dyad_type_id), ] <- 0
            dimnames(new_mat)[[3]] <- slice_name
            new_slices[[length(new_slices) + 1]] <- new_mat
          }
        }
        next
      }

      # "separate" (or "interact" + ext=FALSE): C slices named stat.pasttype
      for (ci in seq_len(C)) {
        type_id   <- types$typeID[ci]
        type_name <- types$typeName[ci]
        slice_name <- paste0(base_name, ".", type_name)

        if (with_type_riskset) {
          # ext=TRUE, "separate" (no multi-pass): no zeroing
          new_mat <- statistics[, , p, drop = FALSE]
        } else {
          # ext=FALSE: map typed columns for this type into D_dyad output space.
          # Typed riskset rows for this type may not cover all untyped dyads
          # (active/manual risksets), so we start with a zero D_dyad matrix and
          # fill in matched positions.
          type_rows <- which(type_ids == type_id)
          new_mat   <- array(0, dim = c(dim(statistics)[1], D_dyad, 1))
          if (!is.null(prepR_untyped)) {
            dst <- match(key_typed[type_rows], key_untyped)
            valid <- !is.na(dst)
            new_mat[, dst[valid], 1] <- statistics[, type_rows[valid], p]
          } else {
            # Fallback: typed rows == dyad rows (full riskset, equal counts per type)
            new_mat[, seq_along(type_rows), 1] <- statistics[, type_rows, p]
          }
        }
        dimnames(new_mat)[[3]] <- slice_name
        new_slices[[length(new_slices) + 1]] <- new_mat
      }
    } else {
      # Keep as-is; strip to D_dyad columns for ext=FALSE.
      sl <- statistics[, , p, drop = FALSE]
      if (!with_type_riskset && dim(sl)[2] == D_typed) {
        if (!is.null(prepR_untyped)) {
          # Use key-matching: for each untyped dyad, find its first matching
          # column in the typed riskset (any type). Using type1_rows would
          # miss dyads that only appear in other types (e.g. work-only dyads).
          dst <- match(key_untyped, key_typed)
          valid <- !is.na(dst)
          new_sl <- array(0, dim = c(dim(sl)[1], D_dyad, 1))
          new_sl[, which(valid), 1] <- sl[, dst[valid], 1]
          sl <- new_sl
        } else {
          sl <- sl[, seq_len(D_dyad), , drop = FALSE]
        }
      }
      dimnames(sl)[[3]] <- stat_names[p]
      new_slices[[length(new_slices) + 1]] <- sl
    }
  }

  # Reconstruct cube (base R, no abind dependency)
  M      <- dim(new_slices[[1]])[1]
  D_out  <- dim(new_slices[[1]])[2]
  P_out  <- length(new_slices)
  snames <- sapply(new_slices, function(s) dimnames(s)[[3]])
  array(
    unlist(lapply(new_slices, function(s) s[, , 1])),
    dim      = c(M, D_out, P_out),
    dimnames = list(NULL, NULL, snames)
  )
}


# Compute the raw untyped prepR matrix for the ext=FALSE case.
# Returns the integer matrix [D_dyad x 4] with 0-based actor/type/dyad IDs.
#
get_prepR_untyped <- function(reh, actors) {
  prepR_untyped <- get_riskset(
    actorID  = actors$actorID,
    typeID   = 0L,
    directed = reh$meta$directed
  )
  if (reh$meta$riskset == "active") {
    # Use dyad_map_active with actor-pair matching (same logic as prepare_tomstats).
    # dyad_map_active contains the correct active untyped dyads in riskset_idx order.
    dyad_map <- reh$index$dyad_map_active %||% reh$index$dyad_map
    act_id   <- setNames(actors$actorID, actors$actorName)
    a1_ids   <- act_id[as.character(dyad_map$actor1)]
    a2_ids   <- act_id[as.character(dyad_map$actor2)]
    pairs    <- paste(a1_ids, a2_ids, sep = "|")
    uniq_pairs <- pairs[!duplicated(pairs)]
    prepR_key  <- paste(prepR_untyped[, 1], prepR_untyped[, 2], sep = "|")
    row_idx    <- match(uniq_pairs, prepR_key)
    row_idx    <- row_idx[!is.na(row_idx)]
    prepR_untyped <- prepR_untyped[row_idx, ]
    prepR_untyped[, 4] <- seq(0, nrow(prepR_untyped) - 1, 1)
  }
  prepR_untyped
}

# Build the user-facing riskset data.frame for the ext=FALSE case, where the
# typed riskset was used internally but the output riskset should be untyped.
#
build_untyped_riskset <- function(reh, actors, types) {
  modify_riskset(get_prepR_untyped(reh, actors), reh, actors, types[1L, , drop = FALSE])
}

# [examples]
# modified_riskset <- modify_riskset(riskset, reh, actors, types)
modify_riskset <- function(riskset, reh, actors, types) {
  riskset <- as.data.frame(riskset)

  if (reh$meta$directed) {
    colnames(riskset) <- c("sender", "receiver", "type", "id")
    riskset$sender <- actors$actorName[match(riskset$sender, actors$actorID)]
    riskset$receiver <- actors$actorName[match(riskset$receiver, actors$actorID)]
    riskset$type <- types$typeName[match(riskset$type, types$typeID)]
  } else {
    colnames(riskset) <- c("actor1", "actor2", "type", "id")
    riskset$actor1 <- actors$actorName[match(riskset$actor1, actors$actorID)]
    riskset$actor2 <- actors$actorName[match(riskset$actor2, actors$actorID)]
    riskset$type <- types$typeName[match(riskset$type, types$typeID)]
  }

  riskset$id <- riskset$id + 1

  if (length(unique(riskset$type)) == 1) {
    riskset <- riskset[, -3]
  }

  riskset # Return the modified riskset object
}

# Process Covariate
#
# This function processes the covariates for different exogenous effects in
# the tie-oriented model.
#
# [param] effects A list of effects containing information about the covariates.
# [param] attr_actors The attr_actors object containing the covariate data.
# [param] actors The actors information.
# [param] edgelist The edgelist representing the network.
# [param] reh The reh object representing the event history data.
# [param] prepR The prepR object representing the risk set.
#
# [return] A list of processed covariates for each effect.
#
# [examples]
# covariates <- process_covariate(effects, attr_actors, actors, edgelist, reh, prepR)
process_covariate <- function(
    effects,
    attr_actors,
    attr_dyads,
    actors,
    edgelist,
    reh,
    prepR) {
  lapply(effects, function(x) {
    effect <- x$effect

    if (effect %in% c(
      "send",
      "receive",
      "same",
      "difference",
      "average",
      "minimum",
      "maximum"
    )) {
      if (is.null(x$x)) { # The variable has to be extracted from 'attr_actors'
        variable <- x$variable

        # Check if the variable name is in the 'attr_actors' object
        if (!(variable %in% colnames(attr_actors))) {
          stop(paste0("Variable '", variable, "' not in attr_actors object for the '", effect, "' effect."))
        }

        # Check if the 'time' variable is available
        if (!("time" %in% colnames(attr_actors))) {
          if (anyDuplicated(attr_actors$name) > 0) {
            warning("Duplicated actor names detected in 'attr_actors'. Did you mean to add a 'time' variable?")
          } else {
            attr_actors$time <- 0
          }
        }

        if (anyNA(attr_actors$time)) {
          stop("Missing 'time' values detected in 'attr_actors'.")
        }

        # Collect the information in a data.frame
        dat <- data.frame(
          name = attr_actors$name,
          time = attr_actors$time,
          x = attr_actors[, variable],
          stringsAsFactors = FALSE
        )

        # Check the data class of the variable
        dat <- variable_check(effect, dat)

        # Warning for missing values
        if (anyNA(dat)) {
          warning(paste0("Missing values in the attr_actors object for the '", effect, "' effect can cause unexpected behavior."))
        }
      } else { # The variable was supplied to the function in effects.R
        dat <- x$x
      }

      # Check the actors in the obtained 'dat' object
      dat$name <- as.character(dat$name)
      if (!all(actors[, 1] %in% dat$name)) {
        stop("Missing actors in the attr_actors object.")
      }

      # Convert 'actorName' to 'actorID'
      dat$name <- actors[match(dat$name, actors[, 1]), 2]

      # Set the third column name equal to the 'variable'
      colnames(dat)[3] <- x$variable

      # Order by actor ID
      dat <- dat[order(as.numeric(dat$name)), ]

      # Remove any redundant actors
      if (any(is.na(dat$name))) {
        warning(paste0("The 'attr_actors' object contains actors that are not in the risk set. These are not included in the computation of the statistics."))
        dat <- dat[!is.na(dat$name), ]
      }

      # Convert to matrix and output
      as.matrix(dat)
    } else if (effect %in% c("tie", "event", "FEtype")) {
      if (effect == "tie") {
        # Return: matrix (actor1, actor2, time, value)
        parse_tie(x, reh, attr_dyads)
      } else if (effect == "event") {
        if (NROW(x$x) != nrow(edgelist) & NROW(x$x) != NROW(unique(edgelist[, 1]))) {
          stop("Length of vector 'x' in event() does not match number of events in edgelist")
        }
        if (!is.numeric(x$x)) {
          x$x <- (as.numeric(factor(x$x)) - 1)
        }
        as.matrix(x$x)
      } else if (effect == "FEtype") {
        as.matrix(x$typeID)
      } 
    } else if (effect == "userStat") {
      if (NROW(x$x) != nrow(edgelist) & NROW(x$x) != NROW(unique(edgelist[, 1]))) {
        stop("Number of rows of matrix 'x' in userStat() does not match number of timepoints or number of events in edgelist")
      }

      if (NCOL(x$x) != nrow(prepR)) {
        stop("Number of columns of matrix 'x' in userStat() does not match number of dyads in risk set")
      }

      as.matrix(x$x)
    } else {
      matrix()
    }
  })
}

validate_aomstats_arguments <- function(attr_actors, reh) {
  reh <- normalize_reh(reh)
  # Check if the deprecated "id" column is used in attr_actors
  if (!is.null(attr_actors) & "id" %in% colnames(attr_actors) & !("name" %in% colnames(attr_actors))) {
    colnames(attr_actors) <- ifelse(colnames(attr_actors) == "id", "name", colnames(attr_actors))
    warning("Use 'name' instead of 'id' in 'attr_actors'")
  }

  # Check the reh class
  if (!inherits(reh, "remify")) {
    stop("Invalid argument: 'reh' must be a remify object.")
  }

  model <- reh$meta$model
  # Check the reh model
  if (is.null(model) || model != "actor") {
    stop("Invalid argument: 'reh' object should be prepared with the model argument set to 'actor' if effects for the actor-oriented model are computed.")
  }

  return(attr_actors)
}

prepare_aomstats_edgelist <- function(reh) {
  reh <- normalize_reh(reh)
  # Extract the edgelist
  edgelist <- reh$edgelist

  # Transform to cpp indexing
  edgelist[, 2] <- unlist(reh$ids$actor1) - 1
  edgelist[, 3] <- unlist(reh$ids$actor2) - 1

  # Origin
  if (reh$meta$ordinal) {
  	if (length(reh$meta$origin)==0) {
  		reh$meta$origin <- edgelist[1,1] - 1
  	}
  }

  # Transform to numeric
  edgelist[, 1] <- cumsum(
    as.numeric(
      diff(
        c(reh$meta$origin, edgelist[, 1])
      )
    )
  )

  # Transform to matrix
  edgelist <- as.matrix(edgelist[,1:3])

  return(edgelist)
}

prepare_aomstats_actors <- function(reh) {
  reh <- normalize_reh(reh)
  # Extract the actors
  actors <- reh$meta$dictionary$actors

  # Transform to cpp indexing
  actors$actorID <- actors$actorID - 1

  return(actors)
}

prepare_aomstats_event_weights <- function(reh) {
  reh <- normalize_reh(reh)
  if (!(reh$meta$weighted)) {
    weights <- rep(1, nrow(reh$edgelist))
  } else {
    weights <- reh$edgelist$weight
  }
  return(weights)
}

prepare_subset <- function(start, stop, edgelist, method, model) {
  if (start < 1) {
    stop("The 'start' value should be set to 1 or a larger number.")
  }
  start <- start - 1
  if (stop == Inf) {
    if (method == "pe") {
      stop <- nrow(edgelist)
    } else if (method == "pt") {
      stop <- NROW(unique(edgelist[, 1]))
    }
  }
  stop <- stop - 1
  if (stop < start) {
    stop("The 'stop' value cannot be smaller than the 'start' value.")
  }
  if (method == "pe") {
    if (stop > nrow(edgelist)) {
      stop("The 'stop' value cannot be larger than the number of events in 'reh'.")
    }
  }
  if (method == "pt") {
    if (stop > NROW(unique(edgelist[, 1]))) {
      stop("The 'stop' value cannot be larger than the number of unique time points in 'reh'.")
    }
  }
  if (model == "receiver" & method == "pt") {
    # For the receiver model, we need all timepoints not only the unique one
    unique_times <- unique(edgelist[, 1])
    start_time <- unique_times[start + 1]
    stop_time <- unique_times[stop + 1]
    all_times <- edgelist[, 1]
    start <- min(which(all_times == start_time)) - 1
    stop <- max(which(all_times == stop_time)) - 1
  }
  list(start = start, stop = stop)
}

prepare_sender_effects <- function(sender_formula) {
  check_formula(sender_formula)
  sender_effects <- parse_formula(sender_formula, "rateEffects")
  sender_effects_names <- sapply(sender_effects, function(x) x$effect)

  # Check correct specification effects
  if (!all(sender_effects_names %in% actor_effects(step = "sender"))) {
    stop(paste("Invalid effect: Attempting to request effects that are not defined for the sender activity model"))
  }

  # Prepare interaction sender_effects
  sender_effects_int <- parse_int(
    sender_formula, "rateEffects",
    sender_effects
  )
  sender_effects_names <- append(sender_effects_names, rep("interact", length(sender_effects_int)), length(sender_effects_names))
  sender_interactions <- list()
  sender_interactions[which(sender_effects_names == "interact")] <- sender_effects_int

  # Output
  list(
    sender_effects = sender_effects,
    sender_effects_names = sender_effects_names,
    sender_interactions = sender_interactions
  )
}

prepare_aomstats_scaling <- function(effects, interactions) {
  scaling <- sapply(effects, function(x) {
    ifelse("scaling" %in% names(x), x$scaling, "none")
  })
  scaling <- append(scaling, rep("none", sum(!(sapply(interactions, is.null)))), length(scaling))
  scaling
}

prepare_receiver_effects <- function(receiver_formula) {
  check_formula(receiver_formula)
  receiver_effects <- parse_formula(receiver_formula, "choiceEffects")
  receiver_effects_names <- sapply(receiver_effects, function(x) x$effect)

  # Check correct specification effects
  if (!all(receiver_effects_names %in% actor_effects(step = "receiver"))) {
    stop(paste("Invalid effect: Attempting to request effects that are not defined for the receiver choice model"))
  }

  # Prepare interaction receiver_effects
  receiver_effects_int <- parse_int(
    receiver_formula, "choiceEffects",
    receiver_effects
  )
  receiver_effects_names <- append(receiver_effects_names, rep("interact", length(receiver_effects_int)), length(receiver_effects_names))
  receiver_interactions <- list()
  receiver_interactions[which(receiver_effects_names == "interact")] <- receiver_effects_int

  # Output
  list(
    receiver_effects = receiver_effects,
    receiver_effects_names = receiver_effects_names,
    receiver_interactions = receiver_interactions
  )
}

# Actor-oriented model: covariates ----------------------------------------
validate_attr_actors <- function(attr_actors, x) {

  # Check if the variable name is in the attr_actors object
  if (!(x$variable %in% colnames(attr_actors))) {
    stop(paste0("Variable '", x$variable, "' not in attr_actors object for the '", x$effect, "' effect."))
  }

  # Check if the 'time' variable is available
  if (!("time" %in% colnames(attr_actors))) {
    if (anyDuplicated(attr_actors$name) > 0) {
      warning("Duplicated actor names detected in 'attr_actors'. Did you mean to add a 'time' variable?")
    } else {
      attr_actors$time <- 0
    }
  }

  if (anyNA(attr_actors$time)) {
    stop("Missing 'time' values detected in 'attr_actors'.")
  }

  return(attr_actors)
}

prepare_covariate_data <- function(attr_actors, x, actors) {
  # Collect the information in a data.frame
  dat <- data.frame(
    name = attr_actors$name,
    time = attr_actors$time,
    x = attr_actors[, x$variable]
  )

  # Check the data class of the variable
  dat <- variable_check(x$effect, dat)

  # Convert 'actorName' to 'actorID'
  dat$name <- actors[match(dat$name, actors[, 1]), 2]

  # Set the third colum name equal to the 'variable'
  colnames(dat)[3] <- x$variable

  dat
}

validate_covariate_data <- function(dat, x, actors) {
  # Warning for missing values
  if (anyNA(dat)) {
    warning(paste0("Missing values in the attr_actors object for the '", x$effect, "' effect can cause unexpected behavior."))
  }
  # Check if all actors are in the attr_actors
  if (!all(actors[, 2] %in% dat$name)) {
    stop("Missing actors in the attr_actors object.")
  }
  # Check for actors in the attr_actors object that are not in the
  # risk set
  if (any(is.na(dat$name))) {
    warning(paste0("attr_actors contain actors that are not in the risk set. These are not included in the computation of the statistics."))
    dat <- dat[!is.na(dat$name), ]
  }
  as.matrix(dat)
}

validate_user_stat <- function(x, edgelist, actors) {
  if (NCOL(x) != nrow(actors)) {
    stop("Number of columns of matrix 'x' in userStat() does not match the number of actors")
  }
  as.matrix(x)
}

validate_covariates <- function(
    x, attr_actors, attr_dyads, actors, edgelist,
    reh) {
  if (x$effect %in% c("send", "receive", "same", "difference", "average")) {
    if (is.null(x$x)) {
      attr_actors <- validate_attr_actors(attr_actors, x)
      dat <- prepare_covariate_data(attr_actors, x, actors)
      validate_covariate_data(dat, x, actors)
    } else {
      dat <- x$x
      dat$name <- actors[match(dat$name, actors[, 1]), 2]
      validate_covariate_data(dat, x, actors)
    }
  } else if (x$effect == "tie") {
    parse_tie(x, reh, attr_dyads)
  } else if (x$effect == "userStat") {
    validate_user_stat(x$x, edgelist, actors)
  } else {
    matrix()
  }
}

prepare_sender_covariates <- function(sender_effects, attr_actors, actors, edgelist, reh) {
  sender_covar <- lapply(sender_effects, function(x) {
    validate_covariates(x, attr_actors, NULL, actors, edgelist, reh)
  })
  sender_covar
}

prepare_receiver_covariates <- function(
    receiver_effects, attr_actors, attr_dyads, actors,
    edgelist, reh) {
  receiver_covar <- lapply(receiver_effects, function(x) {
    validate_covariates(x, attr_actors, attr_dyads, actors, edgelist, reh)
  })

  return(receiver_covar)
}

# tie/dyad effect ---------------------------------------------------------
prep_tie <- function(variable, attr_dyads, scaling) {
  if (is.null(attr_dyads)) {
    list(
      effect = "tie",
      variable = variable,
      x = NULL,
      scaling = scaling
    )
  } else {
    # Wide or long format?
    # - First check
    wide <- ifelse(nrow(attr_dyads) == ncol(attr_dyads), TRUE, FALSE)
    # - Second check
    wide <- ifelse(wide & !(variable %in% colnames(attr_dyads)), TRUE, FALSE)

    # Prep long format
    if (!wide) {
      # Check for presence of time-column
      if ("time" %in% colnames(attr_dyads)) {
        time <- attr_dyads[, "time"]
      } else {
        # Check for multiple actor appearances
        if (any(duplicated(attr_dyads[, c(1, 2)]))) {
          warning("Duplicated dyads in 'attr_dyads': Did you mean to include a 'time' column?")
        }
        time <- rep(0, nrow(attr_dyads))
      }

      # Check for presence of variable
      if (!(variable %in% colnames(attr_dyads))) {
        stop(paste("Invalid variable: Cannot find", variable, "in attr_dyads"))
      }

      # Check for missing values
      if (anyNA(time)) {
        warning("Missing time values in 'attr_dyads'")
      }

      if (anyNA(attr_dyads[, 1]) | anyNA(attr_dyads[, 2])) {
        warning("Missing actors in 'attr_dyads'")
      }

      if (anyNA(attr_dyads[, variable])) {
        warning("Missing variable values in 'attr_dyads'")
      }

      # Collect data
      dat <- data.frame(
        actor1 = attr_dyads[, 1],
        actor2 = attr_dyads[, 2],
        time = time,
        x = attr_dyads[, variable]
      )

      colnames(dat)[4] <- variable
    }

    if (wide) {
      dat <- attr_dyads
    }

    # Output
    list(
      effect = "tie",
      variable = variable,
      x = dat,
      scaling = scaling
    )
  }
}

tie_convert_wide_to_long <- function(x, variable, reh) {
  actors <- reh$meta$dictionary$actors

  # Check actors in x
  if (is.null(rownames(x)) | is.null(colnames(x))) {
    if (nrow(x) != nrow(actors) | ncol(x) != nrow(actors)) {
      stop("Invalid attr_dyads object: Number of rows and columns should match number of actors in the network")
    }
    rownames(x) <- colnames(x) <- actors$actorName
  }

  # Error message in the case of missing actors
  if ((!all(actors$actorName %in% rownames(x))) |
    (!all(actors$actorName %in% colnames(x)))) {
    warning("Missing actors in 'attr_dyads'")
  }

  # Recode
  rownames(x) <- actors$actorID[match(rownames(x), actors$actorName)] - 1
  colnames(x) <- actors$actorID[match(colnames(x), actors$actorName)] - 1

  # Reorder
  x <- x[order(as.numeric(rownames(x))), ]
  x <- x[, order(as.numeric(colnames(x)))]

  # Undirected events
  if (!reh$meta$directed) {
    if (!isSymmetric(x)) {
      if (all(is.na(x[upper.tri(x)]))) {
        x[upper.tri(x)] <- t(x)[upper.tri(x)]
      } else if (all(is.na(x[lower.tri(x)]))) {
        x[lower.tri(x)] <- t(x)[lower.tri(x)]
      } else {
        warning("Undirected events: attr_dyads matrix is expected to be symmetrical")
      }
    }
  }

  # Error message in the case of missing values
  if (anyNA(x[lower.tri(x)]) | anyNA(x[upper.tri(x)])) {
    warning("Missing variable values in 'attr_dyads'")
  }

  # Get row and column names
  row_ids <- rownames(x)
  col_ids <- colnames(x)

  # Initialize empty vectors to store data
  row_data <- character(0)
  col_data <- character(0)
  value_data <- numeric(0)

  # Iterate through the matrix
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      row_data <- c(row_data, row_ids[i])
      col_data <- c(col_data, col_ids[j])
      value_data <- c(value_data, x[i, j])
    }
  }

  # Create a data frame
  result_df <- data.frame(
    actor1 = as.integer(row_data),
    actor2 = as.integer(col_data),
    time = 0,
    x = value_data
  )
  colnames(result_df)[4] <- variable
  result_df
  as.matrix(result_df)
}

parse_tie <- function(prepped_effect, reh, attr_dyads) {
  x <- prepped_effect$x

  if (is.null(x)) {
    x <- prep_tie(prepped_effect$variable, attr_dyads, prepped_effect$scaling)$x
  }
  # Check the format
  wide <- ifelse("actor1" %in% colnames(x), FALSE, TRUE)
  if (wide) {
    tie_convert_wide_to_long(x, prepped_effect$variable, reh)
    # return: converted matrix
  } else {
    actors <- reh$meta$dictionary$actors
    x$actor1 <- actors$actorID[match(x$actor1, actors$actorName)] - 1
    x$actor2 <- actors$actorID[match(x$actor2, actors$actorName)] - 1
    as.matrix(x) # return
  }
}
