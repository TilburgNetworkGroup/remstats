# # inst/tinytest/test_compare_sampled_stats.R
# # No using("tinytest") / library(tinytest) here.
# 
# # ------------------------------------------------------------------------------
# # Helper: Compare sampled tomstats2 to full tomstats2, restricted to sampled dyads
# # ------------------------------------------------------------------------------
# 
# check_sampled_equals_full <- function(effects,
# 																			samp_num = 10L,
# 																			seed = 1L,
# 																			tol = 1e-12,
# 																			memory = "full",
# 																			memory_value = NULL,
# 																			start = NULL,
# 																			stop = NULL,
# 																			origin = NULL) {
# 
# 	data(history, package = "remstats")
# 	data(info, package = "remstats")
# 	data(both_male_long, package = "remstats")
# 
# 	# Edge cases for pt: multiple events at same time, duplicate row
# 	history$time[5:6] <- history$time[7]
# 	history[16, ] <- history[17, ]
# 
# 	# Build remify object FIRST (so event/userStat covariates match reh$edgelist)
# 	remify_args <- list(edgelist = history, model = "tie", riskset = "active")
# 	if (!is.null(origin)) remify_args$origin <- origin
# 	reh <- do.call(remify::remify, remify_args)
# 
# 	# event() covariate must match reh$edgelist rows
# 	# reh$edgelist has columns: time, actor1, actor2, weight, (maybe type)
# 	edg <- reh$edgelist
# 	# "work" indicator based on the original history$setting aligned by row index.
# 	# If reh keeps all rows, this is identical; if reh filters, we still need alignment.
# 	# Best-effort: match by (time, actor1, actor2, weight) back to history.
# 	# For this package example, row-order is typically preserved; validate length.
# 	if (nrow(edg) == nrow(history)) {
# 		work_vec <- ifelse(history$setting == "work", 1, 0)
# 	} else {
# 		# Safer alignment using a key (handles rare filtering/reordering)
# 		key_hist <- paste(history$time, history$actor1, history$actor2, history$weight, sep = "|")
# 		key_edg  <- paste(edg[,1],      edg[,2],      edg[,3],      edg[,4],      sep = "|")
# 		idx <- match(key_edg, key_hist)
# 		expect_true(!anyNA(idx))
# 		work_vec <- ifelse(history$setting[idx] == "work", 1, 0)
# 	}
# 	expect_true(length(work_vec) == nrow(edg))
# 
# 	# userStat() covariate: event x dyad matrix must match nrow(reh$edgelist)
# 	actor101Events <- which(edg[,2] == "101" | edg[,3] == "101")
# 	actor101_stat <- t(vapply(
# 		seq_len(nrow(edg)),
# 		function(i) rep(i %in% actor101Events, reh$D),
# 		FUN.VALUE = rep(FALSE, reh$D)
# 	))
# 
# 	# Ensure objects used inside effects are visible when tomstats2 evaluates the formula
# 	env <- environment()
# 
# 	base_args <- list(
# 		reh = reh,
# 		attr_actors = info,
# 		method = "pt",
# 		memory = memory,
# 		memory_value = memory_value
# 	)
# 	if (!is.null(start)) base_args$start <- start
# 	if (!is.null(stop))  base_args$stop  <- stop
# 
# 	# Sampled
# 	set.seed(seed)
# 	ts_samp <- do.call(
# 		remstats::tomstats2,
# 		c(
# 			list(effects = eval(effects, envir = env)),
# 			base_args,
# 			list(sampling = TRUE, samp_num = samp_num, seed = seed)
# 		)
# 	)
# 
# 	# Full
# 	ts_full <- do.call(
# 		remstats::tomstats2,
# 		c(
# 			list(effects = eval(effects, envir = env)),
# 			base_args,
# 			list(sampling = FALSE)
# 		)
# 	)
# 
# 	# Mappings
# 	sample_map <- attr(ts_samp, "sample_map")
# 	expect_true(!is.null(sample_map))
# 
# 	riskset <- attr(ts_full, "riskset")
# 	expect_true(!is.null(riskset))
# 
# 	dyad_id <- as.integer(riskset[, ncol(riskset)])
# 	dyad_id_key <- if (min(dyad_id, na.rm = TRUE) == 0L) dyad_id else (dyad_id - 1L)
# 	col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)
# 
# 	M <- dim(ts_samp)[1]
# 	S <- dim(ts_samp)[2]
# 
# 	for (m in seq_len(M)) {
# 		for (s in seq_len(S)) {
# 			d <- as.integer(sample_map[m, s])
# 			j <- unname(col_index_by_dyad[as.character(d)])
# 			expect_true(!is.na(j))
# 
# 			expect_equal(
# 				as.numeric(ts_samp[m, s, ]),
# 				as.numeric(ts_full[m, j, ]),
# 				tol = tol
# 			)
# 		}
# 	}
# 
# 	invisible(TRUE)
# }
# 
# # ------------------------------------------------------------------------------
# # Undirected + ordinal + active risk set helper
# # ------------------------------------------------------------------------------
# 
# check_sampled_equals_full_undirected_ordinal <- function(effects,
# 																												 samp_num = 10L,
# 																												 seed = 1L,
# 																												 tol = 1e-12) {
# 	data(history, package = "remstats")
# 	data(info, package = "remstats")
# 
# 	# Make undirected by sorting endpoints within each event
# 	a1 <- pmin(history$actor1, history$actor2)
# 	a2 <- pmax(history$actor1, history$actor2)
# 	history$actor1 <- a1
# 	history$actor2 <- a2
# 
# 	# Make ordinal time (1..n)
# 	history$time <- seq_len(nrow(history))
# 
# 	reh <- remify::remify(
# 		edgelist = history,
# 		model = "tie",
# 		riskset = "active",
# 		directed = FALSE,
# 		ordinal = TRUE
# 	)
# 
# 	set.seed(seed)
# 	ts_samp <- remstats::tomstats2(
# 		effects, reh = reh, attr_actors = info, method = "pt",
# 		sampling = TRUE, samp_num = samp_num, seed = seed
# 	)
# 
# 	ts_full <- remstats::tomstats2(
# 		effects, reh = reh, attr_actors = info, method = "pt",
# 		sampling = FALSE
# 	)
# 
# 	sample_map <- attr(ts_samp, "sample_map")
# 	expect_true(!is.null(sample_map))
# 
# 	riskset <- attr(ts_full, "riskset")
# 	expect_true(!is.null(riskset))
# 
# 	dyad_id <- as.integer(riskset[, ncol(riskset)])
# 	dyad_id_key <- if (min(dyad_id, na.rm = TRUE) == 0L) dyad_id else (dyad_id - 1L)
# 	col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)
# 
# 	M <- dim(ts_samp)[1]
# 	S <- dim(ts_samp)[2]
# 
# 	for (m in seq_len(M)) {
# 		for (s in seq_len(S)) {
# 			d <- as.integer(sample_map[m, s])
# 			j <- unname(col_index_by_dyad[as.character(d)])
# 			expect_true(!is.na(j))
# 
# 			expect_equal(
# 				as.numeric(ts_samp[m, s, ]),
# 				as.numeric(ts_full[m, j, ]),
# 				tol = tol
# 			)
# 		}
# 	}
# 
# 	invisible(TRUE)
# }
# 
# # ------------------------------------------------------------------------------
# # Execute tests (top-level expects)
# # ------------------------------------------------------------------------------
# 
# # tests <- list(
# # 	# Exogenous
# # 	exo_send_receive = quote(~ send("extraversion") + receive("extraversion")),
# # 	exo_event        = quote(~ event(x = work_vec, variableName = "setting_is_work")),
# # 	exo_tie          = quote(~ tie(variable = "both_male", attr_dyads = both_male_long)),
# # 	exo_userStat     = quote(~ userStat(x = actor101_stat, variableName = "actor101event")),
# # 
# # 	# Endogenous
# # 	degrees_basic    = quote(~ indegreeSender() + indegreeReceiver() + outdegreeSender() + outdegreeReceiver()),
# # 	degrees_total    = quote(~ totaldegreeSender() + totaldegreeReceiver() + totaldegreeDyad()),
# # 	degrees_derived  = quote(~ degreeMin() + degreeMax() + degreeDiff()),
# # 	inertia_recip    = quote(~ inertia() + reciprocity()),
# # 	triads           = quote(~ otp() + itp() + osp() + isp() + sp()),
# # 	pshifts          = quote(~ psABBA() + psABBY() + psABXA() + psABXB() + psABXY() + psABAY() + psABAB()),
# # 	recency          = quote(~ recencySendSender() + recencySendReceiver() +
# # 													 	recencyReceiveSender() + recencyReceiveReceiver() + recencyContinue()),
# # 	fetype           = quote(~ FEtype())
# # )
# 
# for (nm in names(tests)) {
# 	check_sampled_equals_full(
# 		effects = tests[[nm]],
# 		samp_num = 10L,
# 		seed = 1L,
# 		tol = 1e-12,
# #		start = 10,
# #		stop = 99,
# 		origin = 100
# 	)
# }
# 
# # Undirected + ordinal
# effects_ud <- ~ inertia() + reciprocity() + otp() + itp()
# check_sampled_equals_full_undirected_ordinal(effects_ud, samp_num = 10L, seed = 1L)
# 
# # Memory settings
# check_sampled_equals_full(
# 	effects = quote(~ inertia() + reciprocity() + indegreeSender()),
# 	memory = "decay",
# 	memory_value = c(500),
# 	samp_num = 10L,
# 	seed = 1L
# )
# 
# check_sampled_equals_full(
# 	effects = quote(~ inertia() + reciprocity() + indegreeSender()),
# 	memory = "window",
# 	memory_value = c(500),
# 	samp_num = 10L,
# 	seed = 1L
# )
