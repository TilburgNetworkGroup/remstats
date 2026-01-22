using("tinytest")

# Compare sampled tomstats2 to full tomstats2, restricted to sampled dyads
check_sampled_equals_full <- function(effects, samp_num = 10L, seed = 1L, tol = 1e-12) {
	
	data(history, package = "remstats")
	data(info, package = "remstats")
	data(both_male_long, package = "remstats")
	
	# Edge cases for pt: multiple events at same time, duplicate row
	history$time[5:6] <- history$time[7]
	history[16, ] <- history[17, ]
	
	# event() covariate
	history$work <- ifelse(history$setting == "work", 1, 0)
	
	reh <- remify::remify(edgelist = history, model = "tie", riskset = "active")
	
	# userStat() covariate: event x dyad matrix
	actor101Events <- which(history$actor1 == "101" | history$actor2 == "101")
	actor101_stat <- t(vapply(
		seq_len(nrow(history)),
		function(i) rep(i %in% actor101Events, reh$D),
		FUN.VALUE = rep(FALSE, reh$D)
	))
	
	# Ensure objects used inside effects are visible when tomstats2 evaluates the formula
	env <- environment()
	
	set.seed(seed)
	ts_samp <- remstats::tomstats2(
		eval(effects, envir = env), reh = reh, attr_actors = info, method = "pt",
		sampling = TRUE, samp_num = samp_num, seed = seed
	)
	
	ts_full <- remstats::tomstats2(
		eval(effects, envir = env), reh = reh, attr_actors = info, method = "pt",
		sampling = FALSE
	)
	
	sample_map <- attr(ts_samp, "sample_map")
	expect_true(!is.null(sample_map))
	
	riskset <- attr(ts_full, "riskset")
	expect_true(!is.null(riskset))
	
	dyad_id <- as.integer(riskset[, ncol(riskset)])
	dyad_id_key <- if (min(dyad_id, na.rm = TRUE) == 0L) dyad_id else (dyad_id - 1L)
	col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)
	
	M <- dim(ts_samp)[1]
	S <- dim(ts_samp)[2]
	
	for (m in seq_len(M)) {
		for (s in seq_len(S)) {
			d <- as.integer(sample_map[m, s])
			j <- unname(col_index_by_dyad[as.character(d)])
			expect_true(!is.na(j))
			
			expect_equal(
				as.numeric(ts_samp[m, s, ]),
				as.numeric(ts_full[m, j, ]),
				tol = tol
			)
		}
	}
	
	invisible(TRUE)
}

tests <- list(
	
	# Exogenous
	exo_send_receive = quote(~ send("extraversion") + receive("extraversion")),
	exo_event        = quote(~ event(x = history$work, variableName = "setting_is_work")),
	exo_tie          = quote(~ tie(variable = "both_male", attr_dyads = both_male_long)),
	exo_userStat     = quote(~ userStat(x = actor101_stat, variableName = "actor101event")),
	
	# Endogenous (sampling-invariant in your implementation)
	degrees_basic    = quote(~ indegreeSender() + indegreeReceiver() + outdegreeSender() + outdegreeReceiver()),
	degrees_total    = quote(~ totaldegreeSender() + totaldegreeReceiver() + totaldegreeDyad()),
	degrees_derived  = quote(~ degreeMin() + degreeMax() + degreeDiff()),
	inertia_recip    = quote(~ inertia() + reciprocity()),
	triads           = quote(~ otp() + itp() + osp() + isp() + sp()),
	pshifts          = quote(~ psABBA() + psABBY() + psABXA() + psABXB() + psABXY() + psABAY() + psABAB()),
	recency          = quote(~ recencySendSender() + recencySendReceiver() +
													 	recencyReceiveSender() + recencyReceiveReceiver() + recencyContinue()),
	fetype           = quote(~ FEtype())
	
	# NOTE: exclude rrankSend()/rrankReceive() from equality tests.
)

for (nm in names(tests)) {
	tinytest::test(sprintf("tomstats2: sampled equals full (%s)", nm), {
		check_sampled_equals_full(tests[[nm]])
	})
}

# for undirected data
check_sampled_equals_full_undirected <- function(effects, samp_num = 10L, seed = 1L, tol = 1e-12) {
	
	data(history, package = "remstats")
	data(info, package = "remstats")
	
	# Make undirected by sorting endpoints within each event
	a1 <- pmin(history$actor1, history$actor2)
	a2 <- pmax(history$actor1, history$actor2)
	history$actor1 <- a1
	history$actor2 <- a2
	
	# Make ordinal time (1..n)
	history$time <- seq_len(nrow(history))
	
	reh <- remify::remify(edgelist = history, model = "tie", riskset = "full", directed = FALSE,
												ordinal = TRUE)
	
	set.seed(seed)
	ts_samp <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, method = "pt",
		sampling = TRUE, samp_num = samp_num, seed = seed
	)
	
	ts_full <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, method = "pt",
		sampling = FALSE
	)
	
	sample_map <- attr(ts_samp, "sample_map")
	expect_true(!is.null(sample_map))
	
	riskset <- attr(ts_full, "riskset")
	expect_true(!is.null(riskset))
	
	dyad_id <- as.integer(riskset[, ncol(riskset)])
	dyad_id_key <- if (min(dyad_id, na.rm = TRUE) == 0L) dyad_id else (dyad_id - 1L)
	col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)
	
	M <- dim(ts_samp)[1]
	S <- dim(ts_samp)[2]
	
	for (m in seq_len(M)) {
		for (s in seq_len(S)) {
			d <- as.integer(sample_map[m, s])
			j <- unname(col_index_by_dyad[as.character(d)])
			expect_true(!is.na(j))
			
			expect_equal(
				as.numeric(ts_samp[m, s, ]),
				as.numeric(ts_full[m, j, ]),
				tol = tol
			)
		}
	}
	
	invisible(TRUE)
}

tinytest::test("tomstats2: sampled equals full (undirected ordinal)", {
	effects_ud <- ~ inertia() + reciprocity() + otp() + itp()
	check_sampled_equals_full_undirected(effects_ud)
})

