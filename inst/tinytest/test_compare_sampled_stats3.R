# tests of whethed the case-control sampled versions of tomstats yields the same stats are the full analysis
# for the sampled dyads.
# here this is tested a dyadic ("tie") model with active riskset, directed events, time-sensitive (not ordinal) model
# Model: tie, manual riskset, time likelihood, undirected events, exponential memory decay

data(history, package = "remstats")
data(info, package = "remstats")

# add some events happening in same interval
history$time[7:8] <- history$time[9]
history[4,] <- history[5,]
history <- history[1:75,]

# take subset for test
start1 <- 3
stop1 <- 60

samp_num <- 5

check_sampled_equals_full <- function(effects,
																			samp_num = 10L,
																			seed = 1L,
																			tol = 1e-12,
																			attr_dyads = NULL) {

	reh <- remify::remify2(edgelist = history, model = "tie",
												 riskset = "full", ordinal = TRUE, directed = FALSE)

	ts_samp <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, attr_dyads = attr_dyads,
		sampling = TRUE, samp_num = samp_num, seed = seed, start = start1, stop = stop1, memory = "decay", memory_value = 3
	)

	# reproducibility (same seed/args)
	ts_samp2 <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, attr_dyads = attr_dyads,
		sampling = TRUE, samp_num = samp_num, seed = seed, start = start1, stop = stop1, memory = "decay", memory_value = 3
	)
	expect_equal(ts_samp, ts_samp2, tol = tol)
	expect_equal(attr(ts_samp, "sample_map"), attr(ts_samp2, "sample_map"))

	ts_full <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, attr_dyads = attr_dyads,
		sampling = FALSE, start = start1, stop = stop1, memory = "decay", memory_value = 3
	)

	sample_map <- attr(ts_samp, "sample_map")
	expect_true(!is.null(sample_map))

	riskset <- attr(ts_full, "riskset")
	expect_true(!is.null(riskset))

	# dyad_id <- as.integer(riskset[, ncol(riskset)])
	# dyad_id_key <- if (min(dyad_id, na.rm = TRUE) == 0L) dyad_id else (dyad_id - 1L)
	# col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)

	M <- dim(ts_samp)[1]
	S <- dim(ts_samp)[2]
	#statnum <- 1

	for (m in seq_len(M)) {
		for (s in seq_len(S)) {
			d <- as.integer(sample_map[m, s])
			j <- d #unname(col_index_by_dyad[as.character(d)])
			expect_true(!is.na(j))

			expect_equal(
				as.numeric(ts_samp[m, s, ]),
				as.numeric(ts_full[m, j, ]),
				tol = tol
			)

			x <- as.numeric(ts_samp[m, s, ])
			y <- as.numeric(ts_full[m, j, ])

			if (max(abs(x - y)) > tol) {
				cat("FAIL at m=", m, " s=", s, " d=", d, " j=", j, "\n")
				cat("samp:", paste(x, collapse=", "), "\n")
				cat("full:", paste(y, collapse=", "), "\n")
				break
			}

		}
	}

	invisible(TRUE)
}

effects_exo <- ~
	same("sex", info) +
	difference("age", info) +
	average("extraversion", info) +
	minimum("age", info) +
	maximum("age", info)

# Optional: dyad covariates shipped with remstats (skip if not present)
dyad_cov_tests <- function() {
	if (exists("both_male_wide", where = asNamespace("remstats"), inherits = FALSE)) {
		data(both_male_wide, package = "remstats")
		check_sampled_equals_full(
			~ tie(variable = "both_male", attr_dyads = both_male_wide),
			samp_num = samp_num, seed = seed, tol = tol,
			attr_dyads = both_male_wide
		)
	}
	if (exists("both_male_long", where = asNamespace("remstats"), inherits = FALSE)) {
		data(both_male_long, package = "remstats")
		check_sampled_equals_full(
			~ tie(variable = "both_male", attr_dyads = both_male_long),
			samp_num = samp_num, seed = seed, tol = tol,
			attr_dyads = both_male_long
		)
	}
	invisible(TRUE)
}

tests <- list(
	inertia_recip = ~ inertia(),
	degrees       = ~ totaldegreeDyad() + degreeMin() + degreeMax() + degreeDiff(),
	triads        = ~ sp(),
	recency       = ~ recencyContinue(),
	pshifts       = ~ psABAY() + psABAB(),
	exo_stats      = effects_exo
)

for (nm in names(tests)) {
	if (is.function(tests[[nm]])) {
		tests[[nm]]()
	} else {
		check_sampled_equals_full(tests[[nm]])
	}
}

# --- userStat test for undirected + ordinal (remify2) ---
make_actor_event_userstat <- function(reh, actor_id, riskset_df) {
	# M = number of unique ordinal time points used by tomstats2 under method="pt"
	time_points <- sort(unique(reh$edgelist$time))
	M <- length(time_points)
	
	# D = number of dyads in the (base) riskset
	D <- nrow(riskset_df)
	
	involved_at_time <- vapply(time_points, function(tt) {
		any(reh$edgelist$time == tt &
					(reh$edgelist$actor1 == actor_id | reh$edgelist$actor2 == actor_id))
	}, logical(1))
	
	matrix(as.numeric(involved_at_time), nrow = M, ncol = D)
}
userstat_actor_test <- function(start1,stop1) {
	reh <- remify::remify2(
		edgelist = history, model = "tie",
		riskset = "full", ordinal = TRUE, directed = FALSE
	)
	actor101Events <- which(history$actor1 == "101" | history$actor2 == "101")
	actor101_stat <- t(sapply(seq_len(nrow(history)), function(i) {
		rep(i %in% actor101Events, reh$D)
	}))#[start1:stop1,]
	effects_us <- ~ userStat(x = actor101_stat, variableName = "actor101event")
	
	check_sampled_equals_full(
		effects_us,
		samp_num = 5L, seed = 1L, tol = 1e-12
	)
}
userstat_actor_test(start1,stop1)




dim(ts_samp)
dim(ts_full)
m <- 31
head(history)
head(reh$edgelist)
riskset[sample_map[m,],]
ts_samp[m,,]
ts_full[m,sample_map[m,],]


exp(-(3 - 2) * log(2) / 3) * 1.64
exp(-(3 - 1) * log(2) / 3) * 1.33


ev_idx <- events_by_row[[5]]
c(ed$actor1[ev_idx], ed$actor2[ev_idx])
keys <- make_key(ed$actor1[ev_idx], ed$actor2[ev_idx])
key_to_base[keys]   # should be 26

