# tests of whethed the case-control sampled versions of tomstats yields the same stats are the full analysis
# for the sampled dyads.
# here this is tested a dyadic ("tie") model with active riskset, directed events, time-sensitive (not ordinal) model
# memroy decay
# with event types

library(tinytest)

data(history, package = "remstats")
data(info, package = "remstats")

# add some events happening in same interval
history$time[7:8] <- history$time[9]
history[4,] <- history[5,]
history <- history[1:33,]

# take subset for test
start1 <- 2
stop1 <- 20

check_sampled_equals_full <- function(effects,
																			samp_num = 5L,
																			seed = 1L,
																			tol = 1e-12,
																			attr_dyads = NULL) {
	
	reh <- remify::remify2(edgelist = history, model = "tie", riskset = "manual", manual.riskset = history[1:15,2:3])
	
	ts_samp <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, attr_dyads = attr_dyads, memory = "decay", memory_value = 1000,
		sampling = TRUE, samp_num = samp_num, seed = seed, start = start1, stop = stop1
	)
	
	# reproducibility (same seed/args)
	ts_samp2 <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, attr_dyads = attr_dyads, memory = "decay", memory_value = 1000,
		sampling = TRUE, samp_num = samp_num, seed = seed, start = start1, stop = stop1
	)
	expect_equal(ts_samp, ts_samp2, tol = tol)
	expect_equal(attr(ts_samp, "sample_map"), attr(ts_samp2, "sample_map"))
	
	ts_full <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, attr_dyads = attr_dyads, memory = "decay", memory_value = 1000,
		sampling = FALSE, start = start1, stop = stop1
	)
	
	sample_map <- attr(ts_samp, "sample_map")
	expect_true(!is.null(sample_map))
	
	riskset <- attr(ts_full, "riskset")
	expect_true(!is.null(riskset))
	
	#dyad_id <- as.integer(riskset[, ncol(riskset)])
	#dyad_id_key <- if (min(dyad_id, na.rm = TRUE) == 0L) dyad_id else (dyad_id - 1L)
	#col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)
	
	M <- dim(ts_samp)[1]
	S <- dim(ts_samp)[2]
	
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
				cat("FAIL at m=", m, " s=", s, " d=", d, " j=", j, "\n", as.character(effects))
				cat("samp:", paste(x, collapse=", "), "\n")
				cat("full:", paste(y, collapse=", "), "\n")
				break
			}
			
		}
	}
	
	invisible(TRUE)
}

tests <- list(
	inertia_recip = ~ inertia() + reciprocity(),
	inertia_recip_prop = ~ inertia(scaling="prop") + reciprocity(scaling="prop"),
	degrees       = ~ indegreeSender() + outdegreeSender() + indegreeReceiver() + outdegreeReceiver(),
	degrees_prop  = ~ indegreeSender(scaling="prop") + outdegreeSender(scaling="prop") + indegreeReceiver(scaling="prop") + outdegreeReceiver(scaling="prop"),
	triads        = ~ otp() + itp() + isp() + osp(),
	# scaling = 'std' (use ccs correction)
	recency       = ~ recencySendReceiver() + recencyReceiveReceiver() + recencyContinue(),
	pshifts       = ~ psABBA() + psABXY() + psABAY(),
	exo_send_receive   = ~ 1 + send("extraversion", info) + receive("extraversion", info),
	exo_send_receive_2 = ~ 1 + send("agreeableness", info) + receive("agreeableness", info),
	exo_same_diff      = ~ 1 + same("sex", info) + difference("age", info),
	exo_aggregate      = ~ 1 + average("extraversion", info) + minimum("age", info) + maximum("age", info),
	
	dyad_tie_wide = function() {
		data(both_male_wide, package = "remstats")
		check_sampled_equals_full(
			~ tie(variable = "both_male", attr_dyads = both_male_wide),
			attr_dyads = both_male_wide
		)
	},
	dyad_tie_long = function() {
		data(both_male_long, package = "remstats")
		check_sampled_equals_full(
			~ tie(variable = "both_male", attr_dyads = both_male_long),
			attr_dyads = both_male_long
		)
	}
)

for (nm in names(tests)) {
	if (is.function(tests[[nm]])) {
		tests[[nm]]()
	} else {
		check_sampled_equals_full(tests[[nm]])
	}
}

# m <- 1
# dim(ts_samp)
# dim(ts_full)
# head(history)
# head(reh$edgelist_id)
# riskset[sample_map[m,],]
# ts_samp[m,,]
# ts_full[m,sample_map[m,],]

# exp(- (345 - 238) * log(2) / 1000) * 1.33
# exp(- (317 - 238) * log(2) / 1000) * 1.33
# exp(- (317 - 238) * log(2) / 1000) * 1.33

# FAIL at m= 1  s= 5  d= 35  j= 35 
# samp: 1, 1.25912912954483, 0 
# full: 1, 1.33, 0 
