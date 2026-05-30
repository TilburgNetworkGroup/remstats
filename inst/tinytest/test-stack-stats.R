# Tests for stack_stats.tomstats()
# Checks correct dimensions, structure, response, offset, and ordinal support.

library(tinytest)

data(history, package = "remstats")
data(info,    package = "remstats")
colnames(history)[colnames(history) == "setting"] <- "type"
history_sub <- history[1:40, ]

effects <- ~ inertia(consider_type = "interact") + 
               indegreeSender(consider_type = FALSE) +
               outdegreeSender(consider_type = FALSE)

# ---------------------------------------------------------------------------
# SECTION 1: Interval timing, active riskset
# ---------------------------------------------------------------------------
reh_int <- remify(edgelist = history_sub, model = "tie",
                            riskset = "active", extend_riskset_by_type = FALSE)

ts_int <- tomstats(effects, reh = reh_int,
                               attr_actors = info,
                               memory = "decay", memory_value = 1000,
                               first = 2, last = 30,
                               sampling = FALSE)
dimnames(ts_int)[[3]]

stacked_int <- stack_stats(ts_int, reh_int)

# Return structure
expect_true(is.list(stacked_int),
  info = "interval: returns a list")
expect_true(all(c("remstats_stack", "subset", "D", "E", "ordinal") %in%
                  names(stacked_int)),
  info = "interval: list has correct elements")

df_int <- stacked_int$remstats_stack
D_int   <- stacked_int$D
E_int   <- stacked_int$E

# Dimensions
expect_equal(nrow(df_int), D_int * E_int,
  info = "interval: nrow = D * E")
expect_equal(D_int, nrow(attr(ts_int, "riskset")),
  info = "interval: D matches riskset size")
expect_equal(E_int, dim(ts_int)[1],
  info = "interval: E matches number of time points")

# Column names
expect_true(all(c("time_index", "obs", "dyad", "log_interevent") %in%
                  colnames(df_int)),
  info = "interval: required columns present")
expect_true(all(c("baseline", "inertia.social", "inertia.work",
									"indegreeSender", "outdegreeSender") %in%
                  colnames(df_int)),
  info = "interval: statistic columns present")

# ordinal flag
expect_false(stacked_int$ordinal,
  info = "interval: ordinal flag is FALSE")

# Response: total observed events should equal E (one event per time point)
expect_equal(sum(df_int$obs), E_int,
  info = "interval: total observed events equals E")

# Response is 0/1 integer (no simultaneous events in this data)
expect_true(all(df_int$obs %in% c(0L, 1L)),
  info = "interval: obs is binary")

# Dyad index cycles 1..D for each event
expect_equal(df_int$dyad, rep(seq_len(D_int), E_int),
  info = "interval: dyad index cycles correctly")

# Event index: each event block has the correct event number
expect_equal(df_int$time_index, 1+rep(seq_len(E_int), each = D_int),
  info = "interval: event index correct")

# Offset: log_interevent matches reh interevent times
iet_expected <- log(reh_int$intereventTime[2:30])
expect_equal(df_int$log_interevent[seq(1, nrow(df_int), by = D_int)],
  iet_expected,
  tolerance = 1e-10,
  info = "interval: log_interevent values match reh intereventTime")

# Baseline column is all 1
expect_true(all(df_int$baseline == 1),
  info = "interval: baseline column is all 1")

# subset stored correctly
expect_equal(stacked_int$subset, c(2L, 30L),
  info = "interval: subset stored correctly")

# ---------------------------------------------------------------------------
# SECTION 2: Ordinal timing, active riskset
# ---------------------------------------------------------------------------
reh_ord <- remify(edgelist = history_sub, model = "tie",
                            riskset = "active", ordinal = TRUE, extend_riskset_by_type = TRUE)

ts_ord <- tomstats(effects, reh = reh_ord,
                               attr_actors = info,
                               memory = "decay", memory_value = 1000,
                               first = 2, last = 30,
                               sampling = FALSE)

stacked_ord <- stack_stats(ts_ord, reh_ord)
df_ord <- stacked_ord$remstats_stack

# ordinal flag
expect_true(stacked_ord$ordinal,
  info = "ordinal: ordinal flag is TRUE")

# No log_interevent column for ordinal
expect_false("log_interevent" %in% colnames(df_ord),
  info = "ordinal: no log_interevent column")

# Dimensions same as interval
expect_equal(nrow(df_ord), stacked_ord$D * stacked_ord$E,
  info = "ordinal: nrow = D * E")

# Response correct
expect_equal(sum(df_ord$obs), stacked_ord$E,
  info = "ordinal: total observed events equals E")

expect_true(all(df_ord$obs %in% c(0L, 1L)),
  info = "ordinal: obs is binary")

# ---------------------------------------------------------------------------
# SECTION 3: Interval timing, full riskset
# ---------------------------------------------------------------------------
reh_full <- remify(edgelist = history_sub, model = "tie",
                             riskset = "full", extend_riskset_by_type = TRUE)

ts_full <- tomstats(effects, reh = reh_full,
                                attr_actors = info,
                                memory = "decay", memory_value = 1000,
                                first = 2, last = 30,
                                sampling = FALSE)

stacked_full <- stack_stats(ts_full, reh_full)
df_full <- stacked_full$remstats_stack

expect_equal(stacked_full$D, dim(ts_full)[2],
  info = "full riskset: D matches stats dimension")
expect_equal(nrow(df_full), stacked_full$D * stacked_full$E,
  info = "full riskset: nrow = D * E")
expect_equal(sum(df_full$obs), stacked_full$E,
  info = "full riskset: total observed events equals E")
expect_false(stacked_full$ordinal,
  info = "full riskset: ordinal flag is FALSE")
expect_true("log_interevent" %in% colnames(df_full),
  info = "full riskset: log_interevent present")

# ---------------------------------------------------------------------------
# SECTION 4: start/stop subsetting respected
# ---------------------------------------------------------------------------
ts_sub <- tomstats(effects, reh = reh_int,
                               attr_actors = info,
                               memory = "decay", memory_value = 1000,
                               first = 5, last = 20,
                               sampling = FALSE)

stacked_sub <- stack_stats(ts_sub, reh_int)

expect_equal(stacked_sub$subset, c(5L, 20L),
  info = "subset: stored correctly")
expect_equal(stacked_sub$E, dim(ts_sub)[1],
  info = "subset: E matches stats rows")
expect_equal(nrow(stacked_sub$remstats_stack),
  stacked_sub$D * stacked_sub$E,
  info = "subset: nrow correct for subset")
expect_equal(sum(stacked_sub$remstats_stack$obs), stacked_sub$E,
  info = "subset: total events correct")

# ---------------------------------------------------------------------------
# SECTION 5: Sampled tomstats — interval
# ---------------------------------------------------------------------------
samp_num <- 5L

ts_samp <- tomstats(effects, reh = reh_int,
                                attr_actors = info,
                                memory = "decay", memory_value = 1000,
                                first = 2, last = 30,
                                sampling = TRUE, samp_num = samp_num, seed = 1L)

stacked_samp <- stack_stats(ts_samp, reh_int)
df_samp <- stacked_samp$remstats_stack

# Return structure
expect_true(is.list(stacked_samp),
  info = "sampled interval: returns list")
expect_true(all(c("remstats_stack", "subset", "S", "E", "ordinal") %in%
                  names(stacked_samp)),
  info = "sampled interval: list has correct elements")

# Dimensions: E*S rows
expect_equal(nrow(df_samp), stacked_samp$E * stacked_samp$S,
  info = "sampled interval: nrow = E * S")
expect_equal(stacked_samp$S, samp_num,
  info = "sampled interval: S matches samp_num")
expect_equal(stacked_samp$E, dim(ts_samp)[1],
  info = "sampled interval: E matches stats rows")

# Column names
expect_true(all(c("time_index", "obs", "dyad", "weight", "log_interevent") %in%
                  colnames(df_samp)),
  info = "sampled interval: required columns present")
expect_true(all(c("inertia.social", "inertia.work", "indegreeSender",
									"outdegreeSender") %in%
                  colnames(df_samp)),
  info = "sampled interval: statistic columns present")

# ordinal flag
expect_false(stacked_samp$ordinal,
  info = "sampled interval: ordinal flag is FALSE")

# Response: exactly one case per event (no simultaneous events here)
expect_equal(sum(df_samp$obs), stacked_samp$E,
  info = "sampled interval: total cases equals E")
expect_true(all(df_samp$obs %in% c(0L, 1L)),
  info = "sampled interval: obs is binary")

# Weights: cases have weight 1, controls have weight 1/pi_s
case_rows <- df_samp$obs == 1L
expect_true(all(df_samp$weight[case_rows] == 1.0),
  info = "sampled interval: case weights are 1")
expect_true(all(df_samp$weight[!case_rows] > 0),
  info = "sampled interval: control weights are positive")

# With uniform sampling, control weights = (D-1)/(samp_num-1)
D_active <- nrow(attr(ts_samp, "riskset"))
expected_control_weight <- (D_active - 1) / (samp_num - 1)
expect_equal(
  mean(df_samp$weight[!case_rows]),
  expected_control_weight,
  tolerance = 1e-10,
  info = "sampled interval: control weights correct for uniform sampling"
)

# subset stored correctly
expect_equal(stacked_samp$subset, c(2L, 30L),
  info = "sampled interval: subset stored correctly")

# ---------------------------------------------------------------------------
# SECTION 6: Sampled tomstats — ordinal
# ---------------------------------------------------------------------------
ts_samp_ord <- tomstats(effects, reh = reh_ord,
                                    attr_actors = info,
                                    memory = "decay", memory_value = 1000,
                                    first = 2, last = 30,
                                    sampling = TRUE, samp_num = samp_num, seed = 1L)

stacked_samp_ord <- stack_stats(ts_samp_ord, reh_ord)
df_samp_ord <- stacked_samp_ord$remstats_stack

# ordinal flag
expect_true(stacked_samp_ord$ordinal,
  info = "sampled ordinal: ordinal flag is TRUE")

# No log_interevent for ordinal
expect_false("log_interevent" %in% colnames(df_samp_ord),
  info = "sampled ordinal: no log_interevent column")

# Dimensions
expect_equal(nrow(df_samp_ord), stacked_samp_ord$E * stacked_samp_ord$S,
  info = "sampled ordinal: nrow = E * S")

# Response and weights
expect_equal(sum(df_samp_ord$obs), stacked_samp_ord$E,
  info = "sampled ordinal: total cases equals E")
expect_true(all(df_samp_ord$weight[df_samp_ord$obs == 1L] == 1.0),
  info = "sampled ordinal: case weights are 1")
