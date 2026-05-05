# Tests for stack_stats() with aggregate_time > 1 in remify
# Tests both tie-oriented and actor-oriented models
# Place in remstats/inst/tinytest/

if (!requireNamespace("remify", quietly = TRUE))
  exit_file("remify not available")

data("randomREH", package = "remify")

effects_tie   <- ~ remstats::inertia()
effects_send  <- ~ 1 + remstats::indegreeSender()
effects_recv  <- ~ remstats::inertia() + remstats::reciprocity()

# ── Tie model ────────────────────────────────────────────────────────────────

reh_t1  <- remify(edgelist = randomREH$edgelist, model = "tie", aggregate_time = 1)
reh_t10 <- remify(edgelist = randomREH$edgelist, model = "tie", aggregate_time = 10)

stats_t1  <- remstats(reh_t1,  effects_tie)
stats_t10 <- remstats(reh_t10, effects_tie)

# stack_stats must not error with aggregate_time = 1
stk_t1 <- stack_stats(stats_t1, reh_t1)
expect_true(
  is.data.frame(stk_t1$remstats_stack),
  info = "tie aggregate_time=1: stack_stats returns a data frame"
)

# stack_stats must not error with aggregate_time = 10
stk_t10 <- stack_stats(stats_t10, reh_t10)
expect_true(
  is.data.frame(stk_t10$remstats_stack),
  info = "tie aggregate_time=10: stack_stats returns a data frame"
)

# obs column must be numeric (not a list) in both cases
expect_true(
  is.numeric(stk_t1$remstats_stack$obs),
  info = "tie aggregate_time=1: obs column is numeric"
)
expect_true(
  is.numeric(stk_t10$remstats_stack$obs),
  info = "tie aggregate_time=10: obs column is numeric"
)

# obs values must be 0 or 1
expect_true(
  all(stk_t1$remstats_stack$obs  %in% c(0, 1)),
  info = "tie aggregate_time=1: obs values are 0/1"
)
expect_true(
  all(stk_t10$remstats_stack$obs %in% c(0, 1, 2)),
  info = "tie aggregate_time=10: obs values are 0/1"
)

# number of observed events must equal number of rows in aggregated edgelist
D_t1  <- reh_t1$D
D_t10 <- reh_t10$D

expect_equal(
  sum(stk_t1$remstats_stack$obs),
  nrow(reh_t1$edgelist) - 1L,   # first event has no stat row
  info = "tie aggregate_time=1: sum(obs) == number of modelled events"
)
expect_equal(
  sum(stk_t10$remstats_stack$obs),
  sum(sapply(reh_t10$edgelist_id$dyad, length)) -
    length(reh_t10$edgelist_id$dyad[[1]]),  # subtract first time point
  info = "tie aggregate_time=10: sum(obs) == total simultaneous events modelled"
)

# number of rows: E * D
E_t1  <- dim(stats_t1)[1]
E_t10 <- dim(stats_t10)[1]

expect_equal(
  nrow(stk_t1$remstats_stack),  E_t1  * D_t1,
  info = "tie aggregate_time=1: nrow == E * D"
)
expect_equal(
  nrow(stk_t10$remstats_stack), E_t10 * D_t10,
  info = "tie aggregate_time=10: nrow == E * D"
)

# ── Actor model ──────────────────────────────────────────────────────────────

reh_a1  <- remify(edgelist = randomREH$edgelist, model = "actor", aggregate_time = 1)
reh_a10 <- remify(edgelist = randomREH$edgelist, model = "actor", aggregate_time = 10)

stats_a1  <- remstats(reh_a1,
                      sender_effects   = effects_send,
                      receiver_effects = effects_recv)
stats_a10 <- remstats(reh_a10,
                      sender_effects   = effects_send,
                      receiver_effects = effects_recv)

# stack_stats must not error with aggregate_time = 1
stk_a1 <- stack_stats(stats_a1, reh_a1, add_actors = TRUE)
expect_true(
  is.data.frame(stk_a1$sender_stack),
  info = "actor aggregate_time=1: sender stack is a data frame"
)

# stack_stats must not error with aggregate_time = 10
stk_a10 <- stack_stats(stats_a10, reh_a10, add_actors = TRUE)
expect_true(
  is.data.frame(stk_a10$sender_stack),
  info = "actor aggregate_time=10: sender stack is a data frame"
)

# obs column must be numeric in both cases
expect_true(
  is.numeric(stk_a1$sender_stack$obs),
  info = "actor aggregate_time=1: sender obs is numeric"
)
expect_true(
  is.numeric(stk_a10$sender_stack$obs),
  info = "actor aggregate_time=10: sender obs is numeric"
)

expect_true(
  is.numeric(stk_a1$receiver_stack$obs),
  info = "actor aggregate_time=1: receiver obs is numeric"
)
expect_true(
  is.numeric(stk_a10$receiver_stack$obs),
  info = "actor aggregate_time=10: receiver obs is numeric"
)

