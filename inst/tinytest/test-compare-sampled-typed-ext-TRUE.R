# Tests that case-control sampled tomstats (sampling=TRUE) matches full tomstats
# at sampled dyad positions, when extend_riskset_by_type = TRUE.
#
# Scenarios covered:
#   A: active riskset,  directed=TRUE,  memory=decay,  ext=TRUE
#   B: full riskset,    directed=FALSE, memory=decay,  ext=TRUE
#   C: manual riskset,  directed=TRUE,  memory=decay,  ext=TRUE
#   D: full riskset,    directed=TRUE,  ordinal=TRUE,  memory=window, ext=TRUE
#
# With ext=TRUE, sample_map indexes into the TYPED riskset columns of ts_full,
# and type-specific slices ("separate"/"interact") are verified for correctness.
# "separate" slices replicate values across dyad types for the same actor pair.
# "interact" slices zero out non-matching dyad type columns.

library(tinytest)

data(history, package = "remstats", envir = environment())
data(info,    package = "remstats", envir = environment())

colnames(history)[colnames(history) == "setting"] <- "type"

# Add some events happening in same interval
history$time[7:8] <- history$time[9]
history[4, ]      <- history[5, ]

# ---------------------------------------------------------------------------
# Core helper
#
# For each scenario and formula:
#   1. Runs sampling with two different seeds -> different sampled dyads.
#      Verifies values at sampled positions match ts_full in both runs.
#   2. Verifies slice names are identical between sampled and full output.
#   3. For "interact" slices: verifies zeroing of non-matching dyad type columns.
#
# sample_map is 1-based and indexes into the TYPED riskset columns of ts_full.
# ---------------------------------------------------------------------------
check_sampled_equals_full <- function(effects, reh,
                                       memory = "full", memory_value = NA,
                                       start, stop,
                                       samp_num = 5L,
                                       seed1 = 1L, seed2 = 42L,
                                       tol = 1e-12) {
  args <- list(
    effects, reh = reh,
    attr_actors  = info,
    memory       = memory,
    memory_value = memory_value,
    first        = start,
    last         = stop
  )

  ts_samp1 <- do.call(tomstats,
    c(args, list(sampling = TRUE, samp_num = samp_num, seed = seed1)))
  ts_samp2 <- do.call(tomstats,
    c(args, list(sampling = TRUE, samp_num = samp_num, seed = seed2)))
  ts_full  <- do.call(tomstats,
    c(args, list(sampling = FALSE)))

  # Slice names must match
  expect_equal(
    dimnames(ts_samp1)[[3]],
    dimnames(ts_full)[[3]],
    info = "slice names match between sampled and full"
  )

  smap1 <- attr(ts_samp1, "sample_map")
  smap2 <- attr(ts_samp2, "sample_map")
  expect_true(!is.null(smap1), info = "sample_map present for seed1")
  expect_true(!is.null(smap2), info = "sample_map present for seed2")
  expect_true(!identical(smap1, smap2), info = "different seeds produce different samples")

  slices <- dimnames(ts_samp1)[[3]]
  M <- dim(ts_samp1)[1]
  S <- dim(ts_samp1)[2]

  # Core value check: ts_samp[m, s, ] == ts_full[m, sample_map[m,s], ]
  # sample_map indexes into typed riskset columns of ts_full (ext=TRUE)
  check_values <- function(ts_samp, smap) {
    for (m in seq_len(M)) {
      for (s in seq_len(S)) {
        d <- smap[m, s]
        expect_true(!is.na(d), info = paste("m=", m, "s=", s, "sample_map not NA"))
        expect_equal(
          as.numeric(ts_samp[m, s, ]),
          as.numeric(ts_full[m, d, ]),
          tol = tol,
          info = paste("m=", m, "s=", s, "d=", d)
        )
      }
    }
  }

  check_values(ts_samp1, smap1)
  check_values(ts_samp2, smap2)

  # For "interact" slices (two dots): verify zeroing of non-matching dyad types.
  # sample_map indexes typed riskset — use riskset type column directly.
  typed_slices <- slices[grepl("\\.", slices)]
  interact_slices <- typed_slices[vapply(typed_slices, function(s) {
    sum(strsplit(s, "", fixed = TRUE)[[1]] == ".") >= 2L
  }, logical(1L))]

  if (length(interact_slices) > 0) {
    rs <- attr(ts_full, "riskset")
    if ("type" %in% colnames(rs)) {
      types_by_col <- as.character(rs$type)
      for (m in seq_len(M)) {
        for (s in seq_len(S)) {
          d         <- smap1[m, s]
          dyad_type <- types_by_col[d]
          for (sl in interact_slices) {
            sl_type <- sub(".*\\.", "", sl)  # last component after final dot
            if (sl_type != dyad_type) {
              expect_equal(
                as.numeric(ts_samp1[m, s, sl]), 0,
                tol = tol,
                info = paste("interact zero: m=", m, "s=", s,
                             "slice=", sl, "dyad_type=", dyad_type)
              )
            }
          }
        }
      }
    }
  }

  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# SCENARIO A: active riskset, directed=TRUE, memory=decay, ext=TRUE
# ---------------------------------------------------------------------------
h_A     <- history[1:33, ]
start_A <- 2; stop_A <- 20

reh_A <- remify(edgelist = h_A, model = "tie", riskset = "active",
                extend_riskset_by_type = TRUE)

tests_A <- list(
  # inertia & reciprocity
  inertia_sep      = ~ inertia(consider_type = "separate"),
  inertia_int      = ~ inertia(consider_type = "interact"),
  inertia_ig       = ~ inertia(consider_type = FALSE),
  reciprocity_sep  = ~ reciprocity(consider_type = "separate"),
  reciprocity_ig   = ~ reciprocity(consider_type = FALSE),

  # degrees (directed)
  degrees_sep      = ~ indegreeSender(consider_type = "separate") +
                       outdegreeSender(consider_type = "separate") +
                       indegreeReceiver(consider_type = "separate") +
                       outdegreeReceiver(consider_type = "separate"),
  degrees_ig       = ~ indegreeSender(consider_type = FALSE) +
                       outdegreeSender(consider_type = FALSE) +
                       indegreeReceiver(consider_type = FALSE) +
                       outdegreeReceiver(consider_type = FALSE),

  # mixed: separate + ignore in same formula
  mixed            = ~ inertia(consider_type = "separate") +
                       outdegreeSender(consider_type = FALSE),

  # triads
  triads_sep       = ~ otp(consider_type = "separate") + itp(consider_type = "separate") +
                       isp(consider_type = "separate") + osp(consider_type = "separate"),
  triads_ig        = ~ otp(consider_type = FALSE) + itp(consider_type = FALSE) +
                       isp(consider_type = FALSE) + osp(consider_type = FALSE),

  # pshifts
  pshifts_sep      = ~ psABBA(consider_type = "separate") + psABXY(consider_type = "separate") +
                       psABAY(consider_type = "separate"),
  pshifts_ig       = ~ psABBA(consider_type = FALSE) + psABXY(consider_type = FALSE) +
                       psABAY(consider_type = FALSE),

  # recency
  recency_sep      = ~ recencySendReceiver(consider_type = "separate") +
                       recencyReceiveReceiver(consider_type = "separate") +
                       recencyContinue(consider_type = "separate"),
  recency_ig       = ~ recencySendReceiver(consider_type = FALSE) +
                       recencyReceiveReceiver(consider_type = FALSE) +
                       recencyContinue(consider_type = FALSE),

  # rrank
  rrank_sep        = ~ rrankSend(consider_type = "separate") +
                       rrankReceive(consider_type = "separate"),
  rrank_ig         = ~ rrankSend(consider_type = FALSE) +
                       rrankReceive(consider_type = FALSE),

  # exogenous
  exo              = ~ send("extraversion", info) + receive("extraversion", info)
)

for (nm in names(tests_A)) {
  check_sampled_equals_full(
    tests_A[[nm]], reh = reh_A,
    memory = "decay", memory_value = 1000,
    start = start_A, stop = stop_A
  )
}

# ---------------------------------------------------------------------------
# SCENARIO B: full riskset, directed=FALSE, memory=decay, ext=TRUE
# ---------------------------------------------------------------------------
h_B     <- history[1:44, ]
start_B <- 3; stop_B <- 33

reh_B <- remify(edgelist = h_B, model = "tie", riskset = "full",
                directed = FALSE, extend_riskset_by_type = TRUE)

tests_B <- list(
  inertia_sep      = ~ inertia(consider_type = "separate"),
  inertia_ig       = ~ inertia(consider_type = FALSE),

  degrees_sep      = ~ totaldegreeDyad(consider_type = "separate") +
                       degreeMin(consider_type = "separate") +
                       degreeMax(consider_type = "separate") +
                       degreeDiff(consider_type = "separate"),
  degrees_ig       = ~ totaldegreeDyad(consider_type = FALSE) +
                       degreeMin(consider_type = FALSE) +
                       degreeMax(consider_type = FALSE) +
                       degreeDiff(consider_type = FALSE),

  triads_sep       = ~ sp(consider_type = "separate"),
  triads_ig        = ~ sp(consider_type = FALSE),

  pshifts_sep      = ~ psABAY(consider_type = "separate") + psABAB(consider_type = "separate"),
  pshifts_ig       = ~ psABAY(consider_type = FALSE) + psABAB(consider_type = FALSE),

  recency_sep      = ~ recencyContinue(consider_type = "separate"),
  recency_ig       = ~ recencyContinue(consider_type = FALSE),

  exo              = ~ same("sex", info) + difference("age", info)
)

for (nm in names(tests_B)) {
  check_sampled_equals_full(
    tests_B[[nm]], reh = reh_B,
    memory = "decay", memory_value = 1000,
    start = start_B, stop = stop_B
  )
}

# ---------------------------------------------------------------------------
# SCENARIO C: manual riskset, directed=TRUE, memory=decay, ext=TRUE
# ---------------------------------------------------------------------------
h_C     <- history[1:22, ]
start_C <- 2; stop_C <- 18

reh_C <- suppressWarnings(
  remify(edgelist = h_C, model = "tie", riskset = "manual",
        manual.riskset = h_C[, c("actor1", "actor2")],
        extend_riskset_by_type = TRUE)
)

tests_C <- list(
  inertia_sep      = ~ inertia(consider_type = "separate"),
  inertia_int      = ~ inertia(consider_type = "interact"),
  inertia_ig       = ~ inertia(consider_type = FALSE),
  reciprocity_sep  = ~ reciprocity(consider_type = "separate"),
  degrees_sep      = ~ indegreeSender(consider_type = "separate") +
                       outdegreeSender(consider_type = "separate"),
  exo              = ~ same("sex", info) + difference("age", info)
)

for (nm in names(tests_C)) {
  check_sampled_equals_full(
    tests_C[[nm]], reh = reh_C,
    memory = "decay", memory_value = 1000,
    start = start_C, stop = stop_C
  )
}

# ---------------------------------------------------------------------------
# SCENARIO D: full riskset, directed=TRUE, ordinal=TRUE, memory=window, ext=TRUE
# ---------------------------------------------------------------------------
h_D     <- history[1:25, ]
start_D <- 3; stop_D <- 20

reh_D <- remify(edgelist = h_D, model = "tie", riskset = "full",
                directed = TRUE, ordinal = TRUE,
                extend_riskset_by_type = TRUE)

tests_D <- list(
  inertia_sep      = ~ inertia(consider_type = "separate"),
  inertia_ig       = ~ inertia(consider_type = FALSE),
  degrees_sep      = ~ indegreeSender(consider_type = "separate") +
                       outdegreeSender(consider_type = "separate"),
  triads_sep       = ~ otp(consider_type = "separate") + itp(consider_type = "separate")
)

for (nm in names(tests_D)) {
  check_sampled_equals_full(
    tests_D[[nm]], reh = reh_D,
    memory = "window", memory_value = 3,
    start = start_D, stop = stop_D,
    samp_num = 10L
  )
}

# ---------------------------------------------------------------------------
# Shape checks: riskset for ext=TRUE should have type column in both modes
# ---------------------------------------------------------------------------
ts_full_shape <- tomstats(
  ~ inertia(consider_type = "separate"),
  reh = reh_A, memory = "decay", memory_value = 1000,
  first = start_A, last = stop_A, sampling = FALSE
)
ts_samp_shape <- tomstats(
  ~ inertia(consider_type = "separate"),
  reh = reh_A, memory = "decay", memory_value = 1000,
  first = start_A, last = stop_A,
  sampling = TRUE, samp_num = 5L, seed = 1L
)

# ext=TRUE: riskset should have type column
expect_true("type" %in% colnames(attr(ts_full_shape, "riskset")),
  info = "ext=TRUE full riskset has type column")
expect_true("type" %in% colnames(attr(ts_samp_shape, "riskset")),
  info = "ext=TRUE sampled riskset has type column")

# Both paths produce identical type-split slice names
expect_equal(
  dimnames(ts_full_shape)[[3]],
  c("baseline", "inertia.social", "inertia.work"),
  info = "full path: separate slice names ext=TRUE"
)
expect_equal(
  dimnames(ts_samp_shape)[[3]],
  c("baseline", "inertia.social", "inertia.work"),
  info = "sampled path: separate slice names ext=TRUE"
)

# "interact" produces C^2 slices
ts_int_shape <- tomstats(
  ~ inertia(consider_type = "interact"),
  reh = reh_A, memory = "decay", memory_value = 1000,
  first = start_A, last = stop_A, sampling = FALSE
)
expect_true(
  all(c("inertia.social.social", "inertia.social.work",
        "inertia.work.social",   "inertia.work.work") %in%
      dimnames(ts_int_shape)[[3]]),
  info = "interact produces C^2 slices ext=TRUE"
)
