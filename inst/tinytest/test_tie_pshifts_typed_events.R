# test-pshift-types.R
# Tests correct type-splitting of pshift statistics for typed events.
# Verifies that pshift slices are only non-zero at rows where the
# triggering event at the previous time point matches the slice type.

library(tinytest)
library(remify)
library(remstats)

# Small directed edgelist, 2 types
el <- data.frame(
	time   = 1:5,
	actor1 = c(1, 2, 1, 3, 2),
	actor2 = c(2, 3, 3, 2, 1),
	type   = c("social", "work", "social", "social", "work")
)

reh_FALSE <- remify(el, model = "tie", directed = TRUE,
										 extend_riskset_by_type = FALSE)
reh_TRUE  <- remify(el, model = "tie", directed = TRUE,
										 extend_riskset_by_type = TRUE)

eff_sep <- ~ psABBA(consider_type = "separate") +
	psABAB(consider_type = "separate") +
	psABAY(consider_type = "separate")
eff_int <- ~ psABBA(consider_type = "interact") +
	psABAB(consider_type = "interact") +
	psABAY(consider_type = "interact")
eff_ign <- ~ psABBA(consider_type = "ignore") +
	psABAB(consider_type = "ignore") +
	psABAY(consider_type = "ignore")

ts_FALSE     <- tomstats(eff_sep, reh = reh_FALSE, sampling = FALSE, start = 2)
ts_sep       <- tomstats(eff_sep, reh = reh_TRUE,  sampling = FALSE, start = 2)
ts_int       <- tomstats(eff_int, reh = reh_TRUE,  sampling = FALSE, start = 2)
ts_ign_FALSE <- tomstats(eff_ign, reh = reh_FALSE, sampling = FALSE, start = 2)
ts_ign_TRUE  <- tomstats(eff_ign, reh = reh_TRUE,  sampling = FALSE, start = 2)

# ---------------------------------------------------------------------------
# Row 4: previous event = (3,2,social) — only social slices can be non-zero
# ---------------------------------------------------------------------------

# ext=FALSE "separate": all .work slices must be 0
expect_true(all(ts_FALSE[4, , grep("\\.work", dimnames(ts_FALSE)[[3]])] == 0),
						info = "ext=FALSE row4: all .work slices = 0")

# correct dyads non-zero in .social
expect_equal(as.numeric(ts_FALSE[4, 4, "psABBA.social"]), 1,
						 info = "ext=FALSE row4: psABBA.social dyad (2,3) = 1")
expect_equal(as.numeric(ts_FALSE[4, 6, "psABAB.social"]), 0,   # curr_sender=2 != prev_sender=3
						 info = "ext=FALSE row4: psABAB.social dyad (3,2) = 0")
expect_equal(as.numeric(ts_FALSE[4, 5, "psABAY.social"]), 0,   # curr_sender=2 != prev_sender=3
						 info = "ext=FALSE row4: psABAY.social dyad (3,1) = 0")

# ext=TRUE "separate": pshift pattern is same across all type slices
# (dyad pattern reflects the ABAB regardless of dyad type)
expect_true(all(ts_sep[4, , grep("psABBA\\.social", dimnames(ts_sep)[[3]])] ==
									ts_sep[4, , grep("psABBA\\.work",   dimnames(ts_sep)[[3]])]),
						info = "ext=TRUE separate row4: social and work slices identical")

# ext=TRUE "interact": all work.* slices must be 0 (past was not work)
expect_true(all(ts_int[4, , grep("work\\.", dimnames(ts_int)[[3]])] == 0),
						info = "ext=TRUE interact row4: all work.* slices = 0")

# ---------------------------------------------------------------------------
# Row 1 (time=3): previous time point = time=2, TWO simultaneous events:
# (1,2,social) and (2,3,work) — both .social AND .work can be non-zero
# ---------------------------------------------------------------------------
el2 <- data.frame(
	time   = c(2, 2, 3:5),
	actor1 = c(1, 2, 1, 3, 2),
	actor2 = c(2, 3, 3, 2, 1),
	type   = c("social", "work", "social", "social", "work")
)
reh2 <- remify(el2, model = "tie", directed = TRUE,
								extend_riskset_by_type = FALSE)
ts2  <- tomstats(eff_sep, reh = reh2, sampling = FALSE, start = 2)

# Row 2 corresponds to time=3, previous time point = time=2
# (1,2,social): psABAB.social → dyad (1,2) = 1 (sender 1 == curr sender 1)
# (2,3,work):   psABAB.work  → dyad (2,3) = 0 (sender 2 != curr sender 1)
expect_equal(as.numeric(ts2[2, 1, "psABAB.social"]), 1,
						 info = "simultaneous row2: psABAB.social dyad (1,2) = 1")
expect_equal(as.numeric(ts2[2, 1, "psABAB.work"]), 0,
						 info = "simultaneous row2: psABAB.work dyad (1,2) = 0")
expect_equal(as.numeric(ts2[2, 4, "psABAB.social"]), 0,
						 info = "simultaneous row2: psABAB.social dyad (2,3) = 0")
expect_equal(as.numeric(ts2[2, 4, "psABAB.work"]), 0,
						 info = "simultaneous row2: psABAB.work dyad (2,3) = 0")

# ---------------------------------------------------------------------------
# consider_type = "ignore": single slice, no type splitting, unaffected by fix
# ---------------------------------------------------------------------------

# Only 3 slices (no type splitting)
expect_equal(dimnames(ts_ign_FALSE)[[3]],
						 c("baseline", "psABBA", "psABAB", "psABAY"),
						 info = "ignore ext=FALSE: no type-split slices")
expect_equal(dimnames(ts_ign_TRUE)[[3]],
						 c("baseline", "psABBA", "psABAB", "psABAY"),
						 info = "ignore ext=TRUE: no type-split slices")

# Row 4 (previous = 3,2,social): correct dyads non-zero
# ext=FALSE (6 dyads)
expect_equal(as.numeric(ts_ign_FALSE[4, 4, "psABBA"]), 1,
						 info = "ignore ext=FALSE row4: psABBA dyad (2,3) = 1")
expect_equal(as.numeric(ts_ign_FALSE[4, 6, "psABAB"]), 1,
						 info = "ignore ext=FALSE row4: psABAB dyad (3,2) = 1")
expect_equal(as.numeric(ts_ign_FALSE[4, 5, "psABAY"]), 1,
						 info = "ignore ext=FALSE row4: psABAY dyad (3,1) = 1")

# ext=TRUE (12 dyads): both type rows should be 1 since ignore aggregates
expect_equal(as.numeric(ts_ign_TRUE[4, 4,  "psABBA"]), 1,
						 info = "ignore ext=TRUE row4: psABBA dyad (2,3,social) = 1")
expect_equal(as.numeric(ts_ign_TRUE[4, 10, "psABBA"]), 1,
						 info = "ignore ext=TRUE row4: psABBA dyad (2,3,work) = 1")
expect_equal(as.numeric(ts_ign_TRUE[4, 6,  "psABAB"]), 1,
						 info = "ignore ext=TRUE row4: psABAB dyad (3,2,social) = 1")
expect_equal(as.numeric(ts_ign_TRUE[4, 12, "psABAB"]), 1,
						 info = "ignore ext=TRUE row4: psABAB dyad (3,2,work) = 1")

