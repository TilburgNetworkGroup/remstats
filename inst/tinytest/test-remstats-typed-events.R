# Tests for typed events: consider_type x extend_riskset_by_type orthogonality
#
# Covers:
#   - riskset = "full",   directed = TRUE
#   - riskset = "active", directed = TRUE
#   - riskset = "manual", directed = TRUE
#   - riskset = "full",   directed = FALSE
#
# For each combination, tests all four cases:
#   ext=TRUE,  consider_type="interact" -> [M x D_typed x C^2] named stat.social.social etc
#   ext=TRUE,  consider_type="separate" -> [M x D_typed x C]   named stat.social, stat.work
#   ext=TRUE,  consider_type="ignore" -> [M x D_typed x 1]  named stat
#   ext=FALSE, consider_type="separate" -> [M x D_dyad x C] named stat.social, stat.work
#   ext=FALSE, consider_type="ignore" -> [M x D_dyad x 1]  named stat
#
# Note: type slices are in alphabetical order of the type label ("social" < "work").

data(history, package = "remstats")
edgelist <- history
colnames(edgelist)[colnames(edgelist) == "setting"] <- "type"

M <- nrow(edgelist)   # 115

# ---------------------------------------------------------------------------
# Helper: compute expected weighted inertia for a given dyad at event m.
# ---------------------------------------------------------------------------
expected_inertia <- function(edgelist, m, a1, a2, type = NULL, directed = TRUE) {
  prior <- edgelist[seq_len(m - 1), ]
  if (!directed) {
    swap <- prior$actor1 > prior$actor2
    tmp <- prior$actor1[swap]
    prior$actor1[swap] <- prior$actor2[swap]
    prior$actor2[swap] <- tmp
    lo <- min(a1, a2); hi <- max(a1, a2)
    a1 <- lo; a2 <- hi
  }
  prior <- prior[prior$actor1 == a1 & prior$actor2 == a2, ]
  if (!is.null(type)) prior <- prior[prior$type == type, ]
  sum(prior$weight)
}

# ---------------------------------------------------------------------------
# Value checker: tests inertia values at given events for a specific dyad.
# Slices are in alphabetical type order: "inertia.social" first, "inertia.work" second.
# ---------------------------------------------------------------------------
check_inertia_values <- function(stats_TT, stats_TF, stats_FT, stats_FF,
                                  rs_TT, rs_FT,
                                  edgelist, a1, a2,
                                  events, label, directed = TRUE) {
  s_col <- if ("sender" %in% names(rs_TT)) "sender" else "actor1"
  r_col <- if ("receiver" %in% names(rs_TT)) "receiver" else "actor2"
  if (!directed) { lo <- min(a1,a2); hi <- max(a1,a2); a1 <- lo; a2 <- hi }

  col_work   <- which(rs_TT[[s_col]] == a1 & rs_TT[[r_col]] == a2 & rs_TT$type == "work")
  col_social <- which(rs_TT[[s_col]] == a1 & rs_TT[[r_col]] == a2 & rs_TT$type == "social")
  col_dyad   <- which(rs_FT[[s_col]] == a1 & rs_FT[[r_col]] == a2)

  for (m in events) {
    exp_w <- expected_inertia(edgelist, m, a1, a2, type = "work",   directed = directed)
    exp_s <- expected_inertia(edgelist, m, a1, a2, type = "social", directed = directed)
    exp_a <- expected_inertia(edgelist, m, a1, a2, type = NULL,     directed = directed)

    if (length(col_work) > 0) {
    	expect_equal(as.numeric(stats_TT[m, col_work, "inertia.work"]), exp_w,
    							 info = paste(label, "TT.work m=", m))
    	expect_equal(as.numeric(stats_TF[m, col_work, "inertia"]), exp_a,
    							 info = paste(label, "TF.ignore m=", m))
    }
    if (length(col_social) > 0) {
    	expect_equal(as.numeric(stats_TT[m, col_social, "inertia.social"]), exp_s,
    							 info = paste(label, "TT.social m=", m))
    }
    if (length(col_dyad) > 0) {
      expect_equal(as.numeric(stats_FT[m, col_dyad, "inertia.work"]),    exp_w,
        info = paste(label, "FT.work m=",    m))
      expect_equal(as.numeric(stats_FT[m, col_dyad, "inertia.social"]),  exp_s,
        info = paste(label, "FT.social m=",  m))
      expect_equal(as.numeric(stats_FF[m, col_dyad, "inertia"]), exp_a,
        info = paste(label, "FF.ignore m=", m))
    }
  }
}

# ---------------------------------------------------------------------------
# Consistency: FT/FF values must match the corresponding columns of TT/TF.
#
# The FT type-slice checks are valid for all riskset types: ext=FALSE just
# restricts the riskset to D_dyad rows, but the per-type values are computed
# from the same typed inertia matrix as TT.
#
# The FF TypeAgg check (FF.TypeAgg == TF.TypeAgg) is only valid for FULL
# risksets. For active/manual risksets, TF's TypeAgg at a (actor1,actor2,type)
# row only accumulates events of that type (because the C++ risksetMatrix only
# has a valid slot for observed type), while FF accumulates all types for that
# pair. They can genuinely differ when a dyad was observed in one type but not
# the other. Pass check_TypeAgg=FALSE to skip this for active/manual risksets.
# ---------------------------------------------------------------------------
check_consistency <- function(stats_TT, stats_TF, stats_FT, stats_FF,
                               rs_TT, rs_FT, label,
                               check_TypeAgg = TRUE) {
  s_col <- if ("sender" %in% names(rs_TT)) "sender" else "actor1"
  r_col <- if ("receiver" %in% names(rs_TT)) "receiver" else "actor2"

  social_cols <- which(rs_TT$type == "social")
  work_cols   <- which(rs_TT$type == "work")

  key_FT   <- paste(rs_FT[[s_col]], rs_FT[[r_col]], sep = "|")
  key_soc  <- paste(rs_TT[[s_col]][social_cols], rs_TT[[r_col]][social_cols], sep = "|")
  key_work <- paste(rs_TT[[s_col]][work_cols],   rs_TT[[r_col]][work_cols],   sep = "|")

  m_soc  <- match(key_FT, key_soc)
  m_work <- match(key_FT, key_work)
  valid_s <- which(!is.na(m_soc))
  valid_w <- which(!is.na(m_work))

  expect_equal(
    stats_FT[, valid_s, "inertia.social"],
    stats_TT[, social_cols[m_soc[valid_s]], "inertia.social"],
    info = paste(label, "FT.social == TT[social_cols]")
  )
  expect_equal(
    stats_FT[, valid_w, "inertia.work"],
    stats_TT[, work_cols[m_work[valid_w]], "inertia.work"],
    info = paste(label, "FT.work == TT[work_cols]")
  )

  # FF inertia == TF inertia: only valid for full risksets.
  # For active/manual, TF TypeAgg at a type-row only sums events of that type
  # (missing-type slot = -1 in risksetMatrix → skipped in transform_inertia),
  # while FF always sums all types. They can legitimately differ.
  if (check_TypeAgg) {
    both <- intersect(key_FT[valid_s], key_FT[valid_w])
    valid_both_FT <- which(key_FT %in% both)
    if (length(valid_both_FT) > 0) {
      soc_idx <- social_cols[match(key_FT[valid_both_FT], key_soc)]
      valid_match <- which(!is.na(soc_idx))
      if (length(valid_match) > 0) {
        expect_equal(
          stats_FF[, valid_both_FT[valid_match], "inertia"],
          stats_TF[, soc_idx[valid_match], "inertia"],
          info = paste(label, "FF.inertia == TF.inertia (full riskset only)")
        )
      }
    }
  }
}

# ===========================================================================
# SECTION 1: riskset = "full", directed = TRUE
# ===========================================================================
N       <- 10L
D_dyad  <- N * (N - 1L)   # 90
D_typed <- D_dyad * 2L    # 180

reh_full_T <- remify(edgelist, model = "tie", riskset = "full",
                      extend_riskset_by_type = TRUE)
reh_full_F <- remify(edgelist, model = "tie", riskset = "full",
                      extend_riskset_by_type = FALSE)

expect_true(reh_full_T$meta$with_type_riskset)
expect_false(reh_full_F$meta$with_type_riskset)

stats_TT_full <- remstats(reh_full_T, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_TT_full_int <- remstats(reh_full_T, tie_effects = ~ inertia(consider_type = "interact"), start = 1)
stats_TF_full <- remstats(reh_full_T, tie_effects = ~ inertia(consider_type = FALSE), start = 1)
stats_FT_full <- remstats(reh_full_F, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_FF_full <- remstats(reh_full_F, tie_effects = ~ inertia(consider_type = FALSE), start = 1)

# Shapes: baseline + inertia.social + inertia.work = 3; baseline + inertia.TypeAgg = 2
expect_equal(dim(stats_TT_full), c(M, D_typed, 3L), info = "full TT shape")
expect_equal(dim(stats_TF_full), c(M, D_typed, 2L), info = "full TF shape")
expect_equal(dim(stats_FT_full), c(M, D_dyad,  3L), info = "full FT shape")
expect_equal(dim(stats_FF_full), c(M, D_dyad,  2L), info = "full FF shape")

# Dimnames: types in alphabetical order ("social" < "work")
expect_equal(dimnames(stats_TT_full)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_TF_full)[[3]], c("baseline", "inertia"))
expect_equal(dimnames(stats_FT_full)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_FF_full)[[3]], c("baseline", "inertia"))

# Riskset attributes
rs_TT_full <- attr(stats_TT_full, "riskset")
rs_FT_full <- attr(stats_FT_full, "riskset")
expect_true("type"  %in% colnames(rs_TT_full))
expect_false("type" %in% colnames(rs_FT_full))
expect_equal(nrow(rs_TT_full), D_typed)
expect_equal(nrow(rs_FT_full), D_dyad)

# Zero-pattern for TT (social cols have zero work-type inertia, vice versa)
social_cols_full <- which(rs_TT_full$type == "social")
work_cols_full   <- which(rs_TT_full$type == "work")
# "interact": social dyad cols should have zero work-inertia (.social.work slice)
expect_true(all(stats_TT_full_int[, social_cols_full, "inertia.social.work"] == 0))
expect_true(all(stats_TT_full_int[, work_cols_full, "inertia.work.social"] == 0))

# Consistency
check_consistency(stats_TT_full, stats_TF_full, stats_FT_full, stats_FF_full,
                  rs_TT_full, rs_FT_full, "full/directed")

# Spot values
check_inertia_values(stats_TT_full, stats_TF_full, stats_FT_full, stats_FF_full,
                     rs_TT_full, rs_FT_full, edgelist,
                     a1 = 105, a2 = 109, events = c(6L, 113L, 114L, 115L),
                     label = "full/dir (105->109)")
check_inertia_values(stats_TT_full, stats_TF_full, stats_FT_full, stats_FF_full,
                     rs_TT_full, rs_FT_full, edgelist,
                     a1 = 105, a2 = 104, events = c(113L, 114L, 115L),
                     label = "full/dir (105->104)")
check_inertia_values(stats_TT_full, stats_TF_full, stats_FT_full, stats_FF_full,
                     rs_TT_full, rs_FT_full, edgelist,
                     a1 = 107, a2 = 105, events = c(113L, 114L, 115L),
                     label = "full/dir (107->105)")

# ===========================================================================
# SECTION 2: riskset = "active", directed = TRUE
# ===========================================================================
reh_active_T <- remify(edgelist, model = "tie", riskset = "active",
                        extend_riskset_by_type = TRUE)
reh_active_F <- remify(edgelist, model = "tie", riskset = "active",
                        extend_riskset_by_type = FALSE)

stats_TT_act <- remstats(reh_active_T, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_TT_act_int <- remstats(reh_active_T, tie_effects = ~ inertia(consider_type = "interact"), start = 1)
stats_TF_act <- remstats(reh_active_T, tie_effects = ~ inertia(consider_type = FALSE), start = 1)
stats_FT_act <- remstats(reh_active_F, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_FF_act <- remstats(reh_active_F, tie_effects = ~ inertia(consider_type = FALSE), start = 1)

rs_TT_act <- attr(stats_TT_act, "riskset")
rs_FT_act <- attr(stats_FT_act, "riskset")
D_act_typed <- nrow(rs_TT_act)
D_act_dyad  <- nrow(rs_FT_act)

# Shapes
expect_equal(dim(stats_TT_act), c(M, D_act_typed, 3L), info = "active TT shape")
expect_equal(dim(stats_TF_act), c(M, D_act_typed, 2L), info = "active TF shape")
expect_equal(dim(stats_FT_act), c(M, D_act_dyad,  3L), info = "active FT shape")
expect_equal(dim(stats_FF_act), c(M, D_act_dyad,  2L), info = "active FF shape")

# Dimnames
expect_equal(dimnames(stats_TT_act)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_TF_act)[[3]], c("baseline", "inertia"))
expect_equal(dimnames(stats_FT_act)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_FF_act)[[3]], c("baseline", "inertia"))

# Riskset
expect_true("type"  %in% colnames(rs_TT_act))
expect_false("type" %in% colnames(rs_FT_act))
# Note: for active riskset D_typed != necessarily 2*D_dyad (work/social active sets differ)

# Zero-pattern
social_cols_act <- which(rs_TT_act$type == "social")
work_cols_act   <- which(rs_TT_act$type == "work")
# "interact": social dyad cols should have zero work-inertia (.social.work slice)
expect_true(all(stats_TT_act_int[, social_cols_act, "inertia.social.work"] == 0))
expect_true(all(stats_TT_act_int[, work_cols_act, "inertia.work.social"] == 0))

# Consistency (only dyads in both type sets for TypeAgg comparison)
check_consistency(stats_TT_act, stats_TF_act, stats_FT_act, stats_FF_act,
                  rs_TT_act, rs_FT_act, "active/directed", check_TypeAgg = FALSE)

# Spot values
check_inertia_values(stats_TT_act, stats_TF_act, stats_FT_act, stats_FF_act,
                     rs_TT_act, rs_FT_act, edgelist,
                     a1 = 105, a2 = 109, events = c(6L, 113L, 114L, 115L),
                     label = "active/dir (105->109)")
check_inertia_values(stats_TT_act, stats_TF_act, stats_FT_act, stats_FF_act,
                     rs_TT_act, rs_FT_act, edgelist,
                     a1 = 107, a2 = 105, events = c(113L, 114L, 115L),
                     label = "active/dir (107->105)")

# ===========================================================================
# SECTION 3: riskset = "manual", directed = TRUE
# ===========================================================================
manual_rs <- data.frame(
  actor1 = c(105, 105, 113, 109, 101),
  actor2 = c(109, 113, 107, 105, 115)
)

reh_manual_T <- suppressWarnings(
  remify(edgelist, model = "tie", riskset = "manual",
          manual.riskset = manual_rs, extend_riskset_by_type = TRUE)
)
reh_manual_F <- suppressWarnings(
  remify(edgelist, model = "tie", riskset = "manual",
          manual.riskset = manual_rs, extend_riskset_by_type = FALSE)
)

stats_TT_man <- remstats(reh_manual_T, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_TT_man_int <- remstats(reh_manual_T, tie_effects = ~ inertia(consider_type = "interact"), start = 1)
stats_TF_man <- remstats(reh_manual_T, tie_effects = ~ inertia(consider_type = FALSE), start = 1)
stats_FT_man <- remstats(reh_manual_F, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_FF_man <- remstats(reh_manual_F, tie_effects = ~ inertia(consider_type = FALSE), start = 1)

rs_TT_man <- attr(stats_TT_man, "riskset")
rs_FT_man <- attr(stats_FT_man, "riskset")
D_man_typed <- nrow(rs_TT_man)
D_man_dyad  <- nrow(rs_FT_man)

# Shapes
expect_equal(dim(stats_TT_man), c(M, D_man_typed, 3L), info = "manual TT shape")
expect_equal(dim(stats_TF_man), c(M, D_man_typed, 2L), info = "manual TF shape")
expect_equal(dim(stats_FT_man), c(M, D_man_dyad,  3L), info = "manual FT shape")
expect_equal(dim(stats_FF_man), c(M, D_man_dyad,  2L), info = "manual FF shape")

# Dimnames
expect_equal(dimnames(stats_TT_man)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_TF_man)[[3]], c("baseline", "inertia"))
expect_equal(dimnames(stats_FT_man)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_FF_man)[[3]], c("baseline", "inertia"))

# Riskset
expect_true("type"  %in% colnames(rs_TT_man))
expect_false("type" %in% colnames(rs_FT_man))

# Zero-pattern
social_cols_man <- which(rs_TT_man$type == "social")
work_cols_man   <- which(rs_TT_man$type == "work")
# "interact": social dyad cols should have zero work-inertia (.social.work slice)
expect_true(all(stats_TT_man_int[, social_cols_man, "inertia.social.work"] == 0))
expect_true(all(stats_TT_man_int[, work_cols_man, "inertia.work.social"] == 0))

# Consistency
check_consistency(stats_TT_man, stats_TF_man, stats_FT_man, stats_FF_man,
                  rs_TT_man, rs_FT_man, "manual/directed", check_TypeAgg = FALSE)

# Spot values: (105->109) is in the manual riskset
check_inertia_values(stats_TT_man, stats_TF_man, stats_FT_man, stats_FF_man,
                     rs_TT_man, rs_FT_man, edgelist,
                     a1 = 105, a2 = 109, events = c(6L, 113L, 114L, 115L),
                     label = "manual/dir (105->109)")

# ===========================================================================
# SECTION 4: riskset = "full", directed = FALSE
# ===========================================================================
reh_undir_T <- remify(edgelist, model = "tie", riskset = "full",
                        directed = FALSE, extend_riskset_by_type = TRUE)
reh_undir_F <- remify(edgelist, model = "tie", riskset = "full",
                        directed = FALSE, extend_riskset_by_type = FALSE)

stats_TT_undir <- remstats(reh_undir_T, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_TT_undir_int <- remstats(reh_undir_T, tie_effects = ~ inertia(consider_type = "interact"), start = 1)
stats_TF_undir <- remstats(reh_undir_T, tie_effects = ~ inertia(consider_type = FALSE), start = 1)
stats_FT_undir <- remstats(reh_undir_F, tie_effects = ~ inertia(consider_type = "separate"), start = 1)
stats_FF_undir <- remstats(reh_undir_F, tie_effects = ~ inertia(consider_type = FALSE), start = 1)

rs_TT_undir <- attr(stats_TT_undir, "riskset")
rs_FT_undir <- attr(stats_FT_undir, "riskset")
# Derive D from actual riskset (avoids assumption about self-loops / actor pairing)
D_undir_typed <- nrow(rs_TT_undir)
D_undir_dyad  <- nrow(rs_FT_undir)
expect_equal(D_undir_typed, 2L * D_undir_dyad, info = "undir: D_typed == 2 * D_dyad")

# Shapes
expect_equal(dim(stats_TT_undir), c(M, D_undir_typed, 3L), info = "undir TT shape")
expect_equal(dim(stats_TF_undir), c(M, D_undir_typed, 2L), info = "undir TF shape")
expect_equal(dim(stats_FT_undir), c(M, D_undir_dyad,  3L), info = "undir FT shape")
expect_equal(dim(stats_FF_undir), c(M, D_undir_dyad,  2L), info = "undir FF shape")

# Dimnames
expect_equal(dimnames(stats_TT_undir)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_TF_undir)[[3]], c("baseline", "inertia"))
expect_equal(dimnames(stats_FT_undir)[[3]], c("baseline", "inertia.social", "inertia.work"))
expect_equal(dimnames(stats_FF_undir)[[3]], c("baseline", "inertia"))

# Riskset
expect_true("type"  %in% colnames(rs_TT_undir))
expect_false("type" %in% colnames(rs_FT_undir))

# Zero-pattern
social_cols_undir <- which(rs_TT_undir$type == "social")
work_cols_undir   <- which(rs_TT_undir$type == "work")
# "interact": social dyad cols should have zero work-inertia (.social.work slice)
expect_true(all(stats_TT_undir_int[, social_cols_undir, "inertia.social.work"] == 0))
expect_true(all(stats_TT_undir_int[, work_cols_undir, "inertia.work.social"] == 0))

# Consistency
check_consistency(stats_TT_undir, stats_TF_undir, stats_FT_undir, stats_FF_undir,
                  rs_TT_undir, rs_FT_undir, "full/undirected")

# Spot values
check_inertia_values(stats_TT_undir, stats_TF_undir, stats_FT_undir, stats_FF_undir,
                     rs_TT_undir, rs_FT_undir, edgelist,
                     a1 = 105, a2 = 109, events = c(6L, 113L, 114L, 115L),
                     label = "full/undir (105-109)", directed = FALSE)
check_inertia_values(stats_TT_undir, stats_TF_undir, stats_FT_undir, stats_FF_undir,
                     rs_TT_undir, rs_FT_undir, edgelist,
                     a1 = 107, a2 = 105, events = c(113L, 114L, 115L),
                     label = "full/undir (105-107)", directed = FALSE)

# ===========================================================================
# SECTION 5: mixed consider_type in one formula, ext=FALSE
# ===========================================================================
stats_mix <- remstats(reh_full_F,
  tie_effects = ~ inertia(consider_type = "interact") +
                  outdegreeSender(consider_type = FALSE), start = 1)

# baseline + inertia.social + inertia.work + outdegreeSender.TypeAgg = 4 slices
expect_equal(dim(stats_mix), c(M, D_dyad, 4L), info = "mixed: shape")
expect_equal(dimnames(stats_mix)[[3]],
  c("baseline", "inertia.social", "inertia.work", "outdegreeSender"),
  info = "mixed: dimnames")

# inertia slices must match standalone FT
expect_equal(stats_mix[, , "inertia.social"], stats_FT_full[, , "inertia.social"],
  info = "mixed: inertia.social matches standalone FT")
expect_equal(stats_mix[, , "inertia.work"],   stats_FT_full[, , "inertia.work"],
  info = "mixed: inertia.work matches standalone FT")

