# inst/tinytest/test-overlap.R
library(tinytest)
library(remify)

# Helper: extract effect value for the event dyad at each time t
extract_event_series <- function(stats, edgelist, directed, effect_prefix) {
  rs <- attr(stats, "riskset")
  if (is.null(rs)) stop("No riskset attribute found on stats")

  effs <- dimnames(stats)[[3]]
  eff_name <- effs[grepl(paste0("^", effect_prefix), effs)][1]
  if (is.na(eff_name) || is.null(eff_name)) stop("No effect '", effect_prefix, "' found")

  if (directed) {
    s_col <- if ("sender" %in% names(rs)) "sender" else names(rs)[1]
    r_col <- if ("receiver" %in% names(rs)) "receiver" else names(rs)[2]
  } else {
    s_col <- if ("actor1" %in% names(rs)) "actor1" else names(rs)[1]
    r_col <- if ("actor2" %in% names(rs)) "actor2" else names(rs)[2]
  }

  rs_s <- suppressWarnings(as.integer(as.character(rs[[s_col]])))
  rs_r <- suppressWarnings(as.integer(as.character(rs[[r_col]])))

  if (!("id" %in% names(rs))) stop("Riskset has no 'id' column")
  rs_id <- rs$id

  T <- nrow(edgelist)
  out <- numeric(T)
  for (t in seq_len(T)) {
    i <- edgelist$actor1[t]
    j <- edgelist$actor2[t]
    if (!directed) {
      aa <- min(i, j); bb <- max(i, j)
      i <- aa; j <- bb
    }
    idx <- which(rs_s == i & rs_r == j)
    if (length(idx) != 1) stop("Could not uniquely match event dyad to riskset row at t=", t)
    out[t] <- stats[t, rs_id[idx], eff_name]
  }
  out
}

# Reference overlap computed from the inertia effect output
# (inertia semantics are whatever remstats uses; overlap is defined purely via inertia > 0).
ref_overlap_from_inertia <- function(inertia_stats, edgelist, directed, N) {
  rs <- attr(inertia_stats, "riskset")
  effs <- dimnames(inertia_stats)[[3]]
  in_name <- effs[effs == "inertia"][1]
  if (is.na(in_name)) stop("No inertia effect in stats")

  if (directed) {
    s_col <- if ("sender" %in% names(rs)) "sender" else names(rs)[1]
    r_col <- if ("receiver" %in% names(rs)) "receiver" else names(rs)[2]
  } else {
    s_col <- if ("actor1" %in% names(rs)) "actor1" else names(rs)[1]
    r_col <- if ("actor2" %in% names(rs)) "actor2" else names(rs)[2]
  }
  rs_s <- suppressWarnings(as.integer(as.character(rs[[s_col]])))
  rs_r <- suppressWarnings(as.integer(as.character(rs[[r_col]])))
  rs_id <- rs$id

  get_in <- function(t, a, b) {
    if (!directed) { aa <- min(a,b); bb <- max(a,b); a <- aa; b <- bb }
    idx <- which(rs_s == a & rs_r == b)
    if (length(idx) != 1) return(0)
    dyad_id <- rs_id[idx]
    inertia_stats[t, dyad_id, in_name]
  }

  T <- nrow(edgelist)
  out <- numeric(T)

  for (t in seq_len(T)) {
    A <- edgelist$actor1[t]
    B <- edgelist$actor2[t]
    if (!directed) { aa <- min(A,B); bb <- max(A,B); A <- aa; B <- bb }

    ks <- setdiff(seq_len(N), c(A, B))

    union_cnt <- 0
    inter_cnt <- 0
    for (k in ks) {
      if (directed) {
        hasA <- (get_in(t, A, k) > 0) || (get_in(t, k, A) > 0)
        hasB <- (get_in(t, B, k) > 0) || (get_in(t, k, B) > 0)
      } else {
        hasA <- (get_in(t, A, k) > 0)
        hasB <- (get_in(t, B, k) > 0)
      }
      if (hasA || hasB) union_cnt <- union_cnt + 1
      if (hasA && hasB) inter_cnt <- inter_cnt + 1
    }

    out[t] <- if (union_cnt > 0) inter_cnt / union_cnt else 0
  }

  out
}

# ---- Directed ----
ed_dir <- data.frame(
  time = 1:8,
  actor1 = c(1, 2, 1, 3, 2, 4, 1, 2),
  actor2 = c(2, 1, 3, 1, 3, 2, 4, 3)
)
reh_dir <- remify::remify(ed_dir, model = "tie", directed = TRUE, riskset = "full")

st_ov_dir <- remstats::remstats(reh_dir, tie_effects = ~ overlap(), method = "pt")
st_in_dir <- remstats::remstats(reh_dir, tie_effects = ~ inertia(), method = "pt")

computed_dir <- extract_event_series(st_ov_dir, ed_dir, directed = TRUE, effect_prefix = "overlap")
expected_dir <- ref_overlap_from_inertia(st_in_dir, ed_dir, directed = TRUE, N = 4)

expect_equal(computed_dir, expected_dir, tol = 1e-12)
expect_true(all(computed_dir >= 0 - 1e-12 & computed_dir <= 1 + 1e-12))

# ---- Undirected ----
ed_undir <- data.frame(
  time = 1:8,
  actor1 = c(1, 1, 2, 3, 2, 1, 2, 3),
  actor2 = c(2, 3, 3, 2, 1, 2, 4, 4)
)
reh_undir <- remify::remify(ed_undir, model = "tie", directed = FALSE, riskset = "full")

st_ov_undir <- remstats::remstats(reh_undir, tie_effects = ~ overlap(), method = "pt")
st_in_undir <- remstats::remstats(reh_undir, tie_effects = ~ inertia(), method = "pt")

computed_undir <- extract_event_series(st_ov_undir, ed_undir, directed = FALSE, effect_prefix = "overlap")
expected_undir <- ref_overlap_from_inertia(st_in_undir, ed_undir, directed = FALSE, N = 4)

expect_equal(computed_undir, expected_undir, tol = 1e-12)
expect_true(all(computed_undir >= 0 - 1e-12 & computed_undir <= 1 + 1e-12))
