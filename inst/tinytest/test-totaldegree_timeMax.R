# inst/tinytest/test-totaldegree_timeMax.R
library(tinytest)
library(remify)

# Helper: extract series for a specific dyad (i,j) at each time t from stats cube
extract_series <- function(stats, i, j, directed = TRUE) {
  rs <- attr(stats, "riskset")
  effs <- dimnames(stats)[[3]]
  if (is.null(rs)) stop("No riskset attribute found")
  
  # find totaldegreeDyad effect name (could be exactly this, or with suffix)
  td_name <- effs[grepl("^totaldegreeDyad", effs)][1]
  if (is.na(td_name)) stop("No totaldegreeDyad effect found in stats")
  
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
  
  idx <- which(rs_s == i & rs_r == j)
  if (length(idx) != 1) stop("Could not uniquely match dyad to riskset row")
  stats[, rs_id[idx], td_name]
}

# Helper: compute expected timeMax scaling from a raw stats cube (same effect slice)
expected_timeMax <- function(raw_mat) {
  # raw_mat: (T x R)
  out <- raw_mat
  for (t in seq_len(nrow(out))) {
    m <- max(out[t, ], na.rm = TRUE)
    if (is.finite(m) && m > 0) out[t, ] <- out[t, ] / m
    else out[t, ] <- 0
  }
  out
}

# -------------------
# Directed case
# -------------------
ed_dir <- data.frame(
  time = 1:10,
  actor1 = c(1,2,1,3,2,4,1,2,3,4),
  actor2 = c(2,1,3,1,3,2,4,3,4,1)
)
reh_dir <- remify::remify(ed_dir, model = "tie", riskset = "active")

# raw (no scaling)
st_raw_dir <- remstats::remstats(reh_dir, tie_effects = ~ totaldegreeDyad(), method = "pt")
# timeMax scaling
st_tm_dir  <- remstats::remstats(reh_dir, tie_effects = ~ totaldegreeDyad(scaling = "timeMax"), method = "pt")

# locate the slice matrices directly (safer than dyad-only checks first)
effs_raw <- dimnames(st_raw_dir)[[3]]
effs_tm  <- dimnames(st_tm_dir)[[3]]
td_raw_name <- effs_raw[grepl("^totaldegreeDyad", effs_raw)][1]
td_tm_name  <- effs_tm[grepl("^totaldegreeDyad", effs_tm)][1]

raw_mat_dir <- st_raw_dir[, , td_raw_name]
tm_mat_dir  <- st_tm_dir[,  , td_tm_name]

exp_tm_dir <- expected_timeMax(raw_mat_dir)

expect_equal(tm_mat_dir, exp_tm_dir, tol = 1e-12)
expect_true(all(is.finite(tm_mat_dir) | is.na(tm_mat_dir)))
expect_true(all(tm_mat_dir >= 0 - 1e-12 & tm_mat_dir <= 1 + 1e-12))

# spot-check a couple dyads are scaled consistently
s12_raw <- extract_series(st_raw_dir, 1, 2, directed = TRUE)
s12_tm  <- extract_series(st_tm_dir,  1, 2, directed = TRUE)
# ratio equals 1/max(raw row) when raw>0; otherwise 0
for (t in seq_along(s12_raw)) {
  m <- max(raw_mat_dir[t, ])
  if (is.finite(m) && m > 0) expect_equal(s12_tm[t], s12_raw[t] / m, tol = 1e-12)
  else expect_equal(s12_tm[t], 0, tol = 1e-12)
}

# -------------------
# Undirected case
# -------------------
ed_undir <- data.frame(
  time = 1:10,
  actor1 = c(1,1,2,3,2,1,2,3,4,4),
  actor2 = c(2,3,3,2,1,2,4,4,1,2)
)
reh_undir <- remify::remify(ed_undir, model = "tie", riskset = "active")

st_raw_undir <- remstats::remstats(reh_undir, tie_effects = ~ totaldegreeDyad(), method = "pt")
st_tm_undir  <- remstats::remstats(reh_undir, tie_effects = ~ totaldegreeDyad(scaling = "timeMax"), method = "pt")

effs_raw <- dimnames(st_raw_undir)[[3]]
effs_tm  <- dimnames(st_tm_undir)[[3]]
td_raw_name <- effs_raw[grepl("^totaldegreeDyad", effs_raw)][1]
td_tm_name  <- effs_tm[grepl("^totaldegreeDyad", effs_tm)][1]

raw_mat_undir <- st_raw_undir[, , td_raw_name]
tm_mat_undir  <- st_tm_undir[,  , td_tm_name]

exp_tm_undir <- expected_timeMax(raw_mat_undir)

expect_equal(tm_mat_undir, exp_tm_undir, tol = 1e-12)
expect_true(all(tm_mat_undir >= 0 - 1e-12 & tm_mat_undir <= 1 + 1e-12))

# dyad symmetry check (undirected: (1,2) exists only as sorted (1,2))
s12_raw <- extract_series(st_raw_undir, 1, 2, directed = FALSE)
s12_tm  <- extract_series(st_tm_undir,  1, 2, directed = FALSE)
for (t in seq_along(s12_raw)) {
  m <- max(raw_mat_undir[t, ])
  if (is.finite(m) && m > 0) expect_equal(s12_tm[t], s12_raw[t] / m, tol = 1e-12)
  else expect_equal(s12_tm[t], 0, tol = 1e-12)
}
