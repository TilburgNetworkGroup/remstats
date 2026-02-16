# inst/tinytest/test-psABYAB.R
library(tinytest)
library(remify)
library(remstats)

# Helper: extract the pshift series from a remstats object
extract_pshift <- function(stats) {
  effs <- dimnames(stats)[[3]]
  ps_name <- effs[grepl("^ps", effs)][1]
  if (is.na(ps_name)) stop("No pshift effect found in stats")
  as.numeric(stats[, 1, ps_name])
}

# Small event list that triggers ABAY and ABXB patterns
ed <- data.frame(
  time   = 1:8,
  actor1 = c(1,2,1,2,1,3,2,1),
  actor2 = c(2,1,3,3,2,2,3,3)
)

reh <- remify::remify(ed, model = "tie", riskset = "full")

# -------------------------
# consider_type = TRUE
# -------------------------
st_sum_T <- remstats::remstats(
  reh,
  tie_effects = ~ psABYAB(consider_type = TRUE),
  method = "pt"
)

st_ay_T <- remstats::remstats(
  reh,
  tie_effects = ~ psABAY(consider_type = TRUE),
  method = "pt"
)

st_xb_T <- remstats::remstats(
  reh,
  tie_effects = ~ psABXB(consider_type = TRUE),
  method = "pt"
)

yab_T <- extract_pshift(st_sum_T)
ay_T  <- extract_pshift(st_ay_T)
xb_T  <- extract_pshift(st_xb_T)

expect_equal(yab_T, ay_T + xb_T)

# -------------------------
# consider_type = FALSE
# -------------------------
st_sum_F <- remstats::remstats(
  reh,
  tie_effects = ~ psABYAB(consider_type = FALSE),
  method = "pt"
)

st_ay_F <- remstats::remstats(
  reh,
  tie_effects = ~ psABAY(consider_type = FALSE),
  method = "pt"
)

st_xb_F <- remstats::remstats(
  reh,
  tie_effects = ~ psABXB(consider_type = FALSE),
  method = "pt"
)

yab_F <- extract_pshift(st_sum_F)
ay_F  <- extract_pshift(st_ay_F)
xb_F  <- extract_pshift(st_xb_F)

expect_equal(yab_F, ay_F + xb_F)
