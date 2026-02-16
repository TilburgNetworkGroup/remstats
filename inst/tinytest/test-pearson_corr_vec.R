library(tinytest)

corr_cpp <- function(x, y) remstats:::pearson_corr_vec_cpp(x, y)

# 1. Identieke profielen -> 1
expect_equal(corr_cpp(c(1,2,3), c(1,2,3)), 1)

# 2. n < 2 -> 0
expect_equal(corr_cpp(1, 1), 0)

# 3. Lengtes ongelijk -> error
expect_error(corr_cpp(c(1,2), c(1)))

# 4. Zero variance + niet identiek -> NA (requested behavior)
expect_true(is.na(corr_cpp(c(1,1,1), c(1,2,3))))

# 5. Zero variance + identiek -> 1
expect_equal(corr_cpp(c(5,5,5), c(5,5,5)), 1)





ed <- data.frame(
    time   = 1:17,
    actor1 = c(1,1,2,2,1,3,4,2,4, 1, 4, 1, 4, 1, 1, 1, 3),
    actor2 = c(2,3,3,4,4,4,1,1,1, 2, 1, 2, 1, 3, 3, 5, 6)
)

# --- Reference implementation that mirrors the C++ logic in calculate_tempEquiv() ---
# - history = events 1..(t-1)
# - directed profile: outgoing then incoming, excluding i and j
# - undirected profile: one vector, excluding i and j
# - l1: normalize by maxI_event (global max inertia at time t)
# - pearson: identical -> 1; zero variance + non-identical -> NA

pearson_or_na <- function(x, y) {
    stopifnot(length(x) == length(y))
    if (length(x) < 2) return(0)
    if (all(x == y)) return(1)
    sx <- stats::var(x)
    sy <- stats::var(y)
    if (isTRUE(all.equal(sx, 0)) || isTRUE(all.equal(sy, 0))) return(NA_real_)
    stats::cor(x, y)
  }

temp_profiles <- function(ed, t, i, j, directed = TRUE) {
    N <- max(ed$actor1, ed$actor2)
    hist <- ed[seq_len(t - 1), , drop = FALSE]
    if (directed) {
        M <- matrix(0L, nrow = N, ncol = N)
        for (r in seq_len(nrow(hist))) {
            a <- hist$actor1[r]
            b <- hist$actor2[r]
            M[a, b] <- M[a, b] + 1L
          }
        offdiag <- M
        diag(offdiag) <- NA
        maxI_event <- max(offdiag, na.rm = TRUE)
        ks <- setdiff(seq_len(N), c(i, j))
        x_raw <- c(M[i, ks], M[ks, i])
        y_raw <- c(M[j, ks], M[ks, j])
        list(N = N, ks = ks, maxI_event = maxI_event, x_raw = as.numeric(x_raw), y_raw = as.numeric(y_raw))
      } else {
          M <- matrix(0L, nrow = N, ncol = N)
          for (r in seq_len(nrow(hist))) {
              a <- hist$actor1[r]
              b <- hist$actor2[r]
              aa <- min(a, b)
              bb <- max(a, b)
              M[aa, bb] <- M[aa, bb] + 1L
            }
          # max over upper triangle (excluding diag)
            maxI_event <- 0L
            for (a in 1:(N - 1)) for (b in (a + 1):N) maxI_event <- max(maxI_event, M[a, b])
            ks <- setdiff(seq_len(N), c(i, j))
            x_raw <- vapply(ks, function(k) M[min(i, k), max(i, k)], numeric(1))
            y_raw <- vapply(ks, function(k) M[min(j, k), max(j, k)], numeric(1))
            list(N = N, ks = ks, maxI_event = maxI_event, x_raw = x_raw, y_raw = y_raw)
          }
  }

temp_l1 <- function(prof) {
    maxI <- prof$maxI_event
    if (maxI <= 0) return(list(val = 1, x = prof$x_raw * 0, y = prof$y_raw * 0, l1 = 0))
    x <- prof$x_raw / maxI
    y <- prof$y_raw / maxI
    l1 <- mean(abs(x - y))
    list(val = 1 - l1, x = x, y = y, l1 = l1)
  }

# --- Hard-coded expected intermediate steps (from hand calculation / reference) ---
expect_steps <- list(
    directed = list(
        `1-2` = list(
            `5`  = list(maxI = 1, x_raw = c(1,0,0,0, 0,0,0,0), y_raw = c(1,1,0,0, 0,0,0,0), l1 = 0.125,   val_l1 = 0.875,   val_p = 0.6546536707079772),
            `16` = list(maxI = 4, x_raw = c(3,1,0,0, 0,4,0,0), y_raw = c(1,1,0,0, 0,0,0,0), l1 = 0.1875,  val_l1 = 0.8125,  val_p = 0.3849001794597505),
            `17` = list(maxI = 4, x_raw = c(3,1,1,0, 0,4,0,0), y_raw = c(1,1,0,0, 0,0,0,0), l1 = 0.21875, val_l1 = 0.78125, val_p = 0.3478327964999673)
          ),
        `2-4` = list(
            `5`  = list(maxI = 1, x_raw = c(0,1,0,0, 1,0,0,0), y_raw = c(0,0,0,0, 0,0,0,0), l1 = 0.25,    val_l1 = 0.75,    val_p = NA_real_),
            `16` = list(maxI = 4, x_raw = c(1,1,0,0, 3,0,0,0), y_raw = c(4,0,0,0, 1,1,0,0), l1 = 0.21875, val_l1 = 0.78125, val_p = 0.31520361922976675),
            `17` = list(maxI = 4, x_raw = c(1,1,0,0, 3,0,0,0), y_raw = c(4,0,0,0, 1,1,0,0), l1 = 0.21875, val_l1 = 0.78125, val_p = 0.31520361922976675)
          )
      ),
    undirected = list(
        `1-2` = list(
            `5`  = list(maxI = 1, x_raw = c(1,0,0,0), y_raw = c(1,1,0,0), l1 = 0.25, val_l1 = 0.75, val_p = 0.5773502691896258),
            `16` = list(maxI = 5, x_raw = c(3,5,0,0), y_raw = c(1,1,0,0), l1 = 0.3,  val_l1 = 0.7,  val_p = 0.9428090415820635),
            `17` = list(maxI = 5, x_raw = c(3,5,1,0), y_raw = c(1,1,0,0), l1 = 0.35, val_l1 = 0.65, val_p = 0.9113223768657671)
          ),
        `2-4` = list(
            `5`  = list(maxI = 1, x_raw = c(1,1,0,0), y_raw = c(0,0,0,0), l1 = 0.5,  val_l1 = 0.5,  val_p = NA_real_),
            `16` = list(maxI = 5, x_raw = c(4,1,0,0), y_raw = c(5,1,0,0), l1 = 0.05, val_l1 = 0.95, val_p = 0.9986310739646673),
            `17` = list(maxI = 5, x_raw = c(4,1,0,0), y_raw = c(5,1,0,0), l1 = 0.05, val_l1 = 0.95, val_p = 0.9986310739646673)
          )
      )
  )

# --- Check intermediate steps (pure hand/reference) ---
for (directed in c(TRUE, FALSE)) {
    key <- if (directed) "directed" else "undirected"
    for (pair in c("1-2", "2-4")) {
        ij <- as.integer(strsplit(pair, "-", fixed = TRUE)[[1]])
        i <- ij[1]; j <- ij[2]
        for (t in c(5, 16, 17)) {
            prof <- temp_profiles(ed, t, i, j, directed = directed)
            l1   <- temp_l1(prof)
            pval <- pearson_or_na(prof$x_raw, prof$y_raw)
            exp  <- expect_steps[[key]][[pair]][[as.character(t)]]
      
              expect_equal(prof$maxI_event, exp$maxI)
            expect_equal(prof$x_raw, exp$x_raw)
            expect_equal(prof$y_raw, exp$y_raw)
            expect_equal(l1$l1, exp$l1, tol = 1e-12)
            expect_equal(l1$val, exp$val_l1, tol = 1e-12)
            if (is.na(exp$val_p)) {
                expect_true(is.na(pval))
              } else {
                  expect_equal(pval, exp$val_p, tol = 1e-12)
                }
          }
      }
  }

# --- Optional integration check against remstats output (end values only) ---
# (kept minimal: if this ever fails, it tells you the C++ effect deviated from the reference steps above)
reh_dir  <- remify::remify(ed, model = "tie", riskset = "full")
st_l1_d  <- remstats::remstats(reh_dir, tie_effects = ~tempEquiv(approach = "l1"),     method = "pt")
st_p_d   <- remstats::remstats(reh_dir, tie_effects = ~tempEquiv(approach = "pearson"), method = "pt")

extract_val <- function(st, t, i, j) {
    rs <- attr(st, "riskset")
    s_col <- if ("sender" %in% names(rs)) "sender" else names(rs)[1]
    r_col <- if ("receiver" %in% names(rs)) "receiver" else names(rs)[2]
    rs_s <- as.integer(as.character(rs[[s_col]]))
    rs_r <- as.integer(as.character(rs[[r_col]]))
    idx <- which(rs_s == i & rs_r == j)
    stopifnot(length(idx) == 1)
    as.numeric(st[t, idx, 2])
}

for (pair in c("1-2", "2-4")) {
    ij <- as.integer(strsplit(pair, "-", fixed = TRUE)[[1]])
    i <- ij[1]; j <- ij[2]
    for (t in c(5, 16, 17)) {
        exp <- expect_steps$directed[[pair]][[as.character(t)]]
        got_l1 <- extract_val(st_l1_d, t, i, j)
        got_p  <- extract_val(st_p_d,  t, i, j)
        expect_equal(got_l1, exp$val_l1, tol = 1e-12)
        if (is.na(exp$val_p)) {
            expect_true(is.na(got_p))
          } else {
              expect_equal(got_p, exp$val_p, tol = 1e-12)
          }
    }
}
