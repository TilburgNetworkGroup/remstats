#' Stack remstats for model fitting
#'
#' Stack a \code{tomstats} object into long format suitable for fitting with
#' standard R model functions. Each row corresponds to one dyad in the risk
#' set at one event time point.
#'
#' For interval timing (\code{ordinal = FALSE}), the stacked data can be
#' fitted with a Poisson GLM using \code{log_interevent} as an offset:
#' \code{glm(obs ~ -1 + . - event - dyad - log_interevent,
#'           offset = log_interevent, family = poisson, data = stacked$remstats_stack)}
#'
#' For ordinal timing (\code{ordinal = TRUE}), use conditional logistic
#' regression stratified by event:
#' \code{survival::clogit(obs ~ -1 + . - event - dyad +
#'                        strata(event), data = stacked$remstats_stack)}
#'
#' @param stats A \code{tomstats} object (output of \code{remstats()} or
#'   \code{tomstats()}).
#' @param reh A \code{remify} object (output of \code{remify::remify2()}).
#'
#' @return A list with elements:
#' \describe{
#'   \item{remstats_stack}{Data frame in long format with columns: \code{event}
#'     (event index), \code{dyad} (dyad index 1..D), all statistic columns,
#'     \code{obs} (1 = observed event, 0 = non-event), and
#'     \code{log_interevent} (log inter-event time; only for interval timing).}
#'   \item{subset}{Integer vector of length 2: first and last event index.}
#'   \item{D}{Number of dyads in the risk set.}
#'   \item{E}{Number of events (time points).}
#'   \item{ordinal}{Logical: whether the ordinal likelihood applies.}
#' }
#'
#' @export
stack_stats <- function(stats, reh) {
  UseMethod("stack_stats")
}

#' @export
#' @method stack_stats tomstats
stack_stats.tomstats <- function(stats, reh) {

  if (!inherits(reh, "remify")) stop("'reh' must be a remify object.")

  # ── Dimensions ──────────────────────────────────────────────────────────────
  D  <- dim(stats)[2]   # number of dyads
  E  <- dim(stats)[1]   # number of time points in stats
  subset_idx <- as.integer(unlist(attr(stats, "subset")))  # [start, stop]

  # ── Ordinal flag ─────────────────────────────────────────────────────────────
  ordinal <- if (!is.null(reh$meta)) isTRUE(reh$meta$ordinal) else
             isTRUE(attr(reh, "ordinal"))

  # ── Riskset type ─────────────────────────────────────────────────────────────
  riskset <- if (!is.null(reh$meta))
    (reh$meta$riskset_source %||% reh$meta$riskset)
  else
    attr(reh, "riskset")

  # ── Stack statistics: [E*D x P] ──────────────────────────────────────────────
  stat_glm <- as.data.frame(
    do.call(rbind, lapply(seq_len(E), function(e) cbind(e, stats[e, , ])))
  )
  colnames(stat_glm)[1] <- "event"

  # ── Offset: log inter-event time (interval timing only) ──────────────────────
  if (!ordinal) {
    iet <- reh$intereventTime[subset_idx[1]:subset_idx[2]]
    stat_glm$log_interevent <- rep(log(iet), each = D)
  }

  # ── Response: observed events per dyad per time point ────────────────────────
  # dyad_active / dyad contain 1-based column indices into the stats array
  if (!is.null(reh$ids)) {
    # New remify2 format
    if (riskset %in% c("active", "manual")) {
      # dyad_active is [N x 1] matrix — flatten to vector, then subset
      dyad_vec <- as.vector(reh$ids$dyad_active)
    } else {
      dyad_vec <- as.vector(reh$ids$dyad)
    }
  } else {
    # Old remify format
    if (riskset %in% c("active", "manual")) {
      dyad_vec <- as.vector(attr(reh, "dyadIDactive") %||% attr(reh, "dyadID"))
    } else {
      dyad_vec <- as.vector(attr(reh, "dyadID"))
    }
  }

  stat_glm$obs <- unlist(lapply(seq_len(E), function(e) {
    ev_idx <- subset_idx[1] + e - 1L
    obs_dyad <- dyad_vec[ev_idx]
    tabulate(obs_dyad, nbins = D)
  }))

  # ── Dyad index ───────────────────────────────────────────────────────────────
  stat_glm$dyad <- rep(seq_len(D), E)

  list(
    remstats_stack = stat_glm,
    subset  = subset_idx,
    D       = D,
    E       = E,
    ordinal = ordinal
  )
}

# NULL-coalescing operator (defined locally if not already available)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

#' @export
#' @method stack_stats tomstats_sampled
stack_stats.tomstats_sampled <- function(stats, reh) {

  if (!inherits(reh, "remify")) stop("'reh' must be a remify object.")

  # ── Dimensions ──────────────────────────────────────────────────────────────
  E  <- dim(stats)[1]   # number of events
  S  <- dim(stats)[2]   # sampled set size per event
  subset_idx <- as.integer(unlist(attr(stats, "subset")))

  # ── Sampling attributes ──────────────────────────────────────────────────────
  sample_map <- attr(stats, "sample_map")  # [E x S] 1-based dyad indices
  samp_prob  <- attr(stats, "samp_prob")   # [E x S] inclusion probabilities
  case_pos   <- attr(stats, "case_pos")    # list of length E, 1-based

  if (is.null(sample_map) || is.null(samp_prob) || is.null(case_pos)) {
    stop("'stats' is missing required sampling attributes.")
  }

  # ── Ordinal flag ─────────────────────────────────────────────────────────────
  ordinal <- if (!is.null(reh$meta)) isTRUE(reh$meta$ordinal) else
             isTRUE(attr(reh, "ordinal"))

  # ── Stack statistics: [E*S x P] ──────────────────────────────────────────────
  stat_glm <- as.data.frame(
    do.call(rbind, lapply(seq_len(E), function(e) cbind(e, stats[e, , ])))
  )
  colnames(stat_glm)[1] <- "event"

  # ── Offset: log inter-event time (interval only) ─────────────────────────────
  if (!ordinal) {
    iet <- reh$intereventTime[subset_idx[1]:subset_idx[2]]
    stat_glm$log_interevent <- rep(log(iet), each = S)
  }

  # ── Response: 1 for case, 0 for control ──────────────────────────────────────
  stat_glm$obs <- unlist(lapply(seq_len(E), function(e) {
    is_case <- integer(S)
    cp <- case_pos[[e]]  # 1-based case positions
    if (length(cp) > 0) is_case[cp] <- 1L
    is_case
  }))

  # ── Importance weights: 1 for cases, 1/pi_s for controls ─────────────────────
  stat_glm$weight <- unlist(lapply(seq_len(E), function(e) {
    w <- 1.0 / samp_prob[e, ]       # start with 1/pi_s for all
    cp <- case_pos[[e]]              # 1-based case positions
    if (length(cp) > 0) w[cp] <- 1.0  # cases get weight 1
    w
  }))

  # ── Sampled dyad index (1-based within active riskset) ───────────────────────
  stat_glm$dyad <- as.vector(t(sample_map))  # [E*S]: dyad ID for each row

  list(
    remstats_stack = stat_glm,
    subset  = subset_idx,
    S       = S,
    E       = E,
    ordinal = ordinal
  )
}
