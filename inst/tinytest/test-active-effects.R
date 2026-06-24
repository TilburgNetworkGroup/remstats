## Smoke tests for active-state effects in duration models.
##
## Goal: every exported active effect, and each argument variation
## (scaling, consider_type), runs through remstats() and returns a valid
## remstats_durem object. One hardcoded assertion per variant (no loops),
## so a broken or missing effect surfaces as a named failure -- this is the
## tripwire for the ".active_effect_cfg missing" class of regression.

library(tinytest)

# ── shared data ──────────────────────────────────────────────────────────────
# 4 directed events, 3 actors, all complete (end at 10).
el <- data.frame(
    time   = c(1, 2, 3, 6),
    actor1 = c("A", "A", "B", "C"),
    actor2 = c("B", "C", "C", "A"),
    end    = c(10, 10, 10, 10)
)

# typed copy for consider_type = "separate" / "interact"
el_typed       <- el
el_typed$type  <- c("x", "y", "x", "y")

suppressWarnings({
    reh       <- remify(el,       duration = TRUE, model = "tie")
    reh_typed <- remify(el_typed, duration = TRUE, model = "tie",
                        extend_riskset_by_type = TRUE)
})

# helpers: does a formula run and return a remstats_durem? (start = directed,
# end = undirected, matching the duration model's two processes)
ok_start <- function(form, r = reh)
    inherits(suppressWarnings(
        remstats(r, start_effects = form, first = 1L, last = Inf)),
        "remstats_durem")

ok_end <- function(form, r = reh)
    inherits(suppressWarnings(
        remstats(r, start_effects = ~ inertia(), end_effects = form,
                 first = 1L, last = Inf)),
        "remstats_durem")

# ── 1. Directed active effects (start model) ─────────────────────────────────

expect_true(ok_start(~ activeTie()),                   info = "activeTie")
expect_true(ok_start(~ activeOutdegreeSender()),       info = "activeOutdegreeSender")
expect_true(ok_start(~ activeIndegreeReceiver()),      info = "activeIndegreeReceiver")
expect_true(ok_start(~ activeTotaldegreeSender()),     info = "activeTotaldegreeSender")
expect_true(ok_start(~ activeTotaldegreeReceiver()),   info = "activeTotaldegreeReceiver")
expect_true(ok_start(~ activeSharedPartners_otp()),    info = "activeSharedPartners_otp")
expect_true(ok_start(~ activeSharedPartners_itp()),    info = "activeSharedPartners_itp")
expect_true(ok_start(~ activeSharedPartners_osp()),    info = "activeSharedPartners_osp")
expect_true(ok_start(~ activeSharedPartners_isp()),    info = "activeSharedPartners_isp")
expect_true(ok_start(~ activeReciprocalTie()),         info = "activeReciprocalTie (R-derived)")
expect_true(ok_start(~ activeTotaldegreeDyad()),       info = "activeTotaldegreeDyad (R-derived)")

# ── 2. Undirected active effects (end model) ─────────────────────────────────

expect_true(ok_end(~ activeDegreeMin()),               info = "activeDegreeMin (R-derived)")
expect_true(ok_end(~ activeDegreeMax()),               info = "activeDegreeMax (R-derived)")
expect_true(ok_end(~ activeDegreeDyad()),              info = "activeDegreeDyad (R-derived)")
expect_true(ok_end(~ activeSharedPartners()),          info = "activeSharedPartners (undirected)")

# ── 3. Multiple effects in one formula ───────────────────────────────────────

expect_true(ok_start(~ activeOutdegreeSender() + activeIndegreeReceiver()),
    info = "two effects in one start formula")

# ── 4. scaling variation ─────────────────────────────────────────────────────

expect_true(ok_start(~ activeOutdegreeSender(scaling = "none")),
    info = "scaling = none (default)")
expect_true(ok_start(~ activeOutdegreeSender(scaling = "std")),
    info = "scaling = std")

# ── 5. consider_type variation (typed events) ────────────────────────────────

expect_true(ok_start(~ activeOutdegreeSender(consider_type = "ignore"),  reh_typed),
    info = "consider_type = ignore")
expect_true(ok_start(~ activeOutdegreeSender(consider_type = "separate"), reh_typed),
    info = "consider_type = separate")
expect_true(ok_start(~ activeOutdegreeSender(consider_type = "interact"), reh_typed),
    info = "consider_type = interact (needs extend_riskset_by_type)")
expect_true(ok_start(~ activeOutdegreeSender(consider_type = TRUE),  reh_typed),
    info = "consider_type = TRUE (alias for separate)")
expect_true(ok_start(~ activeOutdegreeSender(consider_type = FALSE), reh_typed),
    info = "consider_type = FALSE (alias for ignore)")

# separate should yield more statistic columns than ignore (one per type)
s_ign <- suppressWarnings(remstats(reh_typed,
    start_effects = ~ activeOutdegreeSender(consider_type = "ignore"),
    first = 1L, last = Inf))
s_sep <- suppressWarnings(remstats(reh_typed,
    start_effects = ~ activeOutdegreeSender(consider_type = "separate"),
    first = 1L, last = Inf))
expect_true(
    length(s_sep$stacked$stat_names_start) > length(s_ign$stacked$stat_names_start),
    info = "separate produces more start statistics than ignore")

# ── 6. statistic names carry the .start suffix ───────────────────────────────

s <- suppressWarnings(remstats(reh,
    start_effects = ~ activeOutdegreeSender(), first = 1L, last = Inf))
expect_true(all(endsWith(s$stacked$stat_names_start, ".start")),
    info = "start effect names end in .start")

# ── 7. invalid consider_type errors (clear message, not a crash) ─────────────

expect_error(
    activeTie(consider_type = "nonsense"),
    pattern = "not supported",
    info = "invalid consider_type raises a clear error"
)


