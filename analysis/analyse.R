#!/usr/bin/env Rscript
# =============================================================================
#  Price-ending effects in language models: full analysis
#
#  Reads  data/panels/*.csv   (written by run_arm() in the collection notebook)
#  Writes output/figures/*.pdf
#         output/tables/*.tex      LaTeX table fragments
#         output/tables/numbers.tex  \newcommand macros for the paper
#         output/estimates.csv     every estimate in one tidy file
#
#  Usage:  Rscript analysis/analyse.R  [--boot N]
# =============================================================================

suppressPackageStartupMessages({library(data.table); library(ggplot2)})

args   <- commandArgs(trailingOnly = TRUE)
N_BOOT <- if (any(grepl("^--boot", args))) as.integer(sub("--boot=?", "", args[grep("^--boot", args)])) else 400
if (is.na(N_BOOT)) N_BOOT <- 400

ROOT <- normalizePath(file.path(dirname(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(ROOT) || !dir.exists(file.path(ROOT, "data"))) ROOT <- getwd()
PAN <- file.path(ROOT, "data", "panels")
FIG <- file.path(ROOT, "output", "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
TAB <- file.path(ROOT, "output", "tables");  dir.create(TAB, showWarnings = FALSE, recursive = TRUE)

MIN_RANGE <- 0.01   # a block must move by at least this much to carry information
set.seed(20260906)

# -----------------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------------
read_arm <- function(f) {
  d <- fread(f, showProgress = FALSE)
  # Position inside the dollar block, 0..100. Recovered from price - anchor because the
  # stored `ending` column is 0 for BOTH $b.00 (block start) and $(b+1).00 (block end),
  # so it cannot distinguish the cheapest price in a block from the dearest.
  d[, offset := as.integer(round((price - anchor) * 100))]
  # What happens to the dollar figure at the top of the block. This is a function of the
  # anchor, so it needs no new field in the schema and no change to any call identifier.
  #   same  : the dollar figure increments inside a ten   (12 -> 13)
  #   roll  : the leftmost digit changes                  (19 -> 20)
  #   widen : the dollar figure gains a digit             (99 -> 100)
  d[, .top := anchor + 1L]
  d[, boundary := fifelse(nchar(as.character(.top)) != nchar(as.character(anchor)), "widen",
                   fifelse(substr(as.character(.top), 1, 1) != substr(as.character(anchor), 1, 1),
                           "roll", "same"))]
  d[, .top := NULL]
  d[, digit_cond := fifelse(anchor %% 100L == 99L, "lead",
                    fifelse(anchor %% 10L == 9L, "inner",
                    fifelse(anchor %% 10L == 4L, "flat", NA_character_)))]
  d
}
files <- list.files(PAN, "\\.csv$", full.names = TRUE)
if (!length(files)) stop("no panels in ", PAN, "; run at least one arm first")
D <- rbindlist(lapply(files, read_arm), fill = TRUE)
cat(sprintf("loaded %d arms, %s cells\n", length(files), format(nrow(D), big.mark = ",")))
print(D[, .(cells = .N, models = uniqueN(model)), by = experiment][order(experiment)])

# -----------------------------------------------------------------------------
# 2. Core estimator
#
#    Within a block (model x product x persona x anchor x condition) remove the block
#    mean and the linear price trend. What remains is the effect of the price ENDING.
#    Then compare the ten one-cent steps .d9 -> .(d+1)0, of which only the tenth crosses
#    a dollar. Step 10 lands on .00, the roundest ending, so it differs from the nine
#    placebos even with no threshold effect: regress the placebos on destination
#    roundness and take step 10's residual from that fit.
# -----------------------------------------------------------------------------
roundness_of <- function(off) fifelse(off %in% c(0, 100), 3L,
                             fifelse(off == 50, 2L, fifelse(off %% 10 == 0, 1L, 0L)))

# A block is one price ladder: everything about the prompt held fixed except the price.
# Any field that varies inside an arm and is not the price defines a separate ladder, so
# it has to enter the key; otherwise two ladders are averaged together and the ending
# profile is a blend of conditions.
BLK_CANDS <- c("template", "currency", "rendering", "cents_cond", "domain", "unit",
               "probe", "preamble_id", "deliberation", "mode")
block_cols <- function(d, by = character()) {
  v <- BLK_CANDS[BLK_CANDS %in% names(d)]
  v <- v[vapply(v, function(c) uniqueN(d[[c]]) > 1L, logical(1))]
  union(c("model", "product", "persona", "anchor"), union(v, by))
}

prep <- function(d, by = character()) {
  d <- copy(d)
  d[, blk := do.call(paste, c(.SD, sep = "|")), .SDcols = block_cols(d, by)]
  d[, x := offset / 100]
  d[, rng := max(p_buy) - min(p_buy), by = blk]
  d <- d[rng >= MIN_RANGE]
  if (!nrow(d)) return(d)
  d[, yc := p_buy - mean(p_buy), by = blk]
  d[, xc := x - mean(x), by = blk]
  d[]
}

ending_profile <- function(d) {
  b <- sum(d$xc * d$yc) / max(sum(d$xc^2), 1e-12)
  e <- d[, .(eff = mean(yc - b * xc) * 100), by = offset][order(offset)]
  list(eff = setNames(e$eff, e$offset), slope = 100 * b, blocks = uniqueN(d$blk))
}

decade_steps <- function(eff) {
  out <- rbindlist(lapply(1:10, function(s) {
    lo <- as.character(s * 10 - 1); hi <- as.character((s * 10) %% 100)
    if (s == 10) hi <- "100"
    if (is.null(eff[lo]) || is.na(eff[lo]) || is.na(eff[hi])) return(NULL)
    data.table(step = s, jump = unname(eff[hi] - eff[lo]),
               r = if (s == 10) 3L else if (s == 5) 2L else 1L)
  }))
  out
}

left_digit <- function(eff) {
  st <- decade_steps(eff)
  if (nrow(st) < 10) return(c(NA_real_, NA_real_))
  pl <- st[step < 10]
  fit <- lm(jump ~ r, data = pl)
  j10 <- st[step == 10, jump]
  c(j10 - predict(fit, data.table(r = 3L)), j10 - mean(pl$jump))   # corrected, naive
}

# theta, the inattention parameter used in the human literature: the share of the
# per-dollar demand decline that arrives discontinuously at the dollar boundary rather
# than smoothly across it. It is a ratio, so it is only interpretable where the
# denominator is solidly away from zero; MIN_SLOPE gates that.
MIN_SLOPE <- 1.0

theta_of <- function(ld, slope) if (is.na(ld) || slope > -MIN_SLOPE) NA_real_ else ld / slope

estimate <- function(d, boot = N_BOOT) {
  if (!nrow(d)) return(NULL)
  ep <- ending_profile(d); ld <- left_digit(ep$eff)
  blocks <- unique(d$blk); idx <- split(seq_len(nrow(d)), d$blk)
  bs <- vapply(seq_len(boot), function(i) {
    pick <- sample(blocks, length(blocks), TRUE)
    e <- ending_profile(d[unlist(idx[pick], use.names = FALSE)])
    c(left_digit(e$eff)[1], e$slope)
  }, numeric(2))
  ok <- is.finite(bs[1, ]); b1 <- bs[1, ok]
  th <- vapply(which(ok), function(j) theta_of(bs[1, j], bs[2, j]), numeric(1))
  th <- th[is.finite(th)]
  data.table(blocks = ep$blocks, cells = nrow(d), slope = ep$slope,
             naive = ld[2], left_digit = ld[1],
             se = if (length(b1) > 2) sd(b1) else NA_real_,
             lo = if (length(b1) > 2) quantile(b1, .025) else NA_real_,
             hi = if (length(b1) > 2) quantile(b1, .975) else NA_real_,
             theta = theta_of(ld[1], ep$slope),
             # the interval follows the point estimate: if the denominator is too small
             # to divide by, a bootstrap distribution of ratios is not a interval for it
             theta_lo = if (length(th) > 2 && !is.na(theta_of(ld[1], ep$slope)))
                          quantile(th, .025) else NA_real_,
             theta_hi = if (length(th) > 2 && !is.na(theta_of(ld[1], ep$slope)))
                          quantile(th, .975) else NA_real_)
}

star <- function(lo, hi) fifelse(!is.na(lo) & (hi < 0 | lo > 0), "*", "")

# How often each price ending is posted, averaged over the corpora counted for E5. Used
# as a measured alternative to the ordinal roundness score, and to report how much
# roundness structure the adjustment is correcting for.
ENDING_SHARE <- NULL
if (file.exists(cf <- file.path(ROOT, "data", "ending_frequency_profile.csv"))) {
  .fr <- fread(cf)
  .cc <- setdiff(names(.fr), c("ending", "mean", "excess"))
  .fr[, share := rowMeans(.SD, na.rm = TRUE), .SDcols = .cc]
  ENDING_SHARE <- setNames(.fr$share, .fr$ending)
}

# -----------------------------------------------------------------------------
# 3. Run every arm
# -----------------------------------------------------------------------------
BY <- list(E0 = "boundary", E0b = "digit_cond",
           E1 = character(), E1b = "unit", E1f = character(), E2 = "template",
           E3 = "domain", E4 = "currency", E6 = "rendering", E7 = "cents_cond",
           E8 = "deliberation", E10 = "preamble_id")
BY <- lapply(BY, function(v) v[v %in% names(D)])
# Readable names. Tables are read by people who do not know the internal arm codes, so
# the name is the primary column and the code is secondary.
LABEL <- c(E0  = "Dollar boundary type",
           E0b = "Digit position",
           E1  = "Core sweep",
           E1b = "Scaled prices",
           E1f = "Newest models",
           E2  = "Prompt phrasing",
           E3  = "Non-price magnitudes",
           E4  = "Currency and locale",
           E6  = "Number formatting",
           E7  = "Rendered price tags",
           E8  = "Explicit reasoning",
           E9  = "Arithmetic probes",
           E10 = "Stated convention")

# A panel written with an out-of-date grouping key silently loses a condition column,
# which then shows up much later as an unrelated error. Check it here instead.
for (a in names(BY0 <- list(E0 = "boundary", E0b = "digit_cond",
                            E1b = "unit", E2 = "template", E3 = "domain",
                            E4 = "currency", E6 = "rendering", E7 = "cents_cond",
                            E10 = "preamble_id"))) {
  if (!a %in% D$experiment) next
  k <- BY0[[a]]
  if (!k %in% names(D) || uniqueN(D[experiment == a][[k]]) < 2L)
    stop(sprintf(paste("panel for %s has no usable '%s' column, so its conditions cannot be",
                       "separated. Rebuild the panels with:  python analysis/build_panels.py"),
                 a, k))
}

RES <- list(); COND <- list(); MOD <- list()
for (arm in intersect(names(BY), unique(D$experiment))) {
  d <- prep(D[experiment == arm], by = BY[[arm]])
  if (!nrow(d)) next
  r <- estimate(d); if (is.null(r)) next
  RES[[arm]] <- cbind(data.table(arm = arm, label = LABEL[[arm]]), r)

  # by condition, where the arm has one
  if (length(BY[[arm]])) {
    key <- BY[[arm]]
    cs <- rbindlist(lapply(sort(unique(d[[key]])), function(v) {
      s <- estimate(d[get(key) == v], boot = max(120, N_BOOT %/% 3))
      if (is.null(s)) NULL else cbind(data.table(arm = arm, condition = as.character(v)), s)
    }), fill = TRUE)
    if (nrow(cs)) COND[[arm]] <- cs
  }
  # by model
  ms <- rbindlist(lapply(sort(unique(d$model)), function(v) {
    s <- estimate(d[model == v], boot = max(120, N_BOOT %/% 3))
    if (is.null(s)) NULL else cbind(data.table(arm = arm, model = v), s)
  }), fill = TRUE)
  if (nrow(ms)) MOD[[arm]] <- ms
  cat(sprintf("  %-4s %-22s blocks %5d  left-digit %+7.2f [%+.2f, %+.2f]\n",
              arm, LABEL[[arm]], r$blocks, r$left_digit, r$lo, r$hi))
}
RES <- rbindlist(RES, fill = TRUE); COND <- rbindlist(COND, fill = TRUE)
MOD  <- rbindlist(MOD,  fill = TRUE)
RES[, sig := star(lo, hi)]; COND[, sig := star(lo, hi)]; MOD[, sig := star(lo, hi)]

# -----------------------------------------------------------------------------
# 4. E9, the arithmetic probes: a different outcome, handled separately
# -----------------------------------------------------------------------------
E9 <- NULL
if ("E9" %in% D$experiment) {
  e9 <- D[experiment == "E9"]
  cmp <- e9[probe == "compare", .(n = .N, p_correct = mean(p_buy),
                                  frac_conf = mean(p_buy > 0.95)), by = model]
  mag <- e9[probe == "magnitude", .(n = .N, median_cents = median(p_buy),
                                    frac_exactly_1 = mean(round(p_buy) == 1)), by = model]
  E9 <- merge(cmp, mag, by = "model", suffixes = c("_cmp", "_mag"))
  cat(sprintf("\nE9 arithmetic: mean P(correct) %.4f, frac answering exactly 1 cent %.4f\n",
              mean(E9$p_correct), mean(E9$frac_exactly_1)))
}

# -----------------------------------------------------------------------------
# 3b. Two checks that do not depend on the ordinal roundness score
#
#     The reported estimator regresses the nine placebo jumps on an ordinal roundness
#     score and extrapolates to a whole dollar. Only one placebo (step 5, landing on
#     .50) sits above the base level, so the slope rests on one contrast. Two
#     alternatives avoid that.
#
#     (i)  Replace the assumed score with a measured one: how often the destination
#          ending actually occurs in the corpora counted for E5. That gives nine
#          distinct values among the placebos instead of two levels.
#     (ii) Drop the functional form entirely. Any roundness account in which rounder
#          destinations are weakly more attractive implies the dollar step should jump
#          at least as much as every placebo, because .00 is the roundest destination
#          on the grid. Step 10 minus the largest placebo jump is therefore a bound on
#          the effect that assumes only monotonicity.
# -----------------------------------------------------------------------------
steps_full <- function(eff) {
  st <- decade_steps(eff)
  if (!nrow(st)) return(st)
  st[, dest := fifelse(step == 10L, 0L, as.integer((step * 10) %% 100))]
  if (!is.null(ENDING_SHARE))
    st[, lshare := log10(pmax(ENDING_SHARE[as.character(dest)], 1e-6))]
  st[]
}

ld_measured <- function(eff) {          # (i) measured typicality of the destination
  st <- steps_full(eff)
  if (nrow(st) < 10 || !"lshare" %in% names(st) || anyNA(st$lshare)) return(NA_real_)
  pl <- st[step < 10]
  st[step == 10, jump] - as.numeric(predict(lm(jump ~ lshare, pl), st[step == 10]))
}
ld_bound <- function(eff) {             # (ii) monotone bound, no functional form
  st <- decade_steps(eff)
  if (nrow(st) < 10) return(NA_real_)
  st[step == 10, jump] - max(st[step < 10]$jump)
}

ALT <- NULL
if ("E1" %in% D$experiment) {
  d1a <- prep(D[experiment == "E1"])
  if (nrow(d1a)) {
    ep <- ending_profile(d1a)
    blocks <- unique(d1a$blk); idx <- split(seq_len(nrow(d1a)), d1a$blk)
    bs <- replicate(N_BOOT, {
      e <- ending_profile(d1a[unlist(idx[sample(blocks, length(blocks), TRUE)],
                                     use.names = FALSE)])$eff
      c(left_digit(e)[1], ld_measured(e), ld_bound(e))
    })
    qs <- function(i) { v <- bs[i, ]; v <- v[is.finite(v)]
      if (length(v) > 2) quantile(v, c(.025, .975)) else c(NA_real_, NA_real_) }
    ALT <- data.table(
      spec = c("Ordinal roundness score (reported)",
               "Measured corpus frequency of the destination",
               "Largest placebo step (monotone bound)"),
      est  = c(left_digit(ep$eff)[1], ld_measured(ep$eff), ld_bound(ep$eff)),
      lo   = c(qs(1)[1], qs(2)[1], qs(3)[1]),
      hi   = c(qs(1)[2], qs(2)[2], qs(3)[2]))
    ALT[, sig := star(lo, hi)]
    cat("\nspecification of the roundness adjustment (E1):\n")
    print(ALT[, .(spec, est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2), sig)])
    fwrite(ALT, file.path(ROOT, "output", "roundness_specs.csv"))
  }
}

# -----------------------------------------------------------------------------
# 3c. E0: the matched estimator
#
#     The estimator above values a .00 destination by extrapolating a roundness fit.
#     E0 removes the extrapolation. Its blocks all end on .00, and differ only in what
#     happens to the dollar figure: it increments inside a ten (same), the leftmost
#     digit changes (roll), or a digit is added (widen). The estimate is the difference
#     between two step-10 jumps, so everything that makes .00 attractive is common to
#     both terms and cancels. No roundness model enters.
# -----------------------------------------------------------------------------
step10 <- function(d) {
  if (!nrow(d)) return(NA_real_)
  e <- ending_profile(d)$eff
  if (is.na(e["100"]) || is.na(e["99"])) return(NA_real_)
  unname(e["100"] - e["99"])
}

MATCH <- NULL
if ("E0" %in% D$experiment) {
  d0 <- prep(D[experiment == "E0"], by = "boundary")
  if (nrow(d0) && uniqueN(d0$boundary) >= 2L) {
    idx <- split(seq_len(nrow(d0)), list(d0$boundary, d0$blk), drop = TRUE)
    key <- data.table(nm = names(idx), cond = sub("\\..*$", "", names(idx)))
    draw <- function() d0[unlist(idx[key[, .(nm = sample(nm, .N, TRUE)), by = cond]$nm],
                                 use.names = FALSE)]
    bs <- replicate(N_BOOT, { dd <- draw()
      c(step10(dd[boundary == "roll"])  - step10(dd[boundary == "same"]),
        step10(dd[boundary == "widen"]) - step10(dd[boundary == "same"])) })
    qs <- function(i) { v <- bs[i, ]; v <- v[is.finite(v)]
      if (length(v) > 2) quantile(v, c(.025, .975)) else c(NA_real_, NA_real_) }
    lev <- c("same", "roll", "widen")
    MATCH <- data.table(
      boundary = lev,
      blocks   = vapply(lev, function(k) uniqueN(d0[boundary == k]$blk), integer(1)),
      jump     = vapply(lev, function(k) step10(d0[boundary == k]), numeric(1)))
    MATCH[, vs_same := jump - jump[boundary == "same"]]
    MATCH[, lo := c(NA_real_, qs(1)[1], qs(2)[1])]
    MATCH[, hi := c(NA_real_, qs(1)[2], qs(2)[2])]
    MATCH[, sig := star(lo, hi)]
    cat("\nE0, step-10 jump by what happens to the dollar figure:\n")
    print(MATCH[, .(boundary, blocks, jump = round(jump, 2),
                    vs_same = round(vs_same, 2), lo = round(lo, 2),
                    hi = round(hi, 2), sig)])
    fwrite(MATCH, file.path(ROOT, "output", "matched.csv"))

    # the same contrast within each model, as a check that it is not one model
    MATCHM <- rbindlist(lapply(sort(unique(d0$model)), function(mm) {
      dm <- d0[model == mm]
      data.table(model = mm, blocks = uniqueN(dm$blk),
                 same = step10(dm[boundary == "same"]),
                 roll = step10(dm[boundary == "roll"]))
    }))
    MATCHM[, diff := roll - same]
    MATCHM <- MATCHM[order(diff)]
    cat("\nE0 by model (roll minus same):\n")
    print(MATCHM[, .(model = sub("^[^/]*/", "", model), blocks,
                     same = round(same, 2), roll = round(roll, 2),
                     diff = round(diff, 2))])
    fwrite(MATCHM, file.path(ROOT, "output", "matched_by_model.csv"))
  }
}

# -----------------------------------------------------------------------------
# 3c2. E0b: digit position versus roundness
#
#      Three-digit prices separate what two-digit prices bundle. The three conditions
#      differ in which digit boundary the block crosses:
#        flat  : x14 -> x15, no boundary at all
#        inner : x19 -> x20, tens digit changes (rounder destination, no leading change)
#        lead  : x99 -> x00, hundreds (leading) digit changes (roundest destination)
#      flat -> inner measures the roundness gradient with no leading-digit change.
#      If lead falls below the extrapolation from flat and inner, the excess is a
#      leading-digit effect over and above roundness.
# -----------------------------------------------------------------------------
MATCH0B <- NULL
if ("E0b" %in% D$experiment) {
  d0b <- prep(D[experiment == "E0b"], by = "digit_cond")
  if (nrow(d0b) && uniqueN(d0b$digit_cond) == 3L) {
    lev <- c("flat", "inner", "lead")
    idx <- split(seq_len(nrow(d0b)), list(d0b$digit_cond, d0b$blk), drop = TRUE)
    key <- data.table(nm = names(idx), cond = sub("\\..*$", "", names(idx)))
    draw <- function() d0b[unlist(idx[key[, .(nm = sample(nm, .N, TRUE)), by = cond]$nm],
                                  use.names = FALSE)]
    # point estimates
    jumps <- vapply(lev, function(k) step10(d0b[digit_cond == k]), numeric(1))
    # the roundness gradient: inner - flat
    round_grad <- jumps["inner"] - jumps["flat"]
    # extrapolate to lead's roundness level: flat + 2 * (inner - flat) = 2*inner - flat
    extrap_lead <- 2 * jumps["inner"] - jumps["flat"]
    # excess below the extrapolation is the leading-digit effect
    lead_excess <- jumps["lead"] - extrap_lead

    bs <- replicate(N_BOOT, { dd <- draw()
      j <- vapply(lev, function(k) step10(dd[digit_cond == k]), numeric(1))
      c(j["inner"] - j["flat"],            # roundness gradient
        j["lead"]  - j["flat"],             # lead vs flat
        j["lead"]  - (2*j["inner"] - j["flat"]))  # lead excess beyond extrapolation
    })
    qs <- function(i) { v <- bs[i, ]; v <- v[is.finite(v)]
      if (length(v) > 2) quantile(v, c(.025, .975)) else c(NA_real_, NA_real_) }

    MATCH0B <- data.table(
      digit_cond = lev,
      blocks = vapply(lev, function(k) uniqueN(d0b[digit_cond == k]$blk), integer(1)),
      jump = jumps[lev],
      vs_flat = jumps[lev] - jumps["flat"])
    MATCH0B[, lo := c(NA_real_, qs(1)[1], qs(2)[1])]
    MATCH0B[, hi := c(NA_real_, qs(1)[2], qs(2)[2])]
    MATCH0B[, sig := star(lo, hi)]
    cat("\nE0b, step-10 jump by digit position:\n")
    print(MATCH0B[, .(digit_cond, blocks, jump = round(jump, 2),
                      vs_flat = round(vs_flat, 2), lo = round(lo, 2),
                      hi = round(hi, 2), sig)])
    cat(sprintf("  roundness gradient (inner - flat): %+.2f [%+.2f, %+.2f]\n",
                round_grad, qs(1)[1], qs(1)[2]))
    cat(sprintf("  lead excess beyond extrapolation:  %+.2f [%+.2f, %+.2f]\n",
                lead_excess, qs(3)[1], qs(3)[2]))
    fwrite(MATCH0B, file.path(ROOT, "output", "matched_e0b.csv"))

    # per-model breakdown
    MATCH0BM <- rbindlist(lapply(sort(unique(d0b$model)), function(mm) {
      dm <- d0b[model == mm]
      data.table(model = mm, blocks = uniqueN(dm$blk),
                 flat  = step10(dm[digit_cond == "flat"]),
                 inner = step10(dm[digit_cond == "inner"]),
                 lead  = step10(dm[digit_cond == "lead"]))
    }))
    MATCH0BM[, round_grad := inner - flat]
    MATCH0BM[, lead_excess := lead - (2 * inner - flat)]
    MATCH0BM <- MATCH0BM[order(lead_excess)]
    cat("\nE0b by model:\n")
    print(MATCH0BM[, .(model = sub("^[^/]*/", "", model), blocks,
                       flat = round(flat, 2), inner = round(inner, 2),
                       lead = round(lead, 2), round_grad = round(round_grad, 2),
                       lead_excess = round(lead_excess, 2))])
    fwrite(MATCH0BM, file.path(ROOT, "output", "matched_e0b_by_model.csv"))
  }
}

# -----------------------------------------------------------------------------
# 3d. How much roundness there is to correct for
#
#     The adjustment only matters if endings really do differ by roundness. This
#     measures that directly: the average ending effect at each roundness level, and
#     how often endings at that level are posted in the corpora.
# -----------------------------------------------------------------------------
ROUNDLEV <- NULL
{
  d1r <- prep(D[experiment == "E1"])
  if (nrow(d1r)) {
    ep <- ending_profile(d1r)
    pr <- data.table(ending = as.integer(names(ep$eff)), eff = as.numeric(ep$eff))
    pr <- pr[ending != 100]                      # .00 appears once, at the block start
    pr[, rho := roundness_of(ending)]
    LEVNAME <- c("0" = "not a multiple of ten", "1" = "a plain multiple of ten",
                 "2" = "$.50$", "3" = "a whole dollar")
    ROUNDLEV <- pr[, .(endings = .N, effect = mean(eff)), by = rho][order(rho)]
    if (!is.null(ENDING_SHARE))
      ROUNDLEV <- merge(ROUNDLEV,
        pr[, .(share = mean(ENDING_SHARE[as.character(ending)], na.rm = TRUE)), by = rho],
        by = "rho")
    ROUNDLEV[, level := LEVNAME[as.character(rho)]]
    cat("\nending effect and posting frequency by roundness level (E1):\n")
    print(ROUNDLEV[, .(rho, level, endings, effect = round(effect, 2),
                       share = round(share, 2))])
    fwrite(ROUNDLEV, file.path(ROOT, "output", "roundness_levels.csv"))
  }
}

# -----------------------------------------------------------------------------
# 3e. Leave one model out
#
#     Whether the pooled estimate depends on any single model.
# -----------------------------------------------------------------------------
LOO <- NULL
{
  d1l <- prep(D[experiment == "E1"])
  if (nrow(d1l)) {
    LOO <- rbindlist(lapply(sort(unique(d1l$model)), function(mm) {
      dd <- d1l[model != mm]
      data.table(dropped = mm, blocks = uniqueN(dd$blk),
                 est = left_digit(ending_profile(dd)$eff)[1])
    }))[order(est)]
    cat(sprintf("\nleave one model out: estimate ranges %+.2f to %+.2f (full sample %+.2f)\n",
                min(LOO$est), max(LOO$est), left_digit(ending_profile(d1l)$eff)[1]))
    fwrite(LOO, file.path(ROOT, "output", "loo.csv"))
  }
}

# -----------------------------------------------------------------------------
# 4a. Robustness: does the estimate survive the choices the estimator makes?
#
#     Three of those choices could plausibly carry the result on their own.
#     (i)  The trend removed inside a block is linear. Step 10 is the only step whose
#          destination sits at the very edge of the price range, where curvature in the
#          true demand curve would show up as a residual; the placebos are interior.
#     (ii) The roundness fit is a line through effectively two points: step 5 is the
#          only placebo with r = 2. Dropping it leaves no slope to estimate at all, so
#          the honest alternative is to compare step 10 against the eight r = 1
#          placebos with no extrapolation, which OVER-states the effect and bounds it.
#     (iii) Blocks that barely move are dropped at MIN_RANGE. Both directions are shown.
# -----------------------------------------------------------------------------
detrended <- function(d, deg) {
  d <- copy(d)
  if (deg == 1L) {
    b <- sum(d$xc * d$yc) / max(sum(d$xc^2), 1e-12)
    e <- d[, .(eff = mean(yc - b * xc) * 100), by = offset][order(offset)]
  } else {
    d[, res := residuals(lm(p_buy ~ poly(x, deg, raw = TRUE))), by = blk]
    e <- d[, .(eff = mean(res) * 100), by = offset][order(offset)]
  }
  setNames(e$eff, e$offset)
}
ld_r1 <- function(eff) {
  st <- decade_steps(eff)
  if (nrow(st) < 10) return(NA_real_)
  st[step == 10, jump] - mean(st[step < 10 & r == 1L]$jump)
}

ROB <- rbindlist(lapply(intersect(names(BY), unique(D$experiment)), function(a) {
  raw <- D[experiment == a]
  d   <- prep(raw, by = BY[[a]])
  if (!nrow(d)) return(NULL)
  wide <- {
    old <- MIN_RANGE; MIN_RANGE <<- 0.00; z <- prep(raw, by = BY[[a]]); MIN_RANGE <<- old; z
  }
  tight <- {
    old <- MIN_RANGE; MIN_RANGE <<- 0.05; z <- prep(raw, by = BY[[a]]); MIN_RANGE <<- old; z
  }
  data.table(arm = a, blocks = uniqueN(d$blk),
             linear    = left_digit(detrended(d, 1L))[1],
             quadratic = left_digit(detrended(d, 2L))[1],
             cubic     = left_digit(detrended(d, 3L))[1],
             r1only    = ld_r1(detrended(d, 1L)),
             allblocks = if (nrow(wide))  left_digit(ending_profile(wide)$eff)[1]  else NA_real_,
             strict    = if (nrow(tight)) left_digit(ending_profile(tight)$eff)[1] else NA_real_)
}), fill = TRUE)
if (nrow(ROB)) {
  cat("\nrobustness (left-digit estimate under each variant):\n")
  print(ROB[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
  fwrite(ROB, file.path(ROOT, "output", "robustness.csv"))
}

# -----------------------------------------------------------------------------
# 4c. E7: is the cents ladder monotone?
#
#     Four conditions post the same prices and differ only in how tall the cents are
#     relative to the dollars. Testing each one against zero separately throws away
#     the design: the prediction is a gradient, not four independent effects. So fit
#     a line through the four estimates against cents height and bootstrap its slope,
#     resampling blocks within condition so the four estimates move together the way
#     they would in a repeat of the experiment.
# -----------------------------------------------------------------------------
SIZE <- c(same = 1.00, r70 = 0.70, r50 = 0.50, r35 = 0.35)
TREND <- NULL
if ("E7" %in% D$experiment) {
  d7 <- prep(D[experiment == "E7"], by = "cents_cond")
  d7 <- d7[cents_cond %in% names(SIZE)]
  if (uniqueN(d7$cents_cond) == length(SIZE)) {
    ld_by_size <- function(dd) {
      v <- vapply(names(SIZE), function(k) {
        e <- dd[cents_cond == k]
        if (!nrow(e)) return(NA_real_)
        left_digit(ending_profile(e)$eff)[1]
      }, numeric(1))
      if (anyNA(v)) return(NA_real_)
      unname(coef(lm(v ~ SIZE))[2])
    }
    idx <- split(seq_len(nrow(d7)), list(d7$cents_cond, d7$blk), drop = TRUE)
    key <- data.table(nm = names(idx), cond = sub("\\..*$", "", names(idx)))
    obs <- ld_by_size(d7)
    bs <- replicate(max(200, N_BOOT %/% 2), {
      pick <- key[, .(nm = sample(nm, .N, TRUE)), by = cond]$nm
      ld_by_size(d7[unlist(idx[pick], use.names = FALSE)])
    })
    bs <- bs[is.finite(bs)]
    TREND <- data.table(model = "pooled", blocks = uniqueN(d7$blk), slope = obs,
                        lo = if (length(bs) > 2) quantile(bs, .025) else NA_real_,
                        hi = if (length(bs) > 2) quantile(bs, .975) else NA_real_,
                        p_pos = mean(bs > 0))
    TREND[, sig := star(lo, hi)]

    # Models below MIN_TREND_BLOCKS are left out of the per-model table and figure
    # rather than shown with a useless interval.
    MIN_TREND_BLOCKS <- 100L
    per <- rbindlist(lapply(sort(unique(d7$model)), function(mm) {
      dm <- d7[model == mm]
      if (uniqueN(dm$cents_cond) < length(SIZE)) return(NULL)
      if (uniqueN(dm$blk) < MIN_TREND_BLOCKS) return(NULL)
      im <- split(seq_len(nrow(dm)), list(dm$cents_cond, dm$blk), drop = TRUE)
      km <- data.table(nm = names(im), cond = sub("\\..*$", "", names(im)))
      o  <- ld_by_size(dm)
      bm <- replicate(max(200, N_BOOT %/% 2), {
        pick <- km[, .(nm = sample(nm, .N, TRUE)), by = cond]$nm
        ld_by_size(dm[unlist(im[pick], use.names = FALSE)])
      })
      bm <- bm[is.finite(bm)]
      data.table(model = mm, blocks = uniqueN(dm$blk), slope = o,
                 lo = if (length(bm) > 2) quantile(bm, .025) else NA_real_,
                 hi = if (length(bm) > 2) quantile(bm, .975) else NA_real_,
                 p_pos = mean(bm > 0))
    }), fill = TRUE)
    if (nrow(per)) {
      per[, sig := star(lo, hi)]
      TREND <- rbind(per, TREND[, names(per), with = FALSE])
    }
    cat(sprintf("\nE7 cents-size trend: %+.2f pp per unit of cents height [%+.2f, %+.2f]%s\n",
                TREND$slope, TREND$lo, TREND$hi, TREND$sig))
    cat(sprintf("  positive slope = effect gets more negative as cents shrink; ")); 
    cat(sprintf("bootstrap share above zero %.3f\n", TREND$p_pos))
    cat("\nper model:\n")
    print(TREND[, .(model, blocks, slope = round(slope, 2), lo = round(lo, 2),
                    hi = round(hi, 2), p_pos = round(p_pos, 3), sig)])
    fwrite(TREND, file.path(ROOT, "output", "vision_trend.csv"))
  }
}

# -----------------------------------------------------------------------------
# 4b. E10, the installed convention: does the preference move to the ending the
#     prompt names?
#
#     The decade-step readout only sees the .99 -> +1.00 boundary, so it answers
#     "did the dollar threshold move". The sharper question is whether a sentence of
#     context builds a NEW preferred ending where none existed. E10 posts .33 and .77
#     on the grid for exactly this: a perceptual distortion cannot be talked into
#     liking .77, a learned convention can.
#
#     Readout: the ending's effect minus the mean of the other endings in its own
#     decade (.70, .75, .79 for .77), so the local price trend cannot produce it.
INSTALLED <- c(33L, 77L)

bump_at <- function(eff, e) {
  dec <- (e %/% 10L) * 10L
  nb  <- setdiff(c(dec, dec + 5L, dec + 9L), e)
  nb  <- nb[as.character(nb) %in% names(eff)]
  if (!(as.character(e) %in% names(eff)) || length(nb) < 2L) return(NA_real_)
  unname(eff[as.character(e)] - mean(eff[as.character(nb)]))
}

BUMP <- NULL
if ("E10" %in% D$experiment && "preamble_id" %in% names(D)) {
  d10 <- prep(D[experiment == "E10"], by = "preamble_id")
  BUMP <- rbindlist(lapply(sort(unique(d10$preamble_id)), function(pid) {
    dd <- d10[preamble_id == pid]
    ep <- ending_profile(dd)
    blocks <- unique(dd$blk); idx <- split(seq_len(nrow(dd)), dd$blk)
    bs <- replicate(max(120, N_BOOT %/% 3), {
      pick <- sample(blocks, length(blocks), TRUE)
      e <- ending_profile(dd[unlist(idx[pick], use.names = FALSE)])$eff
      vapply(INSTALLED, function(x) bump_at(e, x), numeric(1))
    })
    rbindlist(lapply(seq_along(INSTALLED), function(i) {
      v <- bs[i, ]; v <- v[is.finite(v)]
      data.table(preamble = pid, ending = INSTALLED[i], blocks = ep$blocks,
                 bump = bump_at(ep$eff, INSTALLED[i]),
                 lo = if (length(v) > 2) quantile(v, .025) else NA_real_,
                 hi = if (length(v) > 2) quantile(v, .975) else NA_real_)
    }))
  }), fill = TRUE)
  BUMP[, sig := star(lo, hi)]
  BUMP[, installed := preamble == paste0("p", sprintf("%02d", ending))]
  cat("\nE10 installed-ending bumps (pp, vs the rest of the same decade):\n")
  print(BUMP[, .(preamble, ending, blocks, bump = round(bump, 2),
                 lo = round(lo, 2), hi = round(hi, 2), sig, installed)])
  fwrite(BUMP, file.path(ROOT, "output", "installed_endings.csv"))
}

fwrite(RES,  file.path(ROOT, "output", "estimates.csv"))
fwrite(COND, file.path(ROOT, "output", "estimates_by_condition.csv"))
fwrite(MOD,  file.path(ROOT, "output", "estimates_by_model.csv"))

# -----------------------------------------------------------------------------
# 5. Figures
# -----------------------------------------------------------------------------
STEPS <- NULL; E1PROFILE <- NULL; VISLADDER <- NULL
base <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.title = element_blank(), legend.margin = margin(t = -4),
        strip.background = element_rect(fill = "grey93", colour = NA))
PAL <- c("#1b6ca8", "#c2410c", "#15803d", "#7c3aed", "#b45309", "#0f766e")
lab_off <- function(o) fifelse(o == 100, "+1.00", sprintf(".%02d", o))

# Fig 1: the ending profile, pooled over E1 -----------------------------------
d1 <- prep(D[experiment == "E1"])
if (nrow(d1)) {
  ep <- ending_profile(d1)
  pe <- data.table(offset = as.integer(names(ep$eff)), eff = as.numeric(ep$eff))
  pe[, kind := fifelse(offset %% 10 == 9, "nine-ending",
                fifelse(offset %in% c(0, 100, 50), "round", "other"))]
  g <- ggplot(pe, aes(factor(offset), eff, fill = kind)) +
    geom_col() + geom_hline(yintercept = 0, linewidth = .3) +
    scale_x_discrete(labels = lab_off(sort(pe$offset))) +
    scale_fill_manual(values = c("nine-ending" = "#c2410c", "round" = "#1b6ca8",
                                 "other" = "grey70")) +
    labs(x = "price ending", y = "effect on P(buy), pp\n(net of block mean and price trend)") +
    base + theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 6))
  ggsave(file.path(FIG, "fig_ending_profile.pdf"), g, width = 7.2, height = 3.2)

  #, Fig 2: the ten decade steps 
  st <- decade_steps(ep$eff)
  STEPS <<- copy(st)[, `:=`(from = sprintf(".%02d", step * 10 - 1),
                            to = fifelse(step == 10, "+1.00",
                                         sprintf(".%02d", (step * 10) %% 100)))]
  E1PROFILE <<- data.table(offset = as.integer(names(ep$eff)),
                           eff = as.numeric(ep$eff))
  # ASCII only: R's default pdf device has no glyph for an arrow and silently
  # substitutes one, which shows up as a warning per label and a wrong character.
  st[, lab := sprintf(".%02d to %s", step * 10 - 1,
                      fifelse(step == 10, "+1.00", sprintf(".%02d", (step * 10) %% 100)))]
  g2 <- ggplot(st, aes(factor(step), jump)) +
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = -Inf, ymax = Inf,
             fill = "#c2410c", alpha = .10) +
    geom_hline(yintercept = 0, linewidth = .3, colour = "grey50") +
    geom_hline(yintercept = mean(st[step < 10]$jump), linetype = 2,
               colour = "#1b6ca8", linewidth = .4) +
    geom_col(aes(fill = step == 10), width = .65) +
    scale_fill_manual(values = c("FALSE" = "grey65", "TRUE" = "#c2410c"), guide = "none") +
    scale_x_discrete(labels = st$lab) +
    labs(x = "one-cent step (only the last crosses a dollar)",
         y = "jump in P(buy), pp",
         subtitle = "dashed line: mean of the nine placebo steps") +
    base + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  ggsave(file.path(FIG, "fig_decade_steps.pdf"), g2, width = 6.4, height = 3.2)
}

# Fig 3: left-digit estimate by arm -------------------------------------------
if (nrow(RES)) {
  r <- copy(RES)[order(left_digit)]
  r[, label2 := sprintf("%s  (%s, %d blocks)", arm, label, blocks)]
  r[, label2 := factor(label2, levels = label2)]
  g3 <- ggplot(r, aes(left_digit, label2)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = .25, linewidth = .4) +
    geom_point(aes(colour = sig == "*"), size = 2.2) +
    scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#c2410c"), guide = "none") +
    labs(x = "left-digit estimate, pp (negative = human-like)", y = NULL) + base
  ggsave(file.path(FIG, "fig_by_arm.pdf"), g3, width = 6.6, height = 3.4)
}

# Fig 4: by model, within E1 --------------------------------------------------
if (nrow(MOD[arm == "E1"])) {
  m <- MOD[arm == "E1"][order(left_digit)]
  m[, ml := factor(model, levels = model)]
  g4 <- ggplot(m, aes(left_digit, ml)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = .25, linewidth = .4) +
    geom_point(aes(colour = sig == "*"), size = 2) +
    scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#c2410c"), guide = "none") +
    labs(x = "left-digit estimate, pp", y = NULL,
         subtitle = "E1 core sweep, per model") + base
  ggsave(file.path(FIG, "fig_by_model.pdf"), g4, width = 6.8, height = 3.6)
}

# Fig 5: vision dose-response -------------------------------------------------
if (nrow(COND[arm == "E7"])) {
  v <- COND[arm == "E7"]
  ord <- c("textonly", "same", "r70", "r50", "r35", "sup")
  v <- v[condition %in% ord]; v[, condition := factor(condition, levels = ord)]
  g5 <- ggplot(v, aes(condition, left_digit)) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .18, linewidth = .4) +
    geom_point(aes(colour = sig == "*"), size = 2.4) +
    scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#c2410c"), guide = "none") +
    labs(x = "cents rendered at ... of the dollar-digit height",
         y = "left-digit estimate, pp",
         subtitle = "E7: text control, then the size ladder") + base
  ggsave(file.path(FIG, "fig_vision.pdf"), g5, width = 6.2, height = 3.2)
}

# Fig 6: condition-level detail for the mechanism arms ------------------------
mech <- COND[arm %in% c("E3", "E4", "E10", "E6")]
if (nrow(mech)) {
  mech[, lab := paste0(arm, ": ", condition)]
  mech <- mech[order(arm, left_digit)]
  mech[, lab := factor(lab, levels = lab)]
  g6 <- ggplot(mech, aes(left_digit, lab)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = .25, linewidth = .4) +
    geom_point(aes(colour = sig == "*"), size = 2) +
    scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#c2410c"), guide = "none") +
    facet_grid(arm ~ ., scales = "free_y", space = "free_y") +
    labs(x = "left-digit estimate, pp", y = NULL) + base
  ggsave(file.path(FIG, "fig_mechanism.pdf"), g6, width = 6.8,
         height = 1.2 + .22 * nrow(mech))
}
# Fig 8: does the preference move to the installed ending? --------------------
if (!is.null(BUMP) && nrow(BUMP)) {
  b <- copy(BUMP)
  b[, ending_lab := sprintf("effect at .%02d", ending)]
  b[, pre := factor(preamble, levels = c("none", "p00", "p33", "p77"),
                    labels = c("no preamble", '"...end in .00"',
                               '"...end in .33"', '"...end in .77"'))]
  g8 <- ggplot(b[!is.na(pre)], aes(pre, bump, fill = installed)) +
    geom_hline(yintercept = 0, linewidth = .3, colour = "grey50") +
    geom_col(width = .62) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .16, linewidth = .35) +
    facet_wrap(~ ending_lab) +
    scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#c2410c"),
                      labels = c("other preambles", "this ending was installed")) +
    labs(x = NULL, y = "effect vs the rest of its decade, pp") +
    base + theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 7))
  ggsave(file.path(FIG, "fig_installed.pdf"), g8, width = 6.6, height = 3.2)
}

cat("figures written to", FIG, "\n")

# Fig 7 / E5: corpus frequency of price endings -------------------------------
#
#     The semantic account says the model reproduces which endings the web posts,
#     so its ending profile should track corpus frequency. The perceptual account
#     makes no such prediction: a magnitude distortion does not know what is common.
CORP <- NULL
cf <- file.path(ROOT, "data", "ending_frequency_profile.csv")
if (file.exists(cf)) {
  fr <- fread(cf)
  corp_cols <- setdiff(names(fr), c("ending", "mean", "excess"))
  fr[, share := rowMeans(.SD, na.rm = TRUE), .SDcols = corp_cols]
  fr[, kind := fifelse(ending %% 10 == 9, "nine-ending",
              fifelse(ending %in% c(0, 50), "round", "other"))]
  gA <- ggplot(fr, aes(factor(ending), share, fill = kind)) +
    geom_col() +
    scale_y_continuous(trans = "log10", labels = function(x) paste0(x, "%")) +
    scale_x_discrete(labels = sprintf(".%02d", sort(fr$ending))) +
    scale_fill_manual(values = c("nine-ending" = "#c2410c", "round" = "#1b6ca8",
                                 "other" = "grey70")) +
    labs(x = "price ending", y = "share of posted prices (log)",
         subtitle = sprintf("(a) web corpora: %s", paste(corp_cols, collapse = ", "))) +
    base + theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 6))

  dc <- prep(D[experiment == "E1"])
  if (nrow(dc)) {
    epc <- ending_profile(dc)
    j <- merge(fr[, .(ending, share)],
               data.table(ending = as.integer(names(epc$eff)),
                          eff = as.numeric(epc$eff)), by = "ending")
    j <- j[share > 0]
    rho <- suppressWarnings(cor(log10(j$share), j$eff, method = "spearman"))
    CORP <- list(rho = rho, n = nrow(j))
    gB <- ggplot(j, aes(share, eff)) +
      geom_hline(yintercept = 0, linewidth = .3, colour = "grey60") +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#1b6ca8",
                  fill = "#1b6ca8", alpha = .12, linewidth = .5) +
      geom_point(aes(colour = ending %% 10 == 9), size = 1.8) +
      scale_x_continuous(trans = "log10", labels = function(x) paste0(x, "%")) +
      scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#c2410c"),
                          labels = c("other", "nine-ending")) +
      labs(x = "corpus share of the ending (log)", y = "effect on P(buy), pp",
           subtitle = sprintf("(b) Spearman %.3f over %d endings", rho, nrow(j))) + base
    g7 <- if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(gA, gB, ncol = 1, heights = c(1, 1.1))
    } else NULL
    if (is.null(g7)) {
      ggsave(file.path(FIG, "fig_corpus.pdf"), gB, width = 5.4, height = 3.2)
      ggsave(file.path(FIG, "fig_corpus_shares.pdf"), gA, width = 7.2, height = 3.0)
    } else {
      ggsave(file.path(FIG, "fig_corpus.pdf"), g7, width = 7.0, height = 6.0)
    }
    cat(sprintf("E5 corpus link: Spearman %.3f over %d endings\n", rho, nrow(j)))
  }
}

# -----------------------------------------------------------------------------
# 6. LaTeX tables
#
#    Fragments only: each file is a bare tabular wrapped in a table environment, so
#    paper.tex can \input it and every number in the paper comes from this script.
# -----------------------------------------------------------------------------
esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", as.character(x))
fmt <- function(x, d = 2) fifelse(is.na(x), "", formatC(x, format = "f", digits = d))
ci  <- function(lo, hi) fifelse(is.na(lo), "",
         sprintf("[%s, %s]", fmt(lo), fmt(hi)))
put <- function(file, body) {
  writeLines(body, file.path(TAB, file)); cat("  ", file, "\n", sep = "")
}
tabular <- function(rows, header, align, caption, label, note = NULL) {
  c("\\begin{table}[t]", "\\centering", sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", label), "\\small",
    sprintf("\\begin{tabular}{%s}", align), "\\toprule", header, "\\midrule",
    rows, "\\bottomrule", "\\end{tabular}",
    if (!is.null(note)) c("\\vspace{2pt}",
      sprintf("\\parbox{\\linewidth}{\\footnotesize %s}", note)),
    "\\end{table}")
}

# main table: one row per arm -------------------------------------------------
if (nrow(RES)) {
  r <- RES[order(match(arm, names(BY)))]
  rows <- r[, sprintf("%s & %s & %s & %s & %s & %s%s & %s & %s \\\\",
              esc(label), esc(arm), format(blocks, big.mark = ","),
              fmt(slope), fmt(naive), fmt(left_digit), sig, ci(lo, hi),
              fifelse(is.na(theta), "n/a", fmt(theta)))]
  put("main_estimates.tex", tabular(rows,
    paste("Experiment & Code & Blocks & Slope & Uncorrected &",
          "Left-digit & 95\\% CI & $\\theta$ \\\\"),
    "@{}llrrrrcr@{}",
    "Left-digit effect by experiment, in percentage points of $P(\\text{buy})$.",
    "main",
    paste("Slope is the within-block per-dollar demand response.",
          "Uncorrected is the step-10 jump minus the mean of the nine placebo steps.",
          "It is confounded by the roundness of $.00$ and takes the opposite sign to the",
          "corrected estimate in four of the ten experiments. Left-digit is step 10's",
          "residual from the placebo regression on destination roundness, and is the",
          "estimate to read.",
          "Intervals are block bootstraps over", N_BOOT, "resamples;",
          "$^{*}$ marks intervals excluding zero.",
          "$\\theta$ is the left-digit effect as a share of the per-dollar decline, the",
          "inattention parameter of the human literature (Lyft $\\approx 0.5$, used cars",
          "$0.30$, scanner data $0.15$--$0.25$). It is a ratio and is blank where the",
          "denominator is within", MIN_SLOPE, "pp of zero; a value above one means the",
          "boundary drop exceeds the entire smooth within-dollar decline and should be",
          "read as all of it, not as a share.")))
}

# per-model table for the core sweep ------------------------------------------
if (nrow(MOD[arm == "E1"])) {
  meta <- unique(D[experiment == "E1", .(model, tier, family, mode)])
  m <- merge(MOD[arm == "E1"], meta, by = "model", all.x = TRUE)[order(tier, left_digit)]
  rows <- m[, sprintf("%s & %s & %s & %s & %s & %s%s & %s \\\\",
              esc(sub("^[^/]*/", "", model)), esc(tier), esc(mode),
              format(blocks, big.mark = ","),
              fmt(slope), fmt(left_digit), sig, ci(lo, hi))]
  put("by_model.tex", tabular(rows,
    "Model & Tier & Readout & Blocks & Slope & Left-digit & 95\\% CI \\\\",
    "lllrrrc",
    "Left-digit effect by model, core sweep (E1).", "bymodel",
    paste("Readout is first-token log-probabilities where the provider exposes them and",
          "the mean of", 12, "sampled binary answers otherwise.")))
}

# condition table (split across two pages) ------------------------------------
if (nrow(COND)) {
  cd <- COND[order(match(arm, names(BY)), condition)]
  cd[, first := !duplicated(arm)]
  make_rows <- function(dt) {
    r <- dt[, sprintf("%s & %s & %s & %s & %s%s & %s \\\\",
              fifelse(first, esc(arm), ""), esc(condition),
              format(blocks, big.mark = ","), fmt(slope),
              fmt(left_digit), sig, ci(lo, hi))]
    r[which(dt$first)[-1] - 1L] <- paste0(r[which(dt$first)[-1] - 1L], " \\addlinespace")
    r
  }
  hdr <- "Arm & Condition & Blocks & Slope & Left-digit & 95\\% CI \\\\"
  aln <- "llrrrc"
  tab1_arms <- c("E0", "E0b", "E1b", "E2")
  cd1 <- cd[arm %in% tab1_arms]
  cd2 <- cd[!arm %in% tab1_arms]
  put("by_condition.tex", tabular(make_rows(cd1), hdr, aln,
    "Left-digit effect by condition within each arm (identification and prompt experiments).",
    "bycond",
    paste("Each arm's conditions are estimated on their own blocks, so the rows are",
          "independent rather than a decomposition of the arm total.")))
  put("by_condition2.tex", tabular(make_rows(cd2), hdr, aln,
    "Left-digit effect by condition within each arm (domain, format, and extension experiments).",
    "bycond2",
    paste("Continued from Table~\\ref{tab:bycond}. Each arm's conditions are estimated",
          "on their own blocks.")))
}

# E9 arithmetic ---------------------------------------------------------------
if (!is.null(E9) && nrow(E9)) {
  e <- E9[order(-p_correct)]
  rows <- e[, sprintf("%s & %s & %s & %s & %s \\\\", esc(sub("^[^/]*/", "", model)),
              format(n_cmp, big.mark = ","), fmt(p_correct, 3),
              fmt(median_cents, 2), fmt(frac_exactly_1, 3))]
  put("arithmetic.tex", tabular(rows,
    "Model & Items & $P(\\text{correct})$ & Median answer & Share saying 1 \\\\", "lrrrr",
    "Arithmetic probes (E9).", "e9",
    "Comparison items ask which of two prices is lower; magnitude items ask how many cents separate them."))
}

# the ten decade steps --------------------------------------------------------
if (!is.null(STEPS) && nrow(STEPS)) {
  pl <- STEPS[step < 10]; fit <- lm(jump ~ r, data = pl)
  STEPS[, pred := as.numeric(predict(fit, STEPS))]
  rows <- STEPS[, sprintf("%d & %s $\\to$ %s & %s & %d & %s & %s & %s \\\\",
             step, from, to, fifelse(step == 10, "yes", "no"), r,
             fmt(jump), fmt(pred), fmt(jump - pred))]
  rows[10] <- paste0("\\addlinespace ", rows[10])
  put("steps.tex", tabular(rows,
    paste("Step & Prices & Crosses \\$ & Roundness & Jump & Roundness fit &",
          "Residual \\\\"), "rlccrrr",
    "The ten one-cent steps, core sweep (E1).", "steps",
    paste("Roundness codes the destination ending: 3 for $.00$, 2 for $.50$, 1 for any",
          "other multiple of ten, 0 otherwise. The fit is estimated on the nine placebo",
          "steps only; step 10's residual from it is the left-digit estimate.")))
}

# vision ladder ---------------------------------------------------------------
if (nrow(COND[arm == "E7"])) {
  ord <- c("textonly", "same", "r70", "r50", "r35", "sup")
  nm  <- c(textonly = "text only, no image", same = "cents at 100\\% height",
           r70 = "cents at 70\\%", r50 = "cents at 50\\%", r35 = "cents at 35\\%",
           sup = "cents as a superscript")
  v <- COND[arm == "E7"][order(match(condition, ord))]
  rows <- v[, sprintf("%s & %s & %s & %s & %s%s & %s \\\\", esc(condition),
              nm[condition], format(blocks, big.mark = ","), fmt(slope),
              fmt(left_digit), sig, ci(lo, hi))]
  put("vision.tex", tabular(rows,
    "Condition & Rendering & Blocks & Slope & Left-digit & 95\\% CI \\\\", "llrrrc",
    "The vision ladder (E7).", "vision",
    paste("Every condition posts the same prices; only the height of the cents relative",
          "to the dollar digits changes. \\texttt{textonly} is the same task with no",
          "image at all.")))
}

# robustness ------------------------------------------------------------------
if (exists("ROB") && nrow(ROB)) {
  rb <- ROB[order(match(arm, names(BY)))]
  rows <- rb[, sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\", esc(arm),
               format(blocks, big.mark = ","), fmt(linear), fmt(quadratic), fmt(cubic),
               fmt(r1only), fmt(allblocks), fmt(strict))]
  put("robustness.tex", tabular(rows,
    paste("Arm & Blocks & Linear & Quadratic & Cubic & Eight $\\rho{=}1$ &",
          "All blocks & Strict \\\\"), "lrrrrrrr",
    "The left-digit estimate under each estimator choice.", "robust",
    paste("Linear is the reported specification. Quadratic and cubic replace the",
          "within-block linear detrend with a polynomial of that degree.",
          "Eight $\\rho{=}1$ drops the roundness extrapolation entirely and compares",
          "step 10 against the eight placebos landing on a plain multiple of ten. That",
          "leaves $.00$'s extra roundness credited to the threshold, so it is the",
          "conservative direction: it understates the effect wherever the roundness",
          "correction matters, and the gap between the two columns is how much work the",
          "correction is doing. All blocks sets the",
          "minimum within-block range to zero, Strict to five percentage points.")))
}

# E0: the matched estimate ----------------------------------------------------
if (exists("MATCH") && !is.null(MATCH) && nrow(MATCH)) {
  nm <- c(same = "the dollar figure increments inside a ten (12 to 13)",
          roll = "the leftmost digit changes (19 to 20)",
          widen = "the dollar figure gains a digit (99 to 100)")
  rows <- MATCH[, sprintf("%s & %s & %s & %s & %s \\\\", esc(nm[boundary]),
             format(blocks, big.mark = ","), fmt(jump),
             fifelse(boundary == "same", "reference", paste0(fmt(vs_same), sig)),
             ci(lo, hi))]
  put("matched.tex", tabular(rows,
    paste("At the top of the block \\dots & Blocks & Step-10 jump &",
          "Against first row & 95\\% CI \\\\"), "p{5.2cm}rrrc",
    "The left-digit effect without a roundness adjustment (E0).", "matched",
    paste("Every block in E0 ends on $.00$, so the attractiveness of $.00$ is common to",
          "all three rows and cancels in the comparison. The estimate is a difference",
          "between two step-10 jumps and no roundness model enters it. Anchors are",
          "matched in level across conditions and every decade carries two products.")))
}
if (exists("MATCHM") && !is.null(MATCHM) && nrow(MATCHM)) {
  rows <- MATCHM[, sprintf("%s & %s & %s & %s & %s \\\\",
             esc(sub("^[^/]*/", "", model)), format(blocks, big.mark = ","),
             fmt(same), fmt(roll), fmt(diff))]
  put("matched_by_model.tex", tabular(rows,
    "Model & Blocks & Increments & Leftmost changes & Difference \\\\", "lrrrr",
    "The E0 contrast within each model.", "matchedmodel",
    "Blocks are the model's total across the three conditions."))
}

# E0b: digit position ---------------------------------------------------------
if (exists("MATCH0B") && !is.null(MATCH0B) && nrow(MATCH0B)) {
  nm <- c(flat  = "no digit boundary ($214 \\to 215$)",
          inner = "tens digit changes ($219 \\to 220$)",
          lead  = "leading digit changes ($199 \\to 200$)")
  rows <- MATCH0B[, sprintf("%s & %s & %s & %s & %s \\\\", nm[digit_cond],
             format(blocks, big.mark = ","), fmt(jump),
             fifelse(digit_cond == "flat", "reference", paste0(fmt(vs_flat), sig)),
             ci(lo, hi))]
  put("matched_e0b.tex", tabular(rows,
    paste("At the top of the block \\dots & Blocks & Step-10 jump &",
          "Against flat & 95\\% CI \\\\"), "p{5.2cm}rrrc",
    "Digit position versus roundness in three-digit prices (E0b).", "matchede0b",
    paste("Three-digit prices separate digit position from roundness.",
          "Flat and inner differ only in destination roundness, with no leading-digit",
          "change in either; their gradient measures roundness alone.",
          "Lead adds the leading-digit change. If lead falls below the gradient",
          "extrapolated from flat and inner, the excess is a leading-digit effect.")))
}
if (exists("MATCH0BM") && !is.null(MATCH0BM) && nrow(MATCH0BM)) {
  rows <- MATCH0BM[, sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
             esc(sub("^[^/]*/", "", model)), format(blocks, big.mark = ","),
             fmt(flat), fmt(inner), fmt(lead), fmt(round_grad), fmt(lead_excess))]
  put("matched_e0b_by_model.tex", tabular(rows,
    paste("Model & Blocks & Flat & Inner & Lead & Roundness &",
          "Lead excess \\\\"), "lrrrrrr",
    "The E0b contrasts within each model.", "matchede0bmodel",
    paste("Roundness is inner minus flat; lead excess is lead minus the",
          "linear extrapolation from flat and inner.")))
}

# how much roundness structure there is to adjust for -------------------------
if (exists("ROUNDLEV") && !is.null(ROUNDLEV) && nrow(ROUNDLEV)) {
  rows <- ROUNDLEV[, sprintf("%d & %s & %d & %s & %s \\\\", rho, level, endings,
             fmt(effect), if ("share" %in% names(ROUNDLEV)) fmt(share) else "")]
  put("roundness_levels.tex", tabular(rows,
    paste("$\\rho$ & Destination is \\dots & Endings & Mean effect &",
          "Corpus share, \\% \\\\"), "rlrrr",
    "How much the endings differ by roundness, measured.", "roundlev",
    paste("Mean effect is the average ending effect at that roundness level in the core",
          "sweep, in percentage points. Corpus share is the average posting frequency of",
          "endings at that level. The two orderings do not agree, because $.99$ and",
          "$.95$ are among the most frequently posted endings and are not round. That is",
          "why the measured-frequency adjustment is a genuine alternative to the ordinal",
          "score rather than a restatement of it.")))
}

# leave one model out ---------------------------------------------------------
if (exists("LOO") && !is.null(LOO) && nrow(LOO)) {
  rows <- LOO[, sprintf("%s & %s & %s \\\\", esc(sub("^[^/]*/", "", dropped)),
             format(blocks, big.mark = ","), fmt(est))]
  put("loo.tex", tabular(rows,
    "Model dropped & Blocks remaining & Estimate \\\\", "lrr",
    "The core estimate with each model removed in turn.", "loo",
    "The full-sample estimate appears in the main table."))
}

# how the estimate depends on the roundness adjustment ------------------------
# E0's matched design belongs in this table: it is the specification that uses no
# roundness model at all, and it is measured on its own data rather than on E1's.
if (exists("ALT") && !is.null(ALT) && nrow(ALT) &&
    exists("MATCH") && !is.null(MATCH) && nrow(MATCH) > 1L) {
  ALT <- rbind(ALT, data.table(
    spec = "Matched design, no roundness model (E0)",
    est = MATCH[boundary == "roll"]$vs_same, lo = MATCH[boundary == "roll"]$lo,
    hi = MATCH[boundary == "roll"]$hi, sig = MATCH[boundary == "roll"]$sig))
}
if (exists("ALT") && !is.null(ALT) && nrow(ALT)) {
  rows <- ALT[, sprintf("%s & %s%s & %s \\\\", esc(spec), fmt(est), sig, ci(lo, hi))]
  put("roundness_specs.tex", tabular(rows,
    "Adjustment for destination roundness & Estimate & 95\\% CI \\\\", "lrc",
    "The core estimate under three adjustments for destination roundness.", "specs",
    paste("Row 1 is the reported estimator, which regresses the nine placebo jumps on an",
          "ordinal roundness score and extrapolates to a whole dollar. Row 2 replaces that",
          "score with the measured frequency of the destination ending in three public",
          "corpora, giving nine distinct values instead of two levels.",
          "Row 3 assumes only that rounder destinations are weakly more attractive: since",
          "$.00$ is the roundest destination on the grid, the dollar step should jump at",
          "least as much as the largest placebo, so the difference between them bounds the",
          "effect without any functional form. Row 4 uses no roundness model at all: it",
          "compares two step-10 jumps that both end on $.00$ and differ only in what",
          "happens to the dollar figure, and it is estimated on E0's own data.",
          "The first three rows are estimated on the core sweep.")))
}

# the vision trend ------------------------------------------------------------
if (exists("TREND") && !is.null(TREND) && nrow(TREND)) {
  rows <- TREND[, sprintf("%s & %s & %s%s & %s & %s \\\\",
             esc(sub("^[^/]*/", "", model)),
             fifelse(is.na(blocks), "", format(blocks, big.mark = ",")),
             fmt(slope), sig, ci(lo, hi), fmt(p_pos, 3))]
  rows[nrow(TREND)] <- paste0("\\midrule ", rows[nrow(TREND)])
  put("vision_trend.tex", tabular(rows,
    "Model & Blocks & Trend & 95\\% CI & $\\Pr(\\gamma > 0)$ \\\\", "lrrcr",
    "The cents-size gradient in E7, by model.", "vistrend",
    paste("Trend is the slope of the left-digit estimate on the height of the cents",
          "relative to the dollar digits, in percentage points per unit of height.",
          "A positive value means the effect gets more negative as the cents shrink,",
          "which is what a perceptual account predicts. The last column is the share of",
          "bootstrap draws above zero. Models contributing fewer than 100 blocks are",
          "excluded.")))
}

# installed conventions -------------------------------------------------------
if (!is.null(BUMP) && nrow(BUMP)) {
  b <- BUMP[order(ending, preamble)]
  rows <- b[, sprintf("%s & .%02d & %s & %s%s & %s \\\\", esc(preamble), ending,
              format(blocks, big.mark = ","), fmt(bump), sig, ci(lo, hi))]
  put("installed.tex", tabular(rows,
    "Preamble & Ending & Blocks & Effect & 95\\% CI \\\\", "llrrc",
    "Installing a pricing convention (E10).", "installed",
    paste("Effect is the ending's own effect minus the mean of the other endings in its",
          "decade, so a local price trend cannot generate it. The prediction is a positive",
          "entry exactly where the preamble names that ending.")))
}

# coverage --------------------------------------------------------------------
cov <- D[, .(models = uniqueN(model), cells = .N, calls = sum(n_obs, na.rm = TRUE),
             usd = sum(cost_usd, na.rm = TRUE)), by = experiment][order(experiment)]
cov[, label := fifelse(experiment %in% names(LABEL), LABEL[experiment], experiment)]
rows <- cov[, sprintf("%s & %s & %d & %s & %s \\\\", esc(label), esc(experiment),
             models, format(cells, big.mark = ","), format(calls, big.mark = ","))]
rows <- c(rows, "\\midrule", sprintf("\\multicolumn{2}{l}{Total} & & %s & %s \\\\",
          format(sum(cov$cells), big.mark = ","),
          format(sum(cov$calls), big.mark = ",")))
put("coverage.tex", tabular(rows,
  "Experiment & Code & Models & Cells & Calls \\\\", "llrrr",
  "Data collected.", "coverage",
  paste("A cell is one condition at one price, averaging the repeated draws that a",
        "sampled read-out needs. Calls is the number of API requests behind those cells.")))

# -----------------------------------------------------------------------------
# 6b. One figure and one table for every arm
#
#     Everything above is either pooled or cross-arm. This produces, for each arm
#     separately: the ten decade steps within every condition, a forest of the
#     condition estimates, the ending profile by condition, and the two tables that
#     go with them. An arm with no condition dimension falls back to its models.
# -----------------------------------------------------------------------------
PRETTY <- list(
  E0  = c(same = "dollar figure increments (12 to 13)",
          roll = "leftmost digit changes (19 to 20)",
          widen = "a digit is added (99 to 100)"),
  E0b = c(flat  = "no boundary (214 to 215)",
          inner = "tens digit changes (219 to 220)",
          lead  = "leading digit changes (199 to 200)"),
  E1b = c("10" = "prices x10", "100" = "prices x100"),
  E2  = NULL,
  E3  = c(odometer = "odometer, miles", calories = "calories per portion",
          weight = "pack weight, kg", delivery_days = "delivery, days",
          rating = "rating out of 10"),
  E4  = c(USD = "US dollar", EUR = "euro", INR = "Indian rupee",
          JPY = "Japanese yen", CHF = "Swiss franc"),
  E6  = c(plain = "$12.99", spaced = "$ 12.99", digitspace = "$1 2 . 9 9",
          iso = "USD 12.99", words = "twelve dollars ninety-nine"),
  E7  = c(textonly = "text only", same = "cents 100%", r70 = "cents 70%",
          r50 = "cents 50%", r35 = "cents 35%", sup = "superscript"),
  E8  = c(high = "think step by step", low = "answer immediately"),
  E10 = c(none = "no preamble", p00 = "prices end in .00",
          p33 = "prices end in .33", p77 = "prices end in .77")
)
KEYNAME <- c(E0 = "Dollar figure", E0b = "Digit position", E1b = "Scale",
             E2 = "Template", E3 = "Domain", E4 = "Currency", E6 = "Rendering",
             E7 = "Tag", E8 = "Instruction", E10 = "Preamble")
keyname_of <- function(a) if (a %in% names(KEYNAME)) unname(KEYNAME[a]) else "Condition"
pretty_of <- function(a, v) {
  m <- PRETTY[[a]]
  if (is.null(m)) return(v)
  out <- unname(m[v]); fifelse(is.na(out), v, out)
}

steps_by <- function(d, key) {
  vals <- if (length(key)) sort(unique(as.character(d[[key]]))) else "pooled"
  rbindlist(lapply(vals, function(v) {
    dd <- if (length(key)) d[as.character(get(key)) == v] else d
    if (!nrow(dd)) return(NULL)
    st <- decade_steps(ending_profile(dd)$eff)
    if (!nrow(st)) return(NULL)
    cbind(data.table(cond = v), st)
  }), fill = TRUE)
}
profile_by <- function(d, key) {
  vals <- if (length(key)) sort(unique(as.character(d[[key]]))) else "pooled"
  rbindlist(lapply(vals, function(v) {
    dd <- if (length(key)) d[as.character(get(key)) == v] else d
    if (!nrow(dd)) return(NULL)
    e <- ending_profile(dd)$eff
    data.table(cond = v, offset = as.integer(names(e)), eff = as.numeric(e))
  }), fill = TRUE)
}

step_lab <- function(k) fifelse(k == 10, ".99 to +1.00",
                        sprintf(".%02d to .%02d", k * 10 - 1, (k * 10) %% 100))

for (a in intersect(names(BY), unique(D$experiment))) {
  key <- BY[[a]]
  d   <- prep(D[experiment == a], by = key)
  if (!nrow(d)) next
  nice <- LABEL[[a]]

  # - the ten steps, one panel per condition 
  st <- steps_by(d, key)
  if (nrow(st)) {
    st[, lab := pretty_of(a, cond)]
    st[, lab := factor(lab, levels = unique(lab[order(cond)]))]
    g <- ggplot(st, aes(factor(step), jump, fill = step == 10)) +
      geom_hline(yintercept = 0, linewidth = .25, colour = "grey55") +
      geom_col(width = .7) +
      scale_fill_manual(values = c("FALSE" = "grey68", "TRUE" = "#c2410c"),
                        labels = c("placebo (stays inside a dollar)",
                                   "treatment (crosses a dollar)")) +
      scale_x_discrete(labels = c(as.character(1:9), "10*")) +
      facet_wrap(~ lab, ncol = min(3L, uniqueN(st$lab))) +
      labs(x = "one-cent step", y = "jump in P(buy), pp",
           subtitle = sprintf("%s (%s): the ten one-cent steps", a, nice)) + base
    nf <- uniqueN(st$lab)
    ggsave(file.path(FIG, sprintf("fig_%s_steps.pdf", a)), g,
           width = 6.8, height = 1.6 + 1.9 * ceiling(nf / min(3L, nf)))
  }

  # - forest of the estimates --
  fr <- if (length(key) && nrow(COND[arm == a])) {
          x <- copy(COND[arm == a]); x[, lab := pretty_of(a, condition)]; x
        } else {
          x <- copy(MOD[arm == a]); x[, lab := model]; x
        }
  if (nrow(fr)) {
    fr <- fr[order(left_digit)][, lab := factor(lab, levels = lab)]
    g <- ggplot(fr, aes(left_digit, lab)) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey55") +
      geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                    width = .22, linewidth = .4) +
      geom_point(aes(colour = sig == "*"), size = 2.2) +
      scale_colour_manual(values = c("FALSE" = "grey50", "TRUE" = "#c2410c"),
                          labels = c("interval covers zero", "excludes zero")) +
      labs(x = "left-digit estimate, pp (negative = human-like)", y = NULL,
           subtitle = sprintf("%s (%s)", a, nice)) + base
    ggsave(file.path(FIG, sprintf("fig_%s_forest.pdf", a)), g,
           width = 6.6, height = 1.5 + .3 * nrow(fr))
  }

  # - ending profile by condition -
  pr <- profile_by(d, key)
  if (nrow(pr) && uniqueN(pr$cond) <= 6) {
    pr[, lab := pretty_of(a, cond)]
    pr[, kind := fifelse(offset %% 10 == 9, "nine-ending",
                 fifelse(offset %in% c(0, 50, 100), "round", "other"))]
    g <- ggplot(pr, aes(offset, eff, fill = kind)) +
      geom_hline(yintercept = 0, linewidth = .25, colour = "grey55") +
      # position = "identity": the endings are 1 to 5 cents apart on a 0-100 axis, so
      # neighbouring bars overlap and the default stacking warns on every panel
      geom_col(width = 2.4, position = "identity") +
      scale_fill_manual(values = c("nine-ending" = "#c2410c", "round" = "#1b6ca8",
                                   "other" = "grey72")) +
      scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                         labels = c(".00", ".25", ".50", ".75", "+1.00")) +
      facet_wrap(~ lab, ncol = min(2L, uniqueN(pr$lab)), scales = "free_y") +
      labs(x = "price ending", y = "effect on P(buy), pp",
           subtitle = sprintf("%s (%s): the ending profile", a, nice)) + base
    np <- uniqueN(pr$lab)
    ggsave(file.path(FIG, sprintf("fig_%s_profile.pdf", a)), g,
           width = 6.8, height = 1.4 + 1.7 * ceiling(np / min(2L, np)))
  }

  # - the two tables 
  if (length(key) && nrow(COND[arm == a])) {
    x <- COND[arm == a][order(condition)]
    has_pretty <- !is.null(PRETTY[[a]])
    rows <- if (has_pretty)
      x[, sprintf("%s & %s & %s & %s & %s & %s%s & %s \\\\",
          esc(condition), esc(pretty_of(a, condition)),
          format(blocks, big.mark = ","), fmt(slope), fmt(naive),
          fmt(left_digit), sig, ci(lo, hi))]
    else
      x[, sprintf("%s & %s & %s & %s & %s%s & %s \\\\",
          esc(condition), format(blocks, big.mark = ","), fmt(slope), fmt(naive),
          fmt(left_digit), sig, ci(lo, hi))]
    put(sprintf("tab_%s_cond.tex", a), tabular(rows,
      if (has_pretty)
        paste(keyname_of(a), "& & Blocks & Slope & Naive & Left-digit & 95\\% CI \\\\")
      else
        paste(keyname_of(a), "& Blocks & Slope & Naive & Left-digit & 95\\% CI \\\\"),
      if (has_pretty) "lp{4.0cm}rrrrc" else "lrrrrc",
      sprintf("%s (%s): estimates by condition.", a, nice),
      sprintf("%scond", tolower(a)),
      paste("Each condition is estimated on its own blocks. Slope is the within-block",
            "per-dollar demand response; naive is the uncorrected step-10 contrast;",
            "left-digit is step 10's residual from the placebo regression on",
            "destination roundness. $^{*}$ marks an interval excluding zero.")))
  }
  if (nrow(MOD[arm == a])) {
    x <- MOD[arm == a][order(left_digit)]
    rows <- x[, sprintf("%s & %s & %s & %s%s & %s \\\\",
                esc(sub("^[^/]*/", "", model)),
                format(blocks, big.mark = ","), fmt(slope),
                fmt(left_digit), sig, ci(lo, hi))]
    put(sprintf("tab_%s_model.tex", a), tabular(rows,
      "Model & Blocks & Slope & Left-digit & 95\\% CI \\\\", "lrrrc",
      sprintf("%s (%s): estimates by model.", a, nice),
      sprintf("%smodel", tolower(a)), NULL))
  }
}

# how to read the figures: the whole pipeline on one block --------------------
#
#     Four panels, left to right, on one real price ladder and then on all of E1.
#     (a) is what the model actually returned; (d) is the number in the abstract.
if ("E1" %in% D$experiment) {
  d1w <- prep(D[experiment == "E1"])
  if (nrow(d1w)) {
    # A deterministic illustration block. It has to teach three things: a full 31-point
    # ladder, a clearly downward-sloping demand curve (so panel (b) shows detrending),
    # and a visible left-digit drop at the dollar boundary in panel (b). Only logprob
    # blocks are considered: sampled blocks give binary 0/1 values that look jagged.
    # The selection requires (i) a visible interior slope (demand that trends down before
    # the boundary), (ii) mean P(buy) in a readable mid-range, and (iii) a strong negative
    # detrended residual at the boundary. Ties break alphabetically.
    logprob_blks <- if ("mode" %in% names(d1w)) unique(d1w[mode == "logprob", blk]) else unique(d1w$blk)
    sl <- d1w[blk %in% logprob_blks, {
      int <- .SD[offset < 99]
      int_slope <- if (nrow(int) > 5) coef(lm(p_buy ~ x, data = int))[2] else NA_real_
      int_rng   <- if (nrow(int) > 5) max(int$p_buy) - min(int$p_buy) else 0
      mid <- mean(int$p_buy)
      xc_ <- x - mean(x); yc_ <- p_buy - mean(p_buy)
      bw_ <- sum(xc_ * yc_) / max(sum(xc_^2), 1e-12)
      res_ <- (yc_ - bw_ * xc_) * 100
      boundary_res <- mean(res_[offset %in% c(99L, 100L)])
      .(n = .N, rng = max(p_buy) - min(p_buy), int_slope = int_slope,
        int_rng = int_rng, mid = mid, boundary_res = boundary_res)
    }, by = blk]
    pick <- sl[n == max(n) & int_rng > 0.10 & int_rng < 0.50 & int_slope < -0.10 &
               mid > 0.30 & mid < 0.85 & boundary_res < -3][order(boundary_res, blk)][1]$blk
    w <- d1w[blk == pick][order(offset)]
    bw <- sum(w$xc * w$yc) / max(sum(w$xc^2), 1e-12)
    w[, res := (yc - bw * xc) * 100]

    pa <- ggplot(w, aes(offset, p_buy * 100)) +
      geom_line(colour = "grey70", linewidth = .3) +
      geom_point(aes(colour = offset %in% c(99, 100)), size = 1.5) +
      scale_colour_manual(values = c("FALSE" = "#1b6ca8", "TRUE" = "#c2410c"),
                          guide = "none") +
      scale_x_continuous(breaks = c(0, 50, 100), labels = c(".00", ".50", "+1.00")) +
      labs(x = NULL, y = "P(buy), %",
           subtitle = "(a) one block, as returned") + base

    pb <- ggplot(w, aes(offset, res)) +
      geom_hline(yintercept = 0, linewidth = .25, colour = "grey55") +
      geom_col(aes(fill = offset %in% c(99, 100)), width = 2.4, position = "identity") +
      scale_fill_manual(values = c("FALSE" = "grey72", "TRUE" = "#c2410c"),
                        guide = "none") +
      scale_x_continuous(breaks = c(0, 50, 100), labels = c(".00", ".50", "+1.00")) +
      labs(x = NULL, y = "residual, pp",
           subtitle = "(b) block mean and trend removed") + base

    epw <- ending_profile(d1w)
    pw  <- data.table(offset = as.integer(names(epw$eff)), eff = as.numeric(epw$eff))
    pc <- ggplot(pw, aes(offset, eff)) +
      geom_hline(yintercept = 0, linewidth = .25, colour = "grey55") +
      geom_col(aes(fill = offset %in% c(99, 100)), width = 2.4, position = "identity") +
      scale_fill_manual(values = c("FALSE" = "grey72", "TRUE" = "#c2410c"),
                        guide = "none") +
      scale_x_continuous(breaks = c(0, 50, 100), labels = c(".00", ".50", "+1.00")) +
      labs(x = "price ending", y = "effect, pp",
           subtitle = sprintf("(c) averaged over %s blocks",
                              format(epw$blocks, big.mark = ","))) + base

    stw <- decade_steps(epw$eff)
    plw <- stw[step < 10]; fw <- lm(jump ~ r, plw)
    stw[, pred := as.numeric(predict(fw, stw))]
    pd <- ggplot(stw, aes(factor(step), jump)) +
      geom_hline(yintercept = 0, linewidth = .25, colour = "grey55") +
      geom_col(aes(fill = step == 10), width = .7) +
      geom_point(aes(y = pred), shape = 95, size = 7, colour = "#1b6ca8") +
      geom_segment(data = stw[step == 10],
                   aes(x = factor(step), xend = factor(step), y = pred, yend = jump),
                   colour = "#c2410c", linewidth = .5,
                   arrow = arrow(length = unit(0.05, "in"))) +
      scale_fill_manual(values = c("FALSE" = "grey72", "TRUE" = "#c2410c"), guide = "none") +
      scale_x_discrete(labels = c(as.character(1:9), "10*")) +
      labs(x = "one-cent step", y = "jump, pp",
           subtitle = "(d) roundness fit (blue) and the estimate (arrow)") + base

    if (requireNamespace("patchwork", quietly = TRUE)) {
      gw <- patchwork::wrap_plots(pa, pb, pc, pd, ncol = 2)
      ggsave(file.path(FIG, "fig_walkthrough.pdf"), gw, width = 7.0, height = 4.6)
      cat("walkthrough block:", pick, "\n")
    }
  }
}

# the roundness correction, drawn ---------------------------------------------
#
#     The one step of the estimator that is not obvious. The nine placebos are
#     regressed on how round their destination is; the tenth step is compared with
#     what that line predicts for a destination as round as a whole dollar, rather
#     than with the placebos themselves.
if (!is.null(STEPS) && nrow(STEPS) == 10) {
  stx <- copy(STEPS)
  pl  <- stx[step < 10]; fitx <- lm(jump ~ r, pl)
  pred10 <- as.numeric(predict(fitx, data.table(r = 3L)))
  act10  <- stx[step == 10]$jump
  line <- data.table(r = seq(1, 3, by = .01))
  line[, jump := as.numeric(predict(fitx, line))]
  pts <- rbind(
    pl[, .(r = as.numeric(r), jump, what = "one placebo step")],
    data.table(r = 3, jump = pred10, what = "what roundness alone predicts"),
    data.table(r = 3, jump = act10,  what = "the dollar-crossing step"))
  gr <- ggplot() +
    geom_hline(yintercept = 0, linewidth = .25, colour = "grey60") +
    geom_line(data = line[r <= 2], aes(r, jump), colour = "#1b6ca8", linewidth = .6) +
    geom_line(data = line[r >= 2], aes(r, jump), colour = "#1b6ca8", linewidth = .6,
              linetype = 2) +
    geom_segment(aes(x = 3, xend = 3, y = pred10, yend = act10), colour = "#c2410c",
                 linewidth = .7, arrow = arrow(length = unit(.07, "in"))) +
    geom_point(data = pts, aes(r, jump, shape = what, colour = what, fill = what),
               size = 2.8, stroke = .7) +
    annotate("text", x = 2.92, y = (pred10 + act10) / 2, hjust = 1, size = 2.9,
             colour = "#c2410c",
             label = sprintf("left-digit estimate\n%.2f pp", act10 - pred10)) +
    scale_shape_manual(values = c("one placebo step" = 16,
                                  "what roundness alone predicts" = 21,
                                  "the dollar-crossing step" = 16)) +
    scale_colour_manual(values = c("one placebo step" = "grey35",
                                   "what roundness alone predicts" = "#1b6ca8",
                                   "the dollar-crossing step" = "#c2410c")) +
    scale_fill_manual(values = c("one placebo step" = "grey35",
                                 "what roundness alone predicts" = "white",
                                 "the dollar-crossing step" = "#c2410c")) +
    scale_x_continuous(breaks = 1:3, limits = c(0.85, 3.15),
                       labels = c("1\nplain .10 .20 .30\n.40 .60 .70 .80 .90",
                                  "2\n.50", "3\na whole dollar")) +
    labs(x = "how round the step's destination is",
         y = "jump in P(buy), pp",
         subtitle = "Placebo steps against destination roundness, with step 10 read off the fit") +
    base + theme(legend.position = "bottom", axis.text.x = element_text(size = 7))
  ggsave(file.path(FIG, "fig_roundness.pdf"), gr, width = 6.4, height = 3.8)
}

# E7: the cents ladder, one panel per model -----------------------------------
if ("E7" %in% D$experiment) {
  dv <- prep(D[experiment == "E7"], by = "cents_cond")
  ord <- c("textonly", "same", "r70", "r50", "r35")
  dv <- dv[cents_cond %in% ord]
  if (nrow(dv)) {
    keep <- dv[, .(nb = uniqueN(blk)), by = model][nb >= 100L]$model
    vm <- rbindlist(lapply(sort(keep), function(mm)
      rbindlist(lapply(ord, function(k) {
        e <- dv[model == mm & cents_cond == k]
        if (!nrow(e)) return(NULL)
        data.table(model = mm, cond = k, blocks = uniqueN(e$blk),
                   ld = left_digit(ending_profile(e)$eff)[1])
      }))))
    vm[, cond := factor(cond, levels = ord,
                        labels = c("text", "100%", "70%", "50%", "35%"))]
    vm[, short := sub("^[^/]*/", "", model)]
    g <- ggplot(vm, aes(cond, ld, group = short)) +
      geom_hline(yintercept = 0, linewidth = .25, colour = "grey55") +
      geom_line(colour = "#1b6ca8", linewidth = .5) +
      geom_point(size = 1.8, colour = "#1b6ca8") +
      facet_wrap(~ short, nrow = 1, scales = "free_y") +
      labs(x = "height of the cents relative to the dollar digits",
           y = "left-digit estimate, pp",
           subtitle = "E7: the cents ladder, model by model") +
      base + theme(axis.text.x = element_text(size = 6.5))
    ggsave(file.path(FIG, "fig_vision_by_model.pdf"), g, width = 7.0, height = 2.6)
    VISLADDER <<- copy(vm)
  }
}

# E9 gets a figure of its own -------------------------------------------------
if (!is.null(E9) && nrow(E9)) {
  e <- melt(E9[, .(model, `P(correct), comparison` = p_correct,
                   `share answering exactly 1 cent` = frac_exactly_1)],
            id.vars = "model", variable.name = "probe", value.name = "v")
  e[, model := factor(model, levels = E9[order(p_correct)]$model)]
  g <- ggplot(e, aes(v, model, colour = probe)) +
    geom_vline(xintercept = 1, linetype = 2, colour = "grey55") +
    geom_point(size = 2.2, alpha = .85) +
    scale_x_continuous(limits = c(min(0.9, min(e$v, na.rm = TRUE)), 1.002)) +
    scale_colour_manual(values = c("#1b6ca8", "#c2410c")) +
    labs(x = NULL, y = NULL,
         subtitle = "E9: accuracy on the arithmetic probes") + base
  ggsave(file.path(FIG, "fig_E9_accuracy.pdf"), g, width = 6.8, height = 3.4)
}

# E5 gets a table of its own --------------------------------------------------
if (file.exists(cf)) {
  fr <- fread(cf)
  cc <- setdiff(names(fr), c("ending", "mean", "excess"))
  fr[, share := rowMeans(.SD, na.rm = TRUE), .SDcols = cc]
  fr <- fr[order(-share)]
  top <- head(fr, 12)
  rows <- top[, do.call(sprintf, c(list(paste0("$.%02d$ & ",
              paste(rep("%s", length(cc)), collapse = " & "), " & %s \\\\")),
              list(ending), lapply(cc, function(k) fmt(get(k), 2)), list(fmt(share, 2))))]
  put("corpus.tex", tabular(rows,
    paste("Ending &", paste(sprintf("\\texttt{%s}", cc), collapse = " & "),
          "& Mean \\\\"), paste0("l", strrep("r", length(cc) + 1L)),
    "The twelve most common price endings on the open web (E5).", "corpus",
    paste("Percentage of all dollar-and-cents prices posted with each ending, counted",
          "with the infini-gram API over three public corpora. The ordering is nearly",
          "identical across the three (Spearman 0.968 to 0.989), which is why the",
          "corpus choice does not matter to the argument.")))
}

# multiplicity correction table ------------------------------------------------
if (nrow(RES)) {
  mc <- RES[!is.na(se) & se > 0, .(arm, label, left_digit, se, lo, hi)]
  mc[, z := left_digit / se]
  mc[, p := 2 * pnorm(-abs(z))]
  mc[, p_bh := p.adjust(p, "BH")]
  mc[, sig_uncorr := fifelse(p < .05, "*", "")]
  mc[, sig_bh := fifelse(p_bh < .05, "*", "")]
  pfmt <- function(x) fifelse(x < .001, "$<$\\,.001",
            fifelse(x < .01, sprintf("%.3f", x), sprintf("%.2f", x)))
  rows <- mc[, sprintf("%s & %s & %s%s & %s%s \\\\",
               esc(label), ci(lo, hi), pfmt(p), sig_uncorr,
               pfmt(p_bh), sig_bh)]
  MULT <<- mc
  put("multiplicity.tex", tabular(rows,
    "Arm & 95\\% CI & $p$ & BH $p$ \\\\",
    "lcrr",
    "Arm-level left-digit estimates with Benjamini--Hochberg correction.", "mult",
    paste("$p$-values are two-sided, computed from the bootstrap standard error under",
          "a normal approximation. The Benjamini--Hochberg (BH) correction is",
          "applied across the", nrow(mc), "arm-level tests.",
          "An asterisk marks $p < .05$.")))
}

# -----------------------------------------------------------------------------
# 7. Macros for the paper
#
#    paper.tex reads these with \num{Name} and prints a red ?? for anything missing,
#    so a number in the paper is either from here or visibly absent.
# -----------------------------------------------------------------------------
NUM <- new.env()
put_num <- function(name, value, d = 2) {
  # one macro, one value. A vector here recycles against the name list when the file
  # is written and silently misaligns every macro after it alphabetically.
  stopifnot(length(value) == 1L)
  assign(name, if (is.character(value)) value else fmt(value, d), envir = NUM)
}

meta_all <- unique(D[, .(model, tier, family, mode)])
put_num("NModels",    format(uniqueN(D$model)))
put_num("NFamilies",  format(uniqueN(meta_all$family)))
put_num("NPrecision", format(uniqueN(meta_all[mode == "logprob"]$model)))
put_num("NArms",      format(uniqueN(D$experiment)))
put_num("NCells",     format(nrow(D), big.mark = ","))
put_num("NCalls",     format(sum(D$n_obs, na.rm = TRUE), big.mark = ","))
put_num("TotalUSD",   sum(D$cost_usd, na.rm = TRUE))
put_num("NBoot",      format(N_BOOT))
put_num("SampleReps", "12")
if (exists("MULT") && nrow(MULT)) {
  put_num("NMultTests", format(nrow(MULT)))
  put_num("NMultSurviveBH", format(sum(MULT$p_bh < .05)))
}

arm_nums <- function(a, prefix) {
  r <- RES[arm == a]; if (!nrow(r)) return(invisible())
  put_num(paste0(prefix, "LD"),     r$left_digit)
  put_num(paste0(prefix, "Lo"),     r$lo)
  put_num(paste0(prefix, "Hi"),     r$hi)
  put_num(paste0(prefix, "Naive"),  r$naive)
  put_num(paste0(prefix, "Slope"),  r$slope)
  put_num(paste0(prefix, "Theta"),  r$theta)
  put_num(paste0(prefix, "ThetaLo"), r$theta_lo)
  put_num(paste0(prefix, "ThetaHi"), r$theta_hi)
  put_num(paste0(prefix, "Blocks"), format(r$blocks, big.mark = ","))
}
for (a in RES$arm) arm_nums(a, gsub("([0-9])", "\\1", a))   # E1 -> E1LD, E7 -> E7LD ...

if (nrow(MOD[arm == "E1"])) {
  m <- MOD[arm == "E1"]
  put_num("NModelsSig",    format(sum(m$sig == "*", na.rm = TRUE)))
  put_num("NModelsSigNeg", format(sum(m$sig == "*" & m$left_digit < 0, na.rm = TRUE)))
  put_num("MaxModelLD",    m[which.max(abs(left_digit))]$left_digit)
  put_num("MedianModelLD", median(m$left_digit, na.rm = TRUE))
}
if (nrow(COND[arm == "E7"])) {
  v <- COND[arm == "E7"]
  for (k in v$condition) {
    n <- paste0("Vis", toupper(substring(k, 1, 1)), substring(k, 2))
    put_num(paste0(n, "LD"), v[condition == k]$left_digit)
    put_num(paste0(n, "Lo"), v[condition == k]$lo)
    put_num(paste0(n, "Hi"), v[condition == k]$hi)
  }
}
# Every condition in every arm gets its own macros, e.g. \E3odometerLD, \E4JPYHi.
if (nrow(COND)) {
  cn <- gsub("[^A-Za-z0-9]", "", COND$condition)
  for (i in seq_len(nrow(COND))) {
    put_num(paste0(COND$arm[i], cn[i], "LD"), COND$left_digit[i])
    put_num(paste0(COND$arm[i], cn[i], "Lo"), COND$lo[i])
    put_num(paste0(COND$arm[i], cn[i], "Hi"), COND$hi[i])
  }
  te <- COND[arm == "E2"]
  if (nrow(te)) {
    put_num("E2TemplateBest",  min(te$left_digit, na.rm = TRUE))
    put_num("E2TemplateWorst", max(te$left_digit, na.rm = TRUE))
    put_num("E2NSig", format(sum(te$sig == "*" & te$left_digit < 0, na.rm = TRUE)))
    put_num("E2NTemplates", format(nrow(te)))
  }
}
# The ten steps, so the walk-through in the text cannot drift from the table.
if (!is.null(STEPS) && nrow(STEPS)) {
  pl <- STEPS[step < 10]
  put_num("PlaceboMean", mean(pl$jump))
  put_num("Step5Jump",   STEPS[step == 5]$jump)
  put_num("Step10Jump",  STEPS[step == 10]$jump)
  put_num("Step10Pred",  as.numeric(predict(lm(jump ~ r, pl), data.table(r = 3L))))
}
if (!is.null(E1PROFILE) && nrow(E1PROFILE)) {
  o <- E1PROFILE[order(-eff)]
  put_num("ProfTopA", o[1]$eff); put_num("ProfTopB", o[2]$eff); put_num("ProfTopC", o[3]$eff)
  put_num("ProfTopAe", sprintf(".%02d", o[1]$offset))
  put_num("ProfTopBe", sprintf(".%02d", o[2]$offset))
  put_num("ProfTopCe", sprintf(".%02d", o[3]$offset))
  put_num("ProfBotA", o[.N]$eff); put_num("ProfBotB", o[.N - 1]$eff)
  put_num("ProfBotAe", fifelse(o[.N]$offset == 100, "+1.00", sprintf(".%02d", o[.N]$offset)))
  put_num("ProfBotBe", fifelse(o[.N-1]$offset == 100, "+1.00", sprintf(".%02d", o[.N-1]$offset)))
}
if (exists("ROB") && nrow(ROB)) {
  for (i in seq_len(nrow(ROB))) {
    a <- ROB$arm[i]
    put_num(paste0(a, "Quad"),  ROB$quadratic[i])
    put_num(paste0(a, "Cubic"), ROB$cubic[i])
    put_num(paste0(a, "R1"),    ROB$r1only[i])
  }
}
if (file.exists(cf)) {
  fr2 <- fread(cf)
  cc <- setdiff(names(fr2), c("ending", "mean", "excess"))
  fr2[, share := rowMeans(.SD, na.rm = TRUE), .SDcols = cc]
  # how much the corpus choice matters: rank agreement between every pair of corpora
  if (length(cc) > 1) {
    pr <- combn(cc, 2, function(k)
      suppressWarnings(cor(fr2[[k[1]]], fr2[[k[2]]], method = "spearman")))
    put_num("CorpusAgreeMin", min(pr), 3); put_num("CorpusAgreeMax", max(pr), 3)
  }
  for (e in c(0, 25, 50, 75, 95, 99)) put_num(sprintf("Share%02d", e), fr2[ending == e]$share, 1)
  put_num("ShareTopThree", sum(fr2[ending %in% c(0, 95, 99)]$share), 1)
}
if (!is.null(BUMP) && nrow(BUMP)) {
  put_num("InstalledOn",  mean(BUMP[installed == TRUE]$bump))
  put_num("InstalledOff", mean(BUMP[installed == FALSE]$bump))
  for (i in seq_len(nrow(BUMP))) {
    nm <- sprintf("Bump%s%02d", toupper(BUMP$preamble[i]), BUMP$ending[i])
    put_num(nm, BUMP$bump[i]); put_num(paste0(nm, "Lo"), BUMP$lo[i])
    put_num(paste0(nm, "Hi"), BUMP$hi[i])
  }
}
if (!is.null(TREND) && nrow(TREND)) {
  pooled <- TREND[model == "all four pooled"]
  if (nrow(pooled)) {
    put_num("VisTrend", pooled$slope[1]); put_num("VisTrendLo", pooled$lo[1])
    put_num("VisTrendHi", pooled$hi[1]);  put_num("VisTrendPos", pooled$p_pos[1], 3)
  }
}
if (!is.null(VISLADDER) && nrow(VISLADDER)) {
  for (i in seq_len(nrow(VISLADDER))) {
    nm <- gsub("[^A-Za-z0-9]", "", paste0(VISLADDER$short[i], VISLADDER$cond[i]))
    put_num(paste0("Lad", nm), VISLADDER$ld[i])
  }
}
if (exists("TREND") && !is.null(TREND) && nrow(TREND)) {
  for (i in seq_len(nrow(TREND))) {
    nm <- gsub("[^A-Za-z0-9]", "", sub("^[^/]*/", "", TREND$model[i]))
    put_num(paste0("Trend", nm),        TREND$slope[i])
    put_num(paste0("Trend", nm, "Lo"),  TREND$lo[i])
    put_num(paste0("Trend", nm, "Hi"),  TREND$hi[i])
    put_num(paste0("Trend", nm, "Pos"), TREND$p_pos[i], 3)
  }
}
# Design constants and derived counts, so that nothing in the write-up is typed by hand.
for (a in unique(D$experiment)) {
  put_num(paste0("N", a, "Models"), format(uniqueN(D[experiment == a]$model)))
  put_num(paste0("N", a, "Cells"),  format(nrow(D[experiment == a]), big.mark = ","))
}
put_num("NPricesPerBlock", format(uniqueN(D[experiment == "E1"]$offset)))
put_num("NDomains",     format(uniqueN(D[experiment == "E3"]$domain)))
put_num("NCurrencies",  format(uniqueN(D[experiment == "E4"]$currency)))
put_num("NRenderings",  format(uniqueN(D[experiment == "E6"]$rendering)))
put_num("NTagConds",    format(uniqueN(D[experiment == "E7"]$cents_cond)))
put_num("NPreambles",   format(uniqueN(D[experiment == "E10"]$preamble_id)))
if (exists("TREND") && !is.null(TREND) && nrow(TREND))
  put_num("NE7ModelsKept", format(nrow(TREND[model != "pooled"])))
if (nrow(RES)) {
  # how often the uncorrected contrast points the other way
  put_num("NSignFlip", format(sum(sign(RES$naive) != sign(RES$left_digit), na.rm = TRUE)))
  put_num("NArmsSig",  format(sum(RES$sig == "*", na.rm = TRUE)))
}
if (!is.null(STEPS) && nrow(STEPS)) {
  pl <- STEPS[step < 10]
  # the confound, measured on the real profile: what roundness alone adds to step 10
  put_num("RoundPremium",
          as.numeric(predict(lm(jump ~ r, pl), data.table(r = 3L))) - mean(pl$jump))
}
if (nrow(RES[arm == "E1"])) put_num("CorrectionSize",
  abs(RES[arm == "E1"]$left_digit - RES[arm == "E1"]$naive))
# every interval this analysis reports, for the multiplicity statement
put_num("NIntervals", format(nrow(RES) + nrow(COND) + nrow(MOD) +
        (if (!is.null(BUMP)) nrow(BUMP) else 0L) +
        (if (exists("TREND") && !is.null(TREND)) nrow(TREND) else 0L)))
if (exists("MATCH") && !is.null(MATCH) && nrow(MATCH)) {
  for (i in seq_len(nrow(MATCH))) {
    b <- MATCH$boundary[i]
    put_num(paste0("M", b, "Jump"), MATCH$jump[i])
    put_num(paste0("M", b, "Blocks"), format(MATCH$blocks[i], big.mark = ","))
    if (b != "same") {
      put_num(paste0("M", b), MATCH$vs_same[i])
      put_num(paste0("M", b, "Lo"), MATCH$lo[i]); put_num(paste0("M", b, "Hi"), MATCH$hi[i])
    }
  }
}
if (exists("MATCHM") && !is.null(MATCHM) && nrow(MATCHM)) {
  put_num("MNegModels", format(sum(MATCHM$diff < 0, na.rm = TRUE)))
  put_num("MNModels",   format(nrow(MATCHM)))
}
if (exists("MATCH0B") && !is.null(MATCH0B) && nrow(MATCH0B)) {
  for (i in seq_len(nrow(MATCH0B))) {
    k <- MATCH0B$digit_cond[i]
    put_num(paste0("M0b", k, "Jump"), MATCH0B$jump[i])
    put_num(paste0("M0b", k, "Blocks"), format(MATCH0B$blocks[i], big.mark = ","))
    if (k != "flat") {
      put_num(paste0("M0b", k), MATCH0B$vs_flat[i])
      put_num(paste0("M0b", k, "Lo"), MATCH0B$lo[i])
      put_num(paste0("M0b", k, "Hi"), MATCH0B$hi[i])
    }
  }
  # the roundness gradient and lead excess
  put_num("M0bRoundGrad", MATCH0B[digit_cond == "inner"]$vs_flat)
  put_num("M0bLeadExcess",
          MATCH0B[digit_cond == "lead"]$jump -
          (2 * MATCH0B[digit_cond == "inner"]$jump - MATCH0B[digit_cond == "flat"]$jump))
}
if (exists("MATCH0BM") && !is.null(MATCH0BM) && nrow(MATCH0BM)) {
  put_num("M0bNegModels", format(sum(MATCH0BM$lead_excess < 0, na.rm = TRUE)))
  put_num("M0bNModels",   format(nrow(MATCH0BM)))
}
if (exists("LOO") && !is.null(LOO) && nrow(LOO)) {
  put_num("LooMin", min(LOO$est, na.rm = TRUE)); put_num("LooMax", max(LOO$est, na.rm = TRUE))
}
if (exists("ROUNDLEV") && !is.null(ROUNDLEV) && nrow(ROUNDLEV)) {
  for (k in seq_len(nrow(ROUNDLEV)))
    put_num(paste0("RhoEff", ROUNDLEV$rho[k]), ROUNDLEV$effect[k])
}
if (exists("ALT") && !is.null(ALT) && nrow(ALT)) {
  nm <- c("SpecOrdinal", "SpecMeasured", "SpecBound", "SpecMatched")
  for (i in seq_len(min(4L, nrow(ALT)))) {
    put_num(nm[i], ALT$est[i]); put_num(paste0(nm[i], "Lo"), ALT$lo[i])
    put_num(paste0(nm[i], "Hi"), ALT$hi[i])
  }
}
if (!is.null(CORP)) { put_num("CorpusRho", CORP$rho, 3); put_num("CorpusN", format(CORP$n)) }
if (!is.null(E9) && nrow(E9)) {
  put_num("E9Correct",  mean(E9$p_correct), 3)
  put_num("E9ExactOne", mean(E9$frac_exactly_1), 3)
  put_num("E9Worst",    min(E9$p_correct), 3)
}
# Human benchmarks, for the comparison sentence. Fixed, not estimated here.
put_num("ThetaLyft", "0.50"); put_num("ThetaCars", "0.30"); put_num("ThetaScanner", "0.15 to 0.25")

nm <- sort(ls(NUM))
# \csname, not \newcommand{\E1LD}: a TeX control word may contain only letters, and
# half of these names carry an arm number. \num{} reads them back the same way.
writeLines(c("% generated by analysis/analyse.R -- do not edit",
             sprintf("\\expandafter\\def\\csname %s\\endcsname{%s}",
                     nm, unlist(mget(nm, NUM)))),
           file.path(TAB, "numbers.tex"))
cat("  numbers.tex (", length(nm), " macros)\n", sep = "")

# -----------------------------------------------------------------------------
# 8. Hand the output to the paper
# -----------------------------------------------------------------------------
for (dest in c("paper", "plan")) {
  if (!dir.exists(file.path(ROOT, dest))) next
  pf <- file.path(ROOT, dest, "figures"); pt <- file.path(ROOT, dest, "tables")
  dir.create(pf, showWarnings = FALSE, recursive = TRUE)
  dir.create(pt, showWarnings = FALSE, recursive = TRUE)
  file.copy(list.files(FIG, "\\.pdf$", full.names = TRUE), pf, overwrite = TRUE)
  file.copy(list.files(TAB, "\\.tex$", full.names = TRUE), pt, overwrite = TRUE)
  cat("copied figures and tables into ", dest, "/\n", sep = "")
}
cat("\ndone.\n")
