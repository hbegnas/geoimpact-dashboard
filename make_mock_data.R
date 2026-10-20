# make_mock_data.R
# Generates MADE-UP data so the dashboard can be built and tested before real MPC data arrives.
# Run once from the project root:   source("make_mock_data.R")
# Later, replace data/*.csv with real monthly aggregates (keep the same column names).

library(dplyr)
library(tidyr)
library(tibble)
library(readr)

set.seed(2026)

months     <- seq(as.Date("2025-04-01"), by = "month", length.out = 18)
sites      <- paste("Town", 1:5)
components <- c("Component 1", "Component 2")

# ---- Base grid: one row per month x site x component ---------------------------
base <- expand_grid(month = months, site = sites, component = components) |>
  mutate(
    t       = match(month, months),
    engaged = rpois(n(), lambda = 14 + t * 0.8 + if_else(component == "Component 1", 6, 0)),
    pwud    = rbinom(n(), engaged, 0.6)
  )

# ---- Helpers --------------------------------------------------------------------
# numerator is a share of PWUD
rate_ind <- function(df, name, p) {
  tibble(month = df$month, site = df$site, component = df$component,
         indicator = name,
         numerator = rbinom(nrow(df), df$pwud, p),
         denominator = df$pwud)
}
# two-stage: some PWUD qualify (denominator), a share of those get the outcome (numerator)
nested_ind <- function(df, name, p_first, p_second) {
  first <- rbinom(nrow(df), df$pwud, p_first)
  tibble(month = df$month, site = df$site, component = df$component,
         indicator = name,
         numerator = rbinom(nrow(df), first, p_second),
         denominator = first)
}

# ---- indicators.csv (long format: one row per month x site x component x indicator)
indicators <- bind_rows(
  tibble(month = base$month, site = base$site, component = base$component,
         indicator = "engaged", numerator = base$engaged, denominator = NA_integer_),
  tibble(month = base$month, site = base$site, component = base$component,
         indicator = "engaged_pwud", numerator = base$pwud, denominator = base$engaged),
  # Reach, Metric 2: gaps among newly engaged PWUD (denominator = PWUD)
  rate_ind(base, "gap_never_hiv_test",   0.55),
  rate_ind(base, "gap_never_hcv_test",   0.65),
  rate_ind(base, "gap_never_sud_assess", 0.50),
  rate_ind(base, "gap_not_on_hiv_med",   0.45),
  rate_ind(base, "gap_not_on_hcv_tx",    0.60),
  rate_ind(base, "gap_not_on_sud_tx",    0.55),
  # Adoption: referral made (numerator) out of referral needed (denominator)
  nested_ind(base, "link_hiv", 0.40, 0.60),
  nested_ind(base, "link_hcv", 0.30, 0.45),
  nested_ind(base, "link_sud", 0.45, 0.55),
  # Effectiveness: retained on medication (denominator = initiated)
  nested_ind(base, "retained_med", 0.25, 0.60)
)

# Organization-level adoption indicators (site level, component = "Total")
orgs <- expand_grid(month = months, site = sites) |>
  mutate(component = "Total", denominator = 12L)
org_ind <- function(name, p) {
  orgs |>
    mutate(indicator = name, numerator = rbinom(n(), denominator, p)) |>
    select(month, site, component, indicator, numerator, denominator)
}
indicators <- bind_rows(
  indicators,
  org_ind("org_referring",    0.55),
  org_ind("org_receiving",    0.40),
  org_ind("org_coordinating", 0.30)
)

# ---- cascade.csv ------------------------------------------------------------------
cascade_defs <- tribble(
  ~cascade,        ~step_order, ~step,                 ~definition,                                                          ~keep,
  "HIV treatment", 1L, "Tested",              "Rapid or confirmatory HIV test",                                      1,
  "HIV treatment", 2L, "Diagnosed",           "New diagnosis, or existing diagnosis verified",                       0.55,
  "HIV treatment", 3L, "Linked to care",      "ART prescribed, existing Rx confirmed, or linked to external care",   0.85,
  "HIV treatment", 4L, "Received care",       "ART picked up; linkage confirmed",                                    0.85,
  "HIV treatment", 5L, "Retained in care",    "2+ CD4 or VL tests at least 3 months apart in past 12 months",        0.80,
  "HIV treatment", 6L, "Virally suppressed",  "Most recent viral load under 200 copies/mL",                          0.85,
  "HIV PrEP",      1L, "Tested",              "HIV-negative test result",                                            1,
  "HIV PrEP",      2L, "Linked to PrEP care", "PrEP discussed or offered",                                           0.70,
  "HIV PrEP",      3L, "Prescribed PrEP",     "New prescription, or existing prescription confirmed",                0.65,
  "HIV PrEP",      4L, "Initiated PrEP",      "Prescription picked up",                                              0.75,
  "HIV PrEP",      5L, "Retained in care",    "Visits and refills on schedule for the formulation",                  0.65,
  "SUD",           1L, "Screened",            "ROUDA, RSUDA, AUDIT, or NIDA quick screen completed",                 1,
  "SUD",           2L, "Diagnosed",           "New diagnosis, or existing diagnosis verified",                       0.60,
  "SUD",           3L, "Linked to care",      "Medication prescribed, referred, or existing Rx confirmed",           0.80,
  "SUD",           4L, "Received care",       "Medication started; referral linkage confirmed",                      0.75,
  "SUD",           5L, "Retained in care",    "Visits and refills on schedule",                                      0.65,
  "HCV",           1L, "Screened",            "Anti-HCV antibody test",                                              1,
  "HCV",           2L, "Diagnosed",           "Active viremia confirmed (RNA PCR or point of care)",                 0.60,
  "HCV",           3L, "Linked to care",      "DAA prescribed or referred for treatment",                            0.70,
  "HCV",           4L, "Cured (SVR)",         "Undetectable viral load 12 weeks after treatment",                    0.65
)

simulate_cascade <- function(defs, m, s) {
  k    <- nrow(defs)
  n    <- integer(k)
  prog <- rep(NA_integer_, k)
  n[1] <- rpois(1, 40)
  for (i in seq_len(k)[-1]) {
    n[i]    <- rbinom(1, n[i - 1], defs$keep[i])                  # raw monthly count at this step
    prog[i] <- rbinom(1, n[i - 1], min(1, defs$keep[i] + 0.05))   # unique patients who moved up from prior step
  }
  defs |>
    mutate(month = m, site = s,
           n_at_step    = n,
           n_first      = n[1],
           n_prior      = c(NA_integer_, n[-k]),
           n_progressed = prog) |>
    select(-keep)
}

cascade <- bind_rows(lapply(split(cascade_defs, cascade_defs$cascade), function(defs) {
  bind_rows(lapply(months, function(m) {
    bind_rows(lapply(sites, function(s) simulate_cascade(defs, m, s)))
  }))
}))

# ---- qual_themes.csv (placeholders) -----------------------------------------------
qual_sites <- c("All sites", sites)
qual <- bind_rows(
  expand_grid(site = qual_sites, i = 1:3) |>
    transmute(site, kind = "theme", tag = "[Facilitator / Barrier]",
              title   = paste("[Theme", i, "title]"),
              summary = "[One to two sentences summarizing what participants said.]",
              quote   = "[Short illustrative quote]",
              role    = "[Role only, no names]"),
  expand_grid(site = qual_sites, i = 1:3) |>
    transmute(site, kind = "reflection", tag = NA_character_, title = NA_character_,
              summary = c("[Adaptation or change noted this quarter]", "[Barrier the team is working on]", "[Something that is working well]")[i],
              quote = NA_character_, role = NA_character_)
)

# ---- Write ------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write_csv(indicators, "data/indicators.csv")
write_csv(cascade,    "data/cascade.csv")
write_csv(qual,       "data/qual_themes.csv")
message("Wrote data/indicators.csv, data/cascade.csv, data/qual_themes.csv (MOCK DATA)")
