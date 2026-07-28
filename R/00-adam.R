#!/usr/bin/env Rscript
#
# Clinical Study Report - Analysis data preparation
# Builds the analysis-ready datasets from pharmaverseadam, including the
# time-to-event dataset derived with admiral.
#
# @license MIT
# @copyright 2026 Mickaël Canouil
# @author Mickaël Canouil

# Source data are the CDISC pilot study (CDISCPILOT01) ADaM datasets shipped by
# pharmaverseadam. ADTTE is not shipped, so it is derived here with admiral.
#
# Run with: Rscript R/00-adam.R

suppressPackageStartupMessages({
  library(dplyr)
  library(admiral)
  library(pharmaverseadam)
})

treatment_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

output_dir <- "data"
dir.create(output_dir, showWarnings = FALSE)

# Subject level -----------------------------------------------------------

adsl <- pharmaverseadam::adsl |>
  filter(ARM %in% treatment_levels) |>
  mutate(
    ITTFL = "Y",
    TRT01P = factor(TRT01P, levels = treatment_levels),
    TRT01A = factor(TRT01A, levels = treatment_levels),
    AGEGR1 = factor(AGEGR1, levels = c("18-64", ">64")),
    SEX = factor(SEX, levels = c("F", "M"), labels = c("Female", "Male")),
    RACE = factor(RACE),
    COMPFL = if_else(EOSSTT == "COMPLETED", "Y", "N", missing = "N"),
    DCSREAS = if_else(EOSSTT == "DISCONTINUED", "Discontinued", "Completed"),
    # No protocol deviation data is shipped with the CDISC pilot study, so the
    # per-protocol population is approximated as safety-population completers.
    PPROTFL = if_else(SAFFL == "Y" & COMPFL == "Y", "Y", "N")
  )

# Adverse events ----------------------------------------------------------

adae <- pharmaverseadam::adae |>
  semi_join(adsl, by = "USUBJID") |>
  filter(SAFFL == "Y", TRTEMFL == "Y") |>
  mutate(
    TRTA = factor(TRT01A, levels = treatment_levels),
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE")),
    RELFL = if_else(AEREL %in% c("PROBABLE", "POSSIBLE", "RELATED"), "Y", "N"),
    DERMFL = if_else(
      AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
      "Y",
      "N",
      missing = "N"
    )
  )

# Time to first dermatologic treatment-emergent adverse event -------------

adsl_tte <- filter(adsl, SAFFL == "Y")

# One record per subject, so the event source is unambiguous.
adae_first_derm <- adae |>
  filter(DERMFL == "Y", !is.na(ASTDT)) |>
  slice_min(ASTDT, by = c(STUDYID, USUBJID), n = 1L, with_ties = FALSE)

first_derm_event <- event_source(
  dataset_name = "adae_derm",
  date = ASTDT,
  set_values_to = exprs(
    EVNTDESC = "First dermatologic treatment-emergent adverse event",
    SRCDOM = "ADAE",
    CNSR = 0L
  )
)

censor_at_last_contact <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT,
  set_values_to = exprs(
    EVNTDESC = "Last date known alive",
    SRCDOM = "ADSL",
    CNSR = 1L
  )
)

adtte <- derive_param_tte(
  dataset_adsl = adsl_tte,
  start_date = TRTSDT,
  event_conditions = list(first_derm_event),
  censor_conditions = list(censor_at_last_contact),
  source_datasets = list(adsl = adsl_tte, adae_derm = adae_first_derm),
  set_values_to = exprs(
    PARAMCD = "TTDERM",
    PARAM = "Time to first dermatologic treatment-emergent adverse event (days)"
  )
) |>
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT,
    out_unit = "days",
    add_one = TRUE
  ) |>
  derive_vars_merged(
    dataset_add = adsl_tte,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRT01P, TRT01A, SAFFL, PPROTFL, AGE, AGEGR1, SEX)
  ) |>
  filter(!is.na(AVAL), AVAL > 0)

# Exposure ----------------------------------------------------------------

adex <- pharmaverseadam::adex |>
  semi_join(adsl, by = "USUBJID") |>
  filter(SAFFL == "Y", PARAMCD == "TDURD") |>
  mutate(TRTA = factor(TRT01A, levels = treatment_levels))

# Vital signs -------------------------------------------------------------

advs <- pharmaverseadam::advs |>
  semi_join(adsl, by = "USUBJID") |>
  filter(SAFFL == "Y", ANL01FL == "Y", is.na(DTYPE)) |>
  mutate(TRTA = factor(TRTA, levels = treatment_levels))

# Laboratory --------------------------------------------------------------

adlb <- pharmaverseadam::adlb |>
  semi_join(adsl, by = "USUBJID") |>
  filter(SAFFL == "Y", !is.na(AVAL)) |>
  mutate(TRTA = factor(TRTA, levels = treatment_levels))

# Write -------------------------------------------------------------------

datasets <- list(
  adsl = adsl,
  adae = adae,
  adtte = adtte,
  adex = adex,
  advs = advs,
  adlb = adlb
)

for (name in names(datasets)) {
  saveRDS(datasets[[name]], file.path(output_dir, paste0(name, ".rds")))
  message(
    sprintf(
      "%s: %d rows, %d subjects",
      toupper(name),
      nrow(datasets[[name]]),
      dplyr::n_distinct(datasets[[name]][["USUBJID"]])
    )
  )
}
