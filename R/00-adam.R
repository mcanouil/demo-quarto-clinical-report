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
# pharmaverseadam. ADTTE is not shipped, so it is derived here with admiral, and
# the reason for discontinuation comes from the SDTM disposition domain shipped
# by pharmaversesdtm, since the pilot ADSL carries no such variable.
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

# Disposition -------------------------------------------------------------

# One record per screened subject. The pilot ADSL reports whether a subject
# completed or discontinued (EOSSTT) but not why, so the reason is taken here
# from the SDTM disposition domain and carried onto ADSL as DCSREAS.
disposition <- pharmaversesdtm::ds |>
  filter(DSCAT == "DISPOSITION EVENT") |>
  transmute(USUBJID, DCSREAS = stringr::str_to_sentence(DSDECOD))

screened <- n_distinct(disposition[["USUBJID"]])

# Reasons are listed from the most to the least frequent, which is how the
# disposition table reads them; screen failures are not randomised, so they
# never reach ADSL and are excluded from the level set.
discontinuation_reasons <- disposition |>
  filter(!DCSREAS %in% c("Completed", "Screen failure")) |>
  count(DCSREAS, sort = TRUE) |>
  pull(DCSREAS)

# Subject level -----------------------------------------------------------

adsl <- pharmaverseadam::adsl |>
  filter(ARM %in% treatment_levels) |>
  derive_vars_merged(
    dataset_add = disposition,
    by_vars = exprs(USUBJID),
    new_vars = exprs(DCSREAS)
  ) |>
  mutate(
    ITTFL = "Y",
    TRT01P = factor(TRT01P, levels = treatment_levels),
    TRT01A = factor(TRT01A, levels = treatment_levels),
    AGEGR1 = factor(AGEGR1, levels = c("18-64", ">64")),
    SEX = factor(SEX, levels = c("F", "M"), labels = c("Female", "Male")),
    RACE = factor(RACE),
    COMPFL = if_else(EOSSTT == "COMPLETED", "Y", "N", missing = "N"),
    # A reason is reported only for subjects who discontinued; the disposition
    # table indents these rows under the discontinuation count.
    DCSREAS = factor(
      if_else(COMPFL == "N", DCSREAS, NA_character_),
      levels = discontinuation_reasons
    ),
    # The pilot ships no protocol deviation dataset. The only deviation signal
    # available is discontinuation for protocol violation, and those subjects
    # are already excluded below as non-completers, so the per-protocol
    # population remains an approximation: safety-population completers.
    PPROTFL = if_else(SAFFL == "Y" & COMPFL == "Y", "Y", "N")
  )

stopifnot(!anyNA(adsl[["DCSREAS"]][adsl[["COMPFL"]] == "N"]))

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

adtte_all <- derive_param_tte(
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
  )

adtte <- filter(adtte_all, !is.na(AVAL), AVAL > 0)
adtte_excluded <- n_distinct(adtte_all[["USUBJID"]]) -
  n_distinct(adtte[["USUBJID"]])

# A few subjects in the pilot data have a last date known alive that precedes
# their first dose. derive_param_tte() censors them at the start date rather than
# dropping them, giving a one-day time on study. The count is carried forward so
# that the time-to-event table can footnote it instead of hiding it.
#
# Counted from the unfiltered derivation, not from `adtte`: were those records
# ever to fall to a non-positive AVAL, the filter above would remove them and
# this count would silently report zero, turning the disclosure in the report
# into a false statement.
censored_before_first_dose <- adtte_all |>
  filter(CNSR == 1L) |>
  semi_join(
    filter(adsl_tte, !is.na(LSTALVDT), LSTALVDT < TRTSDT),
    by = "USUBJID"
  ) |>
  nrow()

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

# Electrocardiogram -------------------------------------------------------

# The analysis records are the average of the replicate readings at each visit,
# so DTYPE is "AVERAGE" rather than missing as it is in ADVS.
adeg <- pharmaverseadam::adeg |>
  semi_join(adsl, by = "USUBJID") |>
  filter(SAFFL == "Y", ANL01FL == "Y") |>
  mutate(TRTA = factor(TRTA, levels = treatment_levels))

# Concomitant medications -------------------------------------------------

adcm <- pharmaverseadam::adcm |>
  semi_join(adsl, by = "USUBJID") |>
  filter(SAFFL == "Y") |>
  mutate(
    TRTA = factor(TRTA, levels = treatment_levels),
    CMCLAS = stringr::str_to_sentence(CMCLAS),
    CMDECOD = stringr::str_to_sentence(CMDECOD)
  )

# Medical history ---------------------------------------------------------

# The primary diagnosis is recorded once per subject with no coded body system;
# it is the indication, not a medical history finding, so it is excluded.
# Medical history is a baseline characteristic, so it is summarised on the
# randomised population by planned treatment.
admh <- pharmaverseadam::admh |>
  semi_join(adsl, by = "USUBJID") |>
  filter(!is.na(MHBODSYS)) |>
  mutate(
    TRT01P = factor(TRT01P, levels = treatment_levels),
    MHBODSYS = stringr::str_to_sentence(MHBODSYS),
    MHDECOD = stringr::str_to_sentence(MHDECOD)
  )

# Write -------------------------------------------------------------------

# Counts no analysis dataset can carry, because they describe subjects the
# analysis datasets exclude. Written alongside the datasets so the subject flow
# figure and the table footnotes read the same frozen snapshot as every other
# number in the report, rather than reaching back into the source packages.
counts <- list(
  screened = screened,
  randomised = nrow(adsl),
  treated = sum(adsl[["SAFFL"]] == "Y"),
  completed = sum(adsl[["COMPFL"]] == "Y"),
  adtte_excluded = adtte_excluded,
  censored_before_first_dose = censored_before_first_dose
)

saveRDS(counts, file.path(output_dir, "counts.rds"))
message(
  sprintf(
    "COUNTS: %d screened, %d randomised, %d excluded from ADTTE",
    counts[["screened"]],
    counts[["randomised"]],
    counts[["adtte_excluded"]]
  )
)

datasets <- list(
  adsl = adsl,
  adae = adae,
  adtte = adtte,
  adex = adex,
  advs = advs,
  adlb = adlb,
  adeg = adeg,
  adcm = adcm,
  admh = admh
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
