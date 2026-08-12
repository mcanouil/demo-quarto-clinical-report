# Clinical Study Report - Tables, listings and figures
# One function per report output, following the pharmaverse cardinal catalogue.
#
# @license MIT
# @copyright 2026 Mickaël Canouil
# @author Mickaël Canouil
#
# gtsummary computes the statistics and keeps the Analysis Results Datasets,
# gt renders them.

suppressPackageStartupMessages({
  library(dplyr)
  library(gtsummary)
  library(ggplot2)
})

#' Read an analysis dataset written by R/00-adam.R.
adam <- function(name) {
  path <- file.path("data", paste0(name, ".rds"))
  if (!file.exists(path)) {
    path <- file.path("..", path)
  }
  if (!file.exists(path)) {
    stop(
      "Analysis data '",
      name,
      "' not found. Run `Rscript R/00-adam.R` first.",
      call. = FALSE
    )
  }
  readRDS(path)
}

#' Counts of subjects the analysis datasets exclude, written by R/00-adam.R.
#'
#' Read from the same frozen snapshot as the analysis datasets, so the subject
#' flow figure cannot drift from the tables by reaching into the source package.
adam_counts <- function() {
  adam("counts")
}

#' Drop the variable labels carried by the ADaM datasets.
#'
#' Without this, gt and gtsummary display the CDISC label rather than the
#' column name chosen for the output.
drop_labels <- function(data) {
  data[] <- lapply(data, function(column) {
    attr(column, "label") <- NULL
    column
  })
  data
}

# Analysis populations ----------------------------------------------------

tlf_populations <- function(adsl) {
  adsl |>
    transmute(
      TRT01P,
      `Randomised` = "Y",
      `Intention-to-treat population` = ITTFL,
      `Safety population` = SAFFL,
      `Per-protocol population` = PPROTFL
    ) |>
    drop_labels() |>
    tbl_summary(
      by = TRT01P,
      statistic = all_categorical() ~ "{n} ({p}%)",
      value = everything() ~ "Y",
      missing = "no"
    ) |>
    add_overall(last = TRUE) |>
    modify_header(label = "**Population**")
}

# Disposition -------------------------------------------------------------

tlf_disposition <- function(adsl) {
  flags <- c(
    "Randomised",
    "Treated (safety population)",
    "Completed the study",
    "Discontinued the study",
    "Died"
  )

  adsl |>
    transmute(
      TRT01P,
      `Randomised` = "Yes",
      `Treated (safety population)` = if_else(SAFFL == "Y", "Yes", "No"),
      `Completed the study` = if_else(COMPFL == "Y", "Yes", "No"),
      `Discontinued the study` = if_else(COMPFL == "N", "Yes", "No"),
      # A reason is present only for subjects who discontinued, so gtsummary
      # indents its levels under the label and takes the discontinuation count
      # as their denominator.
      `Reason for discontinuation` = DCSREAS,
      `Died` = if_else(!is.na(DTHFL) & DTHFL == "Y", "Yes", "No")
    ) |>
    drop_labels() |>
    tbl_summary(
      by = TRT01P,
      statistic = all_categorical() ~ "{n} ({p}%)",
      value = stats::setNames(as.list(rep("Yes", length(flags))), flags),
      missing = "no"
    ) |>
    add_overall(last = TRUE) |>
    modify_header(label = "**Disposition**")
}

# Demographics ------------------------------------------------------------

tlf_demographics <- function(adsl) {
  adsl |>
    select(TRT01P, AGE, AGEGR1, SEX, RACE, ETHNIC, TRTDURD) |>
    tbl_summary(
      by = TRT01P,
      label = list(
        AGE = "Age (years)",
        AGEGR1 = "Age group (years)",
        SEX = "Sex",
        RACE = "Race",
        ETHNIC = "Ethnicity",
        TRTDURD = "Treatment duration (days)"
      ),
      statistic = list(
        all_continuous() ~ "{mean} ({sd}); {median} [{min}, {max}]",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing = "no"
    ) |>
    add_overall(last = TRUE) |>
    modify_header(label = "**Characteristic**")
}

# Exposure ----------------------------------------------------------------

tlf_exposure <- function(adex) {
  adex |>
    transmute(
      TRTA,
      `Exposure (days)` = AVAL,
      # The lowest break is open below zero so that a subject randomised but
      # never dosed lands in a visible category instead of being dropped as
      # missing, which would let the categories sum to less than the population.
      `Exposure category` = cut(
        AVAL,
        breaks = c(-Inf, 0, 28, 84, 168, Inf),
        labels = c("0", "1 to 28", "29 to 84", "85 to 168", ">168"),
        right = TRUE
      )
    ) |>
    drop_labels() |>
    tbl_summary(
      by = TRTA,
      statistic = list(
        all_continuous() ~ "{mean} ({sd}); {median} [{min}, {max}]",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing = "no"
    ) |>
    modify_header(label = "**Exposure**")
}

# Incidence by hierarchy --------------------------------------------------

#' Terms reached by at least `threshold` of the subjects in any one treatment
#' group.
#'
#' The threshold is applied per group rather than to the pooled population: a
#' term concentrated in a single treatment group is exactly what these tables
#' exist to surface, and pooling would hide it behind the other groups.
common_terms <- function(events, denominator, term, group, threshold) {
  group_sizes <- count(denominator, .data[[group]], name = "denominator")

  events |>
    distinct(.data[["USUBJID"]], .data[[group]], .data[[term]]) |>
    count(.data[[group]], .data[[term]], name = "subjects") |>
    left_join(group_sizes, by = group) |>
    filter(.data[["subjects"]] / .data[["denominator"]] >= threshold) |>
    pull(.data[[term]]) |>
    unique()
}

#' Subject incidence by a two-level hierarchy, against a fixed denominator.
#'
#' Shared by the adverse event, concomitant medication and medical history
#' tables: all three count subjects once per term within a grouping term, and
#' take their percentages from the population rather than from the event count.
tbl_incidence <- function(
  events,
  denominator,
  variables,
  group,
  labels,
  overall_label,
  header
) {
  tbl_hierarchical(
    events,
    variables = all_of(variables),
    by = all_of(group),
    id = USUBJID,
    denominator = denominator,
    overall_row = TRUE,
    label = c(labels, list(..ard_hierarchical_overall.. = overall_label))
  ) |>
    modify_header(label = header)
}

#' Incidence by a two-level hierarchy, restricted to the terms that reach
#' `threshold` in any treatment group.
#'
#' The table is computed on every event and the sub-threshold rows are dropped
#' from the display afterwards, rather than the events being filtered first.
#' Filtering first would make the "any event" row count only the subjects whose
#' term survived the threshold, which understates it: the same quantity is
#' reported by the overview table, and the two would disagree.
tlf_incidence <- function(
  events,
  denominator,
  variables,
  group,
  threshold,
  labels,
  overall_label,
  header
) {
  common <- common_terms(
    events,
    denominator,
    variables[[2L]],
    group,
    threshold
  )

  events |>
    tbl_incidence(
      denominator = denominator,
      variables = variables,
      group = group,
      labels = labels,
      overall_label = overall_label,
      header = header
    ) |>
    trim_incidence(events, variables, common)
}

#' Drop the rows below the reporting threshold from a hierarchical table.
#'
#' The grouping rows keep the terms they still contain, so a system organ class
#' with no term above the threshold disappears rather than standing empty.
trim_incidence <- function(tbl, events, variables, common) {
  grouping <- variables[[1L]]
  term <- variables[[2L]]
  shown <- unique(events[[grouping]][events[[term]] %in% common])

  modify_table_body(tbl, function(body) {
    filter(
      body,
      (.data[["variable"]] != term | .data[["label"]] %in% common) &
        (.data[["variable"]] != grouping | .data[["label"]] %in% shown)
    )
  })
}

# Medical history ---------------------------------------------------------

tlf_medical_history <- function(admh, adsl, threshold = 0.05) {
  tlf_incidence(
    events = admh,
    denominator = adsl,
    variables = c("MHBODSYS", "MHDECOD"),
    group = "TRT01P",
    threshold = threshold,
    labels = list(MHBODSYS = "Body system", MHDECOD = "Preferred term"),
    overall_label = "Any medical history finding",
    header = "**Body system / preferred term**"
  )
}

# Concomitant medications -------------------------------------------------

tlf_conmeds <- function(adcm, adsl, threshold = 0.05) {
  tlf_incidence(
    events = adcm,
    denominator = safety_denominator(adsl),
    variables = c("CMCLAS", "CMDECOD"),
    group = "TRTA",
    threshold = threshold,
    labels = list(CMCLAS = "Drug class", CMDECOD = "Preferred term"),
    overall_label = "Any prior or concomitant medication",
    header = "**Drug class / preferred term**"
  )
}

# Adverse events ----------------------------------------------------------

#' The safety population, keyed by actual treatment under the name the
#' on-treatment datasets use, so it can serve as their denominator directly.
safety_denominator <- function(adsl) {
  adsl |>
    filter(SAFFL == "Y") |>
    rename(TRTA = TRT01A)
}

tlf_ae_overview <- function(adae, adsl) {
  subject_flags <- adsl |>
    filter(SAFFL == "Y") |>
    select(USUBJID, TRT01A) |>
    left_join(
      adae |>
        group_by(USUBJID) |>
        summarise(
          any_ae = TRUE,
          serious = any(AESER == "Y", na.rm = TRUE),
          severe = any(AESEV == "SEVERE", na.rm = TRUE),
          related = any(RELFL == "Y", na.rm = TRUE),
          fatal = any(AESDTH == "Y", na.rm = TRUE),
          withdrawn = any(AEACN == "DRUG WITHDRAWN", na.rm = TRUE),
          dermatologic = any(DERMFL == "Y", na.rm = TRUE),
          .groups = "drop"
        ),
      by = "USUBJID"
    ) |>
    transmute(
      TRT01A,
      `Any treatment-emergent adverse event (TEAE)` = coalesce(any_ae, FALSE),
      `Any serious TEAE` = coalesce(serious, FALSE),
      `Any severe TEAE` = coalesce(severe, FALSE),
      `Any drug-related TEAE` = coalesce(related, FALSE),
      `Any TEAE leading to withdrawal` = coalesce(withdrawn, FALSE),
      `Any dermatologic TEAE` = coalesce(dermatologic, FALSE),
      `Any TEAE with fatal outcome` = coalesce(fatal, FALSE)
    )

  subject_flags |>
    drop_labels() |>
    tbl_summary(
      by = TRT01A,
      statistic = all_categorical() ~ "{n} ({p}%)",
      value = everything() ~ TRUE,
      missing = "no"
    ) |>
    add_overall(last = TRUE) |>
    modify_header(label = "**Subjects with at least one event**")
}

#' Adverse events with their coded terms in the case the tables display, and
#' restricted to the preferred terms that reach `threshold` in any one
#' treatment group.
common_ae <- function(adae, adsl, threshold) {
  safety <- safety_denominator(adsl)
  events <- mutate(
    adae,
    AEBODSYS = stringr::str_to_sentence(AEBODSYS),
    AEDECOD = stringr::str_to_sentence(AEDECOD)
  )
  common_pt <- common_terms(events, safety, "AEDECOD", "TRTA", threshold)

  list(
    safety = safety,
    events = events,
    common = common_pt
  )
}

#' Subject incidence by system organ class / preferred term, safety
#' population denominator.
ae_soc_pt_hierarchical <- function(data, safety) {
  tbl_incidence(
    data,
    denominator = safety,
    variables = c("AEBODSYS", "AEDECOD"),
    group = "TRTA",
    labels = list(AEBODSYS = "System organ class", AEDECOD = "Preferred term"),
    overall_label = "Any treatment-emergent adverse event",
    header = "**System organ class / preferred term**"
  )
}

tlf_ae_by_soc_pt <- function(adae, adsl, threshold = 0.05) {
  common <- common_ae(adae, adsl, threshold)

  ae_soc_pt_hierarchical(common$events, common$safety) |>
    trim_incidence(common$events, c("AEBODSYS", "AEDECOD"), common$common)
}

#' System organ class / preferred term incidence, split by `strata`.
#'
#' The preferred terms to report are chosen once, across all strata, so that
#' every stratum reports the same set. Row sets still differ across strata (a
#' preferred term need not occur in every stratum); rows merge on matching
#' labels rather than position, so the resulting gtsummary message is safe to
#' silence.
ae_soc_pt_by_strata <- function(data, strata, safety, common) {
  tbl_strata(
    data,
    strata = {{ strata }},
    .tbl_fun = function(stratum) {
      ae_soc_pt_hierarchical(stratum, safety) |>
        trim_incidence(stratum, c("AEBODSYS", "AEDECOD"), common)
    },
    .combine_args = list(quiet = TRUE)
  )
}

#' Subject incidence by system organ class / preferred term, at the
#' subject's maximum severity for that preferred term.
tlf_ae_by_severity <- function(adae, adsl, threshold = 0.05) {
  common <- common_ae(adae, adsl, threshold)

  common$events |>
    slice_max(AESEV, n = 1L, by = c(USUBJID, AEDECOD), with_ties = FALSE) |>
    mutate(
      AESEV = factor(
        stringr::str_to_sentence(as.character(AESEV)),
        levels = c("Mild", "Moderate", "Severe")
      )
    ) |>
    ae_soc_pt_by_strata(AESEV, common$safety, common$common)
}

#' Subject incidence by system organ class / preferred term, classified as
#' related if any event for that preferred term was investigator-assessed
#' as related to study drug.
tlf_ae_by_relationship <- function(adae, adsl, threshold = 0.05) {
  common <- common_ae(adae, adsl, threshold)

  common$events |>
    mutate(any_related = any(RELFL == "Y"), .by = c(USUBJID, AEDECOD)) |>
    slice_head(n = 1L, by = c(USUBJID, AEDECOD)) |>
    mutate(
      Relationship = factor(
        if_else(any_related, "Related", "Not related"),
        levels = c("Related", "Not related")
      )
    ) |>
    ae_soc_pt_by_strata(Relationship, common$safety, common$common)
}

tlf_ae_serious <- function(adae, adsl) {
  safety <- safety_denominator(adsl)
  serious <- adae |>
    filter(AESER == "Y") |>
    mutate(AEDECOD = stringr::str_to_sentence(AEDECOD))

  if (nrow(serious) == 0L) {
    stop("No serious adverse events in the analysis data.", call. = FALSE)
  }

  serious |>
    tbl_hierarchical(
      variables = AEDECOD,
      by = TRTA,
      id = USUBJID,
      denominator = safety,
      overall_row = TRUE,
      label = list(
        AEDECOD = "Preferred term",
        ..ard_hierarchical_overall.. = "Any serious treatment-emergent adverse event"
      )
    ) |>
    modify_header(label = "**Preferred term**")
}

# Laboratory --------------------------------------------------------------

# The supine baseline. Vital signs are measured in three postures, each with its
# own baseline, so every vital-signs analysis in this report fixes one posture
# rather than pooling them.
vitals_basetype <- "LAST: AFTER LYING DOWN FOR 5 MINUTES"

#' Records for one parameter under a single baseline definition.
#'
#' A subject has one record per visit per `BASETYPE`, so leaving `BASETYPE` open
#' lets the same subject enter an analysis several times: it would inflate a
#' shift-table count and break the independence assumption of a model. Where a
#' parameter has more than one baseline definition, the caller must choose.
parameter_records <- function(data, parameter, basetype = NULL) {
  records <- filter(data, PARAM == parameter)

  if (nrow(records) == 0L) {
    stop("No records for parameter '", parameter, "'.", call. = FALSE)
  }

  if (!"BASETYPE" %in% names(records)) {
    stop(
      "Parameter '",
      parameter,
      "' comes from a dataset with no BASETYPE, ",
      "so its baseline definition cannot be checked.",
      call. = FALSE
    )
  }

  # A missing BASETYPE would be dropped by the filter below without a word, so
  # it is refused rather than silently excluded from the analysis.
  if (anyNA(records[["BASETYPE"]])) {
    stop(
      "Parameter '",
      parameter,
      "' has ",
      sum(is.na(records[["BASETYPE"]])),
      " records with no baseline definition.",
      call. = FALSE
    )
  }

  available <- sort(unique(records[["BASETYPE"]]))

  if (is.null(basetype)) {
    if (length(available) > 1L) {
      stop(
        "Parameter '",
        parameter,
        "' has ",
        length(available),
        " baseline definitions (",
        paste(available, collapse = "; "),
        "). Pass `basetype` to choose one.",
        call. = FALSE
      )
    }
    basetype <- available
  } else if (!basetype %in% available) {
    stop(
      "Baseline definition '",
      basetype,
      "' not found for parameter '",
      parameter,
      "'. Available: ",
      paste(available, collapse = "; "),
      ".",
      call. = FALSE
    )
  }

  filter(records, BASETYPE == basetype)
}

#' Shift from baseline to worst post-baseline reference-range category.
#'
#' Shared by ADLB (laboratory), ADVS (vital signs) and ADEG (electrocardiogram):
#' all three carry a baseline (`BNRIND`) and a per-record (`ANRIND`)
#' reference-range indicator.
tlf_shift <- function(data, parameter, basetype = NULL) {
  shift <- data |>
    parameter_records(parameter, basetype = basetype) |>
    filter(!is.na(BNRIND), !is.na(ANRIND), AVISIT != "Baseline") |>
    group_by(USUBJID, TRTA, BNRIND) |>
    summarise(
      worst = case_when(
        any(ANRIND == "HIGH") ~ "High",
        any(ANRIND == "LOW") ~ "Low",
        TRUE ~ "Normal"
      ),
      .groups = "drop"
    ) |>
    mutate(
      `Baseline category` = factor(
        stringr::str_to_sentence(BNRIND),
        levels = c("Low", "Normal", "High")
      ),
      `Worst post-baseline category` = factor(
        worst,
        levels = c("Low", "Normal", "High")
      )
    )

  shift |>
    select(TRTA, `Baseline category`, `Worst post-baseline category`) |>
    drop_labels() |>
    tbl_strata(
      strata = `Baseline category`,
      .tbl_fun = ~ tbl_summary(
        .x,
        by = TRTA,
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "no"
      )
    )
}

#' Render a shift table for each parameter in `panel`, one gt table per
#' parameter, sub-numbered under the number `key` carries in the TLF index.
#'
#' Prints directly; call from a chunk with `#| output: asis`, since knitr
#' does not auto-print gt output produced inside a loop.
#'
#' `knitr::knit_print()` is called explicitly rather than `print()`: `print()`
#' dumps the tag text without knitr's `html_preserve` markers, so Pandoc reads
#' the gt stylesheet as Markdown and the table loses its styling.
tlf_shift_panel <- function(data, panel) {
  for (i in seq_along(panel)) {
    key <- panel[[i]]
    cat("::: {#", tlf_ref(key), "}\n\n", sep = "")
    cat(knitr::knit_print(tlf_output(
      tlf_shift(data, names(panel)[[i]]),
      key
    )))
    cat("\n\n", tlf_caption(key), "\n\n:::\n\n", sep = "")
  }
}

#' Subjects meeting a biochemical criterion for potential drug-induced liver
#' injury, as a multiple of the upper limit of the reference range.
#'
#' The combined criterion requires an aminotransferase elevation and a bilirubin
#' elevation in the same subject; it is a screen, not a diagnosis, since it takes
#' no account of alkaline phosphatase or of the time between the two elevations.
hys_law_flags <- function(adlb) {
  worst <- adlb |>
    filter(
      PARAMCD %in% c("ALT", "AST", "BILI"),
      AVISIT != "Baseline",
      !is.na(AVAL),
      !is.na(ANRHI),
      ANRHI > 0
    ) |>
    mutate(ratio = AVAL / ANRHI) |>
    slice_max(ratio, n = 1L, by = c(USUBJID, PARAMCD), with_ties = FALSE)

  worst |>
    summarise(
      alt_3x = any(PARAMCD == "ALT" & ratio >= 3),
      ast_3x = any(PARAMCD == "AST" & ratio >= 3),
      bili_2x = any(PARAMCD == "BILI" & ratio >= 2),
      .by = c(USUBJID, TRTA)
    )
}

tlf_hys_law <- function(adlb, adsl) {
  flags <- hys_law_flags(adlb)

  safety_denominator(adsl) |>
    select(USUBJID, TRTA) |>
    left_join(
      select(flags, USUBJID, alt_3x, ast_3x, bili_2x),
      by = "USUBJID"
    ) |>
    transmute(
      TRTA,
      `Alanine aminotransferase at least 3 x ULN` = coalesce(alt_3x, FALSE),
      `Aspartate aminotransferase at least 3 x ULN` = coalesce(ast_3x, FALSE),
      `Total bilirubin at least 2 x ULN` = coalesce(bili_2x, FALSE),
      `Aminotransferase at least 3 x ULN with bilirubin at least 2 x ULN` = coalesce(
        (alt_3x | ast_3x) & bili_2x,
        FALSE
      )
    ) |>
    drop_labels() |>
    tbl_summary(
      by = TRTA,
      statistic = all_categorical() ~ "{n} ({p}%)",
      value = everything() ~ TRUE,
      missing = "no"
    ) |>
    add_overall(last = TRUE) |>
    modify_header(label = "**Subjects meeting the criterion**")
}

# Electrocardiogram -------------------------------------------------------

#' Shift from baseline to worst post-baseline category for the rederived QTcF
#' interval, which is the only ECG parameter carrying reference-range
#' indicators in the pilot data.
tlf_ecg_shift <- function(adeg) {
  tlf_shift(adeg, "QTcF - Fridericia's Correction Formula Rederived (ms)")
}


# Efficacy ----------------------------------------------------------------

#' Fit a Cox proportional hazards model and tidy the exponentiated estimates.
cox_hazard_ratio <- function(data, formula) {
  survival::coxph(formula, data = data) |>
    broom::tidy(exponentiate = TRUE, conf.int = TRUE)
}

tlf_tte_summary <- function(adtte) {
  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ TRT01P,
    data = adtte
  )
  events <- adtte |>
    group_by(TRT01P) |>
    summarise(
      n = dplyr::n(),
      events = sum(CNSR == 0L),
      .groups = "drop"
    )

  quantiles <- summary(fit)$table
  medians <- tibble::tibble(
    TRT01P = sub("TRT01P=", "", rownames(quantiles)),
    median = quantiles[, "median"],
    lower = quantiles[, "0.95LCL"],
    upper = quantiles[, "0.95UCL"]
  )

  hazard <- cox_hazard_ratio(adtte, survival::Surv(AVAL, 1 - CNSR) ~ TRT01P) |>
    transmute(
      TRT01P = sub("TRT01P", "", term),
      hazard = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
      p = format.pval(p.value, digits = 2, eps = 0.001)
    )

  events |>
    mutate(TRT01P = as.character(TRT01P)) |>
    left_join(medians, by = "TRT01P") |>
    left_join(hazard, by = "TRT01P") |>
    transmute(
      `Treatment group` = TRT01P,
      `Subjects` = n,
      `Subjects with an event` = sprintf(
        "%d (%.1f%%)",
        events,
        100 * events / n
      ),
      `Median time to event (days)` = if_else(
        is.na(median),
        "Not reached",
        sprintf("%.0f (%.0f, %.0f)", median, lower, upper)
      ),
      `Hazard ratio (95% CI)` = coalesce(hazard, "Reference"),
      `p-value` = coalesce(p, "")
    )
}

tlf_vitals_ancova <- function(
  advs,
  parameter = "Systolic Blood Pressure (mmHg)",
  visit = "Week 24",
  basetype = vitals_basetype
) {
  analysis <- advs |>
    parameter_records(parameter, basetype = basetype) |>
    filter(AVISIT == visit, !is.na(CHG), !is.na(BASE))

  if (nrow(analysis) == 0L) {
    stop(
      "No records for ",
      parameter,
      " at ",
      visit,
      ".",
      call. = FALSE
    )
  }

  stopifnot(!anyDuplicated(analysis[["USUBJID"]]))

  model <- stats::lm(CHG ~ TRTA + BASE, data = analysis)
  means <- emmeans::emmeans(model, specs = "TRTA")
  # `trt.vs.ctrl` applies a Dunnett-style multiplicity adjustment by default,
  # which would widen the intervals and raise the p-values beyond what the
  # statistical methods chapter and the SAP describe. The secondary endpoint is
  # descriptive, so no adjustment is applied.
  contrasts <- emmeans::contrast(
    means,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "none"
  )

  summary_means <- as.data.frame(means)
  summary_contrasts <- as.data.frame(stats::confint(contrasts)) |>
    left_join(
      as.data.frame(contrasts)[, c("contrast", "p.value")],
      by = "contrast"
    )

  observed <- analysis |>
    group_by(TRTA) |>
    summarise(n = dplyr::n(), .groups = "drop")

  summary_means |>
    mutate(TRTA = as.character(TRTA)) |>
    left_join(mutate(observed, TRTA = as.character(TRTA)), by = "TRTA") |>
    mutate(
      contrast = if_else(
        TRTA == levels(analysis[["TRTA"]])[[1L]],
        NA_character_,
        paste(TRTA, "- Placebo")
      )
    ) |>
    left_join(
      transmute(
        summary_contrasts,
        contrast = as.character(contrast),
        difference = sprintf("%.2f (%.2f, %.2f)", estimate, lower.CL, upper.CL),
        p = format.pval(p.value, digits = 2, eps = 0.001)
      ),
      by = "contrast"
    ) |>
    transmute(
      `Treatment group` = TRTA,
      `n` = n,
      `LS mean change (SE)` = sprintf("%.2f (%.2f)", emmean, SE),
      `95% CI` = sprintf("(%.2f, %.2f)", lower.CL, upper.CL),
      `Difference versus placebo (95% CI)` = coalesce(difference, "Reference"),
      `p-value` = coalesce(p, "")
    )
}

# Figures -----------------------------------------------------------------

fig_patient_flow <- function(counts) {
  boxes <- tibble::tibble(
    y = c(4, 3, 2, 1),
    label = c(
      sprintf("Screened\nn = %d", counts[["screened"]]),
      sprintf("Randomised\nn = %d", counts[["randomised"]]),
      sprintf("Treated (safety population)\nn = %d", counts[["treated"]]),
      sprintf("Completed the study\nn = %d", counts[["completed"]])
    )
  )
  drops <- tibble::tibble(
    y = c(3.5, 2.5, 1.5),
    label = c(
      sprintf(
        "Not randomised\nn = %d",
        counts[["screened"]] - counts[["randomised"]]
      ),
      sprintf(
        "Not treated\nn = %d",
        counts[["randomised"]] - counts[["treated"]]
      ),
      sprintf(
        "Discontinued\nn = %d",
        counts[["treated"]] - counts[["completed"]]
      )
    )
  )

  brand <- brand()

  ggplot(boxes) +
    aes(x = 1, y = .data[["y"]], label = .data[["label"]]) +
    geom_segment(
      data = boxes[-nrow(boxes), ],
      mapping = aes(
        x = 1,
        xend = 1,
        y = .data[["y"]] - 0.12,
        yend = .data[["y"]] - 0.88
      ),
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      colour = brand[["colour"]][["secondary"]],
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = drops,
      mapping = aes(x = 1, xend = 1.55, y = .data[["y"]], yend = .data[["y"]]),
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      colour = brand[["colour"]][["secondary"]],
      inherit.aes = FALSE
    ) +
    geom_label(
      fill = brand[["colour"]][["tertiary"]],
      colour = brand[["ink"]],
      linewidth = 0,
      size = 3,
      lineheight = 1.1
    ) +
    geom_label(
      data = drops,
      mapping = aes(x = 1.9, y = .data[["y"]], label = .data[["label"]]),
      fill = brand[["paper"]],
      colour = brand[["colour"]][["secondary"]],
      linewidth = 0.2,
      size = 2.7,
      lineheight = 1.1
    ) +
    scale_x_continuous(limits = c(0.4, 2.4)) +
    scale_y_continuous(limits = c(0.6, 4.4)) +
    theme_void(base_family = brand[["base_family"]])
}

fig_km <- function(adtte) {
  fit <- ggsurvfit::survfit2(
    survival::Surv(AVAL, 1 - CNSR) ~ TRT01P,
    data = adtte
  )

  ggsurvfit::ggsurvfit(fit, type = "risk", linewidth = 0.7) +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable(risktable_stats = "n.risk") +
    scale_colour_manual(values = unname(treatment_colours())) +
    scale_fill_manual(values = unname(treatment_colours())) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      x = "Days since first dose",
      y = "Cumulative incidence",
      colour = NULL,
      fill = NULL
    ) +
    theme_nordvale()
}

fig_forest_subgroup <- function(adtte) {
  hazard_ratio <- function(data) {
    cox_hazard_ratio(data, survival::Surv(AVAL, 1 - CNSR) ~ TRTBIN) |>
      transmute(hr = estimate, lower = conf.low, upper = conf.high)
  }

  data <- adtte |>
    mutate(
      TRTBIN = factor(
        if_else(TRT01P == "Placebo", "Placebo", "Xanomeline"),
        levels = c("Placebo", "Xanomeline")
      )
    )

  overall <- hazard_ratio(data) |>
    mutate(subgroup = "Overall", level = "All subjects")

  by_sex <- data |>
    group_by(SEX) |>
    group_modify(~ hazard_ratio(.x)) |>
    ungroup() |>
    transmute(subgroup = "Sex", level = as.character(SEX), hr, lower, upper)

  by_age <- data |>
    group_by(AGEGR1) |>
    group_modify(~ hazard_ratio(.x)) |>
    ungroup() |>
    transmute(
      subgroup = "Age group",
      level = as.character(AGEGR1),
      hr,
      lower,
      upper
    )

  forest_data <- bind_rows(overall, by_sex, by_age) |>
    mutate(
      subgroup = factor(subgroup, levels = c("Overall", "Sex", "Age group")),
      level = factor(level, levels = rev(level))
    )

  brand <- brand()

  ggplot(forest_data) +
    aes(x = hr, y = level, xmin = lower, xmax = upper) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = brand[["colour"]][["secondary"]]
    ) +
    geom_pointrange(colour = brand[["colour"]][["primary"]], fatten = 2.5) +
    scale_x_log10() +
    facet_grid(
      rows = vars(subgroup),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    labs(
      x = "Hazard ratio (95% CI), xanomeline (any dose) vs placebo",
      y = NULL
    ) +
    theme_nordvale() +
    theme(strip.placement = "outside")
}

fig_vitals_profile <- function(
  advs,
  parameter = "Systolic Blood Pressure (mmHg)",
  basetype = vitals_basetype
) {
  visits <- c(
    "Baseline",
    "Week 2",
    "Week 4",
    "Week 8",
    "Week 12",
    "Week 16",
    "Week 20",
    "Week 24"
  )

  advs |>
    parameter_records(parameter, basetype = basetype) |>
    filter(AVISIT %in% visits, !is.na(AVAL)) |>
    mutate(AVISIT = factor(AVISIT, levels = visits)) |>
    group_by(TRTA, AVISIT) |>
    summarise(
      mean = mean(AVAL),
      se = stats::sd(AVAL) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    ggplot() +
    aes(
      x = .data[["AVISIT"]],
      y = .data[["mean"]],
      colour = .data[["TRTA"]],
      group = .data[["TRTA"]]
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.6) +
    geom_errorbar(
      aes(
        ymin = .data[["mean"]] - .data[["se"]],
        ymax = .data[["mean"]] + .data[["se"]]
      ),
      width = 0.15
    ) +
    scale_colour_manual(values = treatment_colours()) +
    labs(
      x = NULL,
      y = paste(parameter, "(mean and standard error)"),
      colour = NULL
    ) +
    theme_nordvale()
}

# Listings ----------------------------------------------------------------

listing_serious_ae <- function(adae) {
  adae |>
    filter(AESER == "Y") |>
    arrange(TRTA, USUBJID, ASTDT) |>
    transmute(
      `Subject` = USUBJID,
      `Treatment` = as.character(TRTA),
      `Preferred term` = stringr::str_to_sentence(AEDECOD),
      `System organ class` = stringr::str_to_sentence(AEBODSYS),
      `Onset day` = ASTDY,
      `Severity` = stringr::str_to_sentence(as.character(AESEV)),
      `Related` = if_else(RELFL == "Y", "Yes", "No"),
      `Outcome` = stringr::str_to_sentence(AEOUT)
    ) |>
    drop_labels()
}

listing_discontinuation <- function(adsl) {
  adsl |>
    filter(COMPFL == "N") |>
    arrange(TRT01P, USUBJID) |>
    transmute(
      `Subject` = USUBJID,
      `Treatment` = as.character(TRT01P),
      `Age (years)` = AGE,
      `Sex` = as.character(SEX),
      `Treatment duration (days)` = TRTDURD,
      `Reason for discontinuation` = as.character(DCSREAS)
    ) |>
    drop_labels()
}

#' Subjects whose study participation ended for a protocol violation.
#'
#' The pilot study ships no protocol deviation dataset, so this is the only
#' deviation information available: violations that did not end participation
#' cannot be listed.
listing_deviation <- function(adsl) {
  adsl |>
    filter(!is.na(DCSREAS), DCSREAS == "Protocol violation") |>
    arrange(TRT01P, USUBJID) |>
    transmute(
      `Subject` = USUBJID,
      `Treatment` = as.character(TRT01P),
      `Age (years)` = AGE,
      `Sex` = as.character(SEX),
      `Treatment duration (days)` = TRTDURD,
      `End of study status` = stringr::str_to_sentence(EOSSTT)
    ) |>
    drop_labels()
}

listing_demographics <- function(adsl) {
  adsl |>
    arrange(TRT01P, USUBJID) |>
    transmute(
      `Subject` = USUBJID,
      `Treatment` = as.character(TRT01P),
      `Age (years)` = AGE,
      `Age group` = as.character(AGEGR1),
      `Sex` = as.character(SEX),
      `Race` = stringr::str_to_sentence(as.character(RACE)),
      `Completed` = if_else(COMPFL == "Y", "Yes", "No")
    ) |>
    drop_labels()
}

#' Worst post-baseline value of each liver-injury analyte, for the subjects
#' meeting at least one criterion in the potential drug-induced liver injury
#' table.
listing_lab_abnormal <- function(adlb) {
  flagged <- hys_law_flags(adlb) |>
    filter(alt_3x | ast_3x | bili_2x) |>
    pull(USUBJID)

  adlb |>
    filter(
      USUBJID %in% flagged,
      PARAMCD %in% c("ALT", "AST", "BILI"),
      AVISIT != "Baseline",
      !is.na(AVAL),
      !is.na(ANRHI),
      ANRHI > 0
    ) |>
    mutate(ratio = AVAL / ANRHI) |>
    slice_max(ratio, n = 1L, by = c(USUBJID, PARAMCD), with_ties = FALSE) |>
    arrange(TRTA, USUBJID, PARAMCD) |>
    transmute(
      `Subject` = USUBJID,
      `Treatment` = as.character(TRTA),
      `Analyte` = PARAM,
      `Visit` = AVISIT,
      `Worst post-baseline value` = sprintf("%.1f", AVAL),
      `Upper limit of reference range` = sprintf("%.1f", ANRHI),
      `Multiple of upper limit` = sprintf("%.1f", ratio)
    ) |>
    drop_labels()
}
