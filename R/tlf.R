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

#' Population counts by planned treatment, used as table denominators.
population_counts <- function(adsl) {
  adsl |>
    count(TRT01P, name = "n") |>
    tibble::deframe()
}

# Disposition -------------------------------------------------------------

tlf_disposition <- function(adsl) {
  adsl |>
    transmute(
      TRT01P,
      `Randomised` = "Yes",
      `Treated (safety population)` = if_else(SAFFL == "Y", "Yes", "No"),
      `Completed the study` = if_else(COMPFL == "Y", "Yes", "No"),
      `Discontinued the study` = if_else(COMPFL == "N", "Yes", "No"),
      `Died` = if_else(!is.na(DTHFL) & DTHFL == "Y", "Yes", "No")
    ) |>
    drop_labels() |>
    tbl_summary(
      by = TRT01P,
      statistic = all_categorical() ~ "{n} ({p}%)",
      value = everything() ~ "Yes",
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
      `Exposure category` = cut(
        AVAL,
        breaks = c(0, 28, 84, 168, Inf),
        labels = c("1 to 28", "29 to 84", "85 to 168", ">168"),
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

# Adverse events ----------------------------------------------------------

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

#' Preferred terms reported by at least `threshold` of the safety population,
#' and the safety population itself (the shared denominator for AE tables).
common_ae_pt <- function(adae, adsl, threshold) {
  safety <- filter(adsl, SAFFL == "Y")
  common_pt <- adae |>
    distinct(USUBJID, AEDECOD) |>
    count(AEDECOD) |>
    filter(n / nrow(safety) >= threshold) |>
    pull(AEDECOD)
  events <- adae |>
    filter(AEDECOD %in% common_pt) |>
    mutate(
      AEBODSYS = stringr::str_to_sentence(AEBODSYS),
      AEDECOD = stringr::str_to_sentence(AEDECOD)
    )
  list(safety = safety, events = events)
}

#' Subject incidence by system organ class / preferred term, safety
#' population denominator.
ae_soc_pt_hierarchical <- function(data, safety) {
  tbl_hierarchical(
    data,
    variables = c(AEBODSYS, AEDECOD),
    by = TRTA,
    id = USUBJID,
    denominator = rename(safety, TRTA = TRT01A),
    overall_row = TRUE,
    label = list(
      AEBODSYS = "System organ class",
      AEDECOD = "Preferred term",
      ..ard_hierarchical_overall.. = "Any treatment-emergent adverse event"
    )
  ) |>
    modify_header(label = "**System organ class / preferred term**")
}

tlf_ae_by_soc_pt <- function(adae, adsl, threshold = 0.05) {
  common <- common_ae_pt(adae, adsl, threshold)
  ae_soc_pt_hierarchical(common$events, common$safety)
}

#' System organ class / preferred term incidence, split by `strata`.
#'
#' Row sets legitimately differ across strata (a preferred term need not
#' occur in every stratum); rows still merge on matching labels rather than
#' position, so the resulting gtsummary message is safe to silence.
ae_soc_pt_by_strata <- function(data, strata, safety) {
  tbl_strata(
    data,
    strata = {{ strata }},
    .tbl_fun = ~ ae_soc_pt_hierarchical(.x, safety),
    .combine_args = list(quiet = TRUE)
  )
}

#' Subject incidence by system organ class / preferred term, at the
#' subject's maximum severity for that preferred term.
tlf_ae_by_severity <- function(adae, adsl, threshold = 0.05) {
  common <- common_ae_pt(adae, adsl, threshold)

  common$events |>
    slice_max(AESEV, n = 1L, by = c(USUBJID, AEDECOD), with_ties = FALSE) |>
    mutate(
      AESEV = factor(
        stringr::str_to_sentence(as.character(AESEV)),
        levels = c("Mild", "Moderate", "Severe")
      )
    ) |>
    ae_soc_pt_by_strata(AESEV, common$safety)
}

#' Subject incidence by system organ class / preferred term, classified as
#' related if any event for that preferred term was investigator-assessed
#' as related to study drug.
tlf_ae_by_relationship <- function(adae, adsl, threshold = 0.05) {
  common <- common_ae_pt(adae, adsl, threshold)

  common$events |>
    mutate(any_related = any(RELFL == "Y"), .by = c(USUBJID, AEDECOD)) |>
    slice_head(n = 1L, by = c(USUBJID, AEDECOD)) |>
    mutate(
      Relationship = factor(
        if_else(any_related, "Related", "Not related"),
        levels = c("Related", "Not related")
      )
    ) |>
    ae_soc_pt_by_strata(Relationship, common$safety)
}

tlf_ae_serious <- function(adae, adsl) {
  safety <- filter(adsl, SAFFL == "Y")
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
      denominator = rename(safety, TRTA = TRT01A),
      overall_row = TRUE,
      label = list(
        AEDECOD = "Preferred term",
        ..ard_hierarchical_overall.. = "Any serious treatment-emergent adverse event"
      )
    ) |>
    modify_header(label = "**Preferred term**")
}

# Laboratory --------------------------------------------------------------

#' Shift from baseline to worst post-baseline reference-range category.
#'
#' Shared by ADLB (laboratory) and ADVS (vital signs): both carry a
#' baseline (`BNRIND`) and per-record (`ANRIND`) reference-range indicator.
tlf_shift <- function(data, parameter) {
  shift <- data |>
    filter(PARAM == parameter, !is.na(BNRIND), !is.na(ANRIND), AVISIT != "Baseline") |>
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

#' Render a shift table for each laboratory parameter in `panel`, one gt
#' table per parameter, numbered sequentially under "14.3.7".
#'
#' Prints directly; call from a chunk with `#| output: asis`, since knitr
#' does not auto-print gt output produced inside a loop.
#'
#' `knitr::knit_print()` is called explicitly rather than `print()`: `print()`
#' dumps the tag text without knitr's `html_preserve` markers, so Pandoc reads
#' the gt stylesheet as Markdown and the table loses its styling.
tlf_shift_panel <- function(data, panel) {
  for (i in seq_along(panel)) {
    tlf_shift(data, names(panel)[[i]]) |>
      tbl_nordvale(
        title = sprintf(
          "Table 14.3.7.%d: Shift from baseline to worst post-baseline category",
          i
        ),
        subtitle = sprintf("%s, safety population", panel[[i]]),
        source_note = "Categories are relative to the reference range of the reporting laboratory. The worst post-baseline category is high if any post-baseline value was high, otherwise low if any value was low."
      ) |>
      knitr::knit_print() |>
      cat()
    cat("\n\n")
  }
}


# Efficacy ----------------------------------------------------------------

#' Fit a Cox proportional hazards model and tidy the exponentiated estimates.
cox_hazard_ratio <- function(data, formula) {
  survival::coxph(formula, data = data) |>
    broom::tidy(exponentiate = TRUE, conf.int = TRUE)
}

tlf_tte_summary <- function(adtte) {
  fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01P, data = adtte)
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
      `Subjects with an event` = sprintf("%d (%.1f%%)", events, 100 * events / n),
      `Median time to event (days)` = if_else(
        is.na(median),
        "Not reached",
        sprintf("%.0f (%.0f, %.0f)", median, lower, upper)
      ),
      `Hazard ratio (95% CI)` = coalesce(hazard, "Reference"),
      `p-value` = coalesce(p, "")
    )
}

tlf_vitals_ancova <- function(advs, parameter = "Systolic Blood Pressure (mmHg)", visit = "Week 24") {
  analysis <- advs |>
    filter(PARAM == parameter, AVISIT == visit, !is.na(CHG), !is.na(BASE))

  if (nrow(analysis) == 0L) {
    stop(
      "No records for ", parameter, " at ", visit, ".",
      call. = FALSE
    )
  }

  model <- stats::lm(CHG ~ TRTA + BASE, data = analysis)
  means <- emmeans::emmeans(model, specs = "TRTA")
  contrasts <- emmeans::contrast(means, method = "trt.vs.ctrl", ref = 1)

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

fig_patient_flow <- function(adsl) {
  screened <- nrow(pharmaverseadam::adsl)
  randomised <- nrow(adsl)
  treated <- sum(adsl[["SAFFL"]] == "Y")
  completed <- sum(adsl[["COMPFL"]] == "Y")

  boxes <- tibble::tibble(
    y = c(4, 3, 2, 1),
    label = c(
      sprintf("Screened\nn = %d", screened),
      sprintf("Randomised\nn = %d", randomised),
      sprintf("Treated (safety population)\nn = %d", treated),
      sprintf("Completed the study\nn = %d", completed)
    )
  )
  drops <- tibble::tibble(
    y = c(3.5, 2.5, 1.5),
    label = c(
      sprintf("Screen failures\nn = %d", screened - randomised),
      sprintf("Not treated\nn = %d", randomised - treated),
      sprintf("Discontinued\nn = %d", treated - completed)
    )
  )

  brand <- brand()

  ggplot(boxes) +
    aes(x = 1, y = .data[["y"]], label = .data[["label"]]) +
    geom_segment(
      data = boxes[-nrow(boxes), ],
      mapping = aes(x = 1, xend = 1, y = .data[["y"]] - 0.12, yend = .data[["y"]] - 0.88),
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
    mutate(TRTBIN = factor(
      if_else(TRT01P == "Placebo", "Placebo", "Xanomeline"),
      levels = c("Placebo", "Xanomeline")
    ))

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
    transmute(subgroup = "Age group", level = as.character(AGEGR1), hr, lower, upper)

  forest_data <- bind_rows(overall, by_sex, by_age) |>
    mutate(
      subgroup = factor(subgroup, levels = c("Overall", "Sex", "Age group")),
      level = factor(level, levels = rev(level))
    )

  brand <- brand()

  ggplot(forest_data) +
    aes(x = hr, y = level, xmin = lower, xmax = upper) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = brand[["colour"]][["secondary"]]) +
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

fig_vitals_profile <- function(advs, parameter = "Systolic Blood Pressure (mmHg)") {
  visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20", "Week 24")

  advs |>
    filter(PARAM == parameter, AVISIT %in% visits, !is.na(AVAL)) |>
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
      aes(ymin = .data[["mean"]] - .data[["se"]], ymax = .data[["mean"]] + .data[["se"]]),
      width = 0.15
    ) +
    scale_colour_manual(values = treatment_colours()) +
    labs(x = NULL, y = paste(parameter, "(mean and standard error)"), colour = NULL) +
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
