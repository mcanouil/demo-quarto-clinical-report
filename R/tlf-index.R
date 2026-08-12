# Clinical Study Report - Table, listing and figure index
# The single source of truth for output numbers, titles, subtitles and source
# notes. Nothing else in the project spells out an output number, so numbering
# stays consistent between the sections, the cross-references and the index
# table in the listings appendix.
#
# @license MIT
# @copyright 2026 Mickaël Canouil
# @author Mickaël Canouil
#
# Numbering follows ICH E3: Section 14 for in-text tables and figures, Section
# 16.2 for individual subject data listings.

#' One entry of the index.
#'
#' Named arguments rather than a tribble: the source notes are paragraphs, and
#' a tribble aligns its columns to the widest cell.
tlf_entry_spec <- function(
  key,
  ref,
  kind,
  number,
  section,
  title,
  subtitle = NA_character_,
  source_note = NA_character_
) {
  tibble::tibble(key, ref, kind, number, section, title, subtitle, source_note)
}

tlf_index <- dplyr::bind_rows(
  # 14.1 Demographic data and disposition
  tlf_entry_spec(
    key = "populations",
    ref = "tbl-populations",
    kind = "Table",
    number = "14.1.1",
    section = "14.1",
    title = "Analysis populations",
    subtitle = "Randomised population",
    source_note = "Percentages use the number of randomised subjects in each treatment group as denominator. Every randomised subject received at least one dose of study drug, so the randomised, intention-to-treat and safety populations contain the same subjects; they are counted here under planned treatment, whereas safety outputs count them under actual treatment."
  ),
  tlf_entry_spec(
    key = "disposition",
    ref = "tbl-disposition",
    kind = "Table",
    number = "14.1.2",
    section = "14.1",
    title = "Subject disposition",
    subtitle = "Randomised population",
    source_note = "Percentages use the number of randomised subjects in each treatment group as denominator, except for the reasons for discontinuation, which use the number of subjects who discontinued in that group and therefore sum to 100%. Reasons come from the disposition domain; the pilot ADSL does not carry them."
  ),
  tlf_entry_spec(
    key = "demographics",
    ref = "tbl-demographics",
    kind = "Table",
    number = "14.1.3",
    section = "14.1",
    title = "Demographic and baseline characteristics",
    subtitle = "Randomised population",
    source_note = "Continuous variables are summarised as mean (standard deviation); median [minimum, maximum]."
  ),
  tlf_entry_spec(
    key = "medical_history",
    ref = "tbl-medical-history",
    kind = "Table",
    number = "14.1.4",
    section = "14.1",
    title = "Medical history by body system and preferred term",
    subtitle = "Conditions reported by at least 5% of subjects in any treatment group, randomised population",
    source_note = "Subjects reporting more than one condition within a body system or preferred term are counted once. Percentages use the number of randomised subjects in each treatment group as denominator. The primary diagnosis of Alzheimer's disease is recorded for every subject and is excluded, since it is the indication rather than a medical history finding. The overall row and the rows for each class count every subject with a qualifying record, including subjects whose only terms fall below the reporting threshold and are therefore not shown."
  ),
  tlf_entry_spec(
    key = "patient_flow",
    ref = "fig-patient-flow",
    kind = "Figure",
    number = "14.1.1",
    section = "14.1",
    title = "Disposition of subjects from screening to study completion [@consort2010]"
  ),
  # 14.2 Efficacy data
  tlf_entry_spec(
    key = "tte",
    ref = "tbl-tte",
    kind = "Table",
    number = "14.2.1",
    section = "14.2",
    title = "Time to first dermatologic treatment-emergent adverse event",
    subtitle = "Intention-to-treat population",
    source_note = "Medians and confidence intervals are Kaplan-Meier estimates. Hazard ratios come from a Cox proportional hazards model with planned treatment as the only covariate and placebo as the reference group. A median is reported as not reached where fewer than half the subjects in the group had an event."
  ),
  tlf_entry_spec(
    key = "tte_pp",
    ref = "tbl-tte-pp",
    kind = "Table",
    number = "14.2.1.1",
    section = "14.2",
    title = "Time to first dermatologic treatment-emergent adverse event (supportive analysis)",
    subtitle = "Per-protocol population",
    source_note = "Medians and confidence intervals are Kaplan-Meier estimates. Hazard ratios come from a Cox proportional hazards model with planned treatment as the only covariate and placebo as the reference group. The per-protocol population is restricted to safety-population subjects who completed the study."
  ),
  tlf_entry_spec(
    key = "ancova",
    ref = "tbl-ancova",
    kind = "Table",
    number = "14.2.2",
    section = "14.2",
    title = "Change from baseline in supine systolic blood pressure at week 24",
    subtitle = "Safety population",
    source_note = "Least-squares means from an analysis of covariance model with actual treatment group as a factor and the baseline value as a covariate. `n` is the number of subjects with both a baseline and a week 24 supine value; no values are imputed. Vital signs are measured in three postures, each with its own baseline; the model uses the supine measurement, so each subject contributes one observation."
  ),
  tlf_entry_spec(
    key = "km",
    ref = "fig-km",
    kind = "Figure",
    number = "14.2.1",
    section = "14.2",
    title = "Cumulative incidence of the first dermatologic treatment-emergent adverse event, intention-to-treat population"
  ),
  tlf_entry_spec(
    key = "forest",
    ref = "fig-forest",
    kind = "Figure",
    number = "14.2.2",
    section = "14.2",
    title = "Hazard ratio for the primary endpoint by age group and sex, intention-to-treat population"
  ),
  tlf_entry_spec(
    key = "vitals_profile",
    ref = "fig-vitals-profile",
    kind = "Figure",
    number = "14.2.3",
    section = "14.2",
    title = "Mean supine systolic blood pressure by visit and treatment group, safety population"
  ),
  # 14.3 Safety data
  tlf_entry_spec(
    key = "exposure",
    ref = "tbl-exposure",
    kind = "Table",
    number = "14.3.1",
    section = "14.3",
    title = "Extent of exposure",
    subtitle = "Safety population",
    source_note = "Exposure is the total treatment duration in days, derived from the exposure analysis dataset. Category percentages use the number of safety-population subjects in each treatment group as denominator, so they sum to 100%."
  ),
  tlf_entry_spec(
    key = "conmeds",
    ref = "tbl-conmeds",
    kind = "Table",
    number = "14.3.2",
    section = "14.3",
    title = "Prior and concomitant medications by drug class and preferred term",
    subtitle = "Medications taken by at least 5% of subjects in any treatment group, safety population",
    source_note = "Subjects taking more than one medication within a drug class or preferred term are counted once. Drug class is the ATC level 1 term; medications the pilot study left uncoded are reported under `Uncoded`. The overall row and the rows for each class count every subject with a qualifying record, including subjects whose only terms fall below the reporting threshold and are therefore not shown."
  ),
  tlf_entry_spec(
    key = "ae_overview",
    ref = "tbl-ae-overview",
    kind = "Table",
    number = "14.3.3",
    section = "14.3",
    title = "Overview of treatment-emergent adverse events",
    subtitle = "Safety population",
    source_note = "Subjects are counted once in each row. A treatment-emergent adverse event started on or after the first dose of study drug."
  ),
  tlf_entry_spec(
    key = "ae_soc_pt",
    ref = "tbl-ae-soc-pt",
    kind = "Table",
    number = "14.3.4",
    section = "14.3",
    title = "Treatment-emergent adverse events by system organ class and preferred term",
    subtitle = "Events reported by at least 5% of subjects in any treatment group, safety population",
    source_note = "Subjects reporting more than one event within a system organ class or preferred term are counted once. Percentages use the number of safety-population subjects in each treatment group as denominator. The overall row and the rows for each class count every subject with a qualifying record, including subjects whose only terms fall below the reporting threshold and are therefore not shown."
  ),
  tlf_entry_spec(
    key = "ae_severity",
    ref = "tbl-ae-severity",
    kind = "Table",
    number = "14.3.5",
    section = "14.3",
    title = "Treatment-emergent adverse events by maximum severity",
    subtitle = "Events reported by at least 5% of subjects in any treatment group, safety population",
    source_note = "Subjects reporting more than one event within a preferred term are counted once, under the highest severity reported for that term. Percentages use the number of safety-population subjects in each treatment group as denominator. The overall row and the rows for each class count every subject with a qualifying record, including subjects whose only terms fall below the reporting threshold and are therefore not shown."
  ),
  tlf_entry_spec(
    key = "ae_relationship",
    ref = "tbl-ae-relationship",
    kind = "Table",
    number = "14.3.6",
    section = "14.3",
    title = "Treatment-emergent adverse events by relationship to study drug",
    subtitle = "Events reported by at least 5% of subjects in any treatment group, safety population",
    source_note = "Subjects reporting more than one event within a preferred term are counted once, as related if any event for that term was investigator-assessed as related. Percentages use the number of safety-population subjects in each treatment group as denominator. The overall row and the rows for each class count every subject with a qualifying record, including subjects whose only terms fall below the reporting threshold and are therefore not shown."
  ),
  tlf_entry_spec(
    key = "ae_serious",
    ref = "tbl-ae-serious",
    kind = "Table",
    number = "14.3.7",
    section = "14.3",
    title = "Serious treatment-emergent adverse events",
    subtitle = "Safety population",
    source_note = "Seriousness was assessed by the investigator according to the protocol definition."
  ),
  tlf_entry_spec(
    key = "lab_shift_alt",
    ref = "tbl-lab-shift-alt",
    kind = "Table",
    number = "14.3.8.1",
    section = "14.3",
    title = "Shift from baseline to worst post-baseline category",
    subtitle = "Alanine aminotransferase, safety population",
    source_note = "Categories are relative to the reference range of the reporting laboratory. The worst post-baseline category is high if any post-baseline value was high, otherwise low if any value was low. Percentages use the number of subjects in each baseline category and treatment group as denominator, so they sum to 100% within each baseline category."
  ),
  tlf_entry_spec(
    key = "lab_shift_ast",
    ref = "tbl-lab-shift-ast",
    kind = "Table",
    number = "14.3.8.2",
    section = "14.3",
    title = "Shift from baseline to worst post-baseline category",
    subtitle = "Aspartate aminotransferase, safety population",
    source_note = "Categories are relative to the reference range of the reporting laboratory. The worst post-baseline category is high if any post-baseline value was high, otherwise low if any value was low. Percentages use the number of subjects in each baseline category and treatment group as denominator, so they sum to 100% within each baseline category."
  ),
  tlf_entry_spec(
    key = "lab_shift_bili",
    ref = "tbl-lab-shift-bili",
    kind = "Table",
    number = "14.3.8.3",
    section = "14.3",
    title = "Shift from baseline to worst post-baseline category",
    subtitle = "Bilirubin, safety population",
    source_note = "Categories are relative to the reference range of the reporting laboratory. The worst post-baseline category is high if any post-baseline value was high, otherwise low if any value was low. Percentages use the number of subjects in each baseline category and treatment group as denominator, so they sum to 100% within each baseline category."
  ),
  tlf_entry_spec(
    key = "lab_shift_creat",
    ref = "tbl-lab-shift-creat",
    kind = "Table",
    number = "14.3.8.4",
    section = "14.3",
    title = "Shift from baseline to worst post-baseline category",
    subtitle = "Creatinine, safety population",
    source_note = "Categories are relative to the reference range of the reporting laboratory. The worst post-baseline category is high if any post-baseline value was high, otherwise low if any value was low. Percentages use the number of subjects in each baseline category and treatment group as denominator, so they sum to 100% within each baseline category."
  ),
  tlf_entry_spec(
    key = "hys_law",
    ref = "tbl-hys-law",
    kind = "Table",
    number = "14.3.9",
    section = "14.3",
    title = "Potential drug-induced liver injury",
    subtitle = "Safety population",
    source_note = "Counts are of subjects with at least one post-baseline value meeting each criterion, relative to the upper limit of the reference range of the reporting laboratory. The combined criterion requires an aminotransferase elevation and a bilirubin elevation in the same subject, not necessarily on the same day, and is a screen for potential drug-induced liver injury rather than a diagnosis."
  ),
  tlf_entry_spec(
    key = "vitals_shift",
    ref = "tbl-vitals-shift",
    kind = "Table",
    number = "14.3.10",
    section = "14.3",
    title = "Shift from baseline to worst post-baseline category",
    subtitle = "Systolic blood pressure measured supine, safety population",
    source_note = "Categories are relative to the reference range of the reporting site. The worst post-baseline category is high (hypertensive) if any post-baseline value was high, otherwise low (hypotensive) if any value was low. Percentages use the number of subjects in each baseline category and treatment group as denominator, so they sum to 100% within each baseline category. Vital signs are measured in three postures, each with its own baseline; this table uses the supine measurement, so a subject is counted once."
  ),
  tlf_entry_spec(
    key = "ecg_shift",
    ref = "tbl-ecg-shift",
    kind = "Table",
    number = "14.3.11",
    section = "14.3",
    title = "Shift from baseline to worst post-baseline category",
    subtitle = "QTcF interval, safety population",
    source_note = "Categories are relative to the reference range of the reporting site. The worst post-baseline category is high if any post-baseline value was high, otherwise low if any value was low. Percentages use the number of subjects in each baseline category and treatment group as denominator, so they sum to 100% within each baseline category."
  ),
  # 16.2 Individual subject data listings
  tlf_entry_spec(
    key = "listing_discontinuation",
    ref = "lst-discontinuation",
    kind = "Listing",
    number = "16.2.1.1",
    section = "16.2.1",
    title = "Subjects who discontinued the study",
    subtitle = "Randomised population",
    source_note = "One row per subject. Reasons come from the disposition domain."
  ),
  tlf_entry_spec(
    key = "listing_deviation",
    ref = "lst-deviation",
    kind = "Listing",
    number = "16.2.2.1",
    section = "16.2.2",
    title = "Subjects who discontinued for a protocol violation",
    subtitle = "Randomised population",
    source_note = "The pilot study ships no protocol deviation dataset, so this listing covers only violations severe enough to end study participation. Deviations that did not lead to discontinuation cannot be listed."
  ),
  tlf_entry_spec(
    key = "listing_demographics",
    ref = "lst-demographics",
    kind = "Listing",
    number = "16.2.4.1",
    section = "16.2.4",
    title = "Demographic data",
    subtitle = "Randomised population",
    source_note = "One row per subject, in treatment group and subject identifier order."
  ),
  tlf_entry_spec(
    key = "listing_sae",
    ref = "lst-sae",
    kind = "Listing",
    number = "16.2.7.1",
    section = "16.2.7",
    title = "Serious treatment-emergent adverse events",
    subtitle = "Safety population",
    source_note = "Onset day is relative to the first dose of study drug. One row per event."
  ),
  tlf_entry_spec(
    key = "listing_lab_abnormal",
    ref = "lst-lab-abnormal",
    kind = "Listing",
    number = "16.2.8.1",
    section = "16.2.8",
    title = "Abnormal laboratory values meeting a liver-injury criterion",
    subtitle = "Safety population",
    source_note = "One row per subject per analyte, showing the worst post-baseline value and its multiple of the upper limit of the reference range. Restricted to the analytes screened in the potential drug-induced liver injury table."
  )
)

#' Look up one output in the index.
tlf_entry <- function(key) {
  entry <- tlf_index[tlf_index[["key"]] == key, ]
  if (nrow(entry) != 1L) {
    stop(
      "No entry for output '",
      key,
      "' in R/tlf-index.R.",
      call. = FALSE
    )
  }
  entry
}

#' The caption of an output, for the cross-reference div that wraps it.
#'
#' The div supplies the number, so the caption carries none of its own. The
#' population follows the title, because `gt` cannot show a subtitle without a
#' title and the title now lives in the caption.
tlf_caption <- function(key) {
  entry <- tlf_entry(key)
  parts <- c(entry[["title"]], entry[["subtitle"]])
  paste0(paste(parts[!is.na(parts)], collapse = ". "), ".")
}

#' The cross-reference identifier of an output, without the leading `@`.
tlf_ref <- function(key) {
  tlf_entry(key)[["ref"]]
}

#' Render an output with its registered source note.
#'
#' Accepts a `gtsummary` table or a data frame. Title and population are
#' deliberately absent: they belong to the caption of the cross-reference div
#' that wraps the output, which is what numbers it.
tlf_output <- function(x, key, source_note = NULL) {
  entry <- tlf_entry(key)
  source_note <- if (is.null(source_note)) {
    entry[["source_note"]]
  } else {
    source_note
  }

  tbl_nordvale(x, source_note = if (is.na(source_note)) NULL else source_note)
}

#' The index of every output, as a Markdown definition list.
#'
#' Each term is a cross-reference, so Quarto resolves it to the number it gave
#' that output, and the description carries the ICH E3 output number and
#' section that number maps to. Print from a chunk with `#| output: asis`.
tlf_catalogue <- function() {
  # An output with no population, such as a figure, ends its entry at the
  # section; paste0() would otherwise spell the missing population as "NA".
  population <- ifelse(
    is.na(tlf_index[["subtitle"]]),
    "",
    paste0(" ", tlf_index[["subtitle"]], ".")
  )

  entries <- paste0(
    "@",
    tlf_index[["ref"]],
    "\n\n",
    ":   ",
    tlf_index[["title"]],
    ".\n",
    "    ICH E3 output ",
    tlf_index[["kind"]],
    " ",
    tlf_index[["number"]],
    ", section ",
    tlf_index[["section"]],
    ".",
    population
  )

  cat(entries, sep = "\n\n")
  cat("\n")
}
