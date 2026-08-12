# Double programming: the inferential results are recomputed by a second route
# and compared with what the report prints. The point is to catch a wrong
# estimate, a wrong reference group or a transposed confidence interval, which
# no structural check can see.

parse_estimate <- function(x) {
  as.numeric(sub("^([-0-9.]+).*$", "\\1", x))
}

parse_interval <- function(x) {
  inner <- sub("^[^(]*[(]([^)]*)[)].*$", "\\1", x)
  vapply(
    strsplit(inner, ",\\s*"),
    function(pair) as.numeric(pair[[1L]]),
    numeric(1L)
  )
}

parse_interval_upper <- function(x) {
  inner <- sub("^[^(]*[(]([^)]*)[)].*$", "\\1", x)
  vapply(
    strsplit(inner, ",\\s*"),
    function(pair) as.numeric(pair[[2L]]),
    numeric(1L)
  )
}

test_that("the hazard ratios match a directly fitted Cox model", {
  adtte <- read_adam("adtte")

  reported <- tlf_tte_summary(adtte)
  fit <- survival::coxph(
    survival::Surv(AVAL, 1 - CNSR) ~ TRT01P,
    data = adtte
  )
  expected <- summary(fit)$conf.int

  # The first row is the reference group and carries no estimate.
  hazard <- reported[["Hazard ratio (95% CI)"]]
  expect_equal(hazard[[1L]], "Reference")

  expect_equal(
    parse_estimate(hazard[-1L]),
    unname(round(expected[, "exp(coef)"], 2))
  )
  expect_equal(
    parse_interval(hazard[-1L]),
    unname(round(expected[, "lower .95"], 2))
  )
  expect_equal(
    parse_interval_upper(hazard[-1L]),
    unname(round(expected[, "upper .95"], 2))
  )
})

test_that("the event counts and Kaplan-Meier medians match a direct fit", {
  adtte <- read_adam("adtte")

  reported <- tlf_tte_summary(adtte)
  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ TRT01P,
    data = adtte
  )
  expected <- summary(fit)$table

  expect_equal(
    reported[["Subjects"]],
    unname(as.integer(expected[, "records"]))
  )
  expect_equal(
    parse_estimate(reported[["Subjects with an event"]]),
    unname(as.numeric(expected[, "events"]))
  )

  # A group in which fewer than half the subjects had an event has no estimable
  # median and is reported as not reached.
  medians <- unname(as.numeric(expected[, "median"]))
  reported_medians <- reported[["Median time to event (days)"]]
  expect_equal(
    reported_medians[is.na(medians)],
    rep("Not reached", sum(is.na(medians)))
  )
  expect_equal(
    parse_estimate(reported_medians[!is.na(medians)]),
    round(medians[!is.na(medians)])
  )
})

test_that("the ANCOVA differences match the treatment coefficients", {
  advs <- read_adam("advs")

  reported <- tlf_vitals_ancova(advs)
  analysis <- advs |>
    parameter_records(
      "Systolic Blood Pressure (mmHg)",
      basetype = vitals_basetype
    ) |>
    dplyr::filter(AVISIT == "Week 24", !is.na(CHG), !is.na(BASE))

  # With treatment as a factor and no interaction, the least-squares mean
  # difference from the reference group is exactly the treatment coefficient, so
  # the linear model gives an independent check on the emmeans contrast.
  model <- stats::lm(CHG ~ TRTA + BASE, data = analysis)
  coefficients <- stats::coef(model)
  intervals <- stats::confint(model)
  terms <- grep("^TRTA", names(coefficients), value = TRUE)

  differences <- reported[["Difference versus placebo (95% CI)"]]
  expect_equal(differences[[1L]], "Reference")

  expect_equal(
    parse_estimate(differences[-1L]),
    unname(round(coefficients[terms], 2))
  )
  expect_equal(
    parse_interval(differences[-1L]),
    unname(round(intervals[terms, 1L], 2))
  )
  expect_equal(
    parse_interval_upper(differences[-1L]),
    unname(round(intervals[terms, 2L], 2))
  )
  expect_equal(
    reported[["n"]],
    unname(as.integer(table(droplevels(analysis[["TRTA"]]))))
  )
})

test_that("the overall row of a threshold table counts every subject", {
  adsl <- read_adam("adsl")
  adae <- read_adam("adae")
  adcm <- read_adam("adcm")
  admh <- read_adam("admh")

  overall <- function(tbl, pattern) {
    body <- as.data.frame(tbl[["table_body"]])
    row <- body[grepl(pattern, body[["label"]]), ][1L, ]
    columns <- grep("^stat_[0-9]+$", names(body), value = TRUE)
    as.integer(sub(" .*$", "", unlist(row[columns])))
  }

  direct <- function(records, population, group) {
    subjects <- unique(records[["USUBJID"]])
    as.integer(table(
      population[[group]][population[["USUBJID"]] %in% subjects]
    ))
  }

  safety <- safety_denominator(adsl)

  # Restricting the events to the reported terms before building the table would
  # make each of these rows count only the subjects whose term cleared the
  # threshold, so it would disagree with the overview table.
  expect_equal(
    overall(tlf_ae_by_soc_pt(adae, adsl), "^Any treatment-emergent"),
    direct(adae, safety, "TRTA")
  )
  expect_equal(
    overall(tlf_ae_by_soc_pt(adae, adsl), "^Any treatment-emergent"),
    overall(tlf_ae_overview(adae, adsl), "^Any treatment-emergent")[1:3]
  )
  expect_equal(
    overall(tlf_conmeds(adcm, adsl), "^Any prior"),
    direct(adcm, safety, "TRTA")
  )
  expect_equal(
    overall(tlf_medical_history(admh, adsl), "^Any medical history"),
    direct(admh, adsl, "TRT01P")
  )
})

test_that("the adverse event overview counts match direct subject counts", {
  adae <- read_adam("adae")
  adsl <- read_adam("adsl")

  reported <- as.data.frame(tlf_ae_overview(adae, adsl)[["table_body"]])
  reported <- reported[reported[["row_type"]] == "label", ]

  safety <- safety_denominator(adsl)
  direct <- function(condition) {
    subjects <- unique(adae[["USUBJID"]][condition])
    as.integer(table(safety[["TRTA"]][safety[["USUBJID"]] %in% subjects]))
  }

  expected <- list(
    "Any treatment-emergent adverse event (TEAE)" = direct(rep(
      TRUE,
      nrow(adae)
    )),
    "Any serious TEAE" = direct(adae[["AESER"]] == "Y"),
    "Any drug-related TEAE" = direct(adae[["RELFL"]] == "Y"),
    "Any dermatologic TEAE" = direct(adae[["DERMFL"]] == "Y")
  )

  for (label in names(expected)) {
    row <- reported[reported[["label"]] == label, ]
    counts <- as.integer(sub(
      " .*$",
      "",
      unlist(row[c("stat_1", "stat_2", "stat_3")])
    ))
    expect_equal(counts, expected[[label]], label = label)
  }
})
