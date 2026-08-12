# Independent test: every headline number the report states in prose is
# recomputed here from the source packages, not from the derived datasets, so a
# mistake in R/00-adam.R shows up as a disagreement rather than being restated.

test_that("the disposition counts match the source data", {
  adsl <- read_adam("adsl")
  counts <- read_adam("counts")

  disposition <- pharmaversesdtm::ds |>
    dplyr::filter(DSCAT == "DISPOSITION EVENT")
  randomised <- source_adsl()

  expect_equal(
    counts[["screened"]],
    dplyr::n_distinct(disposition[["USUBJID"]])
  )
  expect_equal(counts[["randomised"]], nrow(randomised))
  expect_equal(
    counts[["screened"]] - counts[["randomised"]],
    sum(disposition[["DSDECOD"]] == "SCREEN FAILURE")
  )
  expect_equal(
    sum(adsl[["COMPFL"]] == "Y"),
    sum(randomised[["EOSSTT"]] == "COMPLETED")
  )
})

test_that("the reasons for discontinuation match the disposition domain", {
  adsl <- read_adam("adsl")

  expected <- pharmaversesdtm::ds |>
    dplyr::filter(
      DSCAT == "DISPOSITION EVENT",
      USUBJID %in% adsl[["USUBJID"]],
      DSDECOD != "COMPLETED"
    ) |>
    dplyr::count(DSDECOD) |>
    dplyr::mutate(DSDECOD = stringr::str_to_sentence(DSDECOD)) |>
    dplyr::arrange(DSDECOD)

  actual <- adsl |>
    dplyr::filter(!is.na(DCSREAS)) |>
    dplyr::count(DCSREAS) |>
    dplyr::mutate(DCSREAS = as.character(DCSREAS)) |>
    dplyr::arrange(DCSREAS)

  expect_equal(actual[["DCSREAS"]], expected[["DSDECOD"]])
  expect_equal(actual[["n"]], expected[["n"]])
  expect_equal(sum(actual[["n"]]), sum(adsl[["COMPFL"]] == "N"))
})

test_that("the adverse event counts quoted in the synopsis match the source data", {
  adsl <- read_adam("adsl")
  adae <- read_adam("adae")

  source_ae <- pharmaverseadam::adae |>
    dplyr::filter(
      USUBJID %in% adsl[["USUBJID"]],
      SAFFL == "Y",
      TRTEMFL == "Y"
    )

  expect_equal(
    dplyr::n_distinct(adae[["USUBJID"]]),
    dplyr::n_distinct(source_ae[["USUBJID"]])
  )
  expect_equal(
    dplyr::n_distinct(adae[["USUBJID"]][adae[["DERMFL"]] == "Y"]),
    dplyr::n_distinct(
      source_ae[["USUBJID"]][
        source_ae[["AEBODSYS"]] == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
      ]
    )
  )
  expect_equal(
    dplyr::n_distinct(adae[["USUBJID"]][adae[["AESER"]] == "Y"]),
    dplyr::n_distinct(source_ae[["USUBJID"]][source_ae[["AESER"]] == "Y"])
  )
})

test_that("the relationship flag matches the investigator assessment", {
  adae <- read_adam("adae")

  expect_equal(
    adae[["RELFL"]] == "Y",
    adae[["AEREL"]] %in% c("PROBABLE", "POSSIBLE", "RELATED")
  )
})

test_that("the time-to-event dataset agrees with a direct derivation", {
  adsl <- read_adam("adsl")
  adae <- read_adam("adae")
  adtte <- read_adam("adtte")

  first_event <- adae |>
    dplyr::filter(DERMFL == "Y", !is.na(ASTDT)) |>
    dplyr::slice_min(ASTDT, by = USUBJID, n = 1L, with_ties = FALSE) |>
    dplyr::select(USUBJID, event_date = ASTDT)

  # `derive_param_tte()` censors at the start date where the censoring date
  # precedes it, rather than dropping the subject, so the direct derivation
  # floors the end date the same way.
  expected <- adsl |>
    dplyr::filter(SAFFL == "Y") |>
    dplyr::select(USUBJID, TRTSDT, LSTALVDT) |>
    dplyr::left_join(first_event, by = "USUBJID") |>
    dplyr::mutate(
      CNSR = as.integer(is.na(event_date)),
      end_date = pmax(dplyr::coalesce(event_date, LSTALVDT), TRTSDT),
      AVAL = as.numeric(end_date - TRTSDT) + 1
    ) |>
    dplyr::filter(!is.na(AVAL), AVAL > 0) |>
    dplyr::arrange(USUBJID)

  actual <- dplyr::arrange(adtte, USUBJID)

  expect_equal(
    as.character(actual[["USUBJID"]]),
    as.character(expected[["USUBJID"]])
  )
  expect_equal(as.integer(actual[["CNSR"]]), expected[["CNSR"]])
  expect_equal(as.numeric(actual[["AVAL"]]), expected[["AVAL"]])
})

test_that("the subjects censored before their first dose are counted", {
  adsl <- read_adam("adsl")
  adtte <- read_adam("adtte")
  counts <- read_adam("counts")

  anomalous <- adsl |>
    dplyr::filter(SAFFL == "Y", !is.na(LSTALVDT), LSTALVDT < TRTSDT)

  expect_equal(
    counts[["censored_before_first_dose"]],
    sum(adtte[["USUBJID"]] %in% anomalous[["USUBJID"]] & adtte[["CNSR"]] == 1L)
  )
  expect_true(
    all(adtte[["AVAL"]][adtte[["USUBJID"]] %in% anomalous[["USUBJID"]]] == 1)
  )
})

test_that("the liver-injury screen agrees with a direct derivation", {
  adlb <- read_adam("adlb")

  direct <- adlb |>
    dplyr::filter(
      PARAMCD %in% c("ALT", "AST", "BILI"),
      AVISIT != "Baseline",
      !is.na(AVAL),
      !is.na(ANRHI),
      ANRHI > 0
    ) |>
    dplyr::summarise(
      alt = max(c(AVAL[PARAMCD == "ALT"] / ANRHI[PARAMCD == "ALT"], -Inf)),
      ast = max(c(AVAL[PARAMCD == "AST"] / ANRHI[PARAMCD == "AST"], -Inf)),
      bili = max(c(AVAL[PARAMCD == "BILI"] / ANRHI[PARAMCD == "BILI"], -Inf)),
      .by = USUBJID
    ) |>
    dplyr::mutate(
      alt_3x = alt >= 3,
      ast_3x = ast >= 3,
      bili_2x = bili >= 2
    ) |>
    dplyr::arrange(USUBJID)

  actual <- dplyr::arrange(hys_law_flags(adlb), USUBJID)

  expect_equal(actual[["USUBJID"]], direct[["USUBJID"]])
  expect_equal(actual[["alt_3x"]], direct[["alt_3x"]])
  expect_equal(actual[["ast_3x"]], direct[["ast_3x"]])
  expect_equal(actual[["bili_2x"]], direct[["bili_2x"]])
})
