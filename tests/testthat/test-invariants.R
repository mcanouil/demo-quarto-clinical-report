# Structural invariants of the analysis datasets and of the TLF index.
# These are the properties every table in the report silently relies on.

test_that("population flags nest as the methods chapter states", {
  adsl <- read_adam("adsl")

  expect_true(all(adsl[["ITTFL"]] == "Y"))
  expect_true(all(adsl[["PPROTFL"]][adsl[["SAFFL"]] == "N"] == "N"))
  expect_true(all(adsl[["COMPFL"]][adsl[["PPROTFL"]] == "Y"] == "Y"))
  expect_true(all(adsl[["SAFFL"]][adsl[["PPROTFL"]] == "Y"] == "Y"))
})

test_that("a reason for discontinuation is present exactly for non-completers", {
  adsl <- read_adam("adsl")

  expect_equal(is.na(adsl[["DCSREAS"]]), adsl[["COMPFL"]] == "Y")
  expect_false(any(
    as.character(adsl[["DCSREAS"]]) == "Completed",
    na.rm = TRUE
  ))
})

test_that("no subject is lost between the source data and the analysis datasets", {
  adsl <- read_adam("adsl")

  expect_equal(nrow(adsl), nrow(source_adsl()))
  expect_equal(dplyr::n_distinct(adsl[["USUBJID"]]), nrow(adsl))

  for (name in c(
    "adae",
    "adex",
    "advs",
    "adlb",
    "adeg",
    "adcm",
    "admh",
    "adtte"
  )) {
    subjects <- unique(read_adam(name)[["USUBJID"]])
    expect_true(
      all(subjects %in% adsl[["USUBJID"]]),
      label = paste(name, "subjects are all in ADSL")
    )
  }
})

test_that("the frozen counts agree with the analysis datasets", {
  adsl <- read_adam("adsl")
  counts <- read_adam("counts")
  adtte <- read_adam("adtte")

  expect_equal(counts[["randomised"]], nrow(adsl))
  expect_equal(counts[["treated"]], sum(adsl[["SAFFL"]] == "Y"))
  expect_equal(counts[["completed"]], sum(adsl[["COMPFL"]] == "Y"))
  expect_gte(counts[["screened"]], counts[["randomised"]])
  expect_equal(
    dplyr::n_distinct(adtte[["USUBJID"]]),
    counts[["treated"]] - counts[["adtte_excluded"]]
  )
})

test_that("exposure categories account for every subject in the safety population", {
  adex <- read_adam("adex")
  adsl <- read_adam("adsl")

  categorised <- as.data.frame(tlf_exposure(adex)[["table_body"]])
  categorised <- categorised[
    categorised[["variable"]] == "Exposure category" &
      categorised[["row_type"]] == "level",
  ]
  counts <- vapply(
    grep("^stat_", names(categorised), value = TRUE),
    function(column) sum(as.integer(sub(" .*$", "", categorised[[column]]))),
    integer(1L)
  )

  safety <- adsl[adsl[["SAFFL"]] == "Y", ]
  expected <- as.integer(table(droplevels(safety[["TRT01A"]])))

  expect_equal(unname(counts), expected)
})

test_that("adverse event tables use the safety population as denominator", {
  adae <- read_adam("adae")
  adsl <- read_adam("adsl")

  denominator <- safety_denominator(adsl)
  expect_equal(nrow(denominator), sum(adsl[["SAFFL"]] == "Y"))

  header <- as.data.frame(tlf_ae_by_soc_pt(adae, adsl)[["table_styling"]][[
    "header"
  ]])
  header <- header[grepl("^stat_[0-9]+$", header[["column"]]), ]
  expected <- as.integer(table(denominator[["TRTA"]]))

  expect_equal(as.integer(header[["modify_stat_n"]]), expected)
  expect_equal(
    as.character(header[["modify_stat_level"]]),
    levels(denominator[["TRTA"]])
  )
})

test_that("a parameter with several baseline definitions must be disambiguated", {
  advs <- read_adam("advs")

  expect_error(
    tlf_shift(advs, "Systolic Blood Pressure (mmHg)"),
    "baseline definitions"
  )
  expect_error(
    parameter_records(
      advs,
      "Systolic Blood Pressure (mmHg)",
      basetype = "SUPINE"
    ),
    "not found for parameter"
  )
})

test_that("the shift tables count every subject with both indicators once", {
  advs <- read_adam("advs")
  parameter <- "Systolic Blood Pressure (mmHg)"

  eligible <- advs |>
    dplyr::filter(
      PARAM == parameter,
      BASETYPE == vitals_basetype,
      !is.na(BNRIND),
      !is.na(ANRIND),
      AVISIT != "Baseline"
    ) |>
    dplyr::distinct(USUBJID)

  body <- as.data.frame(
    tlf_shift(advs, parameter, basetype = vitals_basetype)[["table_body"]]
  )
  counted <- sum(
    vapply(
      grep("^stat_", names(body), value = TRUE),
      function(column) {
        sum(as.integer(sub(" .*$", "", body[[column]])), na.rm = TRUE)
      },
      integer(1L)
    )
  )

  expect_equal(counted, nrow(eligible))
})

test_that("the TLF index assigns each key, ref and number exactly once", {
  expect_false(any(duplicated(tlf_index[["key"]])))
  expect_false(any(duplicated(tlf_index[["ref"]])))
  expect_false(any(duplicated(tlf_index[c("kind", "number")])))
  expect_false(any(is.na(tlf_index[["number"]])))
  expect_false(any(is.na(tlf_index[["title"]])))
})

test_that("each cross-reference identifier carries the prefix for its kind", {
  expected <- c(Table = "tbl-", Figure = "fig-", Listing = "listing-")

  expect_true(all(
    startsWith(tlf_index[["ref"]], expected[tlf_index[["kind"]]])
  ))
})

section_sources <- function() {
  files <- list.files(
    file.path(project_root, "sections"),
    pattern = "[.]qmd$",
    full.names = TRUE
  )
  unlist(lapply(files, readLines, warn = FALSE))
}

test_that("every indexed output is rendered by the report", {
  text <- section_sources()

  unreferenced <- Filter(
    function(key) !any(grepl(paste0('"', key, '"'), text, fixed = TRUE)),
    tlf_index[["key"]]
  )

  expect_equal(unreferenced, character(0))
})

test_that("the index of outputs names every entry without spelling NA", {
  printed <- paste(capture.output(tlf_catalogue()), collapse = "\n")

  expect_false(grepl("NA", printed, fixed = TRUE))
  for (ref in tlf_index[["ref"]]) {
    expect_true(
      grepl(paste0("@", ref, "\n"), printed, fixed = TRUE),
      label = ref
    )
  }
})

test_that("every indexed output is wrapped in a cross-reference div", {
  text <- section_sources()

  # The shift-table panel emits its divs from R, one per parameter, so those
  # identifiers never appear literally in a section file.
  panelled <- startsWith(tlf_index[["key"]], "lab_shift_")
  refs <- tlf_index[["ref"]][!panelled]

  missing_div <- Filter(
    function(ref) !any(grepl(paste0("::: {#", ref, "}"), text, fixed = TRUE)),
    refs
  )

  expect_equal(missing_div, character(0))
})

test_that("each output number sits under the ICH E3 section it records", {
  expect_true(all(
    startsWith(tlf_index[["number"]], tlf_index[["section"]])
  ))
})

test_that("the numbers written for the filter match the index", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  tlf_write_numbers(path)
  written <- jsonlite::read_json(path, simplifyVector = TRUE)

  expect_equal(
    written[tlf_index[["ref"]]],
    stats::setNames(tlf_index[["number"]], tlf_index[["ref"]])
  )
})
