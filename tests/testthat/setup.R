# Clinical Study Report - Test setup
#
# The tests read the frozen analysis datasets in data/, so they check the same
# numbers the report renders. Run `Rscript R/00-adam.R` first.

project_root <- normalizePath(file.path("..", ".."))

source(file.path(project_root, "R", "brand.R"))
source(file.path(project_root, "R", "tlf-index.R"))
source(file.path(project_root, "R", "tlf.R"))

# The same gtsummary defaults the report renders with, so the tests parse the
# same formatted strings.
configure_gtsummary()

data_dir <- file.path(project_root, "data")

#' Read a frozen analysis dataset, or skip the test if the data are not built.
read_adam <- function(name) {
  path <- file.path(data_dir, paste0(name, ".rds"))
  testthat::skip_if_not(
    file.exists(path),
    paste0(
      "Analysis data not built; run `Rscript R/00-adam.R` first (",
      name,
      ")."
    )
  )
  readRDS(path)
}

treatment_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

#' The pilot source data, restricted the way R/00-adam.R restricts it.
#'
#' Read straight from the source packages so that the independent tests do not
#' rely on the derivation they are checking.
source_adsl <- function() {
  dplyr::filter(pharmaverseadam::adsl, ARM %in% treatment_levels)
}
