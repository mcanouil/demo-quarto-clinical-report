# Clinical Study Report - ICH E3 numbers in the HTML cross-references
# Quarto writes the crossref index before extension filters run, and a book
# fills every cross-reference from that index after Pandoc, so the numbers
# `tlf-numbers.lua` applies reach the captions but not the references of the
# HTML book. This step rewrites those references in the rendered HTML.
#
# The single document profile resolves its references in Lua, where the filter
# already numbers them, so this step finds nothing to do there.
#
# @license MIT
# @copyright 2026 Mickaël Canouil
# @author Mickaël Canouil

numbers_path <- "data/tlf-numbers.json"

if (!file.exists(numbers_path)) {
  stop(
    "No output numbers at ",
    numbers_path,
    "; run `Rscript R/01-tlf-numbers.R` first.",
    call. = FALSE
  )
}

numbers <- unlist(jsonlite::read_json(numbers_path))

outputs <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")
files <- if (nzchar(outputs)) {
  strsplit(outputs, "\n", fixed = TRUE)[[1L]]
} else {
  character(0L)
}
files <- files[endsWith(files, ".html")]

#' Give every cross-reference to an indexed output its ICH E3 number.
#'
#' A reference is an anchor whose target is the output, holding the prefix and
#' then the number, which a book wraps in a span of its own. Only the number is
#' replaced, so the prefix and the markup around it stay as Quarto wrote them.
#'
#' @param html One rendered page.
#' @return The page, with the numbers replaced.
renumber_references <- function(html) {
  for (ref in names(numbers)) {
    pattern <- paste0(
      "(<a [^>]*href=\"[^\"]*#",
      ref,
      "\"[^>]*>[^<0-9]*(?:<span[^>]*>)?)",
      "([0-9]+(?:[.][0-9]+)*|[A-Z](?:[.][0-9]+)*)"
    )
    html <- gsub(pattern, paste0("\\1", numbers[[ref]]), html, perl = TRUE)
  }

  html
}

changed <- 0L
for (file in files) {
  html <- readLines(file, warn = FALSE)
  renumbered <- renumber_references(html)
  if (!identical(renumbered, html)) {
    writeLines(renumbered, file)
    changed <- changed + 1L
  }
}

message("Renumbered the cross-references of ", changed, " HTML page(s).")
