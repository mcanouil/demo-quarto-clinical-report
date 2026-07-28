# Clinical Study Report - Brand helpers
# Reads the resolved brand and applies it to fonts, ggplot2 themes, gt tables
# and gtsummary output.
#
# @license MIT
# @copyright 2026 Mickaël Canouil
# @author Mickaël Canouil

first_non_empty <- function(...) {
  for (value in list(...)) {
    if (is.null(value) || length(value) == 0L) {
      next
    }
    if (is.character(value) && !nzchar(value[[1L]])) {
      next
    }
    return(value)
  }
  NULL
}

brand_file <- function() {
  candidates <- c("_brand.yml", "../_brand.yml", "../../_brand.yml")
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) {
    stop("Cannot locate '_brand.yml' from ", getwd(), ".", call. = FALSE)
  }
  found[[1L]]
}

brand_raw <- function() {
  execute_info <- Sys.getenv("QUARTO_EXECUTE_INFO", unset = "")
  if (nzchar(execute_info)) {
    info <- jsonlite::fromJSON(execute_info)
    brand <- info[["format"]][["render"]][["brand"]]
    if (!is.null(brand) && length(brand) > 0L) {
      mode <- if ("light" %in% names(brand)) "light" else names(brand)[[1L]]
      return(brand[[mode]][["data"]])
    }
  }
  yaml::read_yaml(brand_file())
}

resolve_colour <- function(value, palette) {
  value <- first_non_empty(value, NULL)
  if (is.null(value)) {
    return(NULL)
  }
  value <- as.character(value)[[1L]]
  if (!is.null(palette[[value]])) {
    return(as.character(palette[[value]]))
  }
  value
}

resolve_family <- function(entry) {
  if (is.null(entry)) {
    return(NULL)
  }
  if (is.list(entry)) {
    return(first_non_empty(entry[["family"]], NULL))
  }
  as.character(entry)[[1L]]
}

#' Resolved brand colours and typography.
#'
#' Reads the brand resolved by Quarto when rendering, and falls back to
#' `_brand.yml` so the same helpers work in a plain R session.
brand <- function() {
  data <- brand_raw()
  colour <- data[["color"]]
  palette <- colour[["palette"]]
  typography <- data[["typography"]]

  named <- c(
    "foreground",
    "background",
    "primary",
    "secondary",
    "tertiary",
    "success",
    "info",
    "warning",
    "danger"
  )
  colours <- lapply(named, function(name) resolve_colour(colour[[name]], palette))
  names(colours) <- named

  list(
    colour = colours,
    palette = unname(unlist(palette, use.names = FALSE)),
    ink = first_non_empty(colours[["foreground"]], "#16202A"),
    paper = first_non_empty(colours[["background"]], "#FDFDFC"),
    base_family = first_non_empty(resolve_family(typography[["base"]]), ""),
    heading_family = first_non_empty(
      resolve_family(typography[["headings"]]),
      ""
    ),
    mono_family = first_non_empty(resolve_family(typography[["monospace"]]), "")
  )
}

#' File paths of every font bundled with the brand.
brand_font_files <- function() {
  data <- brand_raw()
  fonts <- data[["typography"]][["fonts"]]
  if (is.null(fonts) || length(fonts) == 0L) {
    return(character(0L))
  }
  if (is.data.frame(fonts)) {
    fonts <- split(fonts, seq_len(nrow(fonts)))
  }

  root <- dirname(brand_file())
  paths <- character(0L)
  for (font in fonts) {
    source <- as.character(first_non_empty(font[["source"]], "system"))[[1L]]
    if (!identical(source, "file")) {
      next
    }
    files <- font[["files"]]
    if (is.data.frame(files)) {
      files <- split(files, seq_len(nrow(files)))
    }
    for (file in files) {
      path <- as.character(first_non_empty(file[["path"]], ""))[[1L]]
      if (!nzchar(path)) {
        next
      }
      if (!file.exists(path)) {
        path <- file.path(root, path)
      }
      if (!file.exists(path)) {
        warning("Font file declared in the brand not found: ", path, call. = FALSE)
        next
      }
      paths <- c(paths, normalizePath(path))
    }
  }

  paths
}

#' Make the bundled brand fonts available to the graphics devices.
#'
#' Call once, before any figure is drawn. Bundled files take precedence over
#' any same-named font installed on the machine, so figures are reproducible.
configure_brand_fonts <- function() {
  paths <- brand_font_files()
  if (length(paths) == 0L) {
    return(invisible(NULL))
  }

  systemfonts::add_fonts(paths)

  dev <- knitr::opts_chunk$get("dev")
  if (is.null(dev) || !any(dev %in% c("svglite", "svg"))) {
    return(invisible(NULL))
  }

  families <- unique(c(brand()[["base_family"]], brand()[["heading_family"]], brand()[["mono_family"]]))
  families <- families[nzchar(families)]
  imports <- vapply(
    families,
    function(family) {
      systemfonts::fonts_as_import(family = family, type = "import", may_embed = TRUE)
    },
    character(1L)
  )
  dev_args <- first_non_empty(knitr::opts_chunk$get("dev.args"), list())
  dev_args[["web_fonts"]] <- unname(imports)
  knitr::opts_chunk$set(dev.args = dev_args)

  invisible(NULL)
}

#' Sponsor ggplot2 theme.
theme_nordvale <- function(base_size = 9) {
  brand <- brand()
  accent <- scales::col_mix(a = brand[["ink"]], b = brand[["paper"]], amount = 0.25)

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = brand[["base_family"]],
    header_family = brand[["heading_family"]],
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22,
    ink = brand[["ink"]],
    paper = brand[["paper"]],
    accent = accent
  ) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = accent),
      axis.line.y = ggplot2::element_line(colour = accent),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_text(colour = accent),
      plot.title = ggplot2::element_text(colour = accent, size = ggplot2::rel(1.4)),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(colour = brand[["ink"]]),
      plot.caption = ggplot2::element_text(colour = accent, hjust = 0)
    )
}

#' Treatment arm colours, in protocol order.
treatment_colours <- function() {
  brand <- brand()
  c(
    "Placebo" = brand[["colour"]][["secondary"]],
    "Xanomeline Low Dose" = brand[["colour"]][["primary"]],
    "Xanomeline High Dose" = brand[["colour"]][["warning"]]
  )
}

#' Apply the sponsor style to a gt table.
#'
#' Styling stays within the subset Quarto can translate to Typst: fonts,
#' colours, borders and alignment.
gt_style_nordvale <- function(gt_tbl) {
  brand <- brand()
  accent <- scales::col_mix(a = brand[["ink"]], b = brand[["paper"]], amount = 0.25)

  gt_tbl |>
    gt::tab_options(
      table.font.names = brand[["base_family"]],
      table.font.size = gt::px(12),
      table.background.color = brand[["paper"]],
      table.font.color = brand[["ink"]],
      heading.title.font.size = gt::px(13),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(11),
      heading.border.bottom.color = brand[["colour"]][["primary"]],
      column_labels.background.color = brand[["colour"]][["primary"]],
      column_labels.font.weight = "bold",
      column_labels.border.top.color = brand[["colour"]][["primary"]],
      column_labels.border.bottom.color = brand[["colour"]][["primary"]],
      row_group.background.color = brand[["colour"]][["tertiary"]],
      row_group.font.weight = "bold",
      table_body.border.top.color = accent,
      table_body.border.bottom.color = accent,
      table_body.hlines.color = brand[["colour"]][["tertiary"]],
      source_notes.font.size = gt::px(10),
      footnotes.font.size = gt::px(10),
      data_row.padding = gt::px(3)
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        font = brand[["heading_family"]],
        color = brand[["paper"]]
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_text(font = brand[["heading_family"]]),
      locations = gt::cells_title()
    )
}

#' Build a sponsor-styled gt table from a data frame.
gt_nordvale <- function(data, ...) {
  gt_style_nordvale(gt::gt(data, ...))
}

#' Render a gtsummary table as a sponsor-styled gt table.
#'
#' `title` and `subtitle` carry the ICH E3 output number and population.
tbl_nordvale <- function(tbl, title = NULL, subtitle = NULL, source_note = NULL) {
  gt_tbl <- gtsummary::as_gt(tbl)
  if (!is.null(title)) {
    gt_tbl <- gt::tab_header(
      gt_tbl,
      title = gt::md(title),
      subtitle = if (is.null(subtitle)) NULL else gt::md(subtitle)
    )
  }
  if (!is.null(source_note)) {
    gt_tbl <- gt::tab_source_note(gt_tbl, gt::md(source_note))
  }
  gt_style_nordvale(gt_tbl)
}

#' Compact gtsummary defaults shared by every table in the report.
configure_gtsummary <- function() {
  gtsummary::set_gtsummary_theme(gtsummary::theme_gtsummary_compact(set_theme = FALSE))
  gtsummary::theme_gtsummary_language("en", big.mark = ",", decimal.mark = ".")
  invisible(NULL)
}
