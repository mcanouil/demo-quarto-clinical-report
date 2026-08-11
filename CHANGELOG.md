# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

- feat: Quarto project rendering an abbreviated ICH E3 clinical study report for the fictional sponsor Nordvale Therapeutics.
- feat: `clinical` format extension contributing a Typst format, with cover page, document control and approval page, running headers and footers, draft watermark, landscape listings and horizontally centred tables and figures, and a matching HTML format with an SCSS theme.
- feat: Typst filter nesting the book appendices under the Appendices divider, so appendix chapters are lettered A and B one level below it.
- feat: brand identity in `_brand.yml`, with fonts bundled in the extension for reproducible rendering.
- feat: analysis layer built on `pharmaverseadam`, including a time-to-event dataset derived with `admiral` and tables built with `gtsummary`.
- feat: book and single-document profiles rendering the same section files.
- feat: Typst table of contents heading taken from the localised `quarto.language.toc-title-document` string, so it follows the document `lang`.
- ci: render workflow publishing the book to GitHub Pages and attaching the PDF.
- build: `DESCRIPTION` declaring the R dependencies, with `renv` configured for explicit snapshots.
- chore: licence, copyright and author headers on every source file, and a copy of the licence inside the extension.
