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
- feat: every table, listing and figure wrapped in a Quarto cross-reference div, so Quarto numbers each output and every reference to it resolves.
- feat: `R/tlf-index.R` holding the caption, population and source note of every output, together with the ICH E3 output number and section it stands for, and an index of outputs generated from it.
- feat: every output is numbered with its ICH E3 number, in its caption and in every reference to it, in the Typst PDF and in the HTML site. Quarto builds a float number from the chapter and a running count, so `tlf-numbers.lua` applies the numbers `R/tlf-index.R` holds instead.
- feat: analysis population table, reasons for discontinuation from the SDTM disposition domain, medical history, prior and concomitant medications, a potential drug-induced liver injury screen, an electrocardiogram shift table, and listings for protocol violations, demographic data and abnormal laboratory values.
- feat: adverse event narrative structure under serious adverse events, with one worked placeholder.
- fix: the Typst book numbers tables, figures and listings by chapter, so the index of outputs no longer lists several outputs as Table 1 or Figure 1. Quarto restarts the float counters at each chapter of a single-file Typst book without prefixing the chapter, which left one Table 1 per chapter in the PDF while the HTML book numbered them 5.1, 6.1 and so on.
- fix: tables and listings taller than a page break across pages with their header row repeated, instead of overrunning the page and printing rows on top of one another, and a section heading stays with the content that follows it. Data listings are centred like the tables, where Quarto left-aligns listing floats because listings usually hold code.
- fix: the HTML book gives its captions the ICH E3 number in every chapter, where a chapter in a subdirectory kept the number Quarto gives it. An HTML book renders each chapter from the directory of that chapter, so the filter now reads the numbers from the project rather than from the working directory.
- fix: the HTML book gives its cross-references the ICH E3 number, as its captions and the PDF already did. Quarto writes the crossref index before an extension filter runs, and a book fills every reference from that index after Pandoc, so the numbers are applied to the rendered pages instead.
- feat: subject data listings have their own cross-reference category, `listing`, instead of borrowing the type Quarto means for code listings. The identifiers change from `lst-` to `listing-`, and the report no longer has to undo the left alignment Quarto applies to code.
- fix: the safety outputs sit under the ICH E3 subsections of section 14.3, so adverse event displays are numbered 14.3.1.x and the laboratory outputs 14.3.4.x, where a flat 14.3.x sequence collided with the subsections the guideline already defines.
- fix: vital signs analyses fix the supine baseline, so a subject no longer enters the shift table and the analysis of covariance once per posture.
- fix: the analysis of covariance reports unadjusted confidence intervals, matching what the statistical methods chapter and the SAP describe, instead of the Dunnett-adjusted intervals `emmeans` applies by default to `trt.vs.ctrl`.
- fix: adverse event, concomitant medication and medical history tables apply their incidence threshold within each treatment group instead of the pooled population.
- fix: the overall row of a threshold table counts every subject with a qualifying record rather than only those whose term cleared the threshold, so the adverse event tables no longer disagree with the overview table in the same chapter; classes left with no reported term are dropped instead of standing empty.
- fix: `parameter_records()` refuses a dataset with no `BASETYPE` and records with a missing one, rather than failing obscurely or dropping them silently.
- fix: the exposure categories include a category for subjects with no exposure, which were previously dropped as missing.
- fix: the subject flow figure reads the frozen counts written alongside the analysis datasets instead of the source package.
- fix: prose, table subtitles and the reviewer's guide agree on which population and which treatment variable each output uses; planned and actual treatment differ for 12 subjects in these data.
- test: `testthat` suite covering dataset invariants, independent recomputation of the reported counts, and double programming of the Cox and analysis of covariance results.
- ci: render workflow publishing the book to GitHub Pages and attaching the PDF, and running the analysis checks before rendering.
- build: `DESCRIPTION` declaring the R dependencies, with `renv` configured for explicit snapshots.
- chore: licence, copyright and author headers on every source file, and a copy of the licence inside the extension.
