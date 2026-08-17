# Clinical Study Report in Quarto

A complete Quarto project that turns pharmaverse ADaM data into a branded, sponsor-style clinical study report, rendered to both a Typst PDF and an HTML site from one source.

> [!NOTE]
> Nordvale Therapeutics is a fictional sponsor.
> The report is built from the public CDISC pilot data (`CDISCPILOT01`) shipped with `pharmaverseadam` and is not a regulatory submission.

## What it demonstrates

- A custom Quarto format extension (`_extensions/nordvale/clinical`) contributing a Typst format and an HTML format from one identity.
- A Typst template with a cover page, a document control and approval page, running headers and footers, page X of Y numbering, a draft watermark driven by document metadata, and landscape pages for wide listings.
- Colours, typography and logos driven entirely by `_brand.yml`, reaching figures and tables as well as the page furniture.
- Fonts bundled in the extension, so the output is reproducible on any machine.
- The same section files rendered either as a Quarto book or as a single long document, selected with a Quarto profile.
- An analysis layer built on the pharmaverse: `pharmaverseadam` data, one `admiral` derivation, `gtsummary` tables that retain their Analysis Results Datasets, and `ggplot2` figures.
- Cross-reference divs around every table, listing and figure, with captions computed in R from a single index that also holds the ICH E3 output number of each one.
- A filter that numbers every output with its ICH E3 number, in the caption and in every reference, for the Typst PDF and for the HTML site.
- A crossref category of its own for the subject data listings, so they are not treated as code listings.

## Structure

| Path                             | Purpose                                                                        |
| -------------------------------- | ------------------------------------------------------------------------------ |
| `_brand.yml`                     | Sponsor colours, typography and logos.                                         |
| `_extensions/nordvale/clinical/` | The Typst and HTML formats, the SCSS theme and the bundled fonts.              |
| `DESCRIPTION`                    | Declares the R dependencies; `renv` snapshots in explicit mode from this file. |
| `R/00-adam.R`                    | Builds the analysis datasets, including the ADTTE derived with `admiral`.      |
| `R/brand.R`                      | Reads the brand and applies it to `ggplot2`, `gt` and `gtsummary`.             |
| `R/tlf-index.R`                  | The caption, source note and ICH E3 number of every output, in one place.      |
| `R/01-tlf-numbers.R`             | Writes those numbers for the filter that applies them. Runs at pre-render.     |
| `R/02-html-numbers.R`            | Numbers the cross-references of the HTML book. Runs at post-render.            |
| `R/tlf.R`                        | One function per table, listing or figure.                                     |
| `tests/testthat/`                | Invariants, independent recomputation and double programming of the results.   |
| `sections/`                      | The ICH E3 sections, used by both profiles.                                    |
| `csr.qmd`                        | Single-document assembly of `sections/`.                                       |
| `sap.qmd`                        | Companion Statistical Analysis Plan, rendered independently of the CSR.        |
| `index.qmd`                      | Landing page of the book.                                                      |

## Requirements

- Quarto 1.10.18 or later, for the `quarto.language` template variables used by the Typst format.
- R 4.4 or later, with the packages pinned in `renv.lock`.

## Rendering

Restore the R environment once:

```sh
Rscript -e 'renv::restore()'
```

Render the book, which is the default profile:

```sh
quarto render
```

Render the single-document variant:

```sh
quarto render --profile doc
```

Both profiles produce an HTML output and a Typst PDF.
The book is written to `_book/`, the single document to `_doc/`.

The book landing page links to all four renderings.
The two single-document links resolve once `_doc/` is copied to `_book/single/`, which the render workflow does before publishing:

```sh
mkdir -p _book/single && cp -R _doc/. _book/single/
```

The analysis datasets are rebuilt automatically by the project pre-render step.
To rebuild them on their own:

```sh
Rscript R/00-adam.R
```

## Switching from draft to final

`status` controls the draft watermark and the draft notice on the cover, and is set in `_metadata-csr.yml`, which both the book profile and `csr.qmd` load.
Set it to `final` and re-render to remove both.
The companion SAP (`sap.qmd`) sets its own `status` separately, since it is a different document with its own document-control history.

## Licence

Copyright (c) 2026 Mickaël Canouil.
MIT Licence.

The bundled fonts are distributed under the SIL Open Font Licence; their licence files sit beside them in `_extensions/nordvale/clinical/fonts/`.
