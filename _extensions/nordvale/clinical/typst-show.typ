// Nordvale Clinical - Typst Show Partial
// Maps Pandoc metadata and the Quarto brand globals onto the clinical template.
//
// @license MIT
// @copyright 2026 Mickaël Canouil
// @author Mickaël Canouil

#show: doc => clinical(
$if(title)$
  title: [$title$],
$endif$
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
$if(sponsor)$
  sponsor: [$sponsor$],
$endif$
$if(protocol)$
  protocol: [$protocol$],
$endif$
$if(compound)$
  compound: [$compound$],
$endif$
$if(phase)$
  phase: [$phase$],
$endif$
$if(indication)$
  indication: [$indication$],
$endif$
$if(report-version)$
  report-version: [$report-version$],
$endif$
$if(report-date)$
  report-date: [$report-date$],
$elseif(date)$
  report-date: [$date$],
$endif$
$if(status)$
  status: "$status$",
$endif$
$if(document-type)$
  document-type: [$document-type$],
$endif$
$if(confidentiality)$
  confidentiality: [$confidentiality$],
$endif$
$if(disclaimer)$
  disclaimer: [$disclaimer$],
$endif$
$if(approvals)$
  approvals: (
$for(approvals)$
    (name: [$it.name$], role: [$it.role$]),
$endfor$
  ),
$endif$
$if(revisions)$
  revisions: (
$for(revisions)$
    (version: [$it.version$], date: [$it.date$], summary: [$it.summary$]),
$endfor$
  ),
$endif$
  logo: {
    let entry = brand-logo-images.at("$cover-logo$", default: none)
    if entry != none {
      image(entry.path, height: 16mm, alt: entry.at("alt", default: none))
    }
  },
  primary: brand-color.at("primary", default: nordvale-primary),
  secondary: brand-color.at("secondary", default: nordvale-secondary),
  tertiary: brand-color.at("tertiary", default: nordvale-tertiary),
  ink: brand-color.at("foreground", default: nordvale-ink),
  paper: brand-color.at("background", default: nordvale-paper),
$if(lang)$
  lang: "$lang$",
$endif$
$if(region)$
  region: "$region$",
$endif$
$if(mainfont)$
  font: ("$mainfont$",),
$elseif(brand.typography.base.family)$
  font: $brand.typography.base.family$,
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$elseif(brand.typography.base.size)$
  fontsize: $brand.typography.base.size$,
$endif$
$if(brand.typography.headings.family)$
  heading-family: $brand.typography.headings.family$,
$elseif(mainfont)$
  heading-family: ("$mainfont$",),
$endif$
$if(brand.typography.headings.weight)$
  heading-weight: $brand.typography.headings.weight$,
$endif$
$if(codefont)$
  codefont: ($for(codefont)$"$codefont$",$endfor$),
$elseif(brand.typography.monospace.family)$
  codefont: $brand.typography.monospace.family$,
$endif$
$if(linestretch)$
  linestretch: $linestretch$,
$endif$
$if(section-numbering)$
  sectionnumbering: "$section-numbering$",
$endif$
$if(toc)$
  toc: $toc$,
$endif$
$if(toc-title)$
  toc_title: [$toc-title$],
$else$
  toc_title: [$quarto.language.toc-title-document$],
$endif$
  toc_depth: $toc-depth$,
  doc,
)
