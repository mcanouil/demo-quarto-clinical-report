// Nordvale Clinical - Typst Template
// Cover page, document control page, running page furniture and heading styles
// for a sponsor-issued clinical document.
//
// @license MIT
// @copyright 2026 Mickaël Canouil
// @author Mickaël Canouil
//
// Brand colours and logos are injected by Quarto as `brand-color` and
// `brand-logo-images`; the show partial reads them and passes them in here.

#let field-label(body, color: black) = text(
  size: 8pt,
  weight: "semibold",
  fill: color,
  tracking: 0.06em,
  upper(body),
)

#let cover-field(label, value, color: black) = {
  if value == none {
    return []
  }
  stack(
    spacing: 5pt,
    field-label(label, color: color),
    text(size: 11pt)[#value],
  )
}

#let signature-block(name, role, ink: black, rule: gray) = block(
  width: 100%,
  inset: (top: 10pt, bottom: 6pt),
)[
  #text(size: 10pt, weight: "semibold")[#name]
  #linebreak()
  #text(size: 9pt, fill: rule)[#role]
  #v(26pt)
  #grid(
    columns: (1fr, 1fr),
    column-gutter: 12pt,
    [#line(length: 100%, stroke: 0.6pt + rule) #text(size: 8pt, fill: rule)[Signature]],
    [#line(length: 100%, stroke: 0.6pt + rule) #text(size: 8pt, fill: rule)[Date]],
  )
]

#let clinical(
  title: none,
  subtitle: none,
  sponsor: none,
  protocol: none,
  compound: none,
  phase: none,
  indication: none,
  report-version: none,
  report-date: none,
  status: "final",
  document-type: none,
  confidentiality: none,
  disclaimer: none,
  approvals: (),
  revisions: (),
  logo: none,
  logo-height: 16mm,
  primary: rgb("#0E5C63"),
  secondary: rgb("#3E5060"),
  tertiary: rgb("#E7EDEE"),
  ink: rgb("#16202A"),
  paper: rgb("#FDFDFC"),
  lang: "en",
  region: "GB",
  font: none,
  fontsize: 10pt,
  heading-family: none,
  heading-weight: "semibold",
  heading-color: none,
  heading-line-height: 0.7em,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: 3,
  toc_indent: 1.2em,
  doc,
) = {
  let heading-fill = if heading-color == none { primary } else { heading-color }
  let doc-type = if document-type == none { title } else { document-type }
  let is-draft = lower(status) == "draft"
  let muted = secondary

  set document(
    title: if title == none { none } else { content-to-string(title) },
    author: if sponsor == none { () } else { (content-to-string(sponsor),) },
  )

  set text(lang: lang, region: region, size: fontsize, fill: ink)
  set text(font: font) if font != none
  show raw: set text(font: codefont) if codefont != none
  set par(justify: true, leading: linestretch * 0.62em)

  set heading(numbering: sectionnumbering)
  show heading: it => block(above: 1.4em, below: 0.8em)[
    #set text(
      font: if heading-family == none { font } else { heading-family },
      weight: heading-weight,
      fill: heading-fill,
    )
    #set par(leading: heading-line-height)
    #it
  ]
  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    block(above: 0em, below: 0.9em)[
      #set text(
        font: if heading-family == none { font } else { heading-family },
        weight: heading-weight,
        fill: heading-fill,
        size: 1.5em,
      )
      #it
      #v(-0.5em)
      #line(length: 100%, stroke: 1pt + primary)
    ]
  }

  set table(inset: 5pt, stroke: none)
  show figure.caption: set text(size: 0.9em, fill: muted)
  show link: set text(fill: primary)

  // Cover page.
  page(
    margin: (x: 0mm, y: 0mm),
    header: none,
    footer: none,
    numbering: none,
    background: none,
  )[
    #block(width: 100%, height: 42mm, fill: primary, inset: (x: 20mm, y: 14mm))[
      #if logo != none {
        block(height: logo-height)[#logo]
      } else {
        text(fill: paper, size: 20pt, weight: "semibold")[#sponsor]
      }
    ]
    #block(inset: (x: 20mm, top: 18mm, bottom: 0mm), width: 100%)[
      #field-label([Protocol #protocol], color: muted)
      #v(6pt)
      #text(size: 24pt, weight: "semibold", fill: primary, font: heading-family)[#doc-type]
      #v(10pt)
      #line(length: 40%, stroke: 2pt + rgb("#C8862B"))
      #v(14pt)
      #if title != none and content-to-string(title) != content-to-string(doc-type) {
        text(size: 14pt)[#title]
      }
      #if subtitle != none {
        v(6pt)
        text(size: 12pt)[#subtitle]
      }
      #v(18pt)
      #grid(
        columns: (1fr, 1fr),
        column-gutter: 14pt,
        row-gutter: 14pt,
        cover-field("Compound", compound, color: muted),
        cover-field("Study phase", phase, color: muted),
        cover-field("Indication", indication, color: muted),
        cover-field("Sponsor", sponsor, color: muted),
        cover-field("Report version", report-version, color: muted),
        cover-field("Report date", report-date, color: muted),
      )
      #if is-draft {
        block(
          width: 100%,
          fill: rgb("#C8862B").lighten(80%),
          stroke: (left: 3pt + rgb("#C8862B")),
          inset: 10pt,
        )[
          #text(weight: "semibold")[Draft]
          #linebreak()
          This document is a draft and is not for regulatory submission.
        ]
      }
    ]
    #place(bottom + left, dx: 20mm, dy: -20mm)[
      #block(width: 170mm)[
        #if confidentiality != none {
          block(
            width: 100%,
            fill: tertiary,
            inset: 10pt,
          )[#text(size: 9pt)[#confidentiality]]
        }
        #if disclaimer != none {
          v(8pt)
          text(size: 8pt, fill: muted, style: "italic")[#disclaimer]
        }
      ]
    ]
  ]

  // Running page furniture for the body.
  set page(
    header: context {
      set text(size: 8pt, fill: muted)
      grid(
        columns: (1fr, auto),
        align: (left + bottom, right + bottom),
        [#sponsor #h(6pt) #text(fill: tertiary)[|] #h(6pt) #doc-type],
        [Protocol #protocol],
      )
      v(-6pt)
      line(length: 100%, stroke: 0.6pt + primary)
    },
    footer: context {
      set text(size: 8pt, fill: muted)
      line(length: 100%, stroke: 0.6pt + tertiary)
      v(-2pt)
      grid(
        columns: (1fr, auto),
        align: (left + top, right + top),
        [#sponsor #h(4pt) #text(fill: tertiary)[|] #h(4pt) Confidential],
        [Page #counter(page).display() of #counter(page).final().first()],
      )
    },
    background: if is-draft {
      place(
        center + horizon,
        rotate(
          -45deg,
          text(
            size: 46pt,
            weight: "bold",
            fill: primary.transparentize(90%),
          )[DRAFT],
        ),
      )
    } else {
      none
    },
  )
  counter(page).update(1)

  // Document control page.
  if revisions.len() > 0 or approvals.len() > 0 {
    heading(level: 1, numbering: none, outlined: false)[Document control]

    if revisions.len() > 0 {
      block(below: 16pt)[
        #table(
          columns: (auto, auto, 1fr),
          fill: (_, y) => if y == 0 { primary } else { none },
          table.header(
            ..([Version], [Date], [Summary of changes]).map(cell => text(
              fill: paper,
              weight: "semibold",
              size: 9pt,
            )[#cell]),
          ),
          ..revisions
            .map(revision => (
              [#revision.version],
              [#revision.date],
              [#revision.summary],
            ))
            .flatten(),
        )
      ]
    }

    if approvals.len() > 0 {
      block(below: 8pt)[
        #text(weight: "semibold")[Approval]
        #v(2pt)
        #text(size: 9pt, fill: muted)[
          By signing this page the approvers confirm that the report describes
          the conduct and results of the study accurately and completely.
        ]
      ]
      grid(
        columns: (1fr, 1fr),
        column-gutter: 18pt,
        ..approvals.map(approval => signature-block(
          approval.name,
          approval.role,
          ink: ink,
          rule: muted,
        ))
      )
    }
  }

  if toc {
    heading(level: 1, numbering: none, outlined: false)[#toc_title]
    block(above: 0.4em, below: 2em)[
      #outline(title: none, depth: toc_depth, indent: toc_indent)
    ]
  }

  doc
}
