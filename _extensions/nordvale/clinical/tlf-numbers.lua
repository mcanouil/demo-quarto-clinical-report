--- Nordvale Clinical - Filter
--- @module "clinical"
--- @license MIT
--- @copyright 2026 Mickaël Canouil
--- @author Mickaël Canouil
--- @brief Number every output with the ICH E3 number recorded in the index.
--- @description Quarto numbers a float by where it falls in the document, which
--- gives `Table 7.1`, while a clinical study report is read by the ICH E3
--- output number, which gives `Table 14.2.1`. The number cannot be set through
--- metadata: Quarto builds it from the chapter and a running count, so this
--- filter applies the numbers `R/tlf-index.R` holds, written to a JSON file by
--- the pre-render step.
--- HTML takes them through the float itself, whose order Quarto stringifies
--- into the caption, and through the cross-reference links, which are already
--- resolved when this filter runs. Typst numbers its own figures, so it takes
--- them through a show rule per label and one show rule for references.

-- ============================================================================
-- CONSTANTS (PRIVATE)
-- ============================================================================

--- Where the pre-render step writes the numbers, inside the project.
local numbers_path = 'data/tlf-numbers.json'

-- ============================================================================
-- STATE (PRIVATE)
-- ============================================================================

--- Cross-reference identifier to ICH E3 number, read once.
local numbers = nil

--- Labels seen in this document, in the order Typst needs its show rules.
local seen = pandoc.List({})

-- ============================================================================
-- HELPER FUNCTIONS (PRIVATE)
-- ============================================================================

--- Where to read the numbers from.
--- An HTML book renders each chapter from the directory of that chapter, so a
--- path relative to the working directory only resolves for a chapter at the
--- root of the project. Anchor it to the project instead, and fall back to the
--- relative path for a document rendered on its own.
--- @return string The path of the file holding the numbers
local function numbers_file()
  local root = quarto.project.directory

  if root == nil or root == '' then
    return numbers_path
  end

  return pandoc.path.join({ root, numbers_path })
end

--- Read the numbers written by the pre-render step.
--- @return table Identifier to number, empty when the file is absent
local function read_numbers()
  if numbers ~= nil then
    return numbers
  end

  local path = numbers_file()
  local file = io.open(path, 'r')
  if file == nil then
    quarto.log.warning(
      'No output numbers at ' ..
      path ..
      '; run `Rscript R/01-tlf-numbers.R` to write them. Outputs keep the numbers Quarto gives them.'
    )
    numbers = {}
    return numbers
  end

  local content = file:read('*all')
  file:close()
  numbers = quarto.json.decode(content)

  return numbers
end

--- The number of an output, or nil when the index does not know it.
--- @param identifier string The cross-reference identifier of the float
--- @return string|nil The ICH E3 number
local function number_of(identifier)
  if identifier == nil or identifier == '' then
    return nil
  end

  return read_numbers()[identifier]
end

--- Typst show rules that number the outputs and their references.
--- A label selector carries the number to the figure before it is laid out,
--- which a `show figure` rule cannot do, because the number is resolved by
--- then. The reference rule reads the same table.
--- @return string The Typst setup, empty when no output carries a number
local function typst_numbering()
  if #seen == 0 then
    return ''
  end

  local entries = pandoc.List({})
  local rules = pandoc.List({})
  for _, item in ipairs(seen) do
    entries:insert('  "' .. item.identifier .. '": "' .. item.number .. '",')
    rules:insert(
      '#show <' ..
      item.identifier ..
      '>: set figure(numbering: _ => nordvale-tlf-numbers.at("' ..
      item.identifier ..
      '"))'
    )
  end

  return table.concat({
    '#let nordvale-tlf-numbers = (',
    table.concat(entries, '\n'),
    ')',
    table.concat(rules, '\n'),
    '#show ref: it => {',
    '  let key = str(it.target)',
    '  if key in nordvale-tlf-numbers {',
    '    let target = query(it.target).first()',
    '    let supplement = if target.has("supplement") { target.supplement } else { it.supplement }',
    '    [#supplement~#nordvale-tlf-numbers.at(key)]',
    '  } else {',
    '    it',
    '  }',
    '}',
  }, '\n')
end

-- ============================================================================
-- PUBLIC FUNCTIONS
-- ============================================================================

--- Give a float the number the index holds for it.
--- Quarto renders the number from `order`, stringifying whatever it holds, so
--- an ICH E3 number reaches the caption unchanged. Typst ignores `order` and
--- is served by `typst_numbering()` instead.
--- @param float table The float to number
--- @return table|nil The float with its number, or nil when the index has none
local function number_float(float)
  local number = number_of(float.identifier)
  if number == nil then
    return nil
  end

  seen:insert({ identifier = float.identifier, number = number })

  if quarto.doc.is_format('typst') then
    return nil
  end

  float.order = { section = nil, order = number }

  return float
end

--- Rewrite a cross-reference so it carries the same number as its target.
--- Quarto resolves references before this filter runs, so the link already
--- reads `Table 7.1` and only its number has to change.
--- @param link pandoc.Link The link to rewrite
--- @return pandoc.Link|nil The rewritten link, or nil to leave it alone
local function number_reference(link)
  if not link.classes:includes('quarto-xref') then
    return nil
  end

  local number = number_of(link.target:gsub('^#', ''))
  if number == nil then
    return nil
  end

  local done = false
  link.content = link.content:walk({
    Str = function(element)
      if done then
        return nil
      end
      local text, count = element.text:gsub('%d[%d%.]*', number, 1)
      if count == 0 then
        return nil
      end
      done = true
      return pandoc.Str(text)
    end,
  })

  return link
end

--- Emit the Typst setup once every float has been seen.
--- @return nil The setup goes to the header rather than to the document
local function emit_typst_numbering()
  if quarto.doc.is_format('typst') then
    local setup = typst_numbering()
    if setup ~= '' then
      quarto.doc.include_text('in-header', setup)
    end
  end

  return nil
end

-- ============================================================================
-- FILTER EXPORT
-- ============================================================================

-- `Pandoc` runs after the floats, so every label is known by the time the
-- Typst setup is written.
return {
  {
    FloatRefTarget = number_float,
    Link = number_reference,
    Pandoc = emit_typst_numbering,
  },
}
