--- Nordvale Clinical - Filter
--- @module "clinical"
--- @license MIT
--- @copyright 2026 Mickaël Canouil
--- @author Mickaël Canouil
--- @brief Nest book appendices under the Appendices divider in Typst output and
--- number floats by chapter.
--- @description Quarto emits the book Appendices divider as a plain level one
--- heading and leaves part handling to the book extension, so the divider and
--- the appendix chapters end up at the same level. This filter keeps the
--- divider at level one, demotes every heading that follows it, and letters the
--- appendix chapters A, B and their subsections A.1, B.1.
--- Demoting the appendix chapters puts them out of reach of the show rule
--- Quarto injects to reset the float counters at each chapter, so this filter
--- resets them itself, and prefixes every float number with its chapter.

-- ============================================================================
-- CONSTANTS (PRIVATE)
-- ============================================================================

--- Typst setup emitted just after the Appendices divider.
--- The divider is unnumbered and Typst does not count unnumbered headings, so
--- the first counter component stays at zero and is dropped, which letters the
--- demoted appendix chapters A, B and their subsections A.1, B.1.
local appendix_numbering = [[
#counter(heading).update(0)
#set heading(numbering: (..numbers) => {
  let levels = numbers.pos()
  if levels.len() < 2 { return none }
  numbering("A.1.1", ..levels.slice(1))
})]]

--- Typst setup emitted at the top of the body of a single-file book.
--- Quarto zeroes every float counter at each chapter but numbers the floats
--- from that counter alone, so `Table 1` names one table per chapter. Reading
--- the chapter off the heading counter restores the chapter-scoped numbers the
--- HTML book already shows. In the appendices the first component is zero, so
--- the appendix letter is the second component.
local float_numbering = [[
#set figure(numbering: num => {
  let levels = counter(heading).get()
  if levels.first() > 0 {
    numbering("1.1", levels.first(), num)
  } else {
    numbering("A.1", levels.at(1, default: 1), num)
  }
})]]

--- Typst counter resets emitted at the start of each appendix chapter.
--- The kinds are the crossref categories this report uses; Quarto derives its
--- own list from `crossref.categories`, so extend this one alongside it.
local float_counter_reset = [[
#counter(figure.where(kind: "quarto-float-fig")).update(0)
#counter(figure.where(kind: "quarto-float-tbl")).update(0)
#counter(figure.where(kind: "quarto-float-lst")).update(0)
#counter(math.equation).update(0)]]

-- ============================================================================
-- STATE (PRIVATE)
-- ============================================================================

--- Whether this render is a book, learnt from the file metadata of a heading.
--- Quarto keeps its own `single-file-book` parameter out of reach of extension
--- filters, so the flag stands in for it. Only a book carries `bookItemType`,
--- and a Typst book is always rendered as a single file, which is the case
--- Quarto resets the float counters for.
local is_book = false

-- ============================================================================
-- PUBLIC FUNCTIONS
-- ============================================================================

--- Number floats by chapter, in a book only.
--- The single document profile numbers its floats in one sequence, which needs
--- no prefix, and Quarto resets the counters only for a single-file book.
--- @return nil The setup goes to the header rather than to the document
local function number_floats_by_chapter()
  if quarto.doc.is_format('typst') and is_book then
    quarto.doc.include_text('in-header', float_numbering)
  end

  return nil
end

--- Keep the Appendices divider at level one and demote what follows.
--- @param el pandoc.Header The heading element to process
--- @return table|pandoc.Header|nil Replacement blocks, the demoted heading, or
--- nil to leave the heading unchanged
local function nest_appendices(el)
  if not quarto.doc.is_format('typst') then
    return nil
  end

  local state = quarto.doc.file_metadata()
  if state == nil or state.file == nil then
    return nil
  end

  is_book = is_book or state.file.bookItemType ~= nil

  if el.level == 1 and state.file.bookItemType == 'appendix' then
    return { el, pandoc.RawBlock('typst', appendix_numbering) }
  end

  if state.appendix then
    local chapter = el.level == 1
    el.level = el.level + 1
    if chapter then
      return { el, pandoc.RawBlock('typst', float_counter_reset) }
    end
    return el
  end

  return nil
end

-- ============================================================================
-- FILTER EXPORT
-- ============================================================================

-- The file metadata markers are raw blocks in the same document, so they have
-- to be parsed during this filter's own traversal for `bookItemType` to be set.
-- `Pandoc` runs after the headings, which is where the book is recognised.
return quarto.utils.combineFilters({
  quarto.utils.file_metadata_filter(),
  { Header = nest_appendices, Pandoc = number_floats_by_chapter }
})
