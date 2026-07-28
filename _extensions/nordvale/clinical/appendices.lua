--- Nordvale Clinical - Filter
--- @module "clinical"
--- @license MIT
--- @copyright 2026 Mickaël Canouil
--- @author Mickaël Canouil
--- @brief Nest book appendices under the Appendices divider in Typst output.
--- @description Quarto emits the book Appendices divider as a plain level one
--- heading and leaves part handling to the book extension, so the divider and
--- the appendix chapters end up at the same level. This filter keeps the
--- divider at level one, demotes every heading that follows it, and letters the
--- appendix chapters A, B and their subsections A.1, B.1.

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

-- ============================================================================
-- PUBLIC FUNCTIONS
-- ============================================================================

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

  if el.level == 1 and state.file.bookItemType == 'appendix' then
    return { el, pandoc.RawBlock('typst', appendix_numbering) }
  end

  if state.appendix then
    el.level = el.level + 1
    return el
  end

  return nil
end

-- ============================================================================
-- FILTER EXPORT
-- ============================================================================

-- The file metadata markers are raw blocks in the same document, so they have
-- to be parsed during this filter's own traversal for bookItemType to be set.
return quarto.utils.combineFilters({
  quarto.utils.file_metadata_filter(),
  { Header = nest_appendices }
})
