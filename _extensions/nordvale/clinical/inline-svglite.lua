--- Nordvale Clinical - Filter
--- @module "clinical"
--- @license MIT
--- @copyright 2026 Mickaël Canouil
--- @author Mickaël Canouil
--- @brief Inline svglite figures in HTML output.
--- @description Replaces references to svglite-produced SVG files with the SVG
--- content itself, so that the page stylesheet resolves the brand fonts instead
--- of the graphics device having to embed them.

-- ============================================================================
-- HELPER FUNCTIONS (PRIVATE)
-- ============================================================================

--- Read a file in full.
--- @param path string Path to the file
--- @return string|nil Contents of the file, or nil when it cannot be opened
local function read_file(path)
  local file = io.open(path, 'r')
  if not file then
    return nil
  end
  local content = file:read('*all')
  file:close()
  return content
end

--- Check whether an SVG was produced by svglite.
--- svglite wraps its output in a group carrying the `svglite` class, which
--- distinguishes it from SVG assets such as logos.
--- @param svg string Contents of the SVG file
--- @return boolean True when the svglite marker group is present
local function is_svglite(svg)
  return svg:match('<g class=["\']svglite["\']') ~= nil
end

--- Remove the XML declaration, which cannot appear inside an HTML document.
--- @param svg string Contents of the SVG file
--- @return string SVG without its XML declaration
local function strip_xml_declaration(svg)
  return (svg:gsub('<%?xml[^?]*%?>%s*', ''))
end

-- ============================================================================
-- PUBLIC FUNCTIONS
-- ============================================================================

--- Replace a reference to an svglite figure with the SVG itself.
--- @param el pandoc.Image The image element to process
--- @return pandoc.RawInline|nil Inline SVG, or nil to leave the image unchanged
local function inline_svglite(el)
  if not quarto.doc.is_format('html') then
    return nil
  end

  if not el.src:match('%.svg$') then
    return nil
  end

  local svg = read_file(el.src)
  if not svg or not is_svglite(svg) then
    return nil
  end

  return pandoc.RawInline('html', strip_xml_declaration(svg))
end

-- ============================================================================
-- FILTER EXPORT
-- ============================================================================

return {
  { Image = inline_svglite }
}
