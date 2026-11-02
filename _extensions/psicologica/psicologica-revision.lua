-- Mark revised text in the revised version of a manuscript.
--
-- Usage in markdown/quarto:
--   [revised text]{.rev}       (also {.blue} and {.review-blue})
--   ::: {.rev}
--   Revised paragraphs.
--   :::
--
-- PDF: blue text. Word: "Revision" character style of the reference document
-- (blue). Set `revision-marks: false` to render the clean version.

local CLASSES = { rev = true, blue = true, ["review-blue"] = true }
local enabled = true

local function is_marked(el)
  for _, class in ipairs(el.classes) do
    if CLASSES[class] then
      return true
    end
  end
  return false
end

local function mark(inlines)
  if not enabled then
    return inlines
  end
  if FORMAT:match("latex") then
    local out = pandoc.Inlines({ pandoc.RawInline("latex", "\\textcolor{blue}{") })
    out:extend(inlines)
    out:insert(pandoc.RawInline("latex", "}"))
    return out
  end
  if FORMAT:match("docx") then
    return pandoc.Inlines({ pandoc.Span(inlines, { ["custom-style"] = "Revision" }) })
  end
  return inlines
end

local function read_options(meta)
  if meta["revision-marks"] ~= nil then
    enabled = pandoc.utils.stringify(meta["revision-marks"]) ~= "false"
  end
end

local function span_filter(el)
  if is_marked(el) then
    return mark(el.content)
  end
end

-- Paragraphs inside a marked div are marked one by one (a colour command
-- cannot span paragraph breaks)
local function div_filter(el)
  if not is_marked(el) then
    return nil
  end
  local blocks = el.content:walk({
    Para = function(p)
      p.content = mark(p.content)
      return p
    end,
    Plain = function(p)
      p.content = mark(p.content)
      return p
    end
  })
  return blocks
end

return {
  { Meta = read_options },
  { Span = span_filter, Div = div_filter }
}
