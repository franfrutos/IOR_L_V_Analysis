--[[
psicologica-options.lua -- runs at `pre-ast`.

DOCX captions: default to "Figure 1. Caption" (period delimiter, as in the
journal's LaTeX class) unless the user set `crossref: title-delim:` in the
document. (Setting `title-delim: "."` directly in _extension.yml does not
work: Quarto resolves "." as a path relative to the extension folder.)
]]

function Meta(meta)
  if not quarto.doc.is_format("docx") then return nil end
  local cr = meta.crossref
  if cr == nil then cr = pandoc.MetaMap({}) end
  if cr["title-delim"] == nil then
    cr["title-delim"] = pandoc.MetaInlines({ pandoc.Str(".") })
    meta.crossref = cr
    return meta
  end
end
