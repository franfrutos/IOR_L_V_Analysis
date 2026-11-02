--[[
psicologica.lua -- front matter for the Psicológica Quarto extension.

Reads Quarto's normalised author schema (`by-author`, with numbered
affiliations) and

  * LaTeX: writes authblk commands (\author[1,2]{Name\,\orcidlink{..}},
    \affil[1]{..}), \corrauthor{..}, \runningauthor{..} and \authornote{..}
    into template variables used by partials/title.tex.
  * DOCX: builds the title page (authors with superscript affiliation
    numbers, affiliations, running head, corresponding author, ORCID iDs,
    author note, abstract, keywords) as styled paragraphs and removes the
    metadata pandoc would otherwise render on its own.

Runs at `post-render` so that Quarto's own filters cannot re-add metadata.
]]

local stringify = pandoc.utils.stringify

-- helpers --------------------------------------------------------------------

local function nonempty(v)
  if v == nil then return false end
  return stringify(v):match("%S") ~= nil
end

local function is_true(v, default)
  if v == nil then return default end
  if type(v) == "boolean" then return v end
  local s = stringify(v):lower()
  return not (s == "false" or s == "no" or s == "0" or s == "")
end

-- metadata value -> pandoc.Inlines
local function inlines(v)
  if v == nil then return pandoc.Inlines({}) end
  local t = pandoc.utils.type(v)
  if t == "Inlines" then return v end
  if t == "Blocks" then return pandoc.utils.blocks_to_inlines(v) end
  if t == "string" or t == "number" then
    local blk = pandoc.read(tostring(v), "markdown").blocks[1]
    if blk and blk.content then return pandoc.Inlines(blk.content) end
    return pandoc.Inlines({ pandoc.Str(tostring(v)) })
  end
  return pandoc.Inlines({ pandoc.Str(stringify(v)) })
end

local function to_latex(ils)
  local out = pandoc.write(pandoc.Pandoc({ pandoc.Plain(ils) }), "latex")
  return (out:gsub("%s+$", ""))
end

local function latex_meta(s)
  return pandoc.MetaInlines({ pandoc.RawInline("latex", s) })
end

local function join_inlines(list, sep)
  local out = pandoc.Inlines({})
  for i, ils in ipairs(list) do
    if i > 1 then out:extend(sep) end
    out:extend(ils)
  end
  return out
end

local function orcid_id(v)
  local s = stringify(v)
  s = s:gsub("^%s*https?://orcid%.org/", ""):gsub("%s+", "")
  return s
end

local AFFIL_FIELDS = { "department", "name", "address", "city", "region", "country" }

local function affiliation_inlines(a)
  local parts = {}
  for _, f in ipairs(AFFIL_FIELDS) do
    if nonempty(a[f]) then table.insert(parts, inlines(a[f])) end
  end
  return join_inlines(parts, { pandoc.Str(","), pandoc.Space() })
end

-- collect authors and affiliations --------------------------------------------

local function collect(meta)
  local authors, affils, seen = {}, {}, {}
  local by_author = meta["by-author"] or {}
  for _, a in ipairs(by_author) do
    local au = {
      name = (a.name and (a.name.literal or a.name)) and inlines(a.name.literal or a.name) or pandoc.Inlines({}),
      family = a.name and a.name.family and stringify(a.name.family) or nil,
      orcid = nonempty(a.orcid) and orcid_id(a.orcid) or nil,
      email = nonempty(a.email) and stringify(a.email) or nil,
      corresponding = a.attributes and is_true(a.attributes.corresponding, false) or false,
      -- Current address of an author who has moved (`present-address`), used in
      -- the corresponding-author note instead of the first affiliation
      present = (a.metadata and nonempty(a.metadata["present-address"]) and inlines(a.metadata["present-address"]))
        or (nonempty(a["present-address"]) and inlines(a["present-address"])) or nil,
      numbers = {},
      affils = {},
    }
    if a.affiliations then
      for _, af in ipairs(a.affiliations) do
        local num = af.number and stringify(af.number) or tostring(#affils + 1)
        table.insert(au.numbers, num)
        table.insert(au.affils, af)
        if not seen[num] then
          seen[num] = true
          table.insert(affils, { number = num, text = affiliation_inlines(af) })
        end
      end
    end
    table.insert(authors, au)
  end
  table.sort(affils, function(x, y)
    return (tonumber(x.number) or 0) < (tonumber(y.number) or 0)
  end)
  return authors, affils
end

local function corresponding_authors(authors)
  local list = {}
  for _, a in ipairs(authors) do
    if a.corresponding then table.insert(list, a) end
  end
  if #list == 0 then
    for _, a in ipairs(authors) do
      if a.email then return { a } end
    end
  end
  return list
end

-- "Garre-Frutos and Lupiáñez" / "Garre-Frutos et al."
local function auto_running_author(authors)
  local fam = {}
  for _, a in ipairs(authors) do
    table.insert(fam, a.family or stringify(a.name))
  end
  if #fam == 0 then return nil end
  if #fam == 1 then return fam[1] end
  if #fam == 2 then return fam[1] .. " and " .. fam[2] end
  return fam[1] .. " et al."
end

local function running_author_inlines(meta, authors)
  for _, key in ipairs({ "runningauthor", "running-author", "shorttitle", "running-head" }) do
    if nonempty(meta[key]) then return inlines(meta[key]) end
  end
  local s = auto_running_author(authors)
  if s then return inlines(s) end
  return nil
end

local function keyword_inlines(meta)
  local kw = meta.keywords
  if kw == nil then return nil end
  if pandoc.utils.type(kw) == "List" then
    local parts = {}
    for _, k in ipairs(kw) do table.insert(parts, inlines(k)) end
    return join_inlines(parts, { pandoc.Str(","), pandoc.Space() })
  end
  return inlines(kw)
end

-- LaTeX -----------------------------------------------------------------------

local function latex_frontmatter(meta)
  local authors, affils = collect(meta)
  local lines = {}
  for _, a in ipairs(authors) do
    local name = to_latex(a.name)
    if a.orcid then name = name .. "\\,\\orcidlink{" .. a.orcid .. "}" end
    -- authblk: an empty mark ([]) would auto-number; a space prints no mark
    local mark = #a.numbers > 0 and table.concat(a.numbers, ",") or " "
    table.insert(lines, "\\author[" .. mark .. "]{" .. name .. "}")
  end
  for _, af in ipairs(affils) do
    table.insert(lines, "\\affil[" .. af.number .. "]{" .. to_latex(af.text) .. "}")
  end
  if #lines > 0 then meta["psicologica-authors"] = latex_meta(table.concat(lines, "\n")) end

  local corr = {}
  for _, a in ipairs(corresponding_authors(authors)) do
    local s = to_latex(a.name)
    if a.present then
      s = s .. ", " .. to_latex(a.present)
    elseif a.affils[1] then
      s = s .. ", " .. to_latex(affiliation_inlines(a.affils[1]))
    end
    if a.email then
      s = s .. ". Email: \\href{mailto:" .. a.email .. "}{" .. to_latex(inlines(a.email)) .. "}"
    end
    table.insert(corr, s)
  end
  if #corr > 0 then meta["psicologica-corrauthor"] = latex_meta(table.concat(corr, "; ")) end

  local ra = running_author_inlines(meta, authors)
  if ra then meta["psicologica-runningauthor"] = latex_meta(to_latex(ra)) end

  return meta
end

-- DOCX ------------------------------------------------------------------------

local function styled(style, blocks)
  return pandoc.Div(blocks, pandoc.Attr("", {}, { ["custom-style"] = style }))
end

local function labelled_para(label, content)
  local ils = pandoc.Inlines({ pandoc.Emph({ pandoc.Str(label .. ":") }), pandoc.Space() })
  ils:extend(content)
  return pandoc.Para(ils)
end

local function docx_frontmatter(doc)
  local meta = doc.meta
  local authors, affils = collect(meta)
  local front = pandoc.Blocks({})

  -- authors line: "A¹˒², B², and C³"
  if #authors > 0 then
    local line = pandoc.Inlines({})
    for i, a in ipairs(authors) do
      if i > 1 then
        if i == #authors then
          if #authors > 2 then line:insert(pandoc.Str(",")) end
          line:extend({ pandoc.Space(), pandoc.Str("and"), pandoc.Space() })
        else
          line:extend({ pandoc.Str(","), pandoc.Space() })
        end
      end
      line:extend(a.name)
      if #a.numbers > 0 then
        line:insert(pandoc.Superscript({ pandoc.Str(table.concat(a.numbers, ",")) }))
      end
    end
    front:insert(styled("Author", { pandoc.Para(line) }))
  end

  -- affiliations
  if #affils > 0 then
    local paras = {}
    for _, af in ipairs(affils) do
      local ils = pandoc.Inlines({ pandoc.Superscript({ pandoc.Str(af.number) }) })
      ils:extend(af.text)
      table.insert(paras, pandoc.Para(ils))
    end
    front:insert(styled("Affiliation", paras))
  end

  -- notes: running head, corresponding author, ORCID iDs, author note
  local notes = {}
  local ra = running_author_inlines(meta, authors)
  if ra then table.insert(notes, labelled_para("Running head", ra)) end

  if is_true(meta["corresponding-note"], true) then
    for _, a in ipairs(corresponding_authors(authors)) do
      local ils = pandoc.Inlines({})
      ils:extend(a.name)
      if a.present then
        ils:extend({ pandoc.Str(","), pandoc.Space() })
        ils:extend(a.present)
      elseif a.affils[1] then
        ils:extend({ pandoc.Str(","), pandoc.Space() })
        ils:extend(affiliation_inlines(a.affils[1]))
      end
      if a.email then
        ils:extend({ pandoc.Str("."), pandoc.Space(), pandoc.Str("Email:"), pandoc.Space(),
          pandoc.Link({ pandoc.Str(a.email) }, "mailto:" .. a.email) })
      end
      table.insert(notes, labelled_para("Corresponding author", ils))
    end
  end

  local orcids = {}
  for _, a in ipairs(authors) do
    if a.orcid then
      local ils = pandoc.Inlines({})
      ils:extend(a.name)
      ils:extend({ pandoc.Space(),
        pandoc.Link({ pandoc.Str("https://orcid.org/" .. a.orcid) }, "https://orcid.org/" .. a.orcid) })
      table.insert(orcids, ils)
    end
  end
  if #orcids > 0 then
    table.insert(notes, labelled_para("ORCID iDs", join_inlines(orcids, { pandoc.Str(";"), pandoc.Space() })))
  end

  if nonempty(meta.authornote) then
    table.insert(notes, labelled_para("Author's notes", inlines(meta.authornote)))
  end
  if #notes > 0 then front:insert(styled("Author Note", notes)) end

  -- abstract and keywords
  if meta.abstract then
    front:insert(styled("Abstract Title", { pandoc.Para({ pandoc.Str("Abstract") }) }))
    local abs = meta.abstract
    local blocks
    if pandoc.utils.type(abs) == "Blocks" then
      blocks = abs
    else
      blocks = pandoc.Blocks({ pandoc.Para(inlines(abs)) })
    end
    front:insert(styled("Abstract", blocks))
    local kw = keyword_inlines(meta)
    if kw then front:insert(styled("Keywords", { labelled_para("Keywords", kw) })) end
  end

  -- pandoc renders title (+ subtitle) itself; drop what we rendered above
  meta.author = nil
  meta.abstract = nil
  meta.date = nil
  doc.meta = meta

  local blocks = pandoc.Blocks(front)
  blocks:extend(doc.blocks)
  doc.blocks = blocks
  return doc
end

-- entry point -------------------------------------------------------------------

function Pandoc(doc)
  if quarto.doc.is_format("latex") then
    doc.meta = latex_frontmatter(doc.meta)
    return doc
  elseif quarto.doc.is_format("docx") then
    return docx_frontmatter(doc)
  end
end
