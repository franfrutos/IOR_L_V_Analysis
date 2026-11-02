-- Runs citeproc (the format sets `citeproc: false`, as in apaquarto) so that the
-- rendered citations can be edited afterwards. APA 7: narrative citations use
-- "and" instead of "&" (e.g., "Bucker and Theeuwes (2014)" but "(Bucker &
-- Theeuwes, 2014)").

local function narrative_and(el)
  if el.citations[1].mode ~= "AuthorInText" then
    return nil
  end
  el.content = el.content:walk({
    Str = function(s)
      if s.text == "&" then
        return pandoc.Str("and")
      end
    end
  })
  return el
end

function Pandoc(doc)
  doc = pandoc.utils.citeproc(doc)
  return doc:walk({ Cite = narrative_and })
end
