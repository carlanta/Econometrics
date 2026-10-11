-- PDF: reserva sitio antes de cada cuadro para que no se parta ni se separe de su pie.
-- Quarto convierte los cuadros con pie en longtable, que sí puede cortarse entre páginas.
-- Se ejecuta en pre-ast (_quarto.yml): ahí cada cuadro es aún un Div «tbl-…» con su LaTeX o su Table dentro.
if not FORMAT:match("latex") then return {} end

local function filas(div)
  local n = 0
  div:walk({
    Table = function(t)
      n = n + #t.head.rows
      for _, b in ipairs(t.bodies) do n = n + #b.body end
    end,
    RawBlock = function(b)
      local _, k = b.text:gsub("\\\\%s*\n", "")
      n = n + k
    end,
  })
  return n
end

function Div(d)
  if d.identifier:match("^tbl%-") then
    local n = filas(d)
    if n > 0 then
      return { pandoc.RawBlock("latex", "\\needspace{" .. (n + 6) .. "\\baselineskip}"), d }
    end
  end
end
