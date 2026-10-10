# ══════════════════════════════════════════════════════════════════
# mEconometria · Ayudantes para presentaciones con identidad CdeA (officer)
# Copiado y adaptado de mRentaFija (método de Financial Economics). Se ejecuta desde la raíz del proyecto.
# Secuencia de cada tema: portada → hoja de ruta → objetivos 01·02·03 → separadores de bloque → contenido → cierre.
# Lecciones heredadas: officer 0.7.3 ignora `bg` (fondos como PNG de color), flextable con fit_to_width(),
# titulares de una línea si debajo empieza un cuadro, espacio duro entre número y unidad.
# ECUACIONES (decisión de Carlos, 10/10/2026: «ha de ser profesional», con el editor de ecuaciones):
#   Toda notación matemática se escribe en LaTeX con M("\\hat\\beta_1") dentro de cualquier texto (deja una marca
#   ⟦EQn⟧) o con formula_caja(x, "LaTeX") para una ecuación destacada. Al guardar, insertar_ecuaciones() convierte el
#   LaTeX a OMML con pandoc y sustituye cada marca por una ecuación nativa de Office (a14:m), con el tamaño y el color
#   del texto que la rodea. En PowerPoint son ecuaciones editables (Insertar → Ecuación). Nunca β̂ en Unicode:
#   PowerPoint no pinta bien el acento combinado en ninguna fuente de texto.
# ══════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

ID <- list(
  navy = "#052A64", navy2 = "#113F86", oro = "#DBB246", oro_cl = "#ECD38E",
  texto = "#1B2430", gris = "#5B6470", gris_med = "#8A939E",
  linea = "#E2E6EB", suave = "#F4F6F9", blanco = "#FFFFFF"
)
ANCHO <- 13.333   # pulgadas (16:9)
ALTO  <- 7.5

# Logotipo personal (círculo marino con «CdeA» en dorado, PNG con transparencia)
LOGO <- "presentaciones/_identidad/CdeA_logo.png"

# ── Presentación vacía a partir del modelo visual (16:9, tema) ─────
nueva_presentacion <- function(plantilla = "presentaciones/_identidad/modelo_visual_CdeA.pptx") {
  reiniciar_ecuaciones()
  x <- read_pptx(plantilla)
  while (length(x) > 0) x <- remove_slide(x, index = 1)
  x
}

diapo_nueva <- function(x) {
  ls <- layout_summary(x)
  fila <- ls[ls$layout %in% c("Blank", "En blanco"), ][1, ]
  add_slide(x, layout = fila$layout, master = fila$master)
}

# ── Texto ──────────────────────────────────────────────────────────
estilo <- function(size, color = ID$texto, bold = FALSE, italic = FALSE, font = "Segoe UI") {
  fp_text(font.size = size, color = color, bold = bold, italic = italic, font.family = font)
}

# ── Ecuaciones ─────────────────────────────────────────────────────
# Registro de las ecuaciones de la presentación en curso: LaTeX y si es de bloque (display) o en línea
EQS <- new.env()
reiniciar_ecuaciones <- function() { EQS$latex <- character(); EQS$display <- logical() }
reiniciar_ecuaciones()
MARCA <- "⟦EQ%d⟧"
M <- function(latex, display = FALSE) {
  EQS$latex <- c(EQS$latex, latex); EQS$display <- c(EQS$display, display)
  sprintf(MARCA, length(EQS$latex))
}

# Trozos de texto: cada marca de ecuación va en su propio trozo (para sustituirla después) y el resto en `font`
runs_texto <- function(texto, size, color, bold = FALSE, italic = FALSE, font = "Segoe UI") {
  if (!grepl("⟦EQ", texto, fixed = TRUE)) return(list(ftext(texto, estilo(size, color, bold, italic, font))))
  partes <- regmatches(texto, gregexpr("⟦EQ[0-9]+⟧", texto), invert = NA)[[1]]
  partes <- partes[nzchar(partes)]
  lapply(partes, function(p) ftext(p, estilo(size, color, bold, italic, if (grepl("^⟦EQ", p)) "Cambria Math" else font)))
}

# Convierte las marcas ⟦EQn⟧ de un .pptx en ecuaciones OMML (pandoc: LaTeX → docx → m:oMath)
PANDOC <- if (file.exists("/opt/quarto/bin/tools/x86_64/pandoc")) "/opt/quarto/bin/tools/x86_64/pandoc" else "pandoc"
omml_de_latex <- function(latex, display) {
  md <- tempfile(fileext = ".md"); dx <- tempfile(fileext = ".docx")
  writeLines(enc2utf8(paste0(ifelse(display, "$$", "$"), latex, ifelse(display, "$$", "$"), "\n")), md, useBytes = TRUE)
  if (system2(PANDOC, c(shQuote(md), "-o", shQuote(dx))) != 0) stop("pandoc no pudo convertir las ecuaciones")
  doc <- paste(readLines(unz(dx, "word/document.xml"), encoding = "UTF-8", warn = FALSE), collapse = "")
  ps <- regmatches(doc, gregexpr("<w:p>.*?</w:p>", doc, perl = TRUE))[[1]]
  ps <- ps[!grepl("<w:sectPr", ps)]
  if (length(ps) != length(latex)) stop("pandoc devolvió ", length(ps), " párrafos para ", length(latex), " ecuaciones")
  out <- vapply(seq_along(ps), function(i) {
    p <- ps[i]
    m <- if (display[i]) regmatches(p, regexpr("<m:oMathPara>.*</m:oMathPara>", p, perl = TRUE)) else regmatches(p, regexpr("<m:oMath>.*</m:oMath>", p, perl = TRUE))
    if (!length(m)) stop("LaTeX no convertido a ecuación: ", latex[i])
    gsub('<m:jc m:val="center" />', '<m:jc m:val="centerGroup"/>', m, fixed = TRUE)
  }, "")
  out
}
insertar_ecuaciones <- function(origen, destino) {
  n <- length(EQS$latex)
  if (!n) { file.copy(origen, destino, overwrite = TRUE); return(invisible()) }
  omml <- omml_de_latex(EQS$latex, EQS$display)
  dir <- tempfile("pptx_"); dir.create(dir); utils::unzip(origen, exdir = dir)
  ns <- 'xmlns:a14="http://schemas.microsoft.com/office/drawing/2010/main" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"'
  for (f in list.files(file.path(dir, "ppt", "slides"), pattern = "^slide[0-9]+\\.xml$", full.names = TRUE)) {
    xml <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    if (!grepl("⟦EQ", xml, fixed = TRUE)) next
    # 1) Cada trozo-marca se sustituye por la ecuación, con el tamaño y el color de su texto
    patron <- "(?s)<a:r>\\s*(<a:rPr[^>]*/>|<a:rPr[^>]*>(?:(?!</a:r>).)*?</a:rPr>)\\s*<a:t>⟦EQ([0-9]+)⟧</a:t>\\s*</a:r>"
    trozos <- regmatches(xml, gregexpr(patron, xml, perl = TRUE))[[1]]
    for (t in trozos) {
      k <- as.integer(sub(paste0(".*⟦EQ([0-9]+)⟧.*"), "\\1", t))
      sz <- sub('.*sz="([0-9]+)".*', "\\1", t); if (!grepl("^[0-9]+$", sz)) sz <- "1800"
      col <- if (grepl('srgbClr val="', t)) sub('.*srgbClr val="([0-9A-Fa-f]{6})".*', "\\1", t) else "1B2430"
      apr <- sprintf('<a:rPr lang="es-ES" sz="%s" b="0" i="0"><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:latin typeface="Cambria Math"/><a:ea typeface="Cambria Math"/><a:cs typeface="Cambria Math"/></a:rPr>', sz, col)
      eq <- gsub("<m:r>(<m:rPr>.*?</m:rPr>)?", paste0("<m:r>\\1", apr), omml[k], perl = TRUE)
      xml <- sub(t, sprintf("<a14:m %s>%s</a14:m>", ns, eq), xml, fixed = TRUE)
    }
    # 2) Las formas con ecuaciones van dentro de mc:AlternateContent (como las guarda PowerPoint); la alternativa,
    #    para programas sin editor de ecuaciones, lleva el LaTeX como texto
    formas <- regmatches(xml, gregexpr("(?s)<p:sp>(?:(?!<p:sp>).)*?</p:sp>", xml, perl = TRUE))[[1]]
    for (sp in formas[grepl("<a14:m", formas, fixed = TRUE)]) {
      alt <- gsub("(?s)<a14:m [^>]*>.*?</a14:m>", "<a:r><a:t>[ecuación]</a:t></a:r>", sp, perl = TRUE)
      xml <- sub(sp, paste0('<mc:AlternateContent xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006">',
                            '<mc:Choice xmlns:a14="http://schemas.microsoft.com/office/drawing/2010/main" Requires="a14">', sp,
                            '</mc:Choice><mc:Fallback>', alt, '</mc:Fallback></mc:AlternateContent>'), xml, fixed = TRUE)
    }
    if (grepl("⟦EQ", xml, fixed = TRUE)) stop("Quedan marcas de ecuación sin sustituir en ", basename(f))
    con <- file(f, open = "wb"); writeBin(charToRaw(enc2utf8(xml)), con); close(con)
  }
  fich <- list.files(dir, recursive = TRUE, all.files = TRUE)
  fich <- c("[Content_Types].xml", setdiff(fich, "[Content_Types].xml"))
  if (file.exists(destino)) file.remove(destino)
  zip::zip(file.path(normalizePath(dirname(destino)), basename(destino)), files = fich, root = dir)
  unlink(dir, recursive = TRUE)
}

parrafo <- function(texto, size = 18, color = ID$texto, bold = FALSE, italic = FALSE,
                    font = "Segoe UI", align = "left", espacio = 6) {
  do.call(fpar, c(runs_texto(texto, size, color, bold, italic, font),
                  list(fp_p = fp_par(text.align = align, padding.bottom = espacio))))
}

poner_texto <- function(x, parrafos, left, top, width, height) {
  if (inherits(parrafos, "fpar")) parrafos <- list(parrafos)
  ph_with(x, value = do.call(block_list, parrafos),
          location = ph_location(left = left, top = top, width = width, height = height))
}

# Rectángulo de color sólido. officer 0.7.3 IGNORA `bg` de ph_location en cajas de texto
# (el fondo no se dibuja y el texto blanco queda invisible), así que el rectángulo se
# dibuja como una imagen PNG de un solo color, estirada al tamaño de la caja.
.png_color <- new.env()
png_color <- function(fill) {
  if (is.null(.png_color[[fill]])) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 8, height = 8)
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::rect(0, 0, 1, 1, col = fill, border = NA)
    grDevices::dev.off()
    .png_color[[fill]] <- f
  }
  .png_color[[fill]]
}

rect <- function(x, left, top, width, height, fill) {
  ph_with(x, external_img(png_color(fill), width = width, height = height),
          location = ph_location(left = left, top = top, width = width, height = height),
          use_loc_size = TRUE)
}

vinetas <- function(x, items, left, top, width, height, size = 20, color = ID$texto) {
  pars <- lapply(items, function(t) do.call(fpar, c(list(ftext("▸  ", estilo(size, ID$oro, bold = TRUE))),
                                                      runs_texto(t, size, color),
                                                      list(fp_p = fp_par(padding.bottom = 12)))))
  ph_with(x, value = do.call(block_list, pars),
          location = ph_location(left = left, top = top, width = width, height = height))
}

# Fórmula destacada en caja gris con filete dorado: `latex` es una ecuación LaTeX (editor de ecuaciones)
formula_caja <- function(x, latex, left, top, width, height = 1.0, size = 28) {
  x <- rect(x, left, top, width, height, ID$suave)
  x <- rect(x, left, top, 0.08, height, ID$oro)
  poner_texto(x, parrafo(M(latex, display = TRUE), size, ID$navy, font = "Cambria Math", align = "center"),
              left + 0.15, top + 0.18, width - 0.3, height - 0.25)
}

# Caja de dato: fondo suave, filete de color arriba, título, valor grande y texto
caja_dato <- function(x, left, top, width, height, titulo, valor, texto = NULL, col = ID$navy, size_valor = 30) {
  x <- rect(x, left, top, width, height, ID$suave)
  x <- rect(x, left, top, width, 0.08, col)
  pars <- list(parrafo(titulo, 16, ID$navy, bold = TRUE), parrafo(valor, size_valor, col, bold = TRUE, font = "Georgia"))
  if (!is.null(texto)) pars <- c(pars, list(parrafo(texto, 14, ID$texto)))
  poner_texto(x, pars, left + 0.25, top + 0.25, width - 0.5, height - 0.35)
}

# Caja destacada en marino (texto blanco) o suave (texto oscuro, filete dorado a la izquierda)
caja_texto <- function(x, left, top, width, height, titulo, texto, oscura = FALSE, size = 16) {
  x <- rect(x, left, top, width, height, if (oscura) ID$navy else ID$suave)
  if (!oscura) x <- rect(x, left, top, 0.07, height, ID$oro)
  poner_texto(x, list(parrafo(titulo, size + 3, if (oscura) ID$oro else ID$navy, bold = TRUE, font = "Georgia"),
                      parrafo(texto, size, if (oscura) ID$blanco else ID$texto)),
              left + 0.25, top + 0.2, width - 0.45, height - 0.3)
}

# ── Imágenes ───────────────────────────────────────────────────────
imagen_ggplot <- function(x, grafico, left, top, width, height, dpi = 200) {
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, grafico, width = width, height = height, dpi = dpi, bg = "white")
  ph_with(x, external_img(f, width = width, height = height),
          location = ph_location(left = left, top = top, width = width, height = height), use_loc_size = TRUE)
}

# Logotipo CdeA, cuadrado de `lado` pulgadas. Discreto: pie de página, portada y separadores.
poner_logo <- function(x, left, top, lado) {
  if (!file.exists(LOGO)) return(x)
  ph_with(x, external_img(LOGO, width = lado, height = lado),
          location = ph_location(left = left, top = top, width = lado, height = lado), use_loc_size = TRUE)
}

# Dimensiones de un JPEG (para no deformar la foto), sin paquetes externos
dim_jpeg <- function(f) {
  b <- readBin(f, "raw", file.info(f)$size)
  i <- 3L
  while (i + 8L <= length(b)) {
    if (b[i] != as.raw(0xFF)) { i <- i + 1L; next }
    marca <- as.integer(b[i + 1L])
    largo <- as.integer(b[i + 2L]) * 256L + as.integer(b[i + 3L])
    if (marca %in% c(0xC0L, 0xC1L, 0xC2L)) {
      return(c(ancho = as.integer(b[i + 7L]) * 256L + as.integer(b[i + 8L]),
               alto  = as.integer(b[i + 5L]) * 256L + as.integer(b[i + 6L])))
    }
    i <- i + 2L + largo
  }
  c(ancho = 3, alto = 4)
}

# ── Tablas ─────────────────────────────────────────────────────────
# Columnas de texto (izquierda) en `texto`; el resto, a la derecha. Las de código (`mono`), en Consolas.
tabla_pptx <- function(df, col_names, digits = 2, size = 14, mono = integer(0), texto = 1L) {
  ft <- flextable(df) |>
    set_header_labels(values = setNames(as.list(col_names), names(df))) |>
    colformat_double(digits = digits, big.mark = tr(".", ","), decimal.mark = tr(",", ".")) |>
    colformat_int(big.mark = "") |>
    font(fontname = "Segoe UI", part = "all") |>
    fontsize(size = size, part = "all") |>
    bg(bg = ID$navy, part = "header") |>
    color(color = "white", part = "header") |>
    bold(part = "header") |>
    color(color = ID$texto, part = "body") |>
    border_remove() |>
    hline_bottom(border = fp_border(color = ID$oro, width = 2), part = "body") |>
    align(align = "right", part = "all") |>
    align(j = texto, align = "left", part = "all") |>
    padding(padding.top = 5, padding.bottom = 5, padding.left = 8, padding.right = 8, part = "all")
  if (nrow(df) >= 2) ft <- bg(ft, i = seq(2, nrow(df), by = 2), bg = ID$suave, part = "body")
  if (length(mono)) {
    ft <- font(ft, j = mono, fontname = "Consolas", part = "body")
    ft <- align(ft, j = mono, align = "left", part = "all")   # el código se lee alineado a la izquierda
  }
  autofit(ft)
}

# Coloca la tabla sin salirse de la diapositiva: se ajusta al ancho disponible
poner_tabla <- function(x, ft, left, top, max_ancho = ANCHO - left - 0.6) {
  ft <- fit_to_width(ft, max_width = max_ancho)
  ph_with(x, value = ft, location = ph_location(left = left, top = top))
}

# ── Elementos comunes ──────────────────────────────────────────────
ASIG <- function() tr("Econometría", "Econometrics")

# Pie: filete, autor a la izquierda, tema y página a la derecha y, al final, el logotipo pequeño
pie <- function(x, etiqueta_tema) {
  n <- length(x)
  lado <- 0.38
  x <- rect(x, 0.6, 6.95, ANCHO - 1.2, 0.015, ID$linea)
  x <- poner_texto(x, parrafo(paste("Carlos de Anta ·", ASIG()), 10, ID$gris_med), 0.6, 7.0, 7, 0.35)
  x <- poner_texto(x, parrafo(paste0(etiqueta_tema, "  ·  ", n), 10, ID$gris_med, align = "right"),
                   ANCHO - 0.6 - lado - 4.1, 7.0, 4, 0.35)
  poner_logo(x, ANCHO - 0.6 - lado, 6.99, lado)
}

cabecera <- function(x, antetitulo, titular) {
  x <- rect(x, 0.6, 0.5, 0.9, 0.05, ID$oro)
  x <- poner_texto(x, parrafo(toupper(antetitulo), 12, ID$oro, bold = TRUE), 0.6, 0.62, 12, 0.4)
  poner_texto(x, parrafo(titular, 28, ID$navy, bold = TRUE, font = "Georgia"), 0.6, 0.98, 12.1, 1.0)
}

# ── Tipos de diapositiva ───────────────────────────────────────────
diapo_portada <- function(x, tema_n, titulo, subtitulo) {
  x <- diapo_nueva(x)
  x <- rect(x, 0, 0, ANCHO, ALTO, ID$navy)
  x <- rect(x, 0, 0, 0.22, ALTO, ID$oro)
  x <- poner_texto(x, parrafo(toupper(ASIG()), 16, ID$oro, bold = TRUE), 1.0, 1.35, 11, 0.5)
  x <- rect(x, 1.0, 1.95, 2.2, 0.045, ID$oro)
  x <- poner_texto(x, parrafo(paste(tr("Tema", "Chapter"), tema_n), 22, ID$oro_cl, font = "Georgia"), 1.0, 2.2, 11, 0.6)
  x <- poner_texto(x, parrafo(titulo, 40, ID$blanco, bold = TRUE, font = "Georgia"), 1.0, 2.8, 10.2, 1.8)
  x <- poner_texto(x, parrafo(subtitulo, 18, ID$linea, italic = TRUE), 1.0, 4.6, 10.2, 0.8)
  x <- poner_texto(x, parrafo("Carlos de Anta Puig", 18, ID$blanco, bold = TRUE), 1.0, 5.6, 8, 0.45)
  x <- poner_texto(x, parrafo(tr("Manual «Econometría: manual práctico con R» · Carlos de Anta Puig (2026)",
                                 "«Econometrics: A Practical Guide with R» textbook · Carlos de Anta Puig (2026)"),
                              11, ID$gris_med), 1.0, 6.75, 10, 0.4)
  poner_logo(x, ANCHO - 1.75, ALTO - 1.8, 1.15)
}

diapo_contenido <- function(x, antetitulo, titular, etiqueta_tema) {
  x <- diapo_nueva(x)
  x <- cabecera(x, antetitulo, titular)
  pie(x, etiqueta_tema)
}

diapo_indice <- function(x, secciones, etiqueta_tema) {
  x <- diapo_contenido(x, tr("Índice", "Contents"), tr("Hoja de ruta del tema", "Chapter roadmap"), etiqueta_tema)
  # Hasta 6 secciones con el espaciado original; con más, filas más juntas para no invadir el pie
  n <- nrow(secciones)
  paso <- if (n <= 6) 0.74 else 0.6
  tam  <- if (n <= 6) c(22, 20) else c(19, 17)
  for (k in seq_len(n)) {
    top <- 2.05 + (k - 1) * paso
    x <- poner_texto(x, parrafo(secciones$num[k], tam[1], ID$oro, bold = TRUE, font = "Georgia"), 1.0, top, 1.3, paso - 0.1)
    x <- poner_texto(x, parrafo(secciones$titulo[k], tam[2], ID$texto), 2.4, top + 0.03, 9.8, paso - 0.1)
    x <- rect(x, 2.4, top + paso - 0.1, 9.6, 0.012, ID$linea)
  }
  x
}

diapo_objetivos <- function(x, objetivos, etiqueta_tema) {
  x <- diapo_contenido(x, tr("Objetivos", "Objectives"), tr("Qué perseguimos en este tema", "What this chapter sets out to do"), etiqueta_tema)
  ancho <- 3.85; hueco <- 0.3
  for (k in seq_along(objetivos)) {
    left <- 0.6 + (k - 1) * (ancho + hueco); top <- 2.2; alto <- 4.4
    x <- rect(x, left, top, ancho, alto, ID$suave)
    x <- rect(x, left, top, ancho, 0.07, ID$navy)
    x <- poner_texto(x, parrafo(sprintf("%02d", k), 32, ID$oro, bold = TRUE, font = "Georgia"), left + 0.3, top + 0.3, ancho - 0.6, 0.8)
    x <- poner_texto(x, parrafo(objetivos[[k]][1], 19, ID$navy, bold = TRUE), left + 0.3, top + 1.15, ancho - 0.6, 1.0)
    x <- poner_texto(x, parrafo(objetivos[[k]][2], 15, ID$gris), left + 0.3, top + 2.15, ancho - 0.6, 2.1)
  }
  x
}

diapo_bloque <- function(x, numero, titulo, items) {
  x <- diapo_nueva(x)
  x <- rect(x, 0, 0, ANCHO, ALTO, ID$navy)
  x <- poner_texto(x, parrafo(sprintf("%02d", numero), 96, ID$oro, bold = TRUE, font = "Georgia"), 1.0, 1.2, 3.8, 1.7)
  x <- poner_texto(x, parrafo(paste(tr("BLOQUE", "BLOCK"), numero), 16, ID$oro_cl, bold = TRUE), 1.0, 3.0, 5, 0.5)
  x <- poner_texto(x, parrafo(titulo, 38, ID$blanco, bold = TRUE, font = "Georgia"), 1.0, 3.5, 5.6, 2.6)
  x <- rect(x, 7.1, 1.6, 0.035, 4.3, ID$oro)
  x <- vinetas(x, items, 7.6, 2.2, 5.2, 3.8, size = 22, color = ID$blanco)
  poner_logo(x, ANCHO - 1.35, ALTO - 1.35, 0.8)
}

# Cierre: ideas clave, qué practicar y la foto del autor. Sin el correo del despacho (lección A11 de Financial Economics).
diapo_cierre <- function(x, ideas, practica, etiqueta_tema, foto = "presentaciones/_identidad/CdeA_PRO_Foto_01.jpg") {
  x <- diapo_contenido(x, tr("Para llevarse", "Key takeaways"), tr("Ideas clave del tema", "Key ideas of the chapter"), etiqueta_tema)
  x <- vinetas(x, ideas, 0.6, 2.05, 7.6, 4.7, size = 17)
  x <- rect(x, 8.6, 2.05, 4.13, 4.65, ID$navy)
  x <- poner_texto(x, list(parrafo(tr("PRACTICA", "PRACTISE"), 14, ID$oro, bold = TRUE),
                           parrafo(practica, 15, ID$blanco)), 8.9, 2.3, 3.6, 2.6)
  d <- dim_jpeg(foto); alto_foto <- 1.45; ancho_foto <- alto_foto * d[["ancho"]] / d[["alto"]]
  x <- ph_with(x, external_img(foto, width = ancho_foto, height = alto_foto),
               location = ph_location(left = 8.9, top = 5.0, width = ancho_foto, height = alto_foto), use_loc_size = TRUE)
  poner_texto(x, list(parrafo("Carlos de Anta Puig", 14, ID$blanco, bold = TRUE, espacio = 2),
                      parrafo(tr("Economista", "Economist"), 11, ID$oro_cl)),
              8.9 + ancho_foto + 0.2, 5.3, 12.73 - (8.9 + ancho_foto + 0.2) - 0.1, 1.0)
}

# Guarda la presentación del idioma en curso
guardar_pptx <- function(x, nombre_es, nombre_en) {
  destino <- file.path("presentaciones", idioma, tr(nombre_es, nombre_en))
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = ".pptx")
  print(x, target = tmp)
  insertar_ecuaciones(tmp, destino)
  cat("Generado:", destino, "·", length(x), tr("diapositivas", "slides"), "·", length(EQS$latex), tr("ecuaciones", "equations"), "\n")
}
