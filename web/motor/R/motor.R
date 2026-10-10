# ══════════════════════════════════════════════════════════════════
# Motor común de las webs docentes (identidad CdeA)
#
# NO SE EDITA EN LAS ASIGNATURAS. La carpeta web/motor/ de cada asignatura es una copia del repositorio webMotor
# (/home/carlos/900·RESEARCH/MANUALES/webMotor) y se sobrescribe con
#     bash /home/carlos/900·RESEARCH/MANUALES/webMotor/actualizar.sh <carpeta de la asignatura>
# Las mejoras se hacen en webMotor y se reparten a todas las asignaturas.
#
# La asignatura, en web/build_web.R, define ANTES de cargar el motor:
#   CURSO <- list(
#     web          = "financialeconomics",            # subdominio y carpeta /srv/webs/<web>
#     nombre       = "Financial Economics",           # título de las páginas
#     marca        = "Financial <b>Economics</b>",    # nombre en la barra superior (HTML)
#     autor        = "Carlos de Anta Puig",
#     clave_idioma = "fe-lang",                       # localStorage: último idioma del alumno
#     dias         = c(lunes = 0L, viernes = 4L),     # días de clase (0 = lunes … 4 = viernes): horarios de los grupos
#     logo         = "presentaciones/_identidad/CdeA_logo.png",
#     nombre_es, marca_es                             # opcionales: nombre y marca en la parte española de alumnos
#     menu_alumnos,                                   # opcional: sustituye al menú de alumnos por defecto
#     pie = c(en = "…", es = "…"),                    # opcional: texto del pie de la parte de alumnos
#     idioma_profesor = "es",                         # opcional: plan del profesor con el texto traducido (por defecto, el de los alumnos en inglés)
#     etiqueta_sesion = function(x, L) …,             # opcional: nombre de la sesión («Session A» por defecto)
#     etiqueta_corta = function(x, L) …)              # opcional: nombre corto en el calendario (el tipo por defecto)
# Una sesión puede llevar `codigo` (una letra mayúscula) si su `tipo` no es una letra: va en los identificadores de bloque.
# y, antes de construir las semanas, `semanas` (plan_semanas()), ES_PLAN (traducciones) y
# material_html(code, zona, temas, L) (materiales propios de la asignatura).
# Cada sesión del plan lleva `tipo` (A, B…), `dia` (0 = lunes) y `bloques` (ini, fin, actividad, detalle, nota);
# cada semana, `inicio` (su lunes).
# ══════════════════════════════════════════════════════════════════
MOTOR_DIR <- "web/motor"
MOTOR_VERSION <- readLines(file.path(MOTOR_DIR, "VERSION"), n = 1, warn = FALSE)
local({
  falta <- setdiff(c("web", "nombre", "marca", "autor", "clave_idioma", "dias", "logo"), names(CURSO))
  if (length(falta)) stop("Falta en CURSO (web/build_web.R): ", paste(falta, collapse = ", "))
})

OUT <- "web/_site"
PUB <- file.path(OUT, "public")
STORE <- file.path(OUT, "store")

# Borra el sitio anterior y copia los recursos comunes (estilos, scripts, banderas y logo).
# Si la asignatura tiene web/assets/curso.css, se añade al final de los estilos comunes.
iniciar_sitio <- function() {
  unlink(OUT, recursive = TRUE)
  dir.create(file.path(PUB, "downloads"), recursive = TRUE)
  dir.create(STORE, recursive = TRUE)
  dir.create(file.path(PUB, "assets"), showWarnings = FALSE)
  for (f in c("style.css", "release.js", "profesor.js", "flag-gb.svg", "flag-es.svg")) copiar(file.path(MOTOR_DIR, "assets", f), file.path(PUB, "assets", f))
  if (file.exists("web/assets/curso.css")) cat(readLines("web/assets/curso.css", encoding = "UTF-8"), file = file.path(PUB, "assets", "style.css"), sep = "\n", append = TRUE)
  copiar(CURSO$logo, file.path(PUB, "assets", "logo.png"))
  escribir("released.json", '{"released": [], "updated": null}')
  # Versión en los enlaces de estilos y scripts: el proxy guarda /assets/ en caché unas horas y serviría la copia vieja
  ASSET_V <<- substr(unname(tools::md5sum(file.path(PUB, "assets", "style.css"))), 1, 8)
  JS_V <<- substr(paste(unname(tools::md5sum(file.path(PUB, "assets", c("release.js", "profesor.js")))), collapse = ""), 1, 8)
}
ASSET_V <- ""; JS_V <- ""

# ── Opciones de la asignatura (todas con valor por defecto) ─────────
op <- function(nombre, defecto) if (is.null(CURSO[[nombre]])) defecto else CURSO[[nombre]]
# Idioma del texto del plan en la zona del profesor ("en": el que ven los alumnos; "es": la traducción)
idioma_profesor <- function() op("idioma_profesor", "en")
# Nombre de una sesión: en la cabecera de la sesión («Session A», «Weekly class · 60 min») y, corto, en el calendario
etiqueta_sesion <- function(x, L) op("etiqueta_sesion", function(x, L) paste(tx(L, "Session", "Sesión"), x$tipo))(x, L)
etiqueta_corta <- function(x, L) op("etiqueta_corta", function(x, L) x$tipo)(x, L)
# Código de la sesión en los identificadores de bloque (una letra mayúscula: A, B, L, S…)
codigo_sesion <- function(x) if (is.null(x$codigo)) x$tipo else x$codigo

# ── Idiomas de la parte de alumnos ─────────────────────────────────
# Inglés por defecto (raíz del sitio) y versión española completa en /es/. Las banderas del menú llevan a la misma
# página en el otro idioma. El texto del plan semanal se traduce con ES_PLAN (web/plan_semanas_es.R).
IDIOMAS <- c("en", "es")
tx <- function(L, en, es) if (L == "es") es else en
u <- function(L, p) if (L == "es") paste0("/es", p) else p
tr_plan <- function(x, L) {
  if (L == "en" || !length(x)) return(x)
  y <- ifelse(nzchar(x), ES_PLAN[x], "")
  if (anyNA(y)) stop("Falta la traducción al español de: ", paste(shQuote(x[is.na(y)]), collapse = ", "))
  unname(y)
}
# Textos de un plan semanal, en orden (para comprobar y emparejar traducciones)
textos_plan <- function(semanas) unlist(lapply(semanas, function(s) c(s$bloque, s$capitulos, s$titulo, s$resumen, s$hitos, s$aviso,
  unlist(lapply(s$sesiones, function(x) c(x$titulo, x$preparar, x$bloques$actividad, x$bloques$detalle))))))
# Diccionario ES_PLAN a partir de dos planes con la misma estructura (inglés y español escritos por separado).
# Se detiene si la estructura no coincide o si un mismo texto inglés tiene dos traducciones distintas.
diccionario_plan <- function(en, es, extra_en = character(), extra_es = character()) {
  a <- c(extra_en, textos_plan(en)); b <- c(extra_es, textos_plan(es))
  if (length(a) != length(b)) stop("Los planes en inglés y en español no tienen la misma estructura (", length(a), " y ", length(b), " textos)")
  ok <- nzchar(a); a <- a[ok]; b <- b[ok]
  par <- unique(data.frame(en = a, es = b, stringsAsFactors = FALSE))
  dup <- par$en[duplicated(par$en)]
  if (length(dup)) stop("Textos con dos traducciones distintas (unifícalas en el plan en español):\n",
                        paste0("  «", dup, "» → ", vapply(dup, function(x) paste(shQuote(par$es[par$en == x]), collapse = " / "), ""), collapse = "\n"))
  stats::setNames(par$es, par$en)
}
# Detiene la construcción si falta alguna traducción del plan y lista las entradas que hay que añadir
comprobar_traducciones <- function(semanas, extra = character()) {
  textos <- unique(c(extra, textos_plan(semanas)))
  textos <- textos[nzchar(textos)]
  falta <- setdiff(textos, names(ES_PLAN))
  if (length(falta)) stop("Faltan ", length(falta), " traducciones en web/plan_semanas_es.R:\n", paste0('  "', falta, '" = "",', collapse = "\n"))
  sobra <- setdiff(names(ES_PLAN), textos)
  if (length(sobra)) message("Aviso: traducciones sin uso en web/plan_semanas_es.R: ", paste(shQuote(sobra), collapse = ", "))
}

h <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}
tam <- function(b) if (b >= 1048576) sprintf("%.1f MB", b / 1048576) else sprintf("%.0f KB", max(1, b / 1024))
escribir <- function(ruta, html) {
  f <- file.path(PUB, ruta)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  con <- file(f, open = "wb"); writeBin(charToRaw(enc2utf8(html)), con); close(con)
}
tkey <- function(nn) paste0("T", nn)
tam_dir <- function(d) sum(file.size(list.files(d, recursive = TRUE, full.names = TRUE))) / 1048576

# ── Registro de ficheros ───────────────────────────────────────────
# zona "store": se sirve en /downloads/<clave>/… cuando está liberado y siempre en /profesor/store/<clave>/…
# zona "prof" : solo en la zona del profesor (o en /recursos/, siempre visible para los alumnos)
REG <- new.env()
copiar <- function(origen, destino) {
  if (length(origen) != 1 || !file.exists(origen)) stop("Falta el fichero: ", origen)
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(origen, destino, overwrite = TRUE)) stop("No se pudo copiar ", origen)
}
guardar <- function(clave, origen, key, nombre = basename(origen)) {
  destino <- file.path(STORE, key, nombre)
  if (!identical(normalizePath(origen, mustWork = FALSE), normalizePath(destino, mustWork = FALSE))) copiar(origen, destino)
  assign(clave, list(zona = "store", key = key, rel = paste0(key, "/", nombre), bytes = file.size(destino), ext = toupper(tools::file_ext(destino))), envir = REG)
}
prof <- function(clave, origen, destino) {
  copiar(origen, file.path(PUB, destino))
  assign(clave, list(zona = "prof", href = paste0("/", destino), bytes = file.size(origen), ext = toupper(tools::file_ext(origen))), envir = REG)
}
reg <- function(clave) { r <- REG[[clave]]; if (is.null(r)) stop("Material no registrado: ", clave); r }
href <- function(r, zona) {
  if (r$zona == "prof") return(r$href)
  paste0(if (zona == "public") "/downloads/" else "/profesor/store/", paste(vapply(strsplit(r$rel, "/")[[1]], utils::URLencode, "", reserved = TRUE), collapse = "/"))
}

# ── PDF por capítulos (el manual y el cuaderno nunca se publican completos) ──
n_paginas <- function(pdf) as.integer(sub("^Pages:\\s*", "", grep("^Pages:", system2("pdfinfo", shQuote(pdf), stdout = TRUE), value = TRUE)))
primeras_lineas <- function(pdf) vapply(seq_len(n_paginas(pdf)), function(p) {
  x <- system2("pdftotext", c("-f", p, "-l", p, "-layout", shQuote(pdf), "-"), stdout = TRUE)
  x <- trimws(gsub("\\s+", " ", x)); x <- x[nzchar(x)]
  if (length(x)) x[1] else ""
}, "")
inicios <- function(lineas, patron, n = 10) {
  s <- vapply(seq_len(n), function(k) { m <- which(grepl(sprintf(patron, k), lineas)); if (!length(m)) stop("No encuentro el inicio del capítulo ", k, " con «", sprintf(patron, k), "»"); max(m) }, 0L)
  if (is.unsorted(s, strictly = TRUE)) stop("Inicios de capítulo desordenados: ", paste(s, collapse = " "))
  s
}
extraer <- function(pdf, desde, hasta, destino) {
  tmp <- tempfile("pags_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE))
  system2("pdfseparate", c("-f", desde, "-l", hasta, shQuote(pdf), shQuote(file.path(tmp, "p-%04d.pdf"))))
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  unido <- file.path(tmp, "unido.pdf")
  if (system2("pdfunite", c(shQuote(sort(list.files(tmp, pattern = "^p-", full.names = TRUE))), shQuote(unido))) != 0 || !file.exists(unido)) stop("No se pudo crear ", destino)
  # pdfunite repite las fuentes en cada página: Ghostscript las vuelve a agrupar (calidad de impresión)
  gs <- system2("gs", c("-q", "-dNOPAUSE", "-dBATCH", "-dSAFER", "-sDEVICE=pdfwrite", "-dCompatibilityLevel=1.6", "-dPDFSETTINGS=/printer",
                        paste0("-sOutputFile=", shQuote(destino)), shQuote(unido)), stdout = FALSE, stderr = FALSE)
  if (gs != 0 || !file.exists(destino) || file.size(destino) >= file.size(unido)) file.copy(unido, destino, overwrite = TRUE)
}
# Corta un PDF en `n` capítulos (patrón de la primera línea de cada uno) y los guarda en el almacén de su tema
cortar <- function(pdf, patron, fin_patron, nombre_fmt, clave_fmt, n = 10) {
  l <- primeras_lineas(pdf); s <- inicios(l, patron, n)
  fin_ult <- if (!is.null(fin_patron) && any(grepl(fin_patron, l))) max(which(grepl(fin_patron, l))) - 1 else length(l)
  e <- c(s[-1] - 1, fin_ult)
  for (k in seq_len(n)) {
    nn <- sprintf("%02d", k); destino <- file.path(STORE, tkey(nn), sprintf(nombre_fmt, k))
    extraer(pdf, s[k], e[k], destino)
    guardar(sprintf(clave_fmt, nn), destino, tkey(nn))
  }
  invisible(rbind(inicio = s, fin = e))
}

# ── Piezas HTML ────────────────────────────────────────────────────
chip <- function(clave, etiqueta, zona = "public", meta = "") {
  r <- reg(clave)
  sprintf('<a class="dl" href="%s"><span class="dl-l">%s</span><span class="dl-m">%s%s · %s</span></a>', h(href(r, zona)), h(etiqueta), meta, r$ext, tam(r$bytes))
}
# Idiomas adicionales para los alumnos de otros países (CURSO$idiomas_extra, p. ej. c("fr", "it", "pt")):
# si existe el material en ese idioma, sale un botón más junto al de ES/EN; si no, no se nota.
NOMBRE_IDIOMA <- c(es = "Versión en español", en = "English version", fr = "Version française",
                   it = "Versione italiana", pt = "Versão em português", de = "Deutsche Fassung")
boton_idioma <- function(clave, l, zona) if (is.null(REG[[clave]])) "" else
  sprintf('<a class="alt" href="%s" title="%s" lang="%s">%s</a>', h(href(reg(clave), zona)), h(NOMBRE_IDIOMA[[l]]), l, toupper(l))
# Fichero en el idioma de la página, botón con el del otro idioma oficial y, si los hay, los idiomas adicionales
par_idiomas <- function(base, etiqueta, zona = "public", meta = "", L = "en") {
  otro <- setdiff(IDIOMAS, L)
  extra <- paste(vapply(op("idiomas_extra", character()), function(l) boton_idioma(paste0(base, ":", l), l, zona), ""), collapse = "")
  sprintf('<span class="pair">%s%s%s</span>', chip(paste0(base, ":", L), etiqueta, zona, meta), boton_idioma(paste0(base, ":", otro), otro, zona), extra)
}
liberable <- function(keys, contenido, pendiente, zona = "public", en_linea = FALSE) {
  if (zona != "public") return(contenido)
  tag <- if (en_linea) "span" else "div"
  paste0(sprintf('<%s data-release="%s" hidden>%s</%s>', tag, paste(keys, collapse = " "), contenido, tag),
         if (nzchar(pendiente)) sprintf('<%s class="pending" data-pending="%s">%s</%s>', tag, paste(keys, collapse = " "), h(pendiente), tag) else "")
}

# ── Fechas (nombres escritos a mano: el servidor tiene la configuración regional en español) ──
meses_es <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
meses_largos <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
dias_es <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
dias_en <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
rango <- function(d, L = "en") {
  f <- d + 4; mm <- if (L == "en") month.abb else meses_es
  di <- as.integer(format(d, "%d")); df <- as.integer(format(f, "%d"))
  mi <- mm[as.integer(format(d, "%m"))]; mf <- mm[as.integer(format(f, "%m"))]
  if (mi == mf) sprintf("%d–%d %s %s", di, df, mf, format(f, "%Y")) else sprintf("%d %s – %d %s %s", di, mi, df, mf, format(f, "%Y"))
}
fecha_es <- function(d) sprintf("%d de %s de %s", as.integer(format(d, "%d")), meses_largos[as.integer(format(d, "%m"))], format(d, "%Y"))
# Día y fecha de una sesión: `dia` son los días desde el lunes de su semana
fecha_sesion <- function(s, x, L) {
  d <- s$inicio + x$dia
  dd <- as.integer(format(d, "%d")); mm <- as.integer(format(d, "%m"))
  if (L == "es") sprintf("%s %d de %s", dias_es[x$dia + 1], dd, meses_es[mm]) else sprintf("%s %d %s", dias_en[x$dia + 1], dd, month.abb[mm])
}

# En la zona del profesor cada bloque lleva su identificador (sid-número) para calcular la hora real según el grupo
# y marcarlo como dado (assets/profesor.js). `dia`: días desde el lunes (0 = lunes, 4 = viernes).
tabla_bloques <- function(bl, profesor = FALSE, L = "en", sid = "", dia = 0L) {
  filas <- vapply(seq_len(nrow(bl)), function(j) {
    x <- bl[j, ]
    es_pausa <- x$actividad == "Break"
    if (profesor)
      return(sprintf('<tr data-bloque="%s-%d" data-ini="%d" data-fin="%d" data-dia="%d"><td class="hora"></td><td class="t">%d–%d</td><td class="blk">%s</td><td>%s</td><td class="nota">%s</td><td class="dado"><input type="checkbox" class="hecho" disabled aria-label="Bloque dado"></td></tr>',
                     sid, j - 1, x$ini, x$fin, dia, x$ini, x$fin, h(if (es_pausa) "Pausa" else tr_plan(x$actividad, L)), h(tr_plan(x$detalle, L)), h(x$nota)))
    sprintf('<tr%s><td class="t">%d–%d</td><td class="blk">%s</td><td>%s</td></tr>',
            if (es_pausa) ' class="brk"' else "", x$ini, x$fin, h(tr_plan(x$actividad, L)), h(tr_plan(x$detalle, L)))
  }, "")
  cab <- if (profesor) "<tr><th>Hora</th><th>Min</th><th>Bloque</th><th>Qué se hace (texto que ve el alumno)</th><th>Nota docente</th><th>Dado</th></tr>"
         else tx(L, "<tr><th>Min</th><th>Block</th><th>What we do</th></tr>", "<tr><th>Min</th><th>Bloque</th><th>Qué hacemos</th></tr>")
  sprintf('<div class="tbl"><table class="agenda"><thead>%s</thead><tbody>%s</tbody></table></div>', cab, paste(filas, collapse = ""))
}

# ── Página completa ────────────────────────────────────────────────
# En la parte de alumnos, `ruta` que empieza por "es/" es la versión española y `actual` es el identificador inglés
# de la sección (Home, Schedule, Materials, Activities). CURSO$menu_alumnos puede sustituir al menú por defecto:
# lista de c(id, texto_en, texto_es, ruta) (ruta sin /es; la del profesor, absoluta).
MENU_PROFESOR <- list(c("Inicio", "/profesor/"), c("Liberar contenidos", "/profesor/liberar.html"), c("Acceso de alumnos", "/profesor/acceso.html"),
                      c("Clases", "/profesor/#clases"), c("Evaluaciones", "/profesor/#evaluaciones"), c("Grupos y avance", "/profesor/grupos.html"),
                      c("Plan docente", "/profesor/plan-docente.html"), c("Web de alumnos", "/home.html"))
menu_profesor <- function() if (isTRUE(CURSO$ingles)) append(MENU_PROFESOR, list(c("Inglés de clase", "/profesor/ingles.html")), after = 6) else MENU_PROFESOR
MENU_ALUMNOS <- list(c("Home", "Home", "Inicio", "/home.html"), c("Schedule", "Schedule", "Calendario", "/schedule.html"),
                     c("Materials", "Materials", "Materiales", "/materials.html"), c("Activities", "Activities", "Actividades", "/activities.html"))
pagina <- function(ruta, titulo, cuerpo, zona = "public", actual = "") {
  prof_z <- zona == "teacher"
  L <- if (!prof_z && startsWith(ruta, "es/")) "es" else "en"
  menu_al <- if (is.null(CURSO$menu_alumnos)) MENU_ALUMNOS else CURSO$menu_alumnos
  items <- if (prof_z) lapply(menu_profesor(), function(it) c(it[1], it))
           else c(lapply(menu_al, function(it) c(it[1], tx(L, it[2], it[3]), u(L, it[4]))), list(c("Teacher", tx(L, "Teacher", "Profesor"), "/profesor/")))
  nav <- paste(vapply(items, function(it) sprintf('<a href="%s"%s>%s</a>', it[3], if (it[1] == actual) ' aria-current="page"' else "", h(it[2])), ""), collapse = "")
  es_al <- if (prof_z) idioma_profesor() == "es" else L == "es"
  marca <- if (es_al && !is.null(CURSO$marca_es)) CURSO$marca_es else CURSO$marca
  nombre <- if (es_al && !is.null(CURSO$nombre_es)) CURSO$nombre_es else CURSO$nombre
  if (!prof_z) {
    base <- sub("^es/", "", ruta)
    bandera <- function(l, nombre, img) sprintf('<a href="%s" hreflang="%s" lang="%s" title="%s"%s><img src="/assets/%s" alt="%s" width="24" height="16"></a>',
      if (l == "es") paste0("/es/", base) else paste0("/", base), l, l, nombre, if (l == L) ' aria-current="true"' else "", img, nombre)
    nav <- paste0(nav, '<span class="lang">', bandera("en", "English", "flag-gb.svg"), bandera("es", "Español", "flag-es.svg"), '</span>')
  }
  html <- paste0(
    '<!doctype html><html lang="', if (prof_z) "es" else L, '"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    if (prof_z) '<meta name="robots" content="noindex,nofollow">' else "",
    '<title>', h(titulo), '</title><link rel="icon" href="/assets/logo.png"><link rel="stylesheet" href="/assets/style.css?v=', ASSET_V, '"></head><body',
    if (prof_z) ' class="teacher"' else "", '>',
    '<header class="top"><div class="bar"><a class="brand" href="', if (prof_z) "/profesor/" else u(L, "/home.html"), '"><img src="/assets/logo.png" alt="" width="36" height="36">',
    '<span>', marca, if (prof_z) '<em>Profesor</em>' else "", '</span></a><nav>', nav, '</nav></div></header>',
    '<main class="wrap">', cuerpo, '</main>',
    '<footer class="foot"><div class="bar"><span>', h(nombre), ' · ', h(CURSO$autor), '</span><span>',
    if (prof_z) "Zona del profesor · no distribuir" else h(op("pie", c(en = "Materials in English, with Spanish versions (ES)", es = "Materiales en español, con versión inglesa (EN)"))[[L]]), '</span></div></footer>',
    if (prof_z) paste0('<script src="/assets/profesor.js?v=', JS_V, '" defer></script>') else "",
    if (!prof_z) paste0('<script src="/assets/release.js?v=', JS_V, '" defer></script><script>try{localStorage.setItem("', CURSO$clave_idioma, '","', L, '")}catch(e){}</script>') else "",
    # Descargas, materiales y enlaces externos en VENTANA nueva (decisión del autor): se cierra y se vuelve a la web.
    # target="_blank" abre pestaña en casi todos los navegadores; la ventana se fuerza con window.open(…, "popup").
    # Con Ctrl, Mayús o botón central se respeta lo que pida el usuario. El menú sigue en la misma ventana.
    # Ojo: dentro del selector, comillas simples (\' en R). Unas comillas dobles cerraban la cadena de JavaScript y el
    # navegador descartaba el script entero sin avisar (2026-09-21).
    '<script>(function(){var sel="main a.dl, main a.alt, main a[href$=\'.pdf\'], main a[href^=\'http\'], main a[href*=\'/downloads/\'], main a[href*=\'/recursos/\'], main a[href*=\'/profesor/store/\']";',
    'document.addEventListener("click",function(e){var a=e.target.closest(sel);if(!a||a.hidden||!a.href||a.getAttribute("href")==="#"||e.defaultPrevented)return;',
    'if(a.origin===location.origin&&(/[.]html$/.test(a.pathname)||/[/]$/.test(a.pathname)))return;',   # páginas de la propia web: misma ventana
    'if(e.button!==0||e.ctrlKey||e.metaKey||e.shiftKey||e.altKey)return;e.preventDefault();',
    'var w=Math.min(1200,screen.availWidth-60),h=Math.max(500,screen.availHeight-80);',
    'var v=window.open(a.href,"_blank","popup=yes,width="+w+",height="+h+",left=30,top=20");if(v){v.opener=null;}else{location.href=a.href;}});})();</script>',
    '</body></html>')
  escribir(ruta, html)
}
ruta_l <- function(L, p) if (L == "es") file.path("es", p) else p

# ── Páginas de semana ──────────────────────────────────────────────
nav_semana <- function(i, prof_z, L = "en") {
  base <- if (prof_z) "/profesor/week/" else u(L, "/week/"); pal <- if (prof_z) "Semana" else tx(L, "Week", "Semana")
  sprintf('<nav class="pager">%s<a href="%s">%s</a>%s</nav>',
          if (i > 1) sprintf('<a href="%s%02d.html">← %s %d</a>', base, i - 1, pal, i - 1) else "<span></span>",
          if (prof_z) "/profesor/#clases" else u(L, "/schedule.html"), if (prof_z) "Todas las semanas" else tx(L, "All weeks", "Todas las semanas"),
          if (i < length(semanas)) sprintf('<a href="%s%02d.html">%s %d →</a>', base, i + 1, pal, i + 1) else "<span></span>")
}
# Sesiones de una semana: agenda por bloques, preparación, materiales (material_html de la asignatura) y enlaces de clase
sesiones_html <- function(s, prof_z, L, temas) {
  zona <- if (prof_z) "teacher" else "public"
  paste(vapply(s$sesiones, function(x) paste0(
    sprintf('<article class="session"><header><span class="tag">%s · %s</span>%s<h2>%s</h2></header>',
            h(etiqueta_sesion(x, if (prof_z) "es" else L)),
            h(fecha_sesion(s, x, if (prof_z) "es" else L)),
            if (prof_z) sprintf('<span class="horario" data-sesion-dia="%d" data-sesion-fin="%d"></span>', x$dia, max(x$bloques$fin)) else "",
            h(tr_plan(x$titulo, L))),
    tabla_bloques(x$bloques, profesor = prof_z, L = L, sid = sprintf("s%02d%s", s$n, codigo_sesion(x)), dia = x$dia),
    if (length(x$preparar)) sprintf('<p class="prep"><b>%s</b> %s</p>', tx(L, "Before class:", "Antes de clase:"), h(paste(tr_plan(x$preparar, L), collapse = " "))) else "",
    if (length(x$materiales)) sprintf('<div class="dls">%s</div>', paste(vapply(x$materiales, material_html, "", zona = zona, temas = temas, L = L), collapse = "")) else "",
    # Vídeos de la sesión (web/videos.R de la asignatura): videos_sesion(s, x) en el plan del profesor, con la nota
    # docente; videos_alumno(s, x, L) en la web de los alumnos, que solo los ve cuando el profesor los libera.
    if (prof_z && exists("videos_sesion", mode = "function")) videos_sesion(s, x)
    else if (!prof_z && exists("videos_alumno", mode = "function")) videos_alumno(s, x, L) else "",
    # Enlaces de clase: los rellena assets/release.js con /enlaces.json (el profesor los pone y los quita en /profesor/acceso.html)
    sprintf('<div class="dls"><a class="dl asistencia" data-enlace="asistencia" href="#" hidden><span class="dl-l">%s</span><span class="dl-m">%s</span></a><a class="dl asistencia" data-enlace="carpeta" href="#" hidden><span class="dl-l">%s</span><span class="dl-m">%s</span></a></div>',
            if (prof_z) "Hoja de asistencia" else tx(L, "Attendance sheet", "Hoja de asistencia"),
            if (prof_z) "Registro compartido de la clase" else tx(L, "Sign in for this session", "Apunta tu asistencia de esta sesión"),
            if (prof_z) "Carpeta compartida" else tx(L, "Shared folder", "Carpeta compartida"),
            if (prof_z) "Para subir el trabajo de clase" else tx(L, "Upload today's class work", "Sube aquí el trabajo de clase")),
    '</article>'), ""), collapse = "")
}
cabecera_semana <- function(s, prof_z, L) {
  paste0(sprintf('<p class="eyebrow">%s %d · %s · %s</p><h1>%s</h1><p class="lede">%s</p>', if (prof_z) "Semana" else tx(L, "Week", "Semana"), s$n,
                 rango(s$inicio, if (prof_z) "es" else L), h(tr_plan(s$capitulos, L)), h(tr_plan(s$titulo, L)), h(tr_plan(s$resumen, L))),
         if (nzchar(s$aviso)) sprintf('<p class="notice">%s</p>', h(tr_plan(s$aviso, L))) else "",
         if (length(s$hitos)) sprintf('<p>%s</p>', paste(sprintf('<span class="pill">%s</span>', h(tr_plan(s$hitos, L))), collapse = " ")) else "")
}

# Nombre de la asignatura en el idioma de la página
nombre_curso <- function(L = "en") if (L == "es" && !is.null(CURSO$nombre_es)) CURSO$nombre_es else CURSO$nombre

# Páginas de semana: la del alumno en cada idioma y el plan de clase del profesor (con grupos, horas y ticks).
#   temas_de(s)            → temas ("01", "02"…) cuya liberación muestra la semana; sin temas, la semana se ve siempre
#   pendiente(temas, L)    → texto mientras no se ha liberado
#   extra_profesor(s, temas) → HTML propio de la asignatura al final del plan del profesor (material, pruebas…)
paginas_semanas <- function(semanas, temas_de, pendiente, extra_profesor = function(s, temas) "") {
  LP <- idioma_profesor()
  for (i in seq_along(semanas)) {
    s <- semanas[[i]]; temas <- temas_de(s); keys <- tkey(temas)
    for (L in IDIOMAS) {
      ses <- sesiones_html(s, FALSE, L, temas)
      cuerpo <- paste0(nav_semana(i, FALSE, L), cabecera_semana(s, FALSE, L), if (length(keys)) liberable(keys, ses, pendiente(temas, L)) else ses, nav_semana(i, FALSE, L))
      pagina(ruta_l(L, sprintf("week/%02d.html", s$n)), sprintf("%s %d · %s · %s", tx(L, "Week", "Semana"), s$n, tr_plan(s$titulo, L), nombre_curso(L)), cuerpo, actual = "Schedule")
    }
    cuerpo <- paste0(nav_semana(i, TRUE), cabecera_semana(s, TRUE, LP),
      '<div id="grupos-sel" class="grupos-sel"></div>',
      if (length(keys)) sprintf('<p>Contenido liberable: <b>%s</b>. Los alumnos ven la agenda y los materiales cuando lo liberas en <a href="/profesor/liberar.html">Liberar contenidos</a>. Página del alumno: <a href="/week/%02d.html">inglés</a> · <a href="/es/week/%02d.html">español</a>.</p>', paste(keys, collapse = ", "), s$n, s$n)
      else sprintf('<p>Semana sin tema propio: los alumnos ven siempre su agenda. Página del alumno: <a href="/week/%02d.html">inglés</a> · <a href="/es/week/%02d.html">español</a>.</p>', s$n, s$n),
      sesiones_html(s, TRUE, LP, temas),
      extra_profesor(s, temas),
      nav_semana(i, TRUE))
    pagina(sprintf("profesor/week/%02d.html", s$n), sprintf("Semana %d · plan de clase", s$n), cuerpo, zona = "teacher")
  }
}

# Filas del calendario de alumnos. Una separación de más de 7 días entre semanas es una pausa (vacaciones):
# `pausa(L, desde)` da su texto (desde = lunes de la primera semana sin clase). `examen`: fechas y temas del final.
filas_calendario <- function(semanas, L, pausa, examen_fechas, examen_temas) {
  filas <- character(); bloque_prev <- ""; ini_prev <- NULL
  for (s in semanas) {
    if (s$bloque != bloque_prev) { filas <- c(filas, sprintf('<tr class="grp"><th colspan="4">%s %s</th></tr>', tx(L, "Block", "Bloque"), h(tr_plan(s$bloque, L)))); bloque_prev <- s$bloque }
    if (!is.null(ini_prev) && as.numeric(s$inicio - ini_prev) > 7) filas <- c(filas, sprintf('<tr class="gap"><td colspan="4">%s</td></tr>', pausa(L, ini_prev + 7)))
    ini_prev <- s$inicio
    ses <- paste(vapply(s$sesiones, function(x) sprintf('<span class="ses"><b>%s</b> %s</span>', h(etiqueta_corta(x, L)), h(tr_plan(x$titulo, L))), ""), collapse = "")
    hit <- paste(sprintf('<span class="pill">%s</span>', h(tr_plan(s$hitos, L))), collapse = "")
    url_s <- u(L, sprintf("/week/%02d.html", s$n))
    filas <- c(filas, sprintf('<tr><td class="wk"><a href="%s">%s %d</a><small>%s</small></td><td><a class="tt" href="%s">%s</a><small>%s</small></td><td>%s</td><td>%s</td></tr>',
                              url_s, tx(L, "Week", "Semana"), s$n, rango(s$inicio, L), url_s, h(tr_plan(s$titulo, L)), h(tr_plan(s$capitulos, L)), ses, hit))
  }
  c(filas, sprintf('<tr class="gap exam"><td colspan="4"><b>%s</b> · %s · %s</td></tr>', tx(L, "Final exam", "Examen final"), h(tr_plan(examen_fechas, L)), examen_temas))
}

# Recuadro «Esta semana» de la página de inicio: el HTML va donde quiera la asignatura y el script, al final
seccion_esta_semana <- function(L) paste0(
  '<section class="now" id="this-week" aria-live="polite"><p class="eyebrow">', tx(L, "This week", "Esta semana"), '</p><p><a href="', u(L, "/schedule.html"), '">',
  tx(L, "See the full schedule", "Ver el calendario completo"), '</a></p></section>')
js_esta_semana <- function(semanas, L) {
  js_semanas <- paste0("[", paste(vapply(semanas, function(s) sprintf('{"n":%d,"s":"%s","t":"%s","u":"%s"}', s$n, format(s$inicio),
    gsub('"', "'", tr_plan(s$titulo, L)), u(L, sprintf("/week/%02d.html", s$n))), ""), collapse = ","), "]")
  js_textos <- sprintf('{"wk":"%s","s1":"%s","s2":"%s","over":"%s","n1":"%s","n2":"%s","su":"%s"}', tx(L, "Week ", "Semana "),
    tx(L, "The course starts on ", "La asignatura empieza en la "), tx(L, "week 1", "semana 1"),
    tx(L, "Teaching weeks are over: good luck with the final exam.", "Han terminado las semanas de clase: suerte en el examen final."),
    tx(L, "No classes this week. ", "Esta semana no hay clase. "), tx(L, "See the schedule", "Ver el calendario"), u(L, "/schedule.html"))
  paste0('<script>(function(){var W=', js_semanas, ';var M=', js_textos, ';var t=new Date();t.setHours(0,0,0,0);var box=document.getElementById("this-week");var cur=null;',
    'W.forEach(function(w){var s=new Date(w.s+"T00:00:00");var e=new Date(s);e.setDate(e.getDate()+7);if(t>=s&&t<e)cur=w;});',
    'var p=box.querySelector("p:last-child");',
    'if(cur){p.innerHTML="<a href=\\""+cur.u+"\\">"+M.wk+cur.n+" · "+cur.t+"</a>";}',
    'else if(t<new Date(W[0].s+"T00:00:00")){p.innerHTML=M.s1+"<a href=\\""+W[0].u+"\\">"+M.s2+"</a>.";}',
    'else if(t>new Date(W[W.length-1].s+"T00:00:00")){p.innerHTML=M.over;}',
    'else{p.innerHTML=M.n1+"<a href=\\""+M.su+"\\">"+M.n2+"</a>.";}})();</script>')
}

# Portada abierta (lo único sin clave): nombre, acceso en inglés o español y enlace del profesor.
# El botón principal lleva al último idioma que usó el alumno. /es/ sin página lleva al inicio en español.
portada_abierta <- function() {
  escribir("es/index.html", sprintf('<!doctype html><html lang="es"><head><meta charset="utf-8"><meta http-equiv="refresh" content="0;url=/es/home.html"><title>%s</title></head><body><a href="/es/home.html">%s</a></body></html>',
                                    h(nombre_curso("es")), h(nombre_curso("es"))))
  escribir("index.html", paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>', h(CURSO$nombre), '</title><link rel="icon" href="/assets/logo.png"><link rel="stylesheet" href="/assets/style.css?v=', ASSET_V, '"></head>',
    '<body class="welcome"><main class="gate"><img src="/assets/logo.png" alt="" width="72" height="72">',
    '<p class="eyebrow">Course website</p><h1>', h(CURSO$nombre), '</h1>',
    '<p class="lede">Schedule, materials and activities for students enrolled in the course.</p>',
    '<a class="enter" id="enter" href="/home.html">Student access</a>',
    '<a class="enter-alt" href="/es/home.html" lang="es"><img src="/assets/flag-es.svg" alt="" width="21" height="14">Acceso en español</a>',
    '<p class="note">You will be asked for the user name and password given in class.</p>',
    '<p class="note"><a href="/profesor/">Teacher</a></p></main>',
    '<script>try{if(localStorage.getItem("', CURSO$clave_idioma, '")==="es"){var a=document.getElementById("enter");a.href="/es/home.html";a.textContent="Acceso de alumnos";a.lang="es";}}catch(e){}</script>',
    '</body></html>'))
}

# ── Zona del profesor: páginas comunes ─────────────────────────────

# Liberar contenidos: `tablas` son tabla_lib(titulo, filas) con filas de fila_lib(clave, nombre, detalle)
fila_lib <- function(key, nombre, detalle) sprintf('<tr data-row="%s"><td class="wk">%s</td><td><b>%s</b><small>%s</small></td><td class="st"><span class="state">…</span></td><td><button type="button" class="toggle" data-key="%s" data-name="%s" aria-pressed="false" disabled>Cargando</button></td></tr>',
  h(key), h(key), h(nombre), h(detalle), h(key), h(nombre))
tabla_lib <- function(titulo, filas) sprintf('<h2>%s</h2><div class="tbl"><table class="lib"><thead><tr><th>Clave</th><th>Contenido</th><th>Estado</th><th></th></tr></thead><tbody>%s</tbody></table></div>', h(titulo), paste(filas, collapse = ""))
pagina_liberar <- function(tablas) {
  js_lib <- paste0('<script>(function(){var API="/profesor/api/";',
    'function pintar(set){document.querySelectorAll("button[data-key]").forEach(function(b){var on=!!set[b.dataset.key];b.setAttribute("aria-pressed",on?"true":"false");b.textContent=on?"Ocultar":"Publicar";b.disabled=false;var tr=b.closest("tr");tr.classList.toggle("on",on);tr.querySelector(".state").textContent=on?"Publicado":"Oculto";});}',
    'function cargar(){return fetch(API+"estado",{cache:"no-store",credentials:"same-origin",headers:{"X-Requested-With":"fetch"}}).then(function(r){if(!r.ok)throw new Error("HTTP "+r.status);return r.json();}).then(function(d){var s={};(d.released||[]).forEach(function(k){s[k]=1;});pintar(s);document.getElementById("err").hidden=true;',
    'document.getElementById("upd").textContent=d.updated?"Último cambio: "+new Date(d.updated).toLocaleString("es-ES"):"Todavía no se ha publicado nada.";}).catch(function(e){var x=document.getElementById("err");x.hidden=false;x.textContent="No se puede leer el estado de publicación ("+e.message+").";});}',
    'document.addEventListener("click",function(ev){var b=ev.target.closest("button[data-key]");if(!b)return;var on=b.getAttribute("aria-pressed")!=="true";',
    'if(!confirm((on?"¿Publicar para los alumnos?\\n\\n":"¿Ocultar a los alumnos?\\n\\n")+b.dataset.name))return;b.disabled=true;',
    'fetch(API+"liberar",{method:"POST",credentials:"same-origin",headers:{"Content-Type":"application/json","X-Requested-With":"fetch"},body:JSON.stringify({key:b.dataset.key,released:on})})',
    '.then(function(r){if(!r.ok)throw new Error("HTTP "+r.status);return cargar();}).catch(function(e){alert("No se pudo guardar el cambio: "+e.message);b.disabled=false;});});cargar();})();</script>')
  pagina("profesor/liberar.html", paste0("Liberar contenidos · ", CURSO$nombre), paste0(
    '<p class="eyebrow">Zona del profesor</p><h1>Liberar contenidos</h1>',
    '<p class="lede">Publica cada tema cuando llegues a él en clase. Hasta entonces los alumnos ven el calendario, pero no la agenda detallada ni los materiales, y sus ficheros no se pueden descargar.</p>',
    '<p class="notice" id="upd">Cargando el estado…</p><p class="notice bad" id="err" hidden></p>',
    paste(tablas, collapse = ""),
    '<p class="note">Los cambios se aplican al momento. Ocultar un contenido lo retira de la web, pero no de los equipos de quien ya lo haya descargado.</p>', js_lib),
    zona = "teacher", actual = "Liberar contenidos")
}

# Acceso de alumnos: usuarios (uno por grupo o universidad) con claves sencillas, modo abierta/cerrada y enlaces de clase
form_enlace <- function(tipo, titulo, ayuda, ejemplo) paste0(
  '<h3>', titulo, '</h3><p class="note">', ayuda, '</p>',
  sprintf('<form data-enlace="%s" class="clave" autocomplete="off"><label for="url-%s">Enlace (https://…)</label>', tipo, tipo),
  sprintf('<input id="url-%s" name="url" type="url" maxlength="500" placeholder="%s">', tipo, ejemplo),
  sprintf('<div class="botones"><button type="submit" class="toggle">Guardar</button><button type="button" class="toggle secundario" data-quitar="%s">Quitar</button></div></form>', tipo),
  sprintf('<p class="notice" id="estado-%s">Cargando…</p>', tipo))
pagina_acceso <- function() {
  js_acceso <- paste0('<script>(function(){var API="/profesor/api/";var msg=document.getElementById("msg");',
    'function aviso(t,mal){msg.textContent=t;msg.hidden=false;msg.className="notice"+(mal?" bad":"");window.scrollTo(0,0);}',
    'function esc(t){var d=document.createElement("div");d.textContent=t;return d.innerHTML;}',
    'function post(ruta,datos){return fetch(API+ruta,{method:"POST",credentials:"same-origin",headers:{"Content-Type":"application/json","X-Requested-With":"fetch"},body:JSON.stringify(datos)})',
    '.then(function(r){return r.json().then(function(d){if(!r.ok)throw new Error(d.error||("HTTP "+r.status));return d;});});}',
    'function cargar(){fetch(API+"estado",{cache:"no-store",credentials:"same-origin",headers:{"X-Requested-With":"fetch"}}).then(function(r){if(!r.ok)throw new Error("HTTP "+r.status);return r.json();}).then(function(d){',
    # Modo de la web
    'var m=d.modo||{};var me=document.getElementById("modo-estado");var mb=document.getElementById("modo-btn");mb.disabled=false;mb.dataset.abrir=m.abierta?"0":"1";',
    'if(m.abierta){me.textContent="Abierta sin clave desde el "+new Date(m.desde).toLocaleString("es-ES")+": cualquiera puede ver la web de los alumnos (solo lo liberado).";me.className="notice bad";mb.textContent="Cerrar: pedir la clave";}',
    'else{me.textContent="Cerrada: los alumnos entran con uno de los usuarios de abajo.";me.className="notice";mb.textContent="Abrir sin clave";}',
    # Usuarios
    'var us=(d.alumnos&&d.alumnos.usuarios)||[];var tb=document.getElementById("usuarios");',
    'tb.innerHTML=us.length?us.map(function(u){return "<tr><td><b>"+esc(u.usuario)+"</b></td><td>"+(u.actualizada?new Date(u.actualizada).toLocaleString("es-ES"):"—")+"</td><td><button type=\\"button\\" class=\\"toggle\\" data-borrar=\\""+esc(u.usuario)+"\\">Borrar</button></td></tr>";}).join(""):"<tr><td colspan=\\"3\\">Todavía no hay usuarios: los alumnos no pueden entrar (salvo con la web abierta).</td></tr>";',
    # Enlaces
    '["asistencia","carpeta"].forEach(function(t){var e=(d.enlaces||{})[t]||{};document.getElementById("url-"+t).value=e.url||"";var st=document.getElementById("estado-"+t);',
    'if(e.url){st.innerHTML="Visible en las sesiones desde el "+new Date(e.actualizada).toLocaleString("es-ES")+" · <a href=\\""+esc(e.url)+"\\" target=\\"_blank\\" rel=\\"noopener\\">abrir</a>";}else{st.textContent="Sin enlace: los alumnos no ven este botón.";}});',
    '}).catch(function(e){aviso("No se puede leer el estado ("+e.message+").",true);});}',
    # Crear usuario o cambiar su clave
    'var f=document.getElementById("form-clave");f.addEventListener("submit",function(ev){ev.preventDefault();',
    'if(f.clave.value!==f.clave2.value){aviso("Las dos claves no coinciden.",true);return;}',
    'var b=f.querySelector("button");b.disabled=true;post("clave-alumnos",{usuario:f.usuario.value.trim(),clave:f.clave.value})',
    '.then(function(){aviso("Usuario «"+f.usuario.value.trim()+"» guardado. Ya puede entrar.",false);f.reset();cargar();})',
    '.catch(function(e){aviso("No se pudo guardar: "+e.message,true);}).then(function(){b.disabled=false;});});',
    # Borrar usuario
    'document.getElementById("usuarios").addEventListener("click",function(ev){var b=ev.target.closest("button[data-borrar]");if(!b)return;var u=b.dataset.borrar;',
    'if(!confirm("¿Borrar el usuario «"+u+"»? Quien entre con él dejará de poder acceder."))return;b.disabled=true;',
    'post("borrar-alumno",{usuario:u}).then(function(){aviso("Usuario «"+u+"» borrado.",false);cargar();}).catch(function(e){aviso("No se pudo borrar: "+e.message,true);b.disabled=false;});});',
    # Enlaces de clase
    'document.querySelectorAll("form[data-enlace]").forEach(function(fe){fe.addEventListener("submit",function(ev){ev.preventDefault();var t=fe.dataset.enlace;var u=fe.url.value.trim();var b=fe.querySelector("button");b.disabled=true;',
    'post("enlace",{tipo:t,url:u}).then(function(){aviso(u?"Enlace guardado: ya aparece en las sesiones.":"Enlace quitado.",false);cargar();})',
    '.catch(function(e){aviso("No se pudo guardar el enlace: "+e.message,true);}).then(function(){b.disabled=false;});});});',
    'document.querySelectorAll("button[data-quitar]").forEach(function(bq){bq.addEventListener("click",function(){var fe=document.querySelector("form[data-enlace=\\""+bq.dataset.quitar+"\\"]");fe.url.value="";fe.requestSubmit();});});',
    # Modo
    'document.getElementById("modo-btn").addEventListener("click",function(){var b=this;var abrir=b.dataset.abrir==="1";',
    'if(!confirm(abrir?"¿Abrir la web de los alumnos sin clave? Cualquiera con la dirección podrá verla.":"¿Cerrar la web? Los alumnos volverán a necesitar un usuario."))return;b.disabled=true;',
    'post("modo",{abierta:abrir}).then(function(){aviso(abrir?"Web abierta: ya no pide clave.":"Web cerrada: vuelve a pedir usuario y clave.",false);cargar();})',
    '.catch(function(e){aviso("No se pudo cambiar el modo: "+e.message,true);b.disabled=false;});});cargar();})();</script>')
  pagina("profesor/acceso.html", paste0("Acceso de alumnos · ", CURSO$nombre), paste0(
    '<p class="eyebrow">Zona del profesor</p><h1>Acceso de alumnos</h1>',
    '<p class="lede">Crea un usuario por grupo o por universidad, con una clave sencilla que puedas dictar en clase. Puedes tener varios a la vez y borrarlos cuando acabe el curso. Aquí pones también los enlaces que ven los alumnos en cada sesión.</p>',
    '<p id="msg" hidden></p>',
    '<h2>Usuarios de los alumnos</h2>',
    '<div class="tbl"><table class="lib"><thead><tr><th>Usuario</th><th>Clave cambiada</th><th></th></tr></thead><tbody id="usuarios"><tr><td colspan="3">Cargando…</td></tr></tbody></table></div>',
    '<form id="form-clave" class="clave" autocomplete="off"><h3>Crear usuario o cambiar su clave</h3>',
    '<label for="usuario">Usuario</label><input id="usuario" name="usuario" required minlength="3" maxlength="32" pattern="[A-Za-z0-9._\\-]+" placeholder="ue2026">',
    '<label for="clave">Clave</label><input id="clave" name="clave" type="text" required minlength="4" maxlength="128" autocomplete="off">',
    '<label for="clave2">Repite la clave</label><input id="clave2" name="clave2" type="text" required minlength="4" maxlength="128" autocomplete="off">',
    '<button type="submit" class="toggle">Guardar usuario</button></form>',
    '<p class="note">Usuario: 3 a 32 letras, números, puntos o guiones, sin espacios. Clave: 4 caracteres como mínimo. Si el usuario ya existe, se le cambia la clave. Las claves se guardan cifradas: no se pueden consultar, solo cambiar. Tu clave de profesor no cambia.</p>',
    '<h2>Enlaces de clase</h2><p class="note">Aparecen como botones en cada sesión de la web, en inglés y en español, sin volver a publicar. Quítalos cuando ya no hagan falta.</p>',
    form_enlace("asistencia", "Hoja de asistencia", "La hoja de Google donde los alumnos se apuntan en cada clase.", "https://docs.google.com/spreadsheets/d/…"),
    form_enlace("carpeta", "Carpeta compartida", "Una carpeta (Drive, OneDrive…) donde los alumnos suben el trabajo de clase, por ejemplo su diapositiva de presentación. Ponla durante la actividad y quítala después.", "https://drive.google.com/drive/folders/…"),
    '<h2>Modo de la web</h2><p class="notice" id="modo-estado">Cargando…</p>',
    '<p><button type="button" id="modo-btn" class="toggle" disabled>Cargando</button></p>',
    '<p class="note">Abre la web cuando quieras enseñarla y ciérrala después. Abierta, solo se ve lo que hayas liberado. Tu zona del profesor pide siempre tu clave.</p>',
    js_acceso),
    zona = "teacher", actual = "Acceso de alumnos")
}

# Grupos y avance: horario (una hora de inicio por cada día de clase de CURSO$dias) y aula de cada grupo, bloques dados
# y lo que toca en la siguiente sesión. El plan completo va en la página como JSON; los grupos y los ticks los gestiona
# assets/profesor.js con el servicio.
pagina_grupos <- function(semanas) {
  plan_json <- jsonlite::toJSON(unlist(lapply(semanas, function(s) lapply(s$sesiones, function(x) list(
    semana = s$n, tipo = x$tipo, etiqueta = etiqueta_sesion(x, "es"), dia = x$dia, fecha = format(s$inicio + x$dia), titulo = tr_plan(x$titulo, "es"),
    fin = max(x$bloques$fin),
    bloques = lapply(seq_len(nrow(x$bloques)), function(j) list(id = sprintf("s%02d%s-%d", s$n, codigo_sesion(x), j - 1),
      ini = x$bloques$ini[j], fin = x$bloques$fin[j], act = tr_plan(x$bloques$actividad[j], "es")))))), recursive = FALSE), auto_unbox = TRUE)
  dias <- CURSO$dias
  en_plan <- sort(unique(unlist(lapply(semanas, function(s) vapply(s$sesiones, `[[`, 0L, "dia")))))
  if (length(setdiff(en_plan, dias))) stop("El plan tiene clases en días que no están en CURSO$dias: ", paste(dias_es[setdiff(en_plan, dias) + 1], collapse = ", "))
  dias_json <- jsonlite::toJSON(lapply(seq_along(dias), function(k) list(dia = unname(dias[k]), nombre = dias_es[dias[k] + 1])), auto_unbox = TRUE)
  cap <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  pagina("profesor/grupos.html", paste0("Grupos y avance · ", CURSO$nombre), paste0(
    '<p class="eyebrow">Zona del profesor</p><h1>Grupos y avance</h1>',
    sprintf('<p class="lede">Da de alta cada grupo con su aula y la hora de inicio de las clases del %s. En cada plan de clase eliges el grupo, ves la hora real de cada bloque y marcas lo que vas dando; aquí ves cómo va cada grupo y qué le toca en la siguiente sesión.</p>',
            paste(dias_es[dias + 1], collapse = " y del ")),
    '<p id="msg" hidden></p><div id="panel-grupos">',
    sprintf('<div class="tbl"><table class="lib"><thead><tr><th>Grupo</th><th>Aula</th>%s<th></th></tr></thead><tbody id="lista-grupos"><tr><td colspan="%d">Cargando…</td></tr></tbody></table></div>',
            paste(sprintf("<th>%s</th>", cap(dias_es[dias + 1])), collapse = ""), length(dias) + 3),
    '<form id="form-grupo" class="clave" autocomplete="off"><h3>Añadir o editar un grupo</h3><input type="hidden" name="id_grupo" value="">',
    '<label for="g-nombre">Nombre</label><input id="g-nombre" name="nombre" required maxlength="40" placeholder="Grupo 1">',
    '<label for="g-aula">Aula</label><input id="g-aula" name="aula" maxlength="40" placeholder="Aula 2.05">',
    paste(sprintf('<label for="g-h%d">Hora de inicio del %s</label><input id="g-h%d" name="h%d" type="time">', dias, dias_es[dias + 1], dias, dias), collapse = ""),
    '<button type="submit" class="toggle">Añadir grupo</button></form>',
    '<div id="avance"></div></div>',
    '<script>window.PLAN = ', plan_json, ';window.DIAS_CLASE = ', dias_json, ';</script>'),
    zona = "teacher", actual = "Grupos y avance")
}

# Inglés de clase: vocabulario, frases y notas de gramática del profesor (solo en su zona; los alumnos no lo ven).
# La asignatura pone CURSO$ingles = TRUE (para el menú) y llama a pagina_ingles(intro, bloques), con
#   bloques: lista de list(id, titulo, resumen = "", terminos = tibble(en, es, nota = ""), frases = tibble(en, es),
#                          notas = character(), videos = tibble(titulo, url, duracion, para))
# `terminos$nota` sirve para la pronunciación aproximada o un matiz de uso; `notas`, para reglas y avisos (HTML).
# `videos`: vídeos cortos para proyectar en clase (se abren en ventana nueva, como todos los enlaces externos).
pagina_ingles <- function(intro, bloques, titulo = "Inglés de clase") {
  tabla <- function(x, cab1, cab2, cab3 = NULL) {
    if (is.null(x) || !nrow(x)) return("")
    hay3 <- !is.null(cab3) && any(nzchar(x$nota))
    filas <- paste(sprintf('<tr><td lang="en"><b>%s</b></td><td>%s</td>%s</tr>', h(x$en), h(x$es),
                           if (hay3) sprintf('<td class="nota">%s</td>', h(x$nota)) else ""), collapse = "")
    sprintf('<div class="tbl"><table class="mat"><thead><tr><th>%s</th><th>%s</th>%s</tr></thead><tbody>%s</tbody></table></div>',
            cab1, cab2, if (hay3) sprintf("<th>%s</th>", cab3) else "", filas)
  }
  indice <- paste(vapply(bloques, function(b) sprintf('<a class="dl" href="#%s"><span class="dl-l">%s</span></a>', h(b$id), h(b$titulo)), ""), collapse = "")
  cuerpo <- paste(vapply(bloques, function(b) paste0(
    sprintf('<section id="%s"><h2>%s</h2>', h(b$id), h(b$titulo)),
    if (!is.null(b$resumen) && nzchar(b$resumen)) sprintf('<p class="lede">%s</p>', h(b$resumen)) else "",
    tabla(b$terminos, "Término", "En español", "Cómo suena / matiz"),
    if (!is.null(b$frases) && nrow(b$frases)) paste0('<h3>Frases para la clase</h3>', tabla(b$frases, "Lo que dices", "Lo que quiere decir")) else "",
    if (length(b$notas)) paste0('<h3>Gramática y avisos</h3><ul class="lista">', paste(sprintf("<li>%s</li>", b$notas), collapse = ""), '</ul>') else "",
    if (!is.null(b$videos) && nrow(b$videos)) paste0('<h3>Vídeos para proyectar</h3><div class="dls">',
      paste(sprintf('<a class="dl" href="%s"><span class="dl-l">%s</span><span class="dl-m">%s · %s</span></a>',
                    h(b$videos$url), h(b$videos$titulo), h(b$videos$duracion), h(b$videos$para)), collapse = ""), '</div>') else "",
    '</section>'), ""), collapse = "")
  pagina("profesor/ingles.html", paste(titulo, "·", CURSO$nombre), paste0(
    '<p class="eyebrow">Zona del profesor</p><h1>', h(titulo), '</h1>',
    sprintf('<p class="lede">%s</p>', intro),
    '<p class="note">Solo lo ves tú: esta página no está en la web de los alumnos.</p>',
    '<div class="dls">', indice, '</div>', cuerpo),
    zona = "teacher", actual = "Inglés de clase")
}

# Plan docente generado con las mismas fuentes que la web: siempre coincide con lo que ven los alumnos.
#   titulo           «Plan docente 2026-27»
#   hechos_antes / hechos_despues   vectores con nombre para la ficha (antes y después de Calendario y Sesiones)
#   evaluacion       tibble(instrumento, peso) en español; nota_evaluacion: HTML de la columna derecha
#   actividades      tibble(id, titulo, formato, peso, periodo, entrega, resumen)
#   pruebas          tibble(id, titulo, cuando (HTML), nota)
#   pruebas_semana   function(n) → títulos de las pruebas de la semana n (etiquetas en el calendario)
#   examen           HTML de la fila final del calendario, tras «Examen final ·»
#   materiales, pendientes   HTML de los elementos de cada lista
pagina_plan_docente <- function(semanas, titulo, hechos_antes, hechos_despues, evaluacion, nota_evaluacion, actividades, pruebas,
                                pruebas_semana = function(n) character(), examen = "", materiales = character(), pendientes = character(),
                                aviso = "Plan sujeto a cambios según avance el curso: los alumnos lo tienen avisado en la web y en el Tema 0.",
                                texto_pausa = "Vacaciones de Navidad · sin clases") {
  n_ses <- sum(vapply(semanas, function(s) length(s$sesiones), 0L))
  primera <- semanas[[1]]$inicio; ultima <- semanas[[length(semanas)]]
  fin_clases <- ultima$inicio + max(vapply(ultima$sesiones, `[[`, 0L, "dia"))
  ficha <- function(x) paste(sprintf('<li><span>%s</span>%s</li>', names(x), x), collapse = "")
  filas_plan <- character(); bloque_prev <- ""; ini_prev <- NULL
  for (s in semanas) {
    if (s$bloque != bloque_prev) { filas_plan <- c(filas_plan, sprintf('<tr class="grp"><th colspan="4">Bloque %s</th></tr>', h(tr_plan(s$bloque, "es")))); bloque_prev <- s$bloque }
    if (!is.null(ini_prev) && as.numeric(s$inicio - ini_prev) > 7) filas_plan <- c(filas_plan, sprintf('<tr class="gap"><td colspan="4">%s</td></tr>', texto_pausa))
    ini_prev <- s$inicio
    ses <- paste(vapply(s$sesiones, function(x) sprintf('<span class="ses"><b>%s</b> %s · %s</span>', h(etiqueta_corta(x, "es")), h(fecha_sesion(s, x, "es")), h(tr_plan(x$titulo, "es"))), ""), collapse = "")
    hit <- paste(c(sprintf('<span class="pill">%s</span>', h(tr_plan(s$hitos, "es"))), sprintf('<span class="pill ev">%s</span>', h(pruebas_semana(s$n)))), collapse = " ")
    av <- if (nzchar(s$aviso)) sprintf('<small class="aviso">%s</small>', h(tr_plan(s$aviso, "es"))) else ""
    filas_plan <- c(filas_plan, sprintf('<tr><td class="wk"><a href="/profesor/week/%02d.html">%d</a></td><td><b>%s</b><small>%s</small>%s</td><td>%s</td><td>%s</td></tr>',
                                        s$n, s$n, h(tr_plan(s$titulo, "es")), h(tr_plan(s$capitulos, "es")), av, ses, hit))
  }
  if (nzchar(examen)) filas_plan <- c(filas_plan, sprintf('<tr class="gap exam"><td colspan="4"><b>Examen final</b> · %s</td></tr>', examen))
  tarjetas_act <- paste(vapply(seq_len(nrow(actividades)), function(i) {
    a <- actividades[i, ]
    sprintf('<article class="act"><header><span class="tag">%s</span><h3>%s</h3></header><dl><dt>Formato</dt><dd>%s</dd><dt>Peso</dt><dd>%s</dd><dt>Periodo</dt><dd>%s</dd><dt>Entrega</dt><dd>%s</dd></dl><p class="note">%s</p></article>',
            a$id, h(a$titulo), h(a$formato), a$peso, a$periodo, a$entrega, h(a$resumen))
  }, ""), collapse = "")
  filas_pruebas <- paste(sprintf('<tr><td class="wk">%s</td><td><b>%s</b></td><td>%s</td><td>%s</td></tr>', pruebas$id, h(pruebas$titulo), pruebas$cuando, pruebas$nota), collapse = "")
  lista <- function(x) if (length(x)) paste0('<ul class="lista">', paste(sprintf("<li>%s</li>", x), collapse = ""), '</ul>') else ""
  pagina("profesor/plan-docente.html", paste0("Plan docente · ", CURSO$nombre), paste0(
    '<p class="eyebrow">Zona del profesor</p><h1>', h(titulo), '</h1>',
    sprintf('<p class="lede">Se genera automáticamente con el calendario, las actividades y las pruebas de la web: siempre coincide con lo que ven los alumnos. Actualizado el %s.</p>', fecha_es(Sys.Date())),
    '<ul class="facts">', ficha(hechos_antes),
    sprintf('<li><span>Calendario</span>%s – %s</li>', fecha_es(primera), fecha_es(fin_clases)),
    sprintf('<li><span>Sesiones</span>%d en %d semanas</li>', n_ses, length(semanas)),
    ficha(hechos_despues), '</ul>',
    if (nzchar(aviso)) sprintf('<p class="notice">%s</p>', aviso) else "",
    '<section><h2>Evaluación</h2><div class="two"><div><table class="plain"><tbody>',
    paste(sprintf('<tr><td>%s</td><td class="num">%s</td></tr>', h(evaluacion$instrumento), evaluacion$peso), collapse = ""),
    '</tbody></table></div><div>', nota_evaluacion, '</div></div></section>',
    if (nrow(actividades)) paste0('<section><h2>Actividades</h2><div class="acts">', tarjetas_act, '</div></section>') else "",
    if (nrow(pruebas)) paste0('<section><h2>Pruebas</h2><div class="tbl"><table class="mat"><thead><tr><th></th><th>Prueba</th><th>Cuándo</th><th>Nota</th></tr></thead><tbody>', filas_pruebas, '</tbody></table></div></section>') else "",
    '<section><h2>Semana a semana</h2><div class="tbl"><table class="cal"><thead><tr><th>Sem.</th><th>Tema</th><th>Sesiones</th><th>Hitos y pruebas</th></tr></thead><tbody>',
    paste(filas_plan, collapse = ""), '</tbody></table></div></section>',
    if (length(materiales)) paste0('<section><h2>Materiales</h2>', lista(materiales), '</section>') else "",
    if (length(pendientes)) paste0('<section><h2>Pendiente</h2>', lista(pendientes), '</section>') else ""),
    zona = "teacher", actual = "Plan docente")
}
