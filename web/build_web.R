# ══════════════════════════════════════════════════════════════════
# Web de la asignatura · construye el sitio en web/_site
# Uso desde la raíz del proyecto:  Rscript web/build_web.R
#   web/_site/public/   páginas de los alumnos en inglés (raíz) y en español (/es/), con banderas para cambiar,
#                       y zona del profesor /profesor/ (en español, con las notas docentes)
#   web/_site/store/    almacén: un directorio por contenido liberable (T01…T10, A1…A3, solutions-A1…A3, S0, S1, F1
#                       y solutions-…). Solo se publica lo que el profesor libera desde /profesor/liberar.html:
#                       el servicio liberar.py crea el enlace public/downloads/<clave> → store/<clave>.
# El manual se publica por capítulos, nunca completo (decisión del autor).
# Materiales: hoy solo en español (manual, presentaciones, Excel de clase, actividades y simulacros). La parte inglesa
# de la web los ofrece en español («Spanish») y, en cuanto exista la versión inglesa de un fichero, sale sola con su
# botón ES/EN. Las piezas que todavía no existen (formulario, examen final) se anuncian como «en preparación».
# Los materiales del profesor y de clase viven fuera de git, en materiales/ (presentaciones, clase, actividades y
# evaluaciones/salida).
# Publicación: bash _tools/publicar_web.sh (desde el equipo local, a través de puerta). web/_site no va a git.
#
# Lo genérico (páginas del profesor, páginas de semana, calendario, portada, estilos, scripts, servicio, nginx y
# publicador) es el motor común de las webs docentes, copiado en web/motor/ desde webMotor: NO se edita aquí
# (ver web/motor/LEEME.md). Este fichero solo tiene lo propio de Econometría.
# ══════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})
CURSO <- list(
  web = "econometrics", nombre = "Econometrics", nombre_es = "Econometría",
  marca = "<b>Econometrics</b>", marca_es = "<b>Econometría</b>", autor = "Carlos de Anta Puig",
  clave_idioma = "ec-lang",
  dias = c(lunes = 0L, jueves = 3L),          # PROVISIONAL: clases el lunes; la segunda de la semana 11, el jueves (DIAS en el plan)
  logo = "presentaciones/_identidad/CdeA_logo.png",
  idioma_profesor = "es",                     # zona del profesor en español, con la agenda en español
  etiqueta_sesion = function(x, L) if (x$tipo == "larga") tx(L, "Live class · 90 min", "Clase en directo · 90 min") else tx(L, "Live class · 60 min", "Clase en directo · 60 min"),
  etiqueta_corta = function(x, L) if (x$tipo == "larga") "90 min" else "60 min")
source("web/motor/R/motor.R")
NCAP <- 10

# El plan lleva cada texto en inglés y en español (TT) y rellena el diccionario ES_PLAN del motor
source("web/plan_semanas.R")
semanas <- plan_semanas()
comprobar_traducciones(semanas, extra = unlist(EXAMEN_FINAL))
iniciar_sitio()

# ── Capítulos ──────────────────────────────────────────────────────
caps <- tibble(
  nn = sprintf("%02d", 1:NCAP),
  libro = c("01-introduccion-econometria", "02-metodo-mco", "03-propiedades-estimadores-mco", "04-contrastes-hipotesis", "05-intervalos-confianza",
            "06-problemas-especificacion", "07-prediccion-residuos", "08-variables-cualitativas", "09-heterocedasticidad", "10-autocorrelacion"),
  app = c("01-tipos-datos-economicos", "02-estimacion-mco", "03-propiedades-mco", "04-contrastes-hipotesis", "05-intervalos-confianza",
          "06-especificacion", "07-prediccion-residuos", "08-variables-dummy", "09-heterocedasticidad", "10-autocorrelacion"),
  titulo_en = c("Introduction to econometrics", "Quantifying the relationship between variables: the OLS method", "Properties of the OLS estimators",
                "Hypothesis testing in the linear regression model", "Confidence intervals in the linear regression model", "Specification problems",
                "Prediction and residual analysis", "Qualitative variables in the linear regression model", "Heteroskedasticity and generalised least squares",
                "Serial autocorrelation and generalised least squares"))
titulo_cap <- function(fich) trimws(sub("\\s*\\{#.*\\}\\s*$", "", sub("^#\\s*", "", grep("^# ", readLines(paste0(fich, ".qmd"), encoding = "UTF-8"), value = TRUE)[1])))
caps$titulo_es <- vapply(caps$libro, titulo_cap, "")
# Semana de repaso («Chapters 1–10»): sin tema propio, se ve siempre
temas_de <- function(s) { if (grepl("–", s$capitulos)) return(character()); x <- as.integer(regmatches(s$capitulos, gregexpr("[0-9]+", s$capitulos))[[1]]); x <- x[x <= NCAP]; if (length(x)) sprintf("%02d", x) else character() }

# ── PDF por capítulos (las funciones de corte están en el motor) ──
pdf_libro_es <- list.files("docs", pattern = "\\.pdf$", full.names = TRUE)[1]
pdf_libro_en <- list.files("en/docs", pattern = "\\.pdf$", full.names = TRUE)[1]     # cuando exista la versión inglesa
paginas <- list(libro_es = cortar(pdf_libro_es, "^Capítulo %d$", "^(Referencias|Bibliografía)$", "Libro_capitulo%02d_ES.pdf", "book:%s:es", n = NCAP))
if (!is.na(pdf_libro_en)) paginas$libro_en <- cortar(pdf_libro_en, "^Chapter %d$", "^References$", "Book_chapter%02d_EN.pdf", "book:%s:en", n = NCAP)

# Fichero en el idioma de la página con el botón del otro idioma; si aún no existe la versión inglesa,
# la parte inglesa ofrece la española con la marca «Spanish»
par2 <- function(base, etiqueta, zona = "public", L = "en", meta = "") {
  if (!is.null(REG[[paste0(base, ":", L)]])) return(par_idiomas(base, etiqueta, zona, meta, L = L))
  chip(paste0(base, ":es"), etiqueta, zona, paste0(meta, if (L == "en") "Spanish · " else ""))
}
nombre_fich <- function(f) gsub("_", " ", tools::file_path_sans_ext(basename(f)))

# ── Resto de materiales por capítulo ───────────────────────────────
SLIDES <- list(); CLASE <- list()
for (i in seq_len(nrow(caps))) {
  n <- caps$nn[i]; k <- tkey(n)
  # Presentación CdeA del tema (R/presentaciones/capNN_pptx.R; PDF exportado con PowerPoint), ES y EN,
  # y su PowerPoint editable para el profesor
  base <- sprintf("slides:%s:1", n)
  for (L in c("es", "en")) {
    f <- list.files(file.path("presentaciones", L), pattern = sprintf("^T%s_mEC_.*\\.pdf$", n), full.names = TRUE)
    if (length(f) == 1) {
      guardar(paste0(base, ":", L), f, k, file.path(L, basename(f)))
      pp <- sub("\\.pdf$", ".pptx", f)
      if (file.exists(pp)) prof(sprintf("pptx:%s:%s", base, L), pp, file.path("profesor/downloads/slides", L, basename(pp)))
    }
  }
  SLIDES[[n]] <- if (!is.null(REG[[paste0(base, ":es")]])) base else character()
  # Presentaciones de clase del curso anterior: solo para el profesor
  for (f in sort(list.files("materiales/presentaciones/es", pattern = sprintf("^T%s_.*\\.pdf$", n), full.names = TRUE))) {
    prof(sprintf("clase_ant:%s:%s", n, basename(f)), f, file.path("profesor/downloads/slides/curso-anterior", basename(f)))
    pp <- file.path("materiales/presentaciones/pptx", sub("\\.pdf$", ".pptx", basename(f)))
    if (file.exists(pp)) prof(sprintf("clase_ant_pptx:%s:%s", n, basename(pp)), pp, file.path("profesor/downloads/slides/curso-anterior", basename(pp)))
  }
  # Excel y datos de clase
  cl <- sort(list.files(file.path("materiales/clase/es", k), full.names = TRUE))
  CLASE[[n]] <- vapply(cl, function(f) { kk <- sprintf("clase:%s:%s", n, basename(f)); guardar(kk, f, k, file.path("clase", basename(f))); kk }, "")
  # Scripts de R del tema: un zip listo para abrir en RStudio (proyecto, datos y scripts)
  sc <- sort(list.files("scripts", pattern = sprintf("^T%s_.*\\.R$", n), full.names = TRUE))
  dir_z <- file.path(tempfile("scripts_"), sprintf("Econometria_T%s", n))
  dir.create(file.path(dir_z, "scripts"), recursive = TRUE); dir.create(file.path(dir_z, "data"))
  file.copy("mEconometria.Rproj", dir_z); file.copy(sc, file.path(dir_z, "scripts"))
  file.copy(list.files("data", pattern = "\\.(csv|xlsx)$", full.names = TRUE), file.path(dir_z, "data"))
  z <- file.path(STORE, k, sprintf("Scripts_T%s.zip", n)); dir.create(dirname(z), recursive = TRUE, showWarnings = FALSE)
  zip::zipr(z, files = dir_z)
  guardar(sprintf("script:%s", n), z, k)
  # App Shiny del tema (genera sus propios datos)
  za <- file.path(STORE, k, sprintf("App%s_%s.zip", n, caps$app[i]))
  zip::zipr(za, files = file.path("apps", caps$app[i]))
  guardar(sprintf("app:%s", n), za, k)
}
prof("clase_prof:05", "materiales/clase/profesor/T05_Practica_Intervalos_SOLUCION.xlsx", "profesor/downloads/clase/T05_Practica_Intervalos_SOLUCION.xlsx")
for (f in c("T00_Presentacion_asignatura.pptx", "S1_Simulacro.pptx")) prof(paste0("pptx:", f), file.path("materiales/presentaciones/pptx", f), file.path("profesor/downloads/slides", f))
prof("bookpdf:es", pdf_libro_es, "profesor/downloads/book/Econometria_ES.pdf")
# Formulario (si todavía no existe, se anuncia como pendiente): siempre visible para los alumnos, sin liberación
FORM <- c(es = "formulario/Formulario_mEC_ES.pdf", en = "formulario/Formula_Sheet_mEC_EN.pdf")
hay_form <- file.exists(FORM[["es"]])
if (hay_form) for (L in c("es", "en")) if (file.exists(FORM[[L]])) prof(sprintf("form:%s", L), FORM[[L]], file.path("recursos", basename(FORM[[L]])))

# ── Actividades (programación: A1 4 puntos, A2 y A3 5 puntos; A3 en grupo) ─────
acts <- tribble(
  ~id,  ~titulo, ~titulo_en, ~resumen, ~resumen_es, ~formato, ~formato_es, ~ventana, ~ventana_es, ~entrega, ~entrega_es, ~peso, ~peso_es,
  "A1", "Estimación MCO a mano con Excel", "OLS estimation by hand in Excel",
  "Estimate a multiple regression by OLS with the matrix formulas in Excel, interpret the coefficients, R² and adjusted R², the effect of a change of units and the error variance.",
  "Estimar una regresión múltiple por MCO con las fórmulas matriciales en Excel e interpretar los coeficientes, el R² y el R² corregido, el efecto de un cambio de unidades y la varianza del error.",
  "Individual · Excel and answer sheet", "Individual · Excel y hoja de respuestas", "Weeks 4–6", "Semanas 4–6", "Tuesday 13 April 2027", "martes 13 de abril de 2027",
  "4 points of continuous assessment", "4 puntos de la evaluación continua",
  "A2", "Modelización y contrastes con Gretl o R", "Modelling and hypothesis testing with Gretl or R",
  "Estimate a model with econometric software, test individual significance with p-values, build 95 % confidence intervals, propose an improved model and test its overall significance.",
  "Estimar un modelo con software econométrico, contrastar la significación individual con p-valores, construir intervalos de confianza al 95 %, proponer un modelo mejorado y contrastar su significación global.",
  "Individual · Gretl or R and answer sheet", "Individual · Gretl o R y hoja de respuestas", "Weeks 7–10", "Semanas 7–10", "Tuesday 11 May 2027", "martes 11 de mayo de 2027",
  "5 points of continuous assessment", "5 puntos de la evaluación continua",
  "A3", "Análisis econométrico completo con diagnóstico", "Full econometric analysis with diagnostics",
  "A complete study in groups: descriptive analysis, estimation, significance tests, Breusch–Pagan and Durbin–Watson diagnostics, corrections and a recommendation.",
  "Un estudio completo en grupo: análisis descriptivo, estimación, contrastes de significación, diagnóstico con Breusch-Pagan y Durbin-Watson, correcciones y una recomendación.",
  "Group · Gretl or R and report", "En grupo · Gretl o R e informe", "Weeks 11–13", "Semanas 11–13", "Tuesday 1 June 2027", "martes 1 de junio de 2027",
  "5 points of continuous assessment", "5 puntos de la evaluación continua")
etiqueta_act <- function(f) {
  if (grepl("enunciado|Brief", f)) "Enunciado"
  else if (grepl("hoja_respuestas|Answer", f)) "Hoja de respuestas"
  else if (grepl("hoja_control", f)) "Hoja de control del grupo"
  else if (grepl("datos|Data", f)) "Datos"
  else if (grepl("rubrica|Rubric", f)) "Rúbrica"
  else if (grepl("\\.R$", f)) "Script de la solución"
  else "Solución comentada"
}
etiqueta_act_en <- function(f) {
  if (grepl("enunciado|Brief", f)) "Brief"
  else if (grepl("hoja_respuestas|Answer", f)) "Answer sheet"
  else if (grepl("hoja_control", f)) "Group control sheet"
  else if (grepl("datos|Data", f)) "Data"
  else if (grepl("rubrica|Rubric", f)) "Rubric"
  else if (grepl("\\.R$", f)) "Solution script"
  else "Worked solution"
}
ACT <- list()
DIR_ACT <- "materiales/actividades"
for (a in acts$id) for (L in c("es", "en")) {
  alumno <- sort(list.files(file.path(DIR_ACT, L, a), pattern = "\\.(docx|xlsx|csv|R|pdf)$"))
  profe <- sort(list.files(file.path(DIR_ACT, L, a, "profesor"), pattern = "\\.(docx|xlsx|csv|R|html|pdf)$"))
  ca <- vapply(alumno, function(f) { k <- sprintf("act:%s:%s:%s", a, L, f); guardar(k, file.path(DIR_ACT, L, a, f), a, file.path(L, f)); k }, "")
  cs <- vapply(profe, function(f) { k <- sprintf("sol:%s:%s:%s", a, L, f); guardar(k, file.path(DIR_ACT, L, a, "profesor", f), paste0("solutions-", a), file.path(L, f)); k }, "")
  ACT[[paste(a, L, "alumno")]] <- tibble(clave = ca, etiqueta = vapply(alumno, etiqueta_act, ""), etiqueta_en = vapply(alumno, etiqueta_act_en, ""))
  ACT[[paste(a, L, "solucion")]] <- tibble(clave = cs, etiqueta = vapply(profe, etiqueta_act, ""), etiqueta_en = vapply(profe, etiqueta_act_en, ""))
}
hay_act <- function(a) nrow(ACT[[paste(a, "es", "alumno")]]) > 0 || nrow(ACT[[paste(a, "en", "alumno")]]) > 0
hay_sol_act <- function(a) nrow(ACT[[paste(a, "es", "solucion")]]) > 0 || nrow(ACT[[paste(a, "en", "solucion")]]) > 0

# Pruebas (fuentes y salida fuera de git, materiales/evaluaciones/salida). Liberables y ocultas por defecto.
# `bloque` es el inicio del nombre del bloque del plan (en inglés) en el que se hace la prueba.
EVAL <- tibble(id = c("S0", "S1", "F1"),
               titulo = c("Supuesto orientativo de examen", "Simulacro del examen final", "Examen final · temas 1–10"),
               titulo_en = c("Practice exam case", "Mock final exam", "Final exam · chapters 1–10"),
               bloque = c("Exam guidance", "Mock exam", NA))
EVF <- list()
dir_ev <- "materiales/evaluaciones/salida"
for (e in EVAL$id) for (L in c("es", "en")) {
  fs <- sort(list.files(file.path(dir_ev, L), pattern = paste0("^", e, "_.*\\.(docx|xlsx|pdf)$")))
  if (!length(fs)) next
  sol <- grepl("Solutions|Soluciones", fs)
  parte <- ifelse(grepl("Datos|Data", fs), "datos", ifelse(sol, "solucionario", "enunciado"))
  parte_en <- ifelse(grepl("Datos|Data", fs), "data", ifelse(sol, "solutions", "exam paper"))
  sub_p <- ifelse(grepl("Gretl", fs), " · Gretl", ifelse(grepl("Manual", fs), " · manual", ""))
  etq <- paste0(parte, sub_p, " (", toupper(L), ")")
  etq_al <- paste0(parte_en, sub_p, ifelse(L == "es", " (Spanish)", ""))
  etq_al_es <- paste0(parte, sub_p, ifelse(L == "en", " (inglés)", ""))
  ks <- vapply(seq_along(fs), function(j) {
    k <- sprintf("ev:%s:%s:%s", e, L, fs[j])
    guardar(k, file.path(dir_ev, L, fs[j]), if (sol[j]) paste0("solutions-", e) else e, file.path(L, fs[j])); k
  }, "")
  EVF[[paste(e, L)]] <- tibble(clave = ks, etiqueta = etq, etiqueta_al = etq_al, etiqueta_al_es = etq_al_es, idioma = L, solucion = sol)
}
EVAL <- EVAL |> mutate(hay = vapply(id, function(e) !is.null(EVF[[paste(e, "es")]]) || !is.null(EVF[[paste(e, "en")]]), TRUE))
ficheros_eval <- function(e, sol = NA) {
  x <- bind_rows(EVF[[paste(e, "es")]], EVF[[paste(e, "en")]])
  if (is.na(sol)) x else x[x$solucion == sol, ]
}
chips_eval <- function(e) { x <- ficheros_eval(e) |> arrange(solucion); paste(mapply(function(k, t) chip(k, t, "teacher"), x$clave, x$etiqueta), collapse = "") }
semana_eval <- function(e) {
  b <- EVAL$bloque[EVAL$id == e]
  if (is.na(b)) return(NA_integer_)
  n <- vapply(semanas, function(s) any(vapply(s$sesiones, function(x) any(startsWith(x$bloques$actividad, b)), TRUE)), TRUE)
  if (any(n)) semanas[[which(n)[1]]]$n else NA_integer_
}

# ── Materiales de la asignatura en las sesiones y en el plan del profesor ──
# Pieza aún no escrita: se anuncia como «en preparación»
pronto <- function(etiqueta, texto) sprintf('<span class="dl soon"><span class="dl-l">%s</span><span class="dl-m">%s</span></span>', h(etiqueta), h(texto))
slides_html <- function(n, zona, L) {
  b <- SLIDES[[n]]; nt <- as.integer(n)
  paste(vapply(seq_along(b), function(j) par2(b[j], if (length(b) > 1) sprintf(tx(L, "Slides · chapter %d (%d/%d)", "Presentación · tema %d (%d/%d)"), nt, j, length(b))
                                                     else sprintf(tx(L, "Slides · chapter %d", "Presentación · tema %d"), nt), zona, L), ""), collapse = "")
}
clase_html <- function(n, zona, L) paste(vapply(CLASE[[n]], function(k) chip(k, nombre_fich(sub("^clase:[0-9]+:", "", k)), zona,if (L == "en") "Spanish · " else ""), ""), collapse = "")
material_html <- function(code, zona = "public", temas = character(), L = "en") {
  p <- strsplit(code, ":", fixed = TRUE)[[1]]
  k <- p[1]; v <- if (length(p) > 1) p[2] else NA_character_
  if (k == "act") {
    if (!hay_act(v)) return(pronto(tx(L, sprintf("Activity %s", sub("A", "", v)), sprintf("Actividad %s", sub("A", "", v))), tx(L, "Coming soon", "En preparación")))
    return(sprintf('<a class="dl" href="%s#%s"><span class="dl-l">%s %s</span><span class="dl-m">%s</span></a>', if (zona == "public") u(L, "/activities.html") else "/profesor/#actividades", tolower(v),
                   tx(L, "Activity", "Actividad"), sub("A", "", v), tx(L, "Brief, data, answer sheet and rubric", "Enunciado, datos, hoja de respuestas y rúbrica")))
  }
  if (k == "ev") {
    t <- if (L == "es") EVAL$titulo[EVAL$id == v] else EVAL$titulo_en[EVAL$id == v]
    return(sprintf('<a class="dl" href="%s#%s"><span class="dl-l">%s</span><span class="dl-m">%s</span></a>', if (zona == "public") u(L, "/activities.html") else "/profesor/#evaluaciones", tolower(v),
                   h(t), tx(L, "Published in class", "Se publica en clase")))
  }
  if (k == "form") return(if (hay_form) par2("form", tx(L, "Formula sheet", "Formulario"), zona, L) else pronto(tx(L, "Formula sheet", "Formulario"), tx(L, "Coming soon", "En preparación")))
  nt <- as.integer(v)
  x <- switch(k,
    book   = par2(sprintf("book:%s", v), sprintf(tx(L, "Book · chapter %d", "Manual · tema %d"), nt), zona, L),
    slides = slides_html(v, zona, L),
    clase  = clase_html(v, zona, L),
    script = chip(sprintf("script:%s", v), sprintf(tx(L, "R scripts · chapter %d", "Scripts de R · tema %d"), nt), zona, if (L == "en") "Spanish · " else ""),
    app    = chip(sprintf("app:%s", v), sprintf(tx(L, "App · chapter %d", "App · tema %d"), nt), zona, "Shiny · "),
    stop("Código de material desconocido: ", code))
  if (!nzchar(x)) return("")
  liberable(tkey(v), x, "", zona, en_linea = TRUE)
}
materiales_profesor <- function(codes) {
  out <- character()
  for (code in unique(codes)) {
    p <- strsplit(code, ":", fixed = TRUE)[[1]]; k <- p[1]; v <- if (length(p) > 1) p[2] else NA_character_
    out <- c(out, switch(k,
      slides = c(unlist(lapply(c("es", "en"), function(L) { kk <- sprintf("pptx:slides:%s:1:%s", v, L); if (is.null(REG[[kk]])) "" else chip(kk, sprintf("Presentación T%s editable (%s)", v, toupper(L)), "teacher") })),
                 vapply(ls(REG, pattern = sprintf("^clase_ant(_pptx)?:%s:", v)), function(kk) chip(kk, sprintf("Curso anterior · %s", nombre_fich(sub("^[^:]+:[0-9]+:", "", kk))), "teacher"), "")),
      clase  = if (v == "05") chip("clase_prof:05", "Práctica de intervalos · solución", "teacher") else character(),
      act    = if (hay_sol_act(v)) { x <- ACT[[paste(v, "es", "solucion")]]; mapply(function(kk, e) chip(kk, sprintf("%s · %s", v, e), "teacher"), x$clave, x$etiqueta) } else character(),
      character()))
  }
  out[nzchar(out)]
}

# ── Páginas de los alumnos: inglés en la raíz y español en /es/ ────
hitos_todos <- bind_rows(lapply(semanas, function(s) if (length(s$hitos)) tibble(n = s$n, inicio = s$inicio, hito = s$hitos)))
evaluacion <- tibble(
  instrumento = c("Final exam · chapters 1–10: a hand-calculation exercise and a Gretl exercise (a grade of at least 5.0 is required)",
                  "Continuous assessment: Activity 1 (4 points), Activity 2 (5 points), Activity 3 in groups (5 points) and a test per chapter (0.1 points each); up to 15 points, with a maximum of 10"),
  instrumento_es = c("Examen final · temas 1–10: un ejercicio de cálculo manual y otro con Gretl (hace falta al menos un 5,0)",
                     "Evaluación continua: actividad 1 (4 puntos), actividad 2 (5 puntos), actividad 3 en grupo (5 puntos) y un test por tema (0,1 puntos cada uno); hasta 15 puntos, con un máximo de 10"),
  peso = c("60 %", "40 %"), peso_es = c("60 %", "40 %"))
AVISO_PLAN <- c(en = "This plan may change as the course progresses. Any change will be announced in class and on this website.",
                es = "Este plan puede cambiar según avance el curso. Cualquier cambio se anunciará en clase y en esta web.")

for (L in IDIOMAS) {
  ASIG <- nombre_curso(L)
  # Inicio
  inicio <- paste0(
    '<section class="hero"><p class="eyebrow">', tx(L, "Course website", "Web de la asignatura"), '</p><h1>', h(ASIG), '</h1>',
    '<p class="lede">', tx(L, "The general linear regression model from start to finish: OLS estimation, properties of the estimators, hypothesis tests and confidence intervals, specification, prediction, qualitative variables, heteroskedasticity and autocorrelation, by hand in Excel and with Gretl and R.",
                           "El modelo de regresión lineal general de principio a fin: estimación por MCO, propiedades de los estimadores, contrastes de hipótesis e intervalos de confianza, especificación, predicción, variables cualitativas, heterocedasticidad y autocorrelación, a mano con Excel y con Gretl y R."), '</p>',
    tx(L, '<ul class="facts"><li><span>Lecturer</span>Carlos de Anta Puig</li><li><span>Classes</span>One live class a week (60 or 90 minutes)</li><li><span>Chapters</span>10, in three blocks</li><li><span>Materials</span>Spanish, with English versions on the way</li></ul></section>',
          '<ul class="facts"><li><span>Profesor</span>Carlos de Anta Puig</li><li><span>Clases</span>Una clase en directo por semana (60 o 90 minutos)</li><li><span>Temas</span>10, en tres bloques</li><li><span>Materiales</span>Español (y versión inglesa en preparación)</li></ul></section>'),
    seccion_esta_semana(L),
    tx(L, paste0('<section class="grid3"><article><h2>The live class</h2><p>Each class starts from a question — how do we obtain a model?, is this variable relevant?, can we trust this forecast? — and answers it with the chapter, an example worked by hand and the software.</p></article>',
                 '<article><h2>Excel, Gretl and R</h2><p>First by hand with the matrix formulas in Excel, so that the software is never a black box; then with Gretl, the exam software, and with the R scripts and apps of each chapter.</p></article>',
                 '<article><h2>Between classes</h2><p>Read the chapter, repeat the class examples and move forward with the current activity. Each week\'s page tells you what to prepare.</p></article></section>'),
          paste0('<section class="grid3"><article><h2>La clase en directo</h2><p>Cada clase parte de una pregunta —¿cómo obtenemos un modelo?, ¿es relevante esta variable?, ¿podemos fiarnos de esta predicción?— y la responde con el tema, un ejemplo resuelto a mano y el software.</p></article>',
                 '<article><h2>Excel, Gretl y R</h2><p>Primero a mano, con las fórmulas matriciales en Excel, para que el software nunca sea una caja negra; después con Gretl, el programa del examen, y con los scripts de R y las apps de cada tema.</p></article>',
                 '<article><h2>Entre clases</h2><p>Lee el tema, repite los ejemplos de clase y avanza en la actividad en curso. La página de cada semana dice qué preparar.</p></article></section>')),
    '<section class="two"><div><h2>', tx(L, "Key dates", "Fechas clave"), '</h2><ul class="dates">',
    paste(sprintf('<li><span class="d">%s %d · %s</span><a href="%s">%s</a></li>', tx(L, "Week", "Semana"), hitos_todos$n, vapply(seq_len(nrow(hitos_todos)), function(j) rango(hitos_todos$inicio[j], L), ""),
                  u(L, sprintf("/week/%02d.html", hitos_todos$n)), h(tr_plan(hitos_todos$hito, L))), collapse = ""),
    sprintf('<li><span class="d">%s</span><span>%s</span></li>', h(tr_plan(EXAMEN_FINAL$fechas, L)), h(tr_plan(EXAMEN_FINAL$texto, L))),
    '</ul><p class="note">', tx(L, "Dates are provisional until the academic calendar is confirmed, and the plan may change as the course progresses: any change will be announced in class and on this website.",
                               "Las fechas son provisionales hasta que se confirme el calendario académico, y el plan puede cambiar según avance el curso: cualquier cambio se anunciará en clase y en esta web."), '</p></div>',
    '<div><h2>', tx(L, "Assessment", "Evaluación"), '</h2><table class="plain"><tbody>',
    paste(sprintf('<tr><td>%s</td><td class="num">%s</td></tr>', h(if (L == "es") evaluacion$instrumento_es else evaluacion$instrumento), if (L == "es") evaluacion$peso_es else evaluacion$peso), collapse = ""),
    '</tbody></table></div></section>',
    '<section><h2>', tx(L, "Start here", "Empieza aquí"), '</h2><div class="dls">',
    sprintf('<a class="dl" href="%s"><span class="dl-l">%s</span><span class="dl-m">%s</span></a>', u(L, "/schedule.html"), tx(L, "Schedule", "Calendario"), tx(L, "Week by week", "Semana a semana")),
    sprintf('<a class="dl" href="%s"><span class="dl-l">%s</span><span class="dl-m">%s</span></a>', u(L, "/materials.html"), tx(L, "Materials", "Materiales"), tx(L, "By chapter, as they are published", "Por temas, según se publican")),
    sprintf('<a class="dl" href="%s"><span class="dl-l">%s</span><span class="dl-m">%s</span></a>', u(L, "/activities.html"), tx(L, "Activities", "Actividades"), tx(L, "Briefs, data and rubrics", "Enunciados, datos y rúbricas")),
    material_html("form", L = L), '</div></section>',
    js_esta_semana(semanas, L))
  pagina(ruta_l(L, "home.html"), ASIG, inicio, actual = "Home")

  # Calendario
  filas_cal <- filas_calendario(semanas, L, function(L, desde) paste(rango(desde, L), "·", tx(L, "Easter break · no classes", "Semana Santa · no hay clase")),
                                EXAMEN_FINAL$fechas, tx(L, "chapters 1–10", "temas 1–10"))
  pagina(ruta_l(L, "schedule.html"), paste(tx(L, "Schedule", "Calendario"), "·", ASIG), paste0(
    tx(L, '<h1>Schedule</h1><p class="lede">Fifteen teaching weeks with one live class a week, of 60 or 90 minutes (two in week 11), an Easter break and the exam week. The detailed agenda and materials for each week appear when its chapter is published.</p>',
          '<h1>Calendario</h1><p class="lede">Quince semanas con una clase en directo por semana, de 60 o de 90 minutos (dos en la semana 11), parada de Semana Santa y semana de exámenes. La agenda detallada y los materiales de cada semana aparecen cuando se publica su tema.</p>'),
    '<p class="notice">', AVISO_PLAN[[L]], '</p>',
    '<div class="tbl"><table class="cal"><thead><tr><th>', tx(L, "Week", "Semana"), '</th><th>', tx(L, "Topic", "Tema"), '</th><th>', tx(L, "Sessions", "Sesiones"), '</th><th>',
    tx(L, "Milestones", "Hitos"), '</th></tr></thead><tbody>', paste(filas_cal, collapse = ""), '</tbody></table></div>'),
    actual = "Schedule")

  # Materiales
  filas_mat <- vapply(seq_len(nrow(caps)), function(i) {
    n <- caps$nn[i]
    descargas <- sprintf('<div class="dls">%s%s%s%s%s</div>',
      par2(sprintf("book:%s", n), tx(L, "Book chapter", "Capítulo del manual"), L = L), slides_html(n, "public", L), clase_html(n, "public", L),
      chip(sprintf("script:%s", n), tx(L, "R scripts", "Scripts de R"), meta = if (L == "en") "Spanish · " else ""), chip(sprintf("app:%s", n), "App", meta = "Shiny · "))
    sprintf('<tr><td class="wk">%d</td><td><b>%s</b><small lang="%s">%s</small></td><td>%s</td></tr>',
            i, h(tx(L, caps$titulo_en[i], caps$titulo_es[i])), setdiff(IDIOMAS, L), h(tx(L, caps$titulo_es[i], caps$titulo_en[i])),
            liberable(tkey(n), descargas, tx(L, "Available once this chapter is published.", "Disponible cuando se publique este tema.")))
  }, "")
  pagina(ruta_l(L, "materials.html"), paste(tx(L, "Materials", "Materiales"), "·", ASIG), paste0(
    tx(L, '<h1>Materials</h1><p class="lede">Each chapter is published when we reach it in class: the book chapter, the slides, the Excel files and data used in class, the R scripts and the app. The materials are in Spanish for now; English versions will appear here as they are ready.</p>',
          '<h1>Materiales</h1><p class="lede">Cada tema se publica cuando llegamos a él en clase: el capítulo del manual, las presentaciones, los Excel y datos de clase, los scripts de R y la app.</p>'),
    '<div class="dls">', material_html("form", L = L), '</div>',
    '<div class="tbl"><table class="mat"><thead><tr><th>#</th><th>', tx(L, "Chapter", "Tema"), '</th><th>', tx(L, "Downloads", "Descargas"), '</th></tr></thead><tbody>', paste(filas_mat, collapse = ""), '</tbody></table></div>',
    tx(L, paste0('<section class="grid3"><article><h2>Excel by hand</h2><p>The class workbooks repeat the calculations with the matrix formulas (MMULT, MINVERSE, TRANSPOSE): the same work you will do in the hand-calculation part of the exam.</p></article>',
                 '<article><h2>R scripts</h2><p>Each zip is an RStudio project with the data and the scripts of the chapter: open the <code>.Rproj</code> file and run the scripts block by block.</p></article>',
                 '<article><h2>Apps</h2><p>The apps are Shiny applications: unzip and run <code>shiny::runApp()</code> in RStudio. They generate the data they use.</p></article></section>'),
          paste0('<section class="grid3"><article><h2>Excel a mano</h2><p>Los Excel de clase repiten los cálculos con las fórmulas matriciales (MMULT, MINVERSA, TRANSPONER): el mismo trabajo de la parte manual del examen.</p></article>',
                 '<article><h2>Scripts de R</h2><p>Cada zip es un proyecto de RStudio con los datos y los scripts del tema: abre el fichero <code>.Rproj</code> y ejecuta los scripts bloque a bloque.</p></article>',
                 '<article><h2>Apps</h2><p>Las apps son aplicaciones Shiny: descomprime y ejecuta <code>shiny::runApp()</code> en RStudio. Generan los datos que usan.</p></article></section>'))),
    actual = "Materials")

  # Actividades
  EN <- L == "en"; L2 <- setdiff(IDIOMAS, L)
  cards <- vapply(seq_len(nrow(acts)), function(i) {
    a <- acts[i, ]
    lista <- function(tipo, idioma) { x <- ACT[[paste(a$id, idioma, tipo)]]; paste(mapply(function(k, e) chip(k, e, meta = if (EN && idioma == "es") "Spanish · " else ""), x$clave, if (EN) x$etiqueta_en else x$etiqueta), collapse = "") }
    bloque_fich <- function(tipo) {
      propio <- nrow(ACT[[paste(a$id, L, tipo)]]) > 0; otro <- nrow(ACT[[paste(a$id, L2, tipo)]]) > 0
      if (propio && otro) sprintf('<div class="dls">%s</div><details><summary lang="%s">%s</summary><div class="dls">%s</div></details>', lista(tipo, L), L2, tx(L, "Versión en español", "English version"), lista(tipo, L2))
      else sprintf('<div class="dls">%s</div>', lista(tipo, if (propio) L else L2))
    }
    cuerpo_desc <- if (hay_act(a$id)) liberable(a$id, bloque_fich("alumno"), tx(L, "The brief and data will be published when the activity is launched.", "El enunciado y los datos se publicarán cuando se lance la actividad."))
                   else sprintf('<div class="dls">%s</div>', pronto(tx(L, "Brief and data", "Enunciado y datos"), tx(L, "Coming soon", "En preparación")))
    cuerpo_sol <- if (hay_sol_act(a$id)) liberable(paste0("solutions-", a$id), bloque_fich("solucion"), tx(L, "Published after the deadline.", "Se publica después de la fecha de entrega."))
                  else sprintf('<div class="dls">%s</div>', pronto(tx(L, "Solution", "Solución"), tx(L, "Published after the deadline", "Se publicará tras la entrega")))
    sprintf(paste0('<article class="act" id="%s"><header><span class="tag">%s</span><h2>%s</h2><p class="es" lang="%s">%s</p></header><p>%s</p>',
                   '<dl><dt>%s</dt><dd>%s</dd><dt>%s</dt><dd>%s</dd><dt>%s</dt><dd>%s</dd><dt>%s</dt><dd>%s</dd></dl>',
                   '<h3>%s</h3>%s<h3>%s</h3>%s</article>'),
            tolower(a$id), a$id, h(tx(L, a$titulo_en, a$titulo)), L2, h(tx(L, a$titulo, a$titulo_en)), h(tx(L, a$resumen, a$resumen_es)),
            tx(L, "Format", "Formato"), h(tx(L, a$formato, a$formato_es)), tx(L, "Weight", "Peso"), tx(L, a$peso, a$peso_es), tx(L, "Period", "Periodo"), tx(L, a$ventana, a$ventana_es), tx(L, "Deadline", "Entrega"), tx(L, a$entrega, a$entrega_es),
            tx(L, "Downloads", "Descargas"), cuerpo_desc, tx(L, "Solution", "Solución"), cuerpo_sol)
  }, "")
  cards_ev <- vapply(EVAL$id[EVAL$hay], function(e) {
    lista <- function(sol) { x <- ficheros_eval(e, sol); x <- x[order(x$idioma != L), ]; paste(mapply(function(k, t) chip(k, t), x$clave, if (EN) x$etiqueta_al else x$etiqueta_al_es), collapse = "") }
    sprintf('<article class="act" id="%s"><header><span class="tag">%s</span><h2>%s</h2></header><h3>%s</h3>%s<h3>%s</h3>%s</article>',
            tolower(e), e, h(tx(L, EVAL$titulo_en[EVAL$id == e], EVAL$titulo[EVAL$id == e])), tx(L, "Exam paper and data", "Enunciado y datos"),
            liberable(e, sprintf('<div class="dls">%s</div>', lista(FALSE)), tx(L, "Published in class, when we solve it together.", "Se publica en clase, cuando lo resolvemos juntos.")),
            tx(L, "Solutions", "Solucionario"),
            liberable(paste0("solutions-", e), sprintf('<div class="dls">%s</div>', lista(TRUE)), tx(L, "Published after the class.", "Se publica después de la clase.")))
  }, "")
  pagina(ruta_l(L, "activities.html"), paste(tx(L, "Activities", "Actividades"), "·", ASIG), paste0(
    tx(L, '<h1>Activities</h1><p class="lede">Three assessed activities, the third one in groups. Each one comes with a brief, the data, an answer sheet and a rubric. Submit the answer sheet the brief asks for before the deadline: outputs copied as text, never screenshots.</p>',
          '<h1>Actividades</h1><p class="lede">Tres actividades evaluables, la tercera en grupo. Cada una incluye enunciado, datos, hoja de respuestas y rúbrica. Entrega la hoja de respuestas que pide el enunciado antes de la fecha límite: salidas copiadas como texto, nunca capturas.</p>'),
    '<div class="acts">', paste(cards, collapse = ""), '</div>',
    if (length(cards_ev)) paste0(tx(L, '<h1 id="pruebas">Exam practice</h1><p class="lede">The practice exam case and the mock exam we solve in class, with their solutions. They appear here when we work on them.</p>',
                                       '<h1 id="pruebas">Preparación del examen</h1><p class="lede">El supuesto orientativo y el simulacro que resolvemos en clase, con su solucionario. Aparecen aquí cuando los trabajamos.</p>'),
                                 '<div class="acts">', paste(cards_ev, collapse = ""), '</div>') else "",
    tx(L, '<p class="note">Rules: the work must be your own; declare any external source or AI tool you have used. The chapter tests are taken on the course platform.</p>',
          '<p class="note">Normas: el trabajo debe ser propio; declara cualquier fuente externa o herramienta de IA que hayas usado. Los tests de cada tema se hacen en la plataforma del curso.</p>')),
    actual = "Activities")
}

# Páginas de cada semana (alumno en los dos idiomas y plan de clase del profesor, en español): motor
paginas_semanas(semanas, temas_de,
  pendiente = function(temas, L) {
    nt <- as.integer(temas)
    if (L == "en") sprintf("This week's agenda and materials will appear when chapter %s is published.", paste(nt, collapse = " and "))
    else sprintf("La agenda y los materiales de esta semana aparecerán cuando se publique el tema %s.", paste(nt, collapse = " y "))
  },
  extra_profesor = function(s, temas) {
    pm <- materiales_profesor(unlist(lapply(s$sesiones, `[[`, "materiales")))
    paste0(if (length(pm)) sprintf('<section><h2>Material del profesor</h2><div class="dls">%s</div></section>', paste(pm, collapse = "")) else "",
           paste(vapply(EVAL$id[EVAL$hay & vapply(EVAL$id, function(e) identical(semana_eval(e), s$n), TRUE)], function(e)
             sprintf('<section><h2>%s</h2><div class="dls">%s</div></section>', h(EVAL$titulo[EVAL$id == e]), chips_eval(e)), ""), collapse = ""))
  })

# Portada abierta (solo el nombre y el acceso; el resto pide la clave de los alumnos) y /es/ → inicio en español
portada_abierta()

# ── Zona del profesor ──────────────────────────────────────────────
cuando_prueba <- function(e) { n <- semana_eval(e); if (e == "F1") h(tr_plan(EXAMEN_FINAL$fechas, "es")) else if (is.na(n)) "—" else sprintf("Semana %d", n) }
faltan <- c(if (!all(vapply(acts$id, hay_act, TRUE))) "las actividades que faltan",
            if (!hay_form) "el formulario (hoja de fórmulas y tablas de valores críticos)",
            if (!all(EVAL$hay)) "los modelos del examen final de este curso")
pagina_plan_docente(semanas,
  titulo = "Plan docente 2026-27 · segundo cuatrimestre",
  hechos_antes = c("Asignatura" = "Econometría · 10 temas en tres bloques: fundamentos del MRLG, evaluación y análisis, avanzando en el MRLG",
                   "Clases" = "Una clase en directo por semana, de 60 minutos y de 90 en las semanas 1, 6, 7, 8, 11 y 14 (dos clases en la semana 11) · días provisionales: lunes; la segunda de la semana 11, jueves"),
  hechos_despues = c("Idioma" = "Docencia en español; web en inglés y en español (materiales en español, versión inglesa en preparación)",
                     "Software" = "Excel (cálculo manual), Gretl (examen) y R"),
  evaluacion = tibble(instrumento = evaluacion$instrumento_es, peso = evaluacion$peso_es),
  nota_evaluacion = '<p class="notice">Pesos de la programación de 2025-26: evaluación continua 40 % (las actividades y los tests suman 15 puntos y la nota máxima es 10) y examen final 60 %, que hay que aprobar. El examen tiene dos ejercicios de 5 puntos: cálculo manual y Gretl (acta de coordinación del 16/03/2026). Confírmalo con la programación de este curso.</p>',
  actividades = tibble(id = acts$id, titulo = acts$titulo, formato = acts$formato_es, peso = acts$peso_es, periodo = acts$ventana_es, entrega = acts$entrega_es,
                       resumen = paste0(acts$resumen_es, ifelse(vapply(acts$id, hay_act, TRUE), "", " (pendiente de escribir)"))),
  pruebas = tibble(id = EVAL$id, titulo = EVAL$titulo, cuando = vapply(EVAL$id, cuando_prueba, ""), nota = c("Sin nota · en clase", "Sin nota · en clase", "60 % · mínimo 5,0")),
  pruebas_semana = function(n) EVAL$titulo[vapply(EVAL$id, function(e) identical(semana_eval(e), n), TRUE)],
  examen = sprintf("%s · temas 1–10", h(tr_plan(EXAMEN_FINAL$fechas, "es"))),
  materiales = c("Manual «Econometría: manual práctico con R» (español), por capítulos en la web y completo en tu zona.",
                 "Presentaciones de clase de los 10 temas (PDF para los alumnos; PowerPoint editable en tu zona, también el Tema 0 y el simulacro).",
                 "Excel y datos de clase por tema; scripts de R de cada tema (proyecto de RStudio con los datos) y una app Shiny por tema.",
                 "Actividades A1–A3 de 2026-27 con datos, hoja de respuestas, rúbrica y, para el profesor, script y solución comentada.",
                 "Supuesto orientativo de examen (S0) y simulacro (S1) con sus solucionarios."),
  pendientes = c("Confirmar el calendario real con la programación de 2026-27: fechas de las 16 semanas, días y horas de las clases, Semana Santa y semana de exámenes (ahora, las de 2025-26 llevadas a marzo de 2027).",
                 "Confirmar qué actividad es en grupo: la programación de 2025-26 pone A2 en grupo y A3 individual; los enunciados de 2026-27 tienen A2 individual y A3 en grupo (la web sigue los enunciados). Revisar también la titulación que figura en la cabecera de los enunciados.",
                 "La A3 pide heterocedasticidad y autocorrelación y se entrega el 1 de junio, antes de la clase de autocorrelación (semana 14): adelantar el tema 10 o mover la entrega.",
                 if (length(faltan)) paste0("Preparar ", if (length(faltan) > 1) paste(paste(faltan[-length(faltan)], collapse = ", "), "y", faltan[length(faltan)]) else faltan, "."),
                 "Versión inglesa de los materiales (manual, presentaciones y actividades): cuando existan, la web los recoge solos.",
                 "Crear el proxy de econometrics.carlosdeanta.net en Nginx Proxy Manager y la clave del profesor; dar de alta los grupos y los usuarios de los alumnos."),
  texto_pausa = "Semana Santa · sin clases")

# Liberar contenidos
semanas_de_tema <- function(nn) paste(vapply(Filter(function(s) nn %in% temas_de(s), semanas), function(s) as.character(s$n), ""), collapse = ", ")
filas_temas <- vapply(seq_len(nrow(caps)), function(i) fila_lib(tkey(caps$nn[i]), sprintf("Tema %d · %s", i, caps$titulo_es[i]),
  sprintf("Semanas %s · capítulo del manual, presentaciones, Excel de clase, scripts y app", semanas_de_tema(caps$nn[i]))), "")
filas_acts <- vapply(seq_len(nrow(acts)), function(i) fila_lib(acts$id[i], sprintf("Actividad %s · %s", sub("A", "", acts$id[i]), acts$titulo[i]),
  sprintf("%s · entrega: %s · %s", acts$ventana_es[i], acts$entrega_es[i], if (hay_act(acts$id[i])) "enunciado, datos, hoja de respuestas y rúbrica" else "pendiente de escribir")), "")
filas_sols <- vapply(seq_len(nrow(acts)), function(i) fila_lib(paste0("solutions-", acts$id[i]), sprintf("Solución de la actividad %s", sub("A", "", acts$id[i])),
  sprintf("Publicar después de la entrega (%s)", acts$entrega_es[i])), "")
cuando_eval <- function(e) { n <- semana_eval(e); if (e == "F1") tr_plan(EXAMEN_FINAL$fechas, "es") else if (is.na(n)) "" else sprintf("semana %d", n) }
filas_ev <- vapply(EVAL$id[EVAL$hay], function(e) fila_lib(e, EVAL$titulo[EVAL$id == e], sprintf("%s · publicar al empezar la clase", cuando_eval(e))), "")
filas_ev_sol <- vapply(EVAL$id[EVAL$hay], function(e) fila_lib(paste0("solutions-", e), sprintf("Solucionario · %s", EVAL$titulo[EVAL$id == e]),
  sprintf("%s · publicar al terminar la clase", cuando_eval(e))), "")
pagina_liberar(c(tabla_lib("Temas", filas_temas), tabla_lib("Actividades", filas_acts), tabla_lib("Soluciones de las actividades", filas_sols),
                 if (length(filas_ev)) c(tabla_lib("Supuesto orientativo y simulacro", filas_ev), tabla_lib("Solucionarios", filas_ev_sol))))

# Acceso de alumnos (usuarios, enlaces de clase y modo de la web) y Grupos y avance: páginas comunes del motor
pagina_acceso()
pagina_grupos(semanas)

# Portada del profesor (en español)
filas_prof <- vapply(semanas, function(s) sprintf('<tr><td class="wk">%d</td><td>%s</td><td><a class="tt" href="/profesor/week/%02d.html">%s</a><small>%s</small></td><td>%s</td></tr>',
  s$n, rango(s$inicio, "es"), s$n, h(tr_plan(s$titulo, "es")), h(paste(vapply(s$sesiones, function(x) paste0(etiqueta_corta(x, "es"), ": ", tr_plan(x$titulo, "es")), ""), collapse = " · ")),
  paste(sprintf('<span class="pill">%s</span>', h(tr_plan(s$hitos, "es"))), collapse = " ")), "")
sol_caps <- vapply(seq_len(nrow(caps)), function(i) {
  n <- caps$nn[i]
  sprintf('<tr><td class="wk">%d</td><td><b>%s</b></td><td><div class="dls">%s%s%s%s</div></td></tr>', i, h(caps$titulo_es[i]),
          par2(sprintf("book:%s", n), "Capítulo del manual", "teacher", L = "es"), slides_html(n, "teacher", "es"),
          paste(materiales_profesor(c(sprintf("slides:%s", n), sprintf("clase:%s", n))), collapse = ""), clase_html(n, "teacher", "es"))
}, "")
sol_acts <- vapply(acts$id, function(a) {
  fichs <- if (hay_act(a)) paste(c({ x <- ACT[[paste(a, "es", "alumno")]]; mapply(function(k, e) chip(k, e, "teacher"), x$clave, x$etiqueta) },
                                  materiales_profesor(paste0("act:", a))), collapse = "")
           else pronto("Sin generar", "Pendiente de escribir la actividad")
  sprintf('<tr id="%s"><td class="wk">%s</td><td><b>%s</b></td><td><div class="dls">%s</div></td></tr>', tolower(a), a, h(acts$titulo[acts$id == a]), fichs)
}, "")
filas_eval <- vapply(EVAL$id, function(e) {
  n <- semana_eval(e)
  cuando <- if (e == "F1") h(tr_plan(EXAMEN_FINAL$fechas, "es")) else if (is.na(n)) "—" else sprintf('<a href="/profesor/week/%02d.html">Semana %d</a>', n, n)
  sprintf('<tr><td class="wk">%s</td><td><b>%s</b><small>%s</small></td><td><div class="dls">%s</div></td></tr>', e, h(EVAL$titulo[EVAL$id == e]), cuando,
          if (!EVAL$hay[EVAL$id == e]) pronto("Sin generar", "Pendiente: modelos del examen de este curso") else chips_eval(e))
}, "")
pagina("profesor/index.html", paste("Zona del profesor ·", CURSO$nombre_es), paste0(
  '<p class="eyebrow">Zona del profesor</p><h1>', h(CURSO$nombre_es), '</h1>',
  '<p class="lede">Plan de cada clase por minutos con notas docentes, publicación de contenidos, versiones del profesor y soluciones. Esta zona está protegida con usuario y clave en el servidor.</p>',
  '<div class="dls big"><a class="dl primary" href="/profesor/liberar.html"><span class="dl-l">Liberar contenidos</span><span class="dl-m">Temas, actividades y soluciones</span></a>',
  '<a class="dl primary" href="/profesor/grupos.html"><span class="dl-l">Grupos y avance</span><span class="dl-m">Horarios, aulas y lo que toca en la próxima clase</span></a>',
  '<a class="dl primary" href="/profesor/acceso.html"><span class="dl-l">Acceso de alumnos</span><span class="dl-m">Usuarios, enlaces de clase y modo de la web</span></a>',
  '<a class="dl" href="/profesor/plan-docente.html"><span class="dl-l">Plan docente</span><span class="dl-m">Semanas, evaluación y pendientes</span></a>',
  chip("bookpdf:es", "Manual completo", "teacher"),
  chip("pptx:T00_Presentacion_asignatura.pptx", "Tema 0 · presentación de la asignatura (PowerPoint)", "teacher"),
  chip("pptx:S1_Simulacro.pptx", "Simulacro · presentación (PowerPoint)", "teacher"),
  if (hay_form) par2("form", "Formulario (siempre visible para los alumnos)", "teacher", L = "es") else "", '</div>',
  '<p class="notice">Calendario y horario provisionales: la programación de 2025-26 llevada a marzo de 2027 (15 semanas de clase desde el 1 de marzo, Semana Santa del 22 al 28 de marzo y semana de exámenes del 21 al 25 de junio) y las clases en lunes. Cuando se confirmen, se ajustan las fechas y <code>DIAS</code> en <code>web/plan_semanas.R</code> y <code>CURSO$dias</code> en <code>web/build_web.R</code>, y se vuelve a publicar. La presentación del Tema 0 no se publica a los alumnos porque lleva la clave del servidor RStudio.</p>',
  '<h2 id="clases">Clases por semana</h2><div class="tbl"><table class="cal"><thead><tr><th>Sem.</th><th>Fechas</th><th>Tema y clases</th><th>Hitos</th></tr></thead><tbody>', paste(filas_prof, collapse = ""), '</tbody></table></div>',
  '<h2 id="soluciones">Material por tema</h2>',
  '<div class="tbl"><table class="mat"><thead><tr><th>#</th><th>Tema</th><th>Ficheros</th></tr></thead><tbody>', paste(sol_caps, collapse = ""), '</tbody></table></div>',
  '<h2 id="actividades">Actividades</h2><p>Ficheros del alumno, script de la solución y solución comentada. La solución se publica a los alumnos desde <a href="/profesor/liberar.html">Liberar contenidos</a>.</p>',
  '<div class="tbl"><table class="mat"><thead><tr><th></th><th>Actividad</th><th>Ficheros</th></tr></thead><tbody>', paste(sol_acts, collapse = ""), '</tbody></table></div>',
  '<h2 id="evaluaciones">Supuesto orientativo, simulacro y examen</h2><p>Los alumnos no los ven hasta que los publicas en <a href="/profesor/liberar.html">Liberar contenidos</a>.</p>',
  '<div class="tbl"><table class="mat"><thead><tr><th></th><th>Prueba</th><th>Ficheros</th></tr></thead><tbody>', paste(filas_eval, collapse = ""), '</tbody></table></div>'),
  zona = "teacher", actual = "Inicio")

# ── Resumen ────────────────────────────────────────────────────────
cat(sprintf("Web construida · público %.1f MB · almacén %.1f MB (%d contenidos liberables) · motor %s\n",
            tam_dir(PUB), tam_dir(STORE), length(list.dirs(STORE, recursive = FALSE)), MOTOR_VERSION))
for (nm in names(paginas)) cat(sprintf("  %-9s capítulos en páginas %s\n", nm, paste(sprintf("%d–%d", paginas[[nm]]["inicio", ], paginas[[nm]]["fin", ]), collapse = " ")))
