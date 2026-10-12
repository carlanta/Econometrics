# ══════════════════════════════════════════════════════════════════
# Examen final de Econometría (F1): 10 modelos x estudios, Excel único de soluciones y plantillas de corrección
#   Modelos con la nomenclatura del autor (la universidad decide cuál sale en cada fecha):
#     ordinaria ORD_A…ORD_F y ORD_1, extraordinaria EXT_A…EXT_C.
#   TODOS LOS MODELOS DE UNA CONVOCATORIA SE CORRIGEN CON LA MISMA SOLUCIÓN: mismos números (datos del ejercicio 1 y
#   especificación del ejercicio 2); solo cambian el contexto, los nombres de las variables y el orden de las filas.
#   La extraordinaria tiene sus propios números. Semilla = curso x 100 + 1 (ordinaria) o + 2 (extraordinaria): al
#   cambiar `curso` cambian los números, los contextos y la especificación del ejercicio 2.
#   ORD_1 es un modelo más de la ordinaria (en los modelos reales no tiene ninguna variante especial).
#   El enunciado no lleva fecha (la pone la universidad): solo «Convocatoria: Ordinaria / Extraordinaria».
#
#   Desde RStudio (proyecto mEconometria.Rproj): cambia los parámetros de abajo y ejecuta el script entero (Source).
#   Desde la terminal, en la raíz del proyecto:   Rscript R/evaluaciones/generar_examen.R [curso] [carpeta_salida]
#
#   Formato, cálculos y Excel (en git):  R/evaluaciones/examen_formato.R
#   Banco, fuera de git:                 materiales/evaluaciones/examen/examen_banco.R  (contextos, enunciados,
#                                        especificaciones del ejercicio 2 y estudios) y data9_12.csv
#   Salida, fuera de git:                materiales/evaluaciones/examen/salida/F1_<curso>/
# ══════════════════════════════════════════════════════════════════

curso            <- 2627   # ← curso académico: 2627 = 2026-27. Cámbialo y cambian los números de todos los modelos.
n_ordinaria      <- 6      # ORD_A … ORD_F
modelo_1         <- TRUE   # y además ORD_1
n_extraordinaria <- 3      # EXT_A, EXT_B, EXT_C
formatos         <- c("docx", "html", "pdf")   # formatos de los enunciados
dir_examen       <- "materiales/evaluaciones/examen"

# ── Argumentos de la línea de órdenes (opcionales) ─────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) curso <- as.integer(args[1])
if (!file.exists("mEconometria.Rproj")) stop("Ejecuta el script desde la raíz del proyecto (abre mEconometria.Rproj)")
salida <- if (length(args) >= 2) args[2] else file.path(dir_examen, "salida", sprintf("F1_%d", curso))
dir.create(salida, recursive = TRUE, showWarnings = FALSE)
salida <- normalizePath(salida)   # ruta absoluta: render() resuelve output_file respecto a la carpeta del Rmd

source("R/evaluaciones/examen_formato.R")
banco <- file.path(dir_examen, "examen_banco.R")
if (!file.exists(banco)) stop("Falta el banco del examen (fuera de git): ", banco)
source(banco, encoding = "UTF-8")   # narrativas, dic_p2, p2_specs, grados, plantilla_examen, plantilla_solucion
datos_p2_csv <- normalizePath(file.path(dir_examen, "data9_12.csv"))
d912 <- read.csv(datos_p2_csv)

# ── Modelos, códigos, especificación del ejercicio 2 y contextos ───
mods <- modelos_examen(n_ordinaria, n_extraordinaria, modelo_1)
mods$codigo <- vapply(mods$modelo, function(m) codigo_examen(curso, m), "")

set.seed(as.integer(curso) * 100L + 50L)
p2_elegidas <- sample(length(p2_specs), 2)   # distinta en la ordinaria y en la extraordinaria

set.seed(as.integer(curso) * 100L + 70L)
orden_ctx <- sample(length(narrativas), nrow(mods))

convs <- lapply(setNames(nm = unique(mods$convocatoria)), function(cv) {
  d <- datos_p1(semilla_conv(curso, cv))
  P <- calcula_p1(d$Y, d$X1, d$X2)
  P$semilla <- semilla_conv(curso, cv)
  P$p2 <- espec_p2(p2_specs[[p2_elegidas[match(cv, c("Ordinaria", "Extraordinaria"))]]], dic_p2, d912)
  P$modelos <- mods$modelo[mods$convocatoria == cv]
  P
})

exs <- lapply(seq_len(nrow(mods)), function(i) {
  narr <- narrativas[[orden_ctx[i]]]
  narr$ctx <- gsub("{ANIO}", anio_examen(curso), narr$ctx, fixed = TRUE)
  set.seed(as.integer(curso) * 1000L + 500L + i)   # orden de las filas de la tabla (la solución no cambia)
  list(modelo = mods$modelo[i], convocatoria = mods$convocatoria[i], codigo = mods$codigo[i],
       narr = narr, perm = sample(1:n))
})

nombre_examen <- function(ex, grado_codigo, ext = "Rmd") sprintf("F1_%d_%s_%s.%s", curso, ex$modelo, grado_codigo, ext)

# ── Enunciados (Rmd) ───────────────────────────────────────────────
cat(sprintf("=== GENERANDO EXÁMENES · curso %s · %d modelos ===\n\n", curso_txt(curso), nrow(mods)))
all_rmds <- character(0)
for (g in grados) {
  carpeta <- file.path(salida, g$carpeta)
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  for (ex in exs) {
    f <- file.path(carpeta, nombre_examen(ex, g$codigo))
    writeLines(rellena(plantilla_examen, valores_examen(ex, convs[[ex$convocatoria]], curso, g$nombre)), f)
    all_rmds <- c(all_rmds, f)
  }
  cat(g$codigo, ":", length(exs), "enunciados en", carpeta, "\n")
}

# ── Excel único de soluciones ──────────────────────────────────────
xl <- file.path(salida, sprintf("F1_%d_Soluciones.xlsx", curso))
excel_soluciones(xl, curso, mods, exs, convs, grados, d912, nombre_examen)
cat("Excel de soluciones:", xl, "\n")

# ── Plantillas de corrección (una por convocatoria) ────────────────
sol_rmds <- character(0)
for (cv in names(convs)) {
  P <- convs[[cv]]
  v <- list(CURSO = curso_txt(curso), CONVOCATORIA = cv, MODELOS = paste(P$modelos, collapse = ", "),
            SEMILLA = P$semilla, Y = paste(P$Y, collapse = ", "), X1 = paste(P$X1, collapse = ", "),
            X2 = paste(P$X2, collapse = ", "), P2_FORMULA = P$p2$formula_txt, P2_FORMULA_R = P$p2$formula_r,
            DATOS_P2 = datos_p2_csv)
  f <- file.path(salida, sprintf("F1_%d_%s_Solucion.Rmd", curso, pref_conv[[cv]]))
  writeLines(rellena(plantilla_solucion, v), f)
  sol_rmds <- c(sol_rmds, f)
}

# ── Renderizado ────────────────────────────────────────────────────
cat("\n=== RENDERIZANDO (", paste(formatos, collapse = " + "), ") ===\n")
for (rmd_file in all_rmds) {
  base <- tools::file_path_sans_ext(basename(rmd_file)); outdir <- dirname(rmd_file)
  cat(base, "...")
  if ("docx" %in% formatos) cat(if (render_uno(rmd_file, "word_document", file.path(outdir, paste0(base, ".docx")))) " Word ✓" else " Word ✗")
  if ("html" %in% formatos) cat(if (render_uno(rmd_file, "html_document", file.path(outdir, paste0(base, ".html")))) " HTML ✓" else " HTML ✗")
  if ("pdf"  %in% formatos) cat(if (render_uno(rmd_file, pdf_fmt,        file.path(outdir, paste0(base, ".pdf"))))  " PDF ✓"  else " PDF ✗")
  cat("\n")
}
for (sol_rmd in sol_rmds) {
  base <- tools::file_path_sans_ext(basename(sol_rmd))
  cat(base, "...")
  cat(if (render_uno(sol_rmd, "html_document", file.path(salida, paste0(base, ".html")))) " HTML ✓" else " HTML ✗")
  cat(if (render_uno(sol_rmd, pdf_fmt, file.path(salida, paste0(base, ".pdf")))) " PDF ✓" else " PDF ✗")
  cat("\n")
}

# ── Resumen ────────────────────────────────────────────────────────
cat("\n")
for (cv in names(convs)) {
  P <- convs[[cv]]
  cat(sprintf("%-14s semilla %d · modelos %s\n  Ej.1: b = (%.4f, %.4f, %.4f) · R2 = %.4f · s2 = %.4f · Ej.2: %s\n",
              cv, P$semilla, paste(P$modelos, collapse = " "), P$beta[1], P$beta[2], P$beta[3], P$R2, P$s2, P$p2$formula_r))
}
for (ex in exs) cat(sprintf("  %-14s %-6s %s · %s (%s)\n", ex$convocatoria, ex$modelo, ex$codigo, ex$narr$persona, ex$narr$negocio))
cat("Salida:", salida, "\n")
