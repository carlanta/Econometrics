# ══════════════════════════════════════════════════════════════════
# mEconometria · Examen final (F1): semillas, cálculos, plantillas y Excel de soluciones
#   Dos ejercicios de 5 puntos: (1) cálculo manual de un MCO con dos regresores y 8 observaciones a partir de tres
#   propuestas (una correcta) y (2) estimación con Gretl sobre data9-12 (Ramanathan) con contrastes.
#   Varios modelos por convocatoria (ordinaria ORD_A…ORD_F y ORD_1, extraordinaria EXT_A…EXT_C) con los MISMOS
#   NÚMEROS y distinto contexto: una sola solución por convocatoria. Sin fecha (la pone la universidad).
#   Aquí solo está lo genérico; los contextos, las especificaciones del ejercicio 2, los textos de los enunciados y
#   los estudios a los que va el examen viven en el banco, fuera de git
#   (materiales/evaluaciones/examen/examen_banco.R). Lo llama R/evaluaciones/generar_examen.R.
# ══════════════════════════════════════════════════════════════════

library(openxlsx)
library(rmarkdown)

curso_txt   <- function(curso) sprintf("20%02d-%02d", curso %/% 100, curso %% 100)   # 2627 -> "2026-27"
anio_examen <- function(curso) 2000L + as.integer(curso %% 100)                      # 2627 -> 2027

# ── Modelos, semillas y códigos ────────────────────────────────────
modelos_examen <- function(n_ord, n_ext, ord_1 = TRUE)
  data.frame(
    modelo = c(paste0("ORD_", LETTERS[seq_len(n_ord)]), if (ord_1) "ORD_1", paste0("EXT_", LETTERS[seq_len(n_ext)])),
    convocatoria = rep(c("Ordinaria", "Extraordinaria"), c(n_ord + ord_1, n_ext)),
    stringsAsFactors = FALSE)
pref_conv <- c(Ordinaria = "ORD", Extraordinaria = "EXT")

# Semilla de los números: depende del curso y de la convocatoria, NO del modelo (una solución por convocatoria)
semilla_conv <- function(curso, convocatoria) as.integer(curso) * 100L + c(Ordinaria = 1L, Extraordinaria = 2L)[[convocatoria]]

# Código de examen de 8 caracteres hexadecimales, uno por modelo (para la hoja ID)
codigo_examen <- function(curso, modelo) {
  cod <- utf8ToInt(modelo)
  set.seed(as.integer(curso) * 1000L + sum(cod * seq_along(cod)))
  paste(sample(c(0:9, LETTERS[1:6]), 8, replace = TRUE), collapse = "")
}

# ── Ejercicio 1: datos y cálculos ──────────────────────────────────
# 8 observaciones enteras; X1 = precio propio (8-22), X2 = precio de la competencia (muy correlacionado con X1),
# Y = ventas en cientos de euros. Se exige: b0 > 0, b1 < 0, b2 > 0, R2 entre 0,75 y 0,98 y que ningún elemento de
# (X'X)^-1 salga 0,0000 con cuatro decimales en el anexo.
n <- 8; k <- 2; gl <- n - k - 1

datos_p1 <- function(semilla) {
  set.seed(semilla)
  for (intento in 1:20000) {
    X1 <- sample(8:22, n)
    X2 <- round(36 - 0.85 * X1 + rnorm(n, 0, 1.3))
    b  <- c(runif(1, 1, 6), runif(1, -0.40, -0.15), runif(1, 0.10, 0.35))
    Y  <- round(b[1] + b[2] * X1 + b[3] * X2 + rnorm(n, 0, 0.6))
    if (any(Y < 2 | Y > 12) || length(unique(Y)) < 4 || abs(cor(X1, X2)) > 0.985) next
    X <- cbind(1, X1, X2)
    XtXi <- tryCatch(solve(t(X) %*% X), error = function(e) NULL)
    if (is.null(XtXi) || min(abs(XtXi)) < 0.0005) next
    beta <- XtXi %*% t(X) %*% Y
    R2 <- sum((X %*% beta - mean(Y))^2) / sum((Y - mean(Y))^2)
    if (beta[1] > 0 && beta[2] < -0.05 && beta[3] > 0.05 && R2 > 0.75 && R2 < 0.98)
      return(list(Y = Y, X1 = X1, X2 = X2))
  }
  stop("No se encontraron datos válidos para la semilla ", semilla)
}

# Todos los cálculos del ejercicio 1 (la solución única) y las tres propuestas
calcula_p1 <- function(Y, X1, X2) {
  X     <- cbind(1, X1, X2)
  XtXi  <- solve(t(X) %*% X)
  XtY   <- t(X) %*% Y
  beta  <- XtXi %*% XtY
  Yhat  <- X %*% beta
  YtY   <- as.numeric(t(Y) %*% Y)
  bXtY  <- as.numeric(t(beta) %*% XtY)
  SCT   <- sum((Y - mean(Y))^2)
  SCE   <- sum((Yhat - mean(Y))^2)
  SCR   <- sum((Y - Yhat)^2)
  s2    <- SCR / gl
  t_tab <- data.frame(alpha    = c("10%", "5%", "1%"),
                      t_alpha  = round(c(qt(0.90,gl), qt(0.95,gl), qt(0.99,gl)), 5),
                      t_alpha2 = round(c(qt(0.95,gl), qt(0.975,gl), qt(0.995,gl)), 5))
  # MARÍA — correcta
  maria <- list(XtY = XtY, beta = beta, bXtY = bXtY, YtY = YtY, SCT = SCT, SCE = SCE, SCR = SCR)
  # JUAN — error: intercambia X1 y X2 en X'Y (y su Y'Y es incoherente)
  juan_XtY  <- matrix(c(XtY[1], XtY[3], XtY[2]), ncol = 1)
  juan_beta <- XtXi %*% juan_XtY
  juan <- list(XtY = juan_XtY, beta = juan_beta, bXtY = as.numeric(t(juan_beta) %*% juan_XtY),
               YtY = YtY + 10, SCT = SCT, SCE = SCE, SCR = SCR)
  # ANA — error: X'Y mal escalado
  ana_XtY  <- matrix(c(sum(Y)/2, sum(X1*Y)/10, sum(X2*Y)/10), ncol = 1)
  ana_beta <- XtXi %*% ana_XtY
  ana <- list(XtY = ana_XtY, beta = ana_beta, bXtY = as.numeric(t(ana_beta) %*% ana_XtY),
              YtY = YtY, SCT = SCT, SCE = SCE, SCR = SCR)
  list(Y = Y, X1 = X1, X2 = X2, XtXi = XtXi, XtY = XtY, beta = beta, YtY = YtY, bXtY = bXtY,
       SCT = SCT, SCE = SCE, SCR = SCR, R2 = SCE / SCT, R2adj = 1 - (SCR/gl) / (SCT/(n-1)),
       s2 = s2, se_b = sqrt(diag(s2 * XtXi)), var_yhat = SCE / n, t_tab = t_tab,
       maria = maria, juan = juan, ana = ana)
}

# ── Ejercicio 2: especificación (vars = regresores; dic = descripción de cada variable) ──
espec_p2 <- function(vars, dic, datos, dep = "pcecars") {
  b <- paste0("b", seq_along(vars), "*", vars, "_i", collapse = " + ")
  txt <- dic[c(dep, vars)]
  list(vars = vars,
       formula_txt = paste0(dep, "_i = b0 + ", b, " + u_i"),
       formula_r = paste(dep, "~", paste(vars, collapse = " + ")),
       desc = paste0("Donde ", paste(txt[-length(txt)], collapse = "; "), "; y ", txt[length(txt)], "."),
       modelo = lm(as.formula(paste(dep, "~", paste(vars, collapse = " + "))), data = datos))
}

# ── Plantillas: {{MARCADOR}} -> valor, literal (sin tocar las barras invertidas del LaTeX) ──
sustituye <- function(x, pat, rep) {
  out <- ""
  repeat {
    p <- regexpr(pat, x, fixed = TRUE)
    if (p < 0) break
    out <- paste0(out, substr(x, 1, p - 1), rep)
    x <- substr(x, p + nchar(pat), nchar(x))
  }
  paste0(out, x)
}
rellena <- function(plantilla, valores) {
  for (nm in names(valores)) plantilla <- sustituye(plantilla, paste0("{{", nm, "}}"), as.character(valores[[nm]]))
  if (grepl("{{", plantilla, fixed = TRUE)) stop("Quedan marcadores sin rellenar en la plantilla")
  plantilla
}
f4 <- function(x) sprintf("%.4f", x)
f0 <- function(x) sprintf("%.0f", x)
f1 <- function(x) sprintf("%.1f", x)

# Bloque de una propuesta (María, Juan o Ana) para el anexo
bloque_propuesta <- function(p, fmt_xty = f0) {
  paste0(
    "$X'Y = \\begin{pmatrix} ", paste(fmt_xty(p$XtY), collapse = " \\\\ "), " \\end{pmatrix}$ $\\quad$ ",
    "$(X'X)^{-1}X'Y = \\begin{pmatrix} ", paste(f4(p$beta), collapse = " \\\\ "), " \\end{pmatrix}$\n\n",
    "$\\hat{\\beta}'X'Y = ", f4(p$bXtY), "$ $\\quad$ $Y'Y = ", f0(p$YtY), "$\n\n",
    "$\\sum(y_i-\\bar{y})^2 = ", f4(p$SCT), "$ $\\quad$ $\\sum(\\hat{y}_i-\\bar{y})^2 = ", f4(p$SCE),
    "$ $\\quad$ $\\sum(y_i-\\hat{y}_i)^2 = ", f4(p$SCR), "$")
}

# Valores de los marcadores de un enunciado (ex: modelo, convocatoria, código, narr, perm; P: su convocatoria)
valores_examen <- function(ex, P, curso, grado_nombre) {
  narr <- ex$narr; perm <- ex$perm
  list(
    MODELO = ex$modelo, CONVOCATORIA = ex$convocatoria, CODIGO = ex$codigo, CURSO = curso_txt(curso),
    GRADO = grado_nombre, CTX = narr$ctx,
    Y = narr$y_name, X1 = narr$x1_name, X2 = narr$x2_name, y_min = tolower(narr$y_name),
    P2_FORMULA = P$p2$formula_txt, P2_DESC = P$p2$desc,
    TABLA = paste(sprintf("| %d | %g | %g | %g |", 1:n, P$Y[perm], P$X1[perm], P$X2[perm]), collapse = "\n"),
    I11 = f4(P$XtXi[1,1]), I12 = f4(P$XtXi[1,2]), I13 = f4(P$XtXi[1,3]),
    I21 = f4(P$XtXi[2,1]), I22 = f4(P$XtXi[2,2]), I23 = f4(P$XtXi[2,3]),
    I31 = f4(P$XtXi[3,1]), I32 = f4(P$XtXi[3,2]), I33 = f4(P$XtXi[3,3]),
    VYHAT = f4(P$var_yhat), VB0 = f4(P$s2*P$XtXi[1,1]), VB1 = f4(P$s2*P$XtXi[2,2]), VB2 = f4(P$s2*P$XtXi[3,3]),
    GL = gl,
    T10 = sprintf("%.5f", P$t_tab$t_alpha[1]), T10b = sprintf("%.5f", P$t_tab$t_alpha2[1]),
    T5  = sprintf("%.5f", P$t_tab$t_alpha[2]), T5b  = sprintf("%.5f", P$t_tab$t_alpha2[2]),
    T1  = sprintf("%.5f", P$t_tab$t_alpha[3]), T1b  = sprintf("%.5f", P$t_tab$t_alpha2[3]),
    MARIA = bloque_propuesta(P$maria), JUAN = bloque_propuesta(P$juan), ANA = bloque_propuesta(P$ana, f1))
}

# ── Excel único de soluciones: hoja ID + 3 hojas por convocatoria ──
excel_soluciones <- function(f, curso, mods, exs, convs, grados, datos_p2, nombre_examen) {
  wb <- createWorkbook(creator = "Carlos de Anta Puig")
  sty_header  <- createStyle(textDecoration = "bold", fgFill = "#4472C4", fontColour = "white", halign = "center")
  sty_result  <- createStyle(fgFill = "#E2EFDA", numFmt = "0.0000")
  sty_formula <- createStyle(fgFill = "#FFF2CC", numFmt = "0.0000")
  sty_titulo  <- createStyle(textDecoration = "bold", fontSize = 13)

  # --- Hoja ID: modelo, convocatoria, código y hojas de solución ---
  addWorksheet(wb, "ID")
  writeData(wb, "ID", sprintf("Examen final de Econometría · curso %s · %d modelos", curso_txt(curso), nrow(mods)), startRow = 1)
  addStyle(wb, "ID", sty_titulo, rows = 1, cols = 1)
  writeData(wb, "ID", "Todos los modelos de una convocatoria tienen los mismos números (semilla = curso x 100 + 1 ordinaria, + 2 extraordinaria): una sola solución por convocatoria; solo cambian el contexto, los nombres de las variables y el orden de las filas.", startRow = 2)
  id <- data.frame(
    MODELO = mods$modelo, CONVOCATORIA = mods$convocatoria, CÓDIGO = mods$codigo, CURSO = curso_txt(curso),
    SEMILLA = vapply(mods$convocatoria, function(cv) semilla_conv(curso, cv), 0L),
    CONTEXTO = vapply(exs, function(e) paste0(e$narr$persona, " (", e$narr$negocio, ")"), ""),
    VARIABLES_EJ1 = vapply(exs, function(e) paste(e$narr$y_name, e$narr$x1_name, e$narr$x2_name, sep = " ~ "), ""),
    EJERCICIO_2 = vapply(mods$convocatoria, function(cv) convs[[cv]]$p2$formula_r, ""),
    HOJAS_SOLUCION = paste0(pref_conv[mods$convocatoria], "_*"),
    check.names = FALSE, stringsAsFactors = FALSE)
  for (g in grados) id[[paste0("ENUNCIADO_", g$codigo)]] <- vapply(exs, nombre_examen, "", grado_codigo = g$codigo, ext = "docx")
  writeData(wb, "ID", id, startRow = 4, headerStyle = sty_header)
  setColWidths(wb, "ID", cols = 1:ncol(id), widths = c(9, 15, 11, 9, 10, 40, 42, 34, 15, rep(26, length(grados))))

  for (cv in names(convs)) {
    P <- convs[[cv]]; pre <- pref_conv[[cv]]
    hD <- paste0(pre, "_Datos_Matrices"); hS <- paste0(pre, "_Solucion"); hF <- paste0(pre, "_Formulas")
    beta <- P$beta; se_b <- P$se_b; R2 <- P$R2; s2 <- P$s2; XtXi <- P$XtXi; Y <- P$Y; X1 <- P$X1; X2 <- P$X2

    # --- Datos y matrices ---
    addWorksheet(wb, hD)
    writeData(wb, hD, sprintf("DATOS BASE (PREGUNTA 1) · %s · modelos %s", cv, paste(P$modelos, collapse = " ")), startRow=1, startCol=1)
    addStyle(wb, hD, sty_header, rows=1, cols=1:4)
    writeData(wb, hD, data.frame(Obs=1:n, Y=Y, X1=X1, X2=X2), startRow=2)
    addStyle(wb, hD, sty_header, rows=2, cols=1:4)
    writeData(wb, hD, "PARÁMETROS", startRow=12, startCol=1)
    addStyle(wb, hD, sty_header, rows=12, cols=1:2)
    params <- data.frame(
      Parámetro = c("n", "k", "gl (n-k-1)", "media(Y)", "beta_0", "beta_1", "beta_2", "SCT", "SCE", "SCR",
                    "R²", "R² ajustado", "sigma²", "sigma", "var(beta_0)", "var(beta_1)", "var(beta_2)",
                    "se(beta_0)", "se(beta_1)", "se(beta_2)", "Y'Y", "beta'X'Y", "var(y_hat)"),
      Valor = c(n, k, gl, mean(Y), beta[1], beta[2], beta[3], P$SCT, P$SCE, P$SCR, R2, P$R2adj, s2, sqrt(s2),
                s2*XtXi[1,1], s2*XtXi[2,2], s2*XtXi[3,3], se_b[1], se_b[2], se_b[3], P$YtY, P$bXtY, P$var_yhat))
    writeData(wb, hD, params, startRow=13)
    addStyle(wb, hD, sty_result, rows=13:(13+nrow(params)), cols=2, gridExpand=TRUE)
    writeData(wb, hD, "(X'X)^-1", startRow=12, startCol=5)
    addStyle(wb, hD, sty_header, rows=12, cols=5:7)
    writeData(wb, hD, as.data.frame(XtXi), startRow=13, startCol=5, colNames=FALSE)
    setColWidths(wb, hD, cols=1:7, widths=c(25, 15, 15, 15, 15, 15, 15))

    # --- Solución de los apartados (ejercicios 1 y 2) ---
    addWorksheet(wb, hS)
    writeData(wb, hS, sprintf("SOLUCIÓN PREGUNTA 1 · %s · válida para %s", cv, paste(P$modelos, collapse = ", ")), startRow=1, startCol=1)
    addStyle(wb, hS, sty_header, rows=1, cols=1:2)
    IC_inf <- beta[2] - qt(0.975,gl)*se_b[2]; IC_sup <- beta[2] + qt(0.975,gl)*se_b[2]
    sol <- data.frame(
      Apartado = c("a) Amigo correcto", "a.1) Ecuación estimada", "a.2) Interpretación beta_1", "a.2) Interpretación beta_2",
                   "b) R²", "b) Interpretación", "c) sigma² (varianza error)",
                   "d) IC 95% beta_1: límite inferior", "d) IC 95% beta_1: límite superior", "d) ¿Significativa beta_1 al 5%?",
                   "e) H0: beta_2 < 1", "e) Estadístico t", "e) t_critico (una cola, 5%)", "e) Decisión",
                   "f) Coeficientes si Y en euros"),
      Resultado = c(
        "MARÍA",
        sprintf("Y = %.4f + (%.4f)*X1 + (%.4f)*X2", beta[1], beta[2], beta[3]),
        sprintf("Un aumento de 1€ en X1 reduce Y en %.4f cientos de €, c.p.", abs(beta[2])),
        sprintf("Un aumento de 1€ en X2 aumenta Y en %.4f cientos de €, c.p.", beta[3]),
        sprintf("%.4f", R2), sprintf("El modelo explica el %.2f%% de la variabilidad de Y", R2*100), sprintf("%.4f", s2),
        sprintf("%.4f", IC_inf), sprintf("%.4f", IC_sup),
        ifelse(IC_inf > 0 | IC_sup < 0, "SÍ (el IC no contiene el 0)", "NO (el IC contiene el 0)"),
        "H0: beta_2 >= 1 vs H1: beta_2 < 1",
        sprintf("%.4f", (beta[3] - 1) / se_b[3]),
        sprintf("-%.5f (cola izquierda)", qt(0.95,gl)),
        ifelse((beta[3]-1)/se_b[3] < -qt(0.95,gl), "Rechazamos H0: beta_2 < 1", "No rechazamos H0: no evidencia de beta_2 < 1"),
        sprintf("beta_0*100 = %.2f, beta_1*100 = %.2f, beta_2*100 = %.2f", beta[1]*100, beta[2]*100, beta[3]*100)))
    writeData(wb, hS, sol, startRow=2)
    m <- P$p2$modelo; s <- summary(m); cf <- coef(s)
    bp <- lmtest::bptest(m); dw <- lmtest::dwtest(m); bg <- lmtest::bgtest(m, order = 2)
    f2 <- 2 + nrow(sol) + 2
    writeData(wb, hS, sprintf("SOLUCIÓN PREGUNTA 2 · %s", P$p2$formula_r), startRow=f2, startCol=1)
    addStyle(wb, hS, sty_header, rows=f2, cols=1:2)
    sol2 <- data.frame(
      Apartado = c(paste0("a) ", rownames(cf)), "b) n", "c) R²", "c) R² ajustado", "c) Error típico de la regresión",
                   "e) Breusch-Pagan", "e) Durbin-Watson", "e) Breusch-Godfrey (orden 2)"),
      Resultado = c(sprintf("coef = %.4f · se = %.4f · t = %.3f · p = %.4f%s", cf[,1], cf[,2], cf[,3], cf[,4],
                            ifelse(cf[,4] < 0.05, " (significativa al 5%)", " (no significativa al 5%)")),
                    as.character(nrow(datos_p2)), sprintf("%.4f", s$r.squared), sprintf("%.4f", s$adj.r.squared),
                    sprintf("%.4f", s$sigma),
                    sprintf("BP = %.4f · p = %.4f", bp$statistic, bp$p.value),
                    sprintf("DW = %.4f · p = %.4g", dw$statistic, dw$p.value),
                    sprintf("LM = %.4f · p = %.4g", bg$statistic, bg$p.value)))
    writeData(wb, hS, sol2, startRow=f2+1)
    setColWidths(wb, hS, cols=1:2, widths=c(35, 80))

    # --- Fórmulas de Excel paso a paso ---
    addWorksheet(wb, hF)
    writeData(wb, hF, "FÓRMULAS Y CÁLCULOS PASO A PASO", startRow=1, startCol=1)
    addStyle(wb, hF, sty_header, rows=1, cols=1:3)
    writeData(wb, hF, "Y", startRow=2, startCol=1)
    writeData(wb, hF, "X1", startRow=2, startCol=2)
    writeData(wb, hF, "X2", startRow=2, startCol=3)
    addStyle(wb, hF, sty_header, rows=2, cols=1:3)
    for (i in 1:n) {
      writeData(wb, hF, Y[i], startRow=2+i, startCol=1)
      writeData(wb, hF, X1[i], startRow=2+i, startCol=2)
      writeData(wb, hF, X2[i], startRow=2+i, startCol=3)
    }
    writeData(wb, hF, "Estadístico", startRow=13, startCol=1)
    writeData(wb, hF, "Fórmula", startRow=13, startCol=2)
    writeData(wb, hF, "Valor", startRow=13, startCol=3)
    addStyle(wb, hF, sty_header, rows=13, cols=1:3)
    labels_f <- c("n", "media(Y)", "sum(Y)", "sum(X1)", "sum(X2)", "sum(Y²)", "sum(X1*Y)", "sum(X2*Y)", "SCT", "Respuesta correcta")
    formulas_f <- c("=COUNT(A3:A10)", "=AVERAGE(A3:A10)", "=SUM(A3:A10)", "=SUM(B3:B10)", "=SUM(C3:C10)",
                    "=SUMPRODUCT(A3:A10,A3:A10)", "=SUMPRODUCT(B3:B10,A3:A10)", "=SUMPRODUCT(C3:C10,A3:A10)",
                    "=SUMPRODUCT((A3:A10-AVERAGE(A3:A10))^2)", "MARÍA")
    for (i in seq_along(labels_f)) {
      writeData(wb, hF, labels_f[i], startRow=13+i, startCol=1)
      if (grepl("^=", formulas_f[i])) writeFormula(wb, hF, formulas_f[i], startRow=13+i, startCol=2)
      else writeData(wb, hF, formulas_f[i], startRow=13+i, startCol=2)
      addStyle(wb, hF, sty_formula, rows=13+i, cols=2)
    }
    setColWidths(wb, hF, cols=1:3, widths=c(25, 50, 15))
  }
  saveWorkbook(wb, f, overwrite = TRUE)
  invisible(f)
}

# ── Renderizado (Word, HTML y PDF con xelatex) ─────────────────────
pdf_fmt <- rmarkdown::pdf_document(latex_engine = "xelatex")
render_uno <- function(rmd_file, formato, destino) {
  ok <- tryCatch({
    render(rmd_file, output_format = formato, output_file = destino, envir = new.env(), quiet = TRUE)
    TRUE
  }, error = function(e) { message("\n  ", conditionMessage(e)); FALSE })
  unlink(file.path(dirname(rmd_file), paste0(tools::file_path_sans_ext(basename(rmd_file)), c(".tex", ".log", "_files"))), recursive = TRUE)
  ok
}
