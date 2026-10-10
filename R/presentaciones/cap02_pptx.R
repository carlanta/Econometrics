# ══════════════════════════════════════════════════════════════════
# Capítulo 02 · El método MCO · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap02_pptx.R
# Contenido y numeración: 02-metodo-mco.qmd (secciones 2.1 a 2.9). Cálculo matricial con DS_mini (modelo simple
# del apartado 2.3.3 y modelo completo del ejercicio 2.1), Caso A con ventas ~ precio + publicidad (como el manual).
# Las formas funcionales con logaritmos y el R² corregido se adelantan aquí porque se explican en clase en el tema 2.
# ══════════════════════════════════════════════════════════════════

# ── Ayudantes propios de este tema (no están en los comunes) ──────
# Matriz LaTeX (pmatrix) a partir de una matriz numérica (con `d` decimales) o de texto ya formateado
pm <- function(m, d = 4) {
  m <- as.matrix(m)
  f <- if (is.character(m)) m else matrix(numtex(m, d), nrow(m))
  paste0("\\begin{pmatrix}", paste(apply(f, 1, paste, collapse = " & "), collapse = " \\\\ "), "\\end{pmatrix}")
}
# Signo y valor absoluto para escribir una ecuación estimada: " + 3,26" o " - 1,83"
sg <- function(v, d = 4) paste0(if (v < 0) " - " else " + ", numtex(abs(v), d))
# Índice con más de 8 secciones: filas más juntas para no invadir el pie
diapo_indice_n <- function(x, secciones, etiqueta_tema) {
  x <- diapo_contenido(x, tr("Índice", "Contents"), tr("Hoja de ruta del tema", "Chapter roadmap"), etiqueta_tema)
  n <- nrow(secciones)
  paso <- min(0.74, 4.75 / n)
  tam <- if (n <= 6) c(22, 20) else if (n <= 8) c(19, 17) else c(17, 16)
  for (k in seq_len(n)) {
    top <- 2.0 + (k - 1) * paso
    x <- poner_texto(x, parrafo(secciones$num[k], tam[1], ID$oro, bold = TRUE, font = "Georgia"), 1.0, top, 1.3, paso - 0.05)
    x <- poner_texto(x, parrafo(secciones$titulo[k], tam[2], ID$texto), 2.4, top + 0.02, 9.8, paso - 0.05)
    x <- rect(x, 2.4, top + paso - 0.08, 9.6, 0.012, ID$linea)
  }
  x
}
# Cuadro dibujado con cajas de texto (admite ecuaciones, a diferencia de flextable)
tabla_eq <- function(x, cab, filas, left, top, anchos, alto = 0.6, alto_cab = 0.5, size = 15) {
  w <- sum(anchos); xs <- left + c(0, cumsum(anchos))[seq_along(anchos)]
  x <- rect(x, left, top, w, alto_cab, ID$navy)
  for (j in seq_along(cab)) x <- poner_texto(x, parrafo(cab[j], size, ID$blanco, bold = TRUE), xs[j] + 0.08, top + 0.07, anchos[j] - 0.16, alto_cab - 0.1)
  for (i in seq_along(filas)) {
    y <- top + alto_cab + (i - 1) * alto
    if (i %% 2 == 0) x <- rect(x, left, y, w, alto, ID$suave)
    for (j in seq_along(filas[[i]])) x <- poner_texto(x, parrafo(filas[[i]][j], size, if (j == 1) ID$navy else ID$texto, bold = (j == 1)),
                                                      xs[j] + 0.08, y + 0.08, anchos[j] - 0.16, alto - 0.1)
  }
  rect(x, left, top + alto_cab + length(filas) * alto - 0.015, w, 0.03, ID$oro)
}
# Rótulo dorado a la izquierda de una fórmula («Paso 1»…)
rotulo <- function(x, texto, left, top, width = 1.25) poner_texto(x, parrafo(texto, 17, ID$oro, bold = TRUE, font = "Georgia"), left, top, width, 0.5)

for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 2", "Chapter 2")
  SCT <- tr("\\text{SCT}", "\\text{TSS}"); SCE <- tr("\\text{SCE}", "\\text{ESS}"); SCR <- tr("\\text{SCR}", "\\text{RSS}")

  # ── Cálculo matricial con DS_mini (apartado 2.3.3: Ventas = b0 + b1·Precio propio) ──
  Y   <- as.matrix(ds_mini$ventas)
  X   <- cbind(1, ds_mini$precio_propio)
  XtX <- t(X) %*% X; XtY <- t(X) %*% Y; XtXi <- solve(XtX); b <- XtXi %*% XtY
  det_XtX <- det(XtX)
  m_simple <- lm(ventas ~ precio_propio, data = ds_mini)
  stopifnot(all(abs(b - coef(m_simple)) < 1e-10))
  # Modelo completo del ejercicio 2.1 (cuatro columnas)
  X4 <- cbind(1, ds_mini$precio_propio, ds_mini$precio_sustit, ds_mini$gasto_publicidad)
  XtX4 <- t(X4) %*% X4; XtY4 <- t(X4) %*% Y; XtXi4 <- solve(XtX4); b4 <- XtXi4 %*% XtY4
  stopifnot(all(abs(b4 - coef(mod_mini)) < 1e-10))
  fmt_xtx <- function(M, d) { f <- matrix(numtex(M, d), nrow(M)); f[1, 1] <- as.character(nrow(ds_mini)); f }

  # ── Caso A: ventas ~ precio + publicidad (modelo del apartado 2.4) ──
  ma <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a); sa <- summary(ma)
  ca <- coef(ma)
  sct_a <- sum((caso_a$ventas - mean(caso_a$ventas))^2); sce_a <- sum((fitted(ma) - mean(caso_a$ventas))^2); scr_a <- sum(resid(ma)^2)
  ma_cent <- lm(ventas ~ I(100 * precio_propio) + gasto_publicidad, data = caso_a)
  ma_log  <- lm(log(ventas) ~ log(precio_propio) + log(gasto_publicidad), data = caso_a); cl <- coef(ma_log)
  # Casos B y C (apartado 2.9)
  mb <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b); sb <- summary(mb)
  mc <- lm(consumo ~ pib, data = caso_c); mc2 <- lm(consumo ~ pib + tipo_interes, data = caso_c)

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 2, tr("El método de mínimos cuadrados ordinarios", "The ordinary least squares method"),
    tr("¿Cómo se pasa de diez filas de datos a una recta de regresión?",
       "How do we get from ten rows of data to a regression line?"))

  # 2 · Índice
  x <- diapo_indice_n(x, data.frame(
    num = sprintf("2.%d", 1:9),
    titulo = tr(c("El modelo de regresión lineal general", "Supuestos básicos del MRLG", "Estimación por mínimos cuadrados ordinarios",
                  "Estimación con R: la función lm()", "Interpretación de los coeficientes", "Efectos del cambio de unidades",
                  paste0("Bondad de ajuste: el coeficiente de determinación ", M("R^2")), "Varianza del término de error",
                  "Aplicación a los casos prácticos"),
                c("The general linear regression model", "Basic assumptions of the GLRM", "Ordinary least squares estimation",
                  "Estimation with R: the lm() function", "Interpreting the coefficients", "Effects of changing units",
                  paste0("Goodness of fit: the coefficient of determination ", M("R^2")), "Variance of the error term",
                  "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Estimar a mano", "Calcular la fórmula matricial de MCO con DS_mini, en el papel y con Excel."),
       c("Estimate by hand", "Work out the OLS matrix formula with DS_mini, on paper and in Excel.")),
    tr(c("Interpretar bien", "Efecto marginal con unidades y ceteris paribus; cambio de unidades y logaritmos."),
       c("Interpret correctly", "Marginal effect with units and ceteris paribus; changes of units and logarithms.")),
    tr(c("Medir el ajuste", paste0("Descomponer la variación de ", M("Y"), " y leer el ", M("R^2"), ", el ", M("\\overline{R}{}^2"), " y el error típico.")),
       c("Measure the fit", paste0("Decompose the variation in ", M("Y"), " and read the ", M("R^2"), ", the ", M("\\overline{R}{}^2"), " and the standard error.")))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("El modelo y sus supuestos", "The model and its assumptions"),
    tr(c("El MRLG en forma matricial", "Seis supuestos básicos", "La base de Gauss-Markov"),
       c("The GLRM in matrix form", "Six basic assumptions", "The basis of Gauss-Markov")))

  # 5 · 2.1 El MRLG
  x <- diapo_contenido(x, tr("2.1 · El modelo de regresión lineal general", "2.1 · The general linear regression model"),
    tr("Una ecuación para n observaciones", "One equation for n observations"), tema)
  x <- formula_caja(x, "Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\cdots + \\beta_k X_{ki} + u_i, \\qquad i = 1, \\ldots, n",
                    0.6, 1.95, 12.1, 0.95, size = 24)
  x <- formula_caja(x, paste0("\\mathbf{Y} = \\mathbf{X}\\boldsymbol{\\beta} + \\mathbf{u}, \\qquad ",
                              "\\mathbf{Y} = \\begin{pmatrix} Y_1 \\\\ Y_2 \\\\ \\vdots \\\\ Y_n \\end{pmatrix}, \\quad ",
                              "\\mathbf{X} = \\begin{pmatrix} 1 & X_{11} & \\cdots & X_{k1} \\\\ 1 & X_{12} & \\cdots & X_{k2} \\\\ \\vdots & \\vdots & \\ddots & \\vdots \\\\ 1 & X_{1n} & \\cdots & X_{kn} \\end{pmatrix}, \\quad ",
                              "\\boldsymbol{\\beta} = \\begin{pmatrix} \\beta_0 \\\\ \\beta_1 \\\\ \\vdots \\\\ \\beta_k \\end{pmatrix}"),
                    0.6, 3.1, 12.1, 2.0, size = 19)
  x <- vinetas(x, tr(
    c(paste0("Dimensiones: ", M("\\mathbf{Y}"), " es ", M("n \\times 1"), ", ", M("\\mathbf{X}"), " es ", M("n \\times (k+1)"), " y ", M("\\boldsymbol{\\beta}"), " es ", M("(k+1) \\times 1"), ". La columna de unos de ", M("\\mathbf{X}"), " es la del intercepto."),
      paste0("Lineal en los parámetros, no en las variables: ", M("Y = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + u"), " también es un MRLG.")),
    c(paste0("Dimensions: ", M("\\mathbf{Y}"), " is ", M("n \\times 1"), ", ", M("\\mathbf{X}"), " is ", M("n \\times (k+1)"), " and ", M("\\boldsymbol{\\beta}"), " is ", M("(k+1) \\times 1"), ". The column of ones in ", M("\\mathbf{X}"), " belongs to the intercept."),
      paste0("Linear in the parameters, not in the variables: ", M("Y = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + u"), " is also a GLRM."))),
    0.6, 5.35, 12.1, 1.5, size = 16)

  # 6 · 2.2 Supuestos
  x <- diapo_contenido(x, tr("2.2 · Supuestos básicos del MRLG", "2.2 · Basic assumptions of the GLRM"),
    tr("Seis supuestos para que MCO funcione bien", "Six assumptions for OLS to work well"), tema)
  sup <- tr(list(
      c("1 · Linealidad", paste0("Lineal en los parámetros ", M("\\beta_j"), "; las variables pueden ir en logaritmos o al cuadrado.")),
      c("2 · Muestreo aleatorio", "Observaciones independientes entre sí. Falla en series temporales (tema 10)."),
      c("3 · No colinealidad perfecta", paste0(M("\\text{rango}(\\mathbf{X}) = k+1"), ". Si falla, ", M("\\mathbf{X}'\\mathbf{X}"), " no tiene inversa y MCO no existe.")),
      c("4 · Media condicional cero", paste0(M("E(u_i \\mid \\mathbf{X}) = 0"), ": regresores exógenos. Garantiza la insesgadez (tema 6).")),
      c("5 · Homocedasticidad", paste0(M("\\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2"), " para toda observación. Si falla, heterocedasticidad (tema 9).")),
      c("6 · Normalidad del error", paste0(M("u_i \\mid \\mathbf{X} \\sim N(0, \\sigma^2)"), ". No la exige Gauss-Markov; permite los contrastes t y F."))),
    list(
      c("1 · Linearity", paste0("Linear in the parameters ", M("\\beta_j"), "; variables may enter in logarithms or squared.")),
      c("2 · Random sampling", "Observations independent of each other. Fails in time series (chapter 10)."),
      c("3 · No perfect collinearity", paste0(M("\\text{rank}(\\mathbf{X}) = k+1"), ". If it fails, ", M("\\mathbf{X}'\\mathbf{X}"), " has no inverse and OLS does not exist.")),
      c("4 · Zero conditional mean", paste0(M("E(u_i \\mid \\mathbf{X}) = 0"), ": exogenous regressors. Ensures unbiasedness (chapter 6).")),
      c("5 · Homoskedasticity", paste0(M("\\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2"), " for every observation. If it fails, heteroskedasticity (chapter 9).")),
      c("6 · Normal errors", paste0(M("u_i \\mid \\mathbf{X} \\sim N(0, \\sigma^2)"), ". Not needed for Gauss-Markov; enables t and F tests."))))
  for (k in 1:6) {
    fila <- (k - 1) %/% 3; col <- (k - 1) %% 3
    x <- caja_texto(x, 0.6 + col * 4.15, 1.95 + fila * 2.45, 3.85, 2.3, sup[[k]][1], sup[[k]][2], oscura = (k == 6), size = 14)
  }

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("MCO a mano con matrices", "OLS by hand with matrices"),
    tr(c("Minimizar la suma de residuos al cuadrado", "Los cuatro pasos con DS_mini", "Excel: MMULT, MINVERSA y TRANSPONER"),
       c("Minimise the sum of squared residuals", "The four steps with DS_mini", "Excel: MMULT, MINVERSE and TRANSPOSE")))

  # 8 · 2.3 Idea y fórmula de MCO
  x <- diapo_contenido(x, tr("2.3 · Estimación por mínimos cuadrados ordinarios", "2.3 · Ordinary least squares estimation"),
    tr("Minimizar la suma de residuos al cuadrado", "Minimise the sum of squared residuals"), tema)
  x <- formula_caja(x, "\\min_{\\hat{\\boldsymbol\\beta}} \\sum_{i=1}^{n} \\hat{u}_i^2 = (\\mathbf{Y} - \\mathbf{X}\\hat{\\boldsymbol\\beta})'(\\mathbf{Y} - \\mathbf{X}\\hat{\\boldsymbol\\beta})",
                    0.6, 1.95, 6.6, 1.1, size = 18)
  x <- poner_texto(x, parrafo(tr("Derivando e igualando a cero (ecuaciones normales):", "Differentiating and setting to zero (normal equations):"), 15, ID$gris, italic = TRUE), 0.6, 3.06, 6.6, 0.4)
  x <- formula_caja(x, "\\mathbf{X}'\\mathbf{X}\\,\\hat{\\boldsymbol\\beta} = \\mathbf{X}'\\mathbf{Y}", 0.6, 3.45, 6.6, 0.85, size = 22)
  x <- poner_texto(x, parrafo(paste0(tr("Si ", "If "), M("\\mathbf{X}'\\mathbf{X}"), tr(" tiene inversa (supuesto 3):", " is invertible (assumption 3):")), 15, ID$gris, italic = TRUE), 0.6, 4.37, 6.6, 0.4)
  x <- formula_caja(x, "\\hat{\\boldsymbol\\beta} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'\\mathbf{Y}", 0.6, 4.8, 6.6, 1.1, size = 30)
  x <- poner_texto(x, parrafo(tr("La fórmula del examen: todo el tema descansa en ella.", "The exam formula: the whole chapter rests on it."), 16, ID$navy, bold = TRUE), 0.6, 6.1, 6.6, 0.6)
  ds_tab <- data.frame(obs = ds_mini$obs, v = ds_mini$ventas, p = ds_mini$precio_propio, s = ds_mini$precio_sustit, g = ds_mini$gasto_publicidad)
  x <- poner_tabla(x, tabla_pptx(ds_tab, tr(c("Obs.", "Ventas", "Precio", "P. sust.", "Public."), c("Obs.", "Sales", "Price", "Subst. p.", "Advert.")), digits = 2, size = 12),
                   7.75, 1.95, max_ancho = 4.98)
  x <- poner_texto(x, parrafo(tr("DS_mini: ventas y publicidad en MM €; precios en €.", "DS_mini: sales and advertising in € million; prices in €."), 12, ID$gris, italic = TRUE),
                   7.75, 5.75, 4.98, 0.5)

  # 9 · 2.3.3 Pasos 1 a 3
  x <- diapo_contenido(x, tr("2.3 · Cálculo matricial paso a paso", "2.3 · Step-by-step matrix calculation"),
    tr("DS_mini a mano: pasos 1 a 3", "DS_mini by hand: steps 1 to 3"), tema)
  x <- poner_texto(x, parrafo(paste0(tr("Modelo simple ", "Simple model "), M(tr("\\text{Ventas} = \\beta_0 + \\beta_1 \\cdot \\text{Precio} + u", "\\text{Sales} = \\beta_0 + \\beta_1 \\cdot \\text{Price} + u")),
                                     tr(" con ", " with "), M("n = 10"), ": ", M("\\mathbf{X}"), tr(" lleva una columna de unos y otra con el precio.", " has a column of ones and a column with the price.")), 16, ID$texto),
                   0.6, 1.85, 12.1, 0.5)
  xtx_txt <- fmt_xtx(XtX, 4); xtx_txt[1, 2] <- xtx_txt[2, 1] <- numtex(XtX[1, 2], 2)
  adj <- matrix(c(XtX[2, 2], -XtX[1, 2], -XtX[2, 1], XtX[1, 1]), 2)
  adj_txt <- matrix(c(numtex(XtX[2, 2], 4), numtex(-XtX[1, 2], 2), numtex(-XtX[2, 1], 2), "10"), 2)
  x <- rotulo(x, tr("Paso 1", "Step 1"), 0.6, 2.75)
  x <- formula_caja(x, paste0("\\mathbf{X}'\\mathbf{X} = \\begin{pmatrix} n & \\sum X_i \\\\ \\sum X_i & \\sum X_i^2 \\end{pmatrix} = ", pm(xtx_txt)),
                    1.9, 2.45, 10.83, 1.3, size = 20)
  x <- rotulo(x, tr("Paso 2", "Step 2"), 0.6, 4.15)
  x <- formula_caja(x, paste0("\\mathbf{X}'\\mathbf{Y} = \\begin{pmatrix} \\sum Y_i \\\\ \\sum X_i Y_i \\end{pmatrix} = ", pm(XtY, 3)),
                    1.9, 3.85, 10.83, 1.3, size = 20)
  x <- rotulo(x, tr("Paso 3", "Step 3"), 0.6, 5.6)
  x <- formula_caja(x, paste0("(\\mathbf{X}'\\mathbf{X})^{-1} = \\frac{1}{|\\mathbf{X}'\\mathbf{X}|}\\begin{pmatrix} \\sum X_i^2 & -\\sum X_i \\\\ -\\sum X_i & n \\end{pmatrix} = \\frac{1}{",
                              numtex(det_XtX, 4), "}", pm(adj_txt), " = ", pm(XtXi, 4)),
                    1.9, 5.25, 10.83, 1.5, size = 18)

  # 10 · 2.3.3 Paso 4
  x <- diapo_contenido(x, tr("2.3 · Cálculo matricial paso a paso", "2.3 · Step-by-step matrix calculation"),
    tr("Paso 4: los coeficientes estimados", "Step 4: the estimated coefficients"), tema)
  x <- formula_caja(x, paste0("\\hat{\\boldsymbol\\beta} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'\\mathbf{Y} = ", pm(XtXi, 4), pm(XtY, 3),
                              " = \\begin{pmatrix} \\hat{\\beta}_0 \\\\ \\hat{\\beta}_1 \\end{pmatrix} = ", pm(b, 4)),
                    0.6, 1.95, 12.1, 1.45, size = 22)
  x <- formula_caja(x, paste0(tr("\\widehat{\\text{Ventas}} = ", "\\widehat{\\text{Sales}} = "), numtex(b[1], 4), sg(b[2], 4), tr(" \\cdot \\text{Precio}", " \\cdot \\text{Price}")),
                    0.6, 3.6, 12.1, 0.95, size = 26)
  p4 <- tr(list(c("Igual que lm()", "R, Gretl y Excel hacen exactamente esta operación: el resultado coincide con lm() hasta el último decimal."),
                c("Residuos que suman cero", paste0(M("\\sum_{i} \\hat{u}_i = 0"), ": lo garantizan las ecuaciones normales cuando el modelo tiene constante.")),
                c("En el examen", paste0(M("(2 \\times 2)(2 \\times 1) = 2 \\times 1"), ": comprueba siempre las dimensiones y trabaja con cuatro decimales."))),
           list(c("Same as lm()", "R, Gretl and Excel perform exactly this operation: the result matches lm() to the last decimal."),
                c("Residuals sum to zero", paste0(M("\\sum_{i} \\hat{u}_i = 0"), ": guaranteed by the normal equations when the model has a constant.")),
                c("In the exam", paste0(M("(2 \\times 2)(2 \\times 1) = 2 \\times 1"), ": always check the dimensions and work with four decimals."))))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 4.8, 3.85, 1.95, p4[[k]][1], p4[[k]][2], oscura = (k == 3), size = 14)

  # 11 · 2.3 Modelo completo (ejercicio 2.1)
  x <- diapo_contenido(x, tr("2.3 · Ejercicio 2.1: el modelo completo", "2.3 · Exercise 2.1: the full model"),
    tr("DS_mini con precio, sustitutivo y publicidad", "DS_mini with price, substitute and advertising"), tema)
  x <- poner_texto(x, parrafo(paste0(M(tr("\\text{Ventas} = \\beta_0 + \\beta_1 \\text{Precio} + \\beta_2 \\text{P. sust.} + \\beta_3 \\text{Public.} + u",
                                           "\\text{Sales} = \\beta_0 + \\beta_1 \\text{Price} + \\beta_2 \\text{Subst.} + \\beta_3 \\text{Advert.} + u")),
                                     tr(": ahora ", ": now "), M("\\mathbf{X}"), tr(" es ", " is "), M("10 \\times 4"), tr(" y las matrices, ", " and the matrices "), M("4 \\times 4"), "."), 16, ID$texto),
                   0.6, 1.85, 12.1, 0.5)
  x <- formula_caja(x, paste0("\\mathbf{X}'\\mathbf{X} = ", pm(fmt_xtx(XtX4, 4))), 0.6, 2.45, 7.35, 1.75, size = 16)
  x <- formula_caja(x, paste0("\\mathbf{X}'\\mathbf{Y} = ", pm(XtY4, 3)), 8.15, 2.45, 4.58, 1.75, size = 16)
  x <- formula_caja(x, paste0("(\\mathbf{X}'\\mathbf{X})^{-1} = ", pm(XtXi4, 4)), 0.6, 4.4, 7.35, 1.75, size = 16)
  x <- formula_caja(x, paste0("\\hat{\\boldsymbol\\beta} = ", pm(b4, 4)), 8.15, 4.4, 4.58, 1.75, size = 16)
  x <- poner_texto(x, parrafo(tr(paste0("Los mismos cuatro pasos; con matrices ", M("4 \\times 4"), " la inversa se deja a Excel (MINVERSA). Coincide con lm()."),
                                 paste0("The same four steps; with ", M("4 \\times 4"), " matrices the inverse is left to Excel (MINVERSE). It matches lm().")),
                              15, ID$navy, bold = TRUE), 0.6, 6.3, 12.1, 0.5)

  # 12 · Excel → R
  x <- diapo_contenido(x, tr("2.3 · Excel → R", "2.3 · Excel → R"), tr("MCO matricial en Excel y en R", "Matrix OLS in Excel and in R"), tema)
  if (idioma == "es") {
    xl <- data.frame(
      op = c("Transponer X", "Producto X'X", "Inversa de X'X", "Producto X'Y", "Coeficientes", "Valores ajustados"),
      es = c("=TRANSPONER(X)", "=MMULT(TRANSPONER(X);X)", "=MINVERSA(XX)", "=MMULT(TRANSPONER(X);Y)", "=MMULT(MINVERSA(XX);XY)", "=MMULT(X;B)"),
      en = c("=TRANSPOSE(X)", "=MMULT(TRANSPOSE(X),X)", "=MINVERSE(XX)", "=MMULT(TRANSPOSE(X),Y)", "=MMULT(MINVERSE(XX),XY)", "=MMULT(X,B)"),
      r  = c("t(X)", "XX <- t(X) %*% X", "solve(XX)", "XY <- t(X) %*% Y", "B <- solve(XX) %*% XY", "X %*% B"))
    ft <- tabla_pptx(xl, c("Paso", "Excel en español", "Excel en inglés", "R"), size = 13, mono = 2:4, texto = 1:4)
  } else {
    xl <- data.frame(
      op = c("Transpose X", "Product X'X", "Inverse of X'X", "Product X'Y", "Coefficients", "Fitted values"),
      en = c("=TRANSPOSE(X)", "=MMULT(TRANSPOSE(X),X)", "=MINVERSE(XX)", "=MMULT(TRANSPOSE(X),Y)", "=MMULT(MINVERSE(XX),XY)", "=MMULT(X,B)"),
      r  = c("t(X)", "XX <- t(X) %*% X", "solve(XX)", "XY <- t(X) %*% Y", "B <- solve(XX) %*% XY", "X %*% B"))
    ft <- tabla_pptx(xl, c("Step", "Excel", "R"), size = 15, mono = 2:3, texto = 1:3)
  }
  x <- poner_tabla(x, ft, 0.6, 1.95, max_ancho = 12.1)
  x <- vinetas(x, tr(
    c(paste0("X, Y, XX, XY y B son rangos con nombre. Selecciona antes el rango de salida (", M("2 \\times 2"), ", ", M("2 \\times 1"), "…) y confirma con Ctrl+Mayús+Intro; en Excel 365 el resultado se desborda solo."),
      "En Excel en español el separador de argumentos es «;»; en inglés, «,». Comprueba el resultado con lm() en R o con Gretl."),
    c(paste0("X, Y, XX, XY and B are named ranges. Select the output range first (", M("2 \\times 2"), ", ", M("2 \\times 1"), "…) and confirm with Ctrl+Shift+Enter; in Excel 365 the result spills automatically."),
      "Check the result with lm() in R or with Gretl: the coefficients must match exactly.")),
    0.6, 5.3, 12.1, 1.55, size = 15)

  # 13 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Interpretar y medir el ajuste", "Interpreting and measuring the fit"),
    tr(c("Efecto marginal ceteris paribus", "Unidades y logaritmos", "SCT, SCE y SCR", paste0(M("R^2"), " y ", M("\\overline{R}{}^2"))),
       c("Ceteris paribus marginal effect", "Units and logarithms", "TSS, ESS and RSS", paste0(M("R^2"), " and ", M("\\overline{R}{}^2")))))

  # 14 · 2.4–2.5 Interpretación ceteris paribus (Caso A)
  x <- diapo_contenido(x, tr("2.5 · Efecto marginal y ceteris paribus", "2.5 · Marginal effect and ceteris paribus"),
    tr("Cada coeficiente, con todo lo demás constante", "Each coefficient, all else held constant"), tema)
  x <- formula_caja(x, paste0(tr("\\widehat{\\text{Ventas}} = ", "\\widehat{\\text{Sales}} = "), numtex(ca[1], 4), sg(ca[2], 4),
                              tr(" \\cdot \\text{Precio}", " \\cdot \\text{Price}"), sg(ca[3], 4), tr(" \\cdot \\text{Publicidad}", " \\cdot \\text{Advertising}")),
                    0.6, 1.95, 12.1, 0.95, size = 24)
  x <- caja_dato(x, 0.6, 3.1, 3.85, 2.6, tr("Intercepto", "Intercept"), M(paste0("\\hat{\\beta}_0 = ", numtex(ca[1], 4))),
                 tr("Ventas con precio y publicidad nulos: sin sentido económico, pero necesario para el ajuste.",
                    "Sales with zero price and advertising: no economic meaning, but needed for the fit."), ID$gris_med, size_valor = 26)
  x <- caja_dato(x, 4.75, 3.1, 3.85, 2.6, tr("Precio propio", "Own price"), M(paste0("\\hat{\\beta}_1 = ", numtex(ca[2], 4))),
                 tr(paste0("Un euro más de precio resta ", num(abs(ca[2]), 2), " MM € de ventas, con la publicidad constante."),
                    paste0("One more euro of price cuts sales by €", num(abs(ca[2]), 2), " million, advertising held constant.")), ID$navy, size_valor = 26)
  x <- caja_dato(x, 8.9, 3.1, 3.85, 2.6, tr("Publicidad", "Advertising"), M(paste0("\\hat{\\beta}_2 = ", numtex(ca[3], 4))),
                 tr(paste0("Un millón más de publicidad suma ", num(ca[3], 2), " MM € de ventas, ceteris paribus."),
                    paste0("One more million of advertising adds €", num(ca[3], 2), " million of sales, ceteris paribus.")), ID$oro, size_valor = 26)
  x <- poner_texto(x, parrafo(tr("Caso A, 35 observaciones. Interpretar = número + unidades + ceteris paribus; y el signo, contrastado con la teoría.",
                                 "Case A, 35 observations. Interpreting = number + units + ceteris paribus; and the sign, checked against theory."),
                              16, ID$navy, bold = TRUE), 0.6, 5.95, 12.1, 0.8)

  # 15 · 2.6 Cambio de unidades
  x <- diapo_contenido(x, tr("2.6 · Efectos del cambio de unidades", "2.6 · Effects of changing units"),
    tr("Cambiar unidades no cambia la significatividad", "Changing units does not change significance"), tema)
  x <- formula_caja(x, "X_j \\times c \\;\\Rightarrow\\; \\hat{\\beta}_j^{*} = \\hat{\\beta}_j / c", 0.6, 1.95, 5.7, 0.95, size = 24)
  x <- formula_caja(x, "Y \\times d \\;\\Rightarrow\\; \\hat{\\beta}_j^{*} = d \\cdot \\hat{\\beta}_j \\;\\; \\forall j", 0.6, 3.1, 5.7, 0.95, size = 24)
  x <- vinetas(x, tr(
    c("Cambian el coeficiente y su error estándar.", paste0("No cambian el estadístico ", M("t"), ", el p-valor ni el ", M("R^2"), "."), "La lectura económica es la misma."),
    c("The coefficient and its standard error change.", paste0("The ", M("t"), " statistic, the p-value and the ", M("R^2"), " do not."), "The economic reading is the same.")),
    0.6, 4.3, 5.7, 2.5, size = 17)
  sca <- summary(ma)$coefficients; scc <- summary(ma_cent)$coefficients
  cu <- data.frame(m = tr(c("Precio en €", "Precio en céntimos"), c("Price in €", "Price in cents")),
                   b = c(num(sca[2, 1], 4), num(scc[2, 1], 6)), se = c(num(sca[2, 2], 4), num(scc[2, 2], 6)),
                   t = num(c(sca[2, 3], scc[2, 3]), 3), r2 = num(c(summary(ma)$r.squared, summary(ma_cent)$r.squared), 4))
  x <- poner_tabla(x, tabla_pptx(cu, tr(c("Caso A", "Coef. precio", "Error est.", "t", "R2"), c("Case A", "Price coef.", "Std. error", "t", "R2")), size = 14),
                   6.6, 2.0, max_ancho = 6.13)
  x <- caja_texto(x, 6.6, 3.2, 6.13, 2.4, tr("Del euro al céntimo", "From euros to cents"),
                  tr(paste0("Al pasar el precio de euros a céntimos el coeficiente se divide por 100 (de ", num(sca[2, 1], 4), " a ", num(scc[2, 1], 6),
                            "), pero el estadístico t sigue en ", num(sca[2, 3], 3), ". La significatividad no depende de las unidades."),
                     paste0("Moving the price from euros to cents divides the coefficient by 100 (from ", num(sca[2, 1], 4), " to ", num(scc[2, 1], 6),
                            "), but the t statistic stays at ", num(sca[2, 3], 3), ". Significance does not depend on units.")),
                  oscura = TRUE, size = 15)

  # 16 · Formas funcionales con logaritmos
  x <- diapo_contenido(x, tr("2.5 · Formas funcionales con logaritmos", "2.5 · Functional forms with logarithms"),
    tr("Logaritmos: de unidades a porcentajes", "Logarithms: from units to percentages"), tema)
  x <- tabla_eq(x, tr(c("Modelo", "Especificación", paste0("Si ", M("X"), " sube…, ", M("Y"), " cambia…")), c("Model", "Specification", paste0("If ", M("X"), " rises…, ", M("Y"), " changes…"))),
    list(
      c(tr("Lineal", "Linear"), M("Y = \\beta_0 + \\beta_1 X + u"), paste0(tr("1 unidad → ", "1 unit → "), M("\\beta_1"), tr(" unidades", " units"))),
      c("Log-lin", M("\\ln(Y) = \\beta_0 + \\beta_1 X + u"), paste0(tr("1 unidad → ", "1 unit → "), M("100 \\cdot \\beta_1 \\, \\%"), tr(" (semielasticidad)", " (semi-elasticity)"))),
      c("Lin-log", M("Y = \\beta_0 + \\beta_1 \\cdot \\ln(X) + u"), paste0("1 % → ", M("\\beta_1 / 100"), tr(" unidades", " units"))),
      c("Log-log", M("\\ln(Y) = \\beta_0 + \\beta_1 \\cdot \\ln(X) + u"), paste0("1 % → ", M("\\beta_1 \\, \\%"), tr(" (elasticidad)", " (elasticity)")))),
    0.6, 1.95, c(2.0, 4.6, 5.5), alto = 0.62, size = 16)
  x <- formula_caja(x, paste0("\\hat{y} = ", numtex(cl[1], 4), sg(cl[2], 4), " \\cdot \\ln\\,P", sg(cl[3], 4), " \\cdot \\ln\\,G"),
                    0.6, 5.15, 7.5, 1.0, size = 22)
  x <- poner_texto(x, parrafo(paste0(M("y = \\ln V"), tr(", con ", ", with "), M("V"), tr(": ventas · ", ": sales · "), M("P"), tr(": precio propio · ", ": own price · "), M("G"), tr(": publicidad", ": advertising")),
                              13, ID$gris, italic = TRUE), 0.6, 6.25, 7.5, 0.45)
  x <- poner_texto(x, parrafo(tr(paste0("Caso A en log-log: si el precio sube un 1 %, las ventas caen un ", num(abs(cl[2]), 2), " %; la elasticidad de la publicidad es ", num(cl[3], 2), "."),
                                 paste0("Case A in log-log: a 1 % price rise cuts sales by ", num(abs(cl[2]), 2), " %; the advertising elasticity is ", num(cl[3], 2), ".")),
                              15, ID$navy, bold = TRUE), 8.35, 5.12, 4.4, 1.6)

  # 17 · 2.7 Descomposición de la variabilidad
  x <- diapo_contenido(x, tr("2.7 · Bondad de ajuste", "2.7 · Goodness of fit"),
    tr(paste0("Cuánta variación de ", M("Y"), " explica el modelo"), paste0("How much variation in ", M("Y"), " the model explains")), tema)
  x <- formula_caja(x, paste0(SCT, " = ", SCE, " + ", SCR, ": \\qquad \\sum_{i=1}^{n} (Y_i - \\overline{Y})^2 = \\sum_{i=1}^{n} (\\hat{Y}_i - \\overline{Y})^2 + \\sum_{i=1}^{n} \\hat{u}_i^2"),
                    0.6, 1.95, 12.1, 1.4, size = 22)
  x <- vinetas(x, tr(
    c(paste0(M("\\text{SCT}"), ": variación total de las ventas = ", num(sct_a, 2), "."),
      paste0(M("\\text{SCE}"), ": la que explica el modelo = ", num(sce_a, 2), " (", pct(sce_a / sct_a), ")."),
      paste0(M("\\text{SCR}"), ": la que queda en los residuos = ", num(scr_a, 2), " (", pct(scr_a / sct_a), ")."),
      "Caso A: ventas según precio y publicidad."),
    c(paste0(M("\\text{TSS}"), ": total variation in sales = ", num(sct_a, 2), "."),
      paste0(M("\\text{ESS}"), ": the part the model explains = ", num(sce_a, 2), " (", pct(sce_a / sct_a), ")."),
      paste0(M("\\text{RSS}"), ": the part left in the residuals = ", num(scr_a, 2), " (", pct(scr_a / sct_a), ")."),
      "Case A: sales by price and advertising.")),
    0.6, 3.65, 5.9, 3.1, size = 17)
  dfa <- data.frame(aj = fitted(ma), ob = caso_a$ventas)
  g_aj <- ggplot(dfa, aes(aj, ob)) +
    geom_abline(slope = 1, intercept = 0, colour = COL$oro, linewidth = 1.1, linetype = "dashed") +
    geom_segment(aes(xend = aj, yend = aj), colour = COL$gris, alpha = 0.7) +
    geom_point(colour = COL$navy, size = 2.3) +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Ventas ajustadas (MM €)", "Fitted sales (€ million)"), y = tr("Ventas observadas (MM €)", "Observed sales (€ million)")) +
    tema_cdea(12)
  x <- imagen_ggplot(x, g_aj, 6.8, 3.55, 5.9, 3.3)

  # 18 · 2.7–2.8 R², R² corregido y varianza del error
  x <- diapo_contenido(x, tr("2.7 · Bondad de ajuste · 2.8 · Varianza del error", "2.7 · Goodness of fit · 2.8 · Error variance"),
    tr("Tres medidas del ajuste del modelo", "Three measures of the model's fit"), tema)
  x <- formula_caja(x, paste0("R^2 = \\frac{", SCE, "}{", SCT, "} = 1 - \\frac{", SCR, "}{", SCT, "}"), 0.6, 1.95, 3.85, 1.25, size = 20)
  x <- formula_caja(x, paste0("\\overline{R}{}^2 = 1 - \\frac{", SCR, "/(n-k-1)}{", SCT, "/(n-1)}"), 4.75, 1.95, 3.85, 1.25, size = 20)
  x <- formula_caja(x, paste0("\\hat{\\sigma}^2 = \\frac{", SCR, "}{n-k-1}"), 8.9, 1.95, 3.85, 1.25, size = 20)
  x <- caja_dato(x, 0.6, 3.4, 3.85, 2.3, tr("Coeficiente de determinación", "Coefficient of determination"), M(paste0("R^2 = ", numtex(sa$r.squared, 4))),
                 tr(paste0("Precio y publicidad explican el ", pct(sa$r.squared), " de la variación de las ventas."),
                    paste0("Price and advertising explain ", pct(sa$r.squared), " of the variation in sales.")), ID$navy, size_valor = 24)
  x <- caja_dato(x, 4.75, 3.4, 3.85, 2.3, tr(paste0(M("R^2"), " corregido"), paste0("Adjusted ", M("R^2"))), M(paste0("\\overline{R}{}^2 = ", numtex(sa$adj.r.squared, 4))),
                 tr("Penaliza cada regresor: sirve para comparar modelos con distinto k (tema 3).",
                    "Penalises each regressor: use it to compare models with different k (chapter 3)."), ID$oro, size_valor = 24)
  x <- caja_dato(x, 8.9, 3.4, 3.85, 2.3, tr("Error típico de la regresión", "Regression standard error"), M(paste0("\\hat{\\sigma} = ", numtex(sa$sigma, 4))),
                 tr(paste0("En MM €, con ", sa$df[2], " grados de libertad: el error típico de las predicciones."),
                    paste0("In € million, with ", sa$df[2], " degrees of freedom: the typical prediction error.")), ID$gris_med, size_valor = 24)
  x <- poner_texto(x, parrafo(paste0(tr("Cuidado: un ", "Beware: a high "), M("R^2"), tr(" alto no implica causalidad, uno bajo no hace inútil el modelo y nunca baja al añadir variables.",
                                                                               " does not imply causality, a low one does not make the model useless, and it never falls when variables are added.")),
                              16, ID$navy, bold = TRUE), 0.6, 5.95, 12.1, 0.8)

  # 19 · 2.9 Casos prácticos
  x <- diapo_contenido(x, tr("2.9 · Aplicación a los casos prácticos", "2.9 · Application to the case studies"),
    tr("Los tres casos estimados por MCO", "The three cases estimated by OLS"), tema)
  pb <- summary(mb)$coefficients
  casos <- tr(list(
      c("A · Demanda", paste0("Precio ", M(paste0("\\hat{\\beta}_1 = ", numtex(ca[2], 4))), " y publicidad ", M(paste0("\\hat{\\beta}_2 = ", numtex(ca[3], 4))),
                              ": signos de la teoría. ", M(paste0("R^2 = ", numtex(sa$r.squared, 3))), ".")),
      c("B · Cifra de negocio", paste0(M(paste0("R^2 = ", numtex(sb$r.squared, 3))), ". Cada trabajador suma ", num(coef(mb)[2], 4), " M€ (", M(paste0("p = ", numtex(pb[2, 4], 3))),
                                       "); la inversión no es significativa (", M(paste0("p = ", numtex(pb[3, 4], 3))), "). Más dispersión en las grandes: tema 9.")),
      c("C · Consumo", paste0("Propensión marginal ", M(paste0("\\hat{\\beta}_1 = ", numtex(coef(mc)[2], 4))), ", entre 0 y 1 como predice Keynes. ",
                              M(paste0("R^2 = ", numtex(summary(mc)$r.squared, 3))), ": cuidado con la tendencia común (tema 10)."))),
    list(
      c("A · Demand", paste0("Price ", M(paste0("\\hat{\\beta}_1 = ", numtex(ca[2], 4))), " and advertising ", M(paste0("\\hat{\\beta}_2 = ", numtex(ca[3], 4))),
                             ": the signs theory expects. ", M(paste0("R^2 = ", numtex(sa$r.squared, 3))), ".")),
      c("B · Turnover", paste0(M(paste0("R^2 = ", numtex(sb$r.squared, 3))), ". Each worker adds €", num(coef(mb)[2], 4), " million (", M(paste0("p = ", numtex(pb[2, 4], 3))),
                               "); investment is not significant (", M(paste0("p = ", numtex(pb[3, 4], 3))), "). More spread among large firms: chapter 9.")),
      c("C · Consumption", paste0("Marginal propensity ", M(paste0("\\hat{\\beta}_1 = ", numtex(coef(mc)[2], 4))), ", between 0 and 1 as Keynes predicts. ",
                                  M(paste0("R^2 = ", numtex(summary(mc)$r.squared, 3))), ": beware of the common trend (chapter 10)."))))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 1.95, 3.85, 2.55, casos[[k]][1], casos[[k]][2], oscura = (k == 2), size = 15)
  x <- caja_texto(x, 0.6, 4.75, 12.13, 1.35, tr("Lo que queda por ver", "What comes next"),
                  tr("Los temas 3 y 4 dirán si estas cifras son precisas y significativas; los temas 9 y 10 revisarán los supuestos que aquí damos por buenos.",
                     "Chapters 3 and 4 will tell whether these figures are precise and significant; chapters 9 and 10 will check the assumptions taken for granted here."),
                  size = 15)
  x <- poner_texto(x, parrafo(tr("Scripts T02_CPA, T02_CPB y T02_CPC: estimación con lm() y gráficos de cada caso.",
                                 "Scripts T02_CPA, T02_CPB and T02_CPC: lm() estimation and charts for each case."), 16, ID$navy, bold = TRUE),
                   0.6, 6.25, 12.1, 0.55)

  # 20 · Cierre
  x <- diapo_cierre(x,
    tr(c(paste0(M("\\hat{\\boldsymbol\\beta} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'\\mathbf{Y}"), ": en Excel, con MMULT, MINVERSA y TRANSPONER."),
         "Supuestos 1 a 5: Gauss-Markov. El 6, la normalidad, para contrastar.",
         paste0(M("\\hat{\\beta}_j"), ": efecto marginal, con unidades y ceteris paribus."),
         paste0("Cambiar unidades mueve el coeficiente, no la ", M("t"), " ni el ", M("R^2"), "."),
         "Log-log: elasticidad. Log-lin: semielasticidad.",
         paste0(M("\\text{SCT} = \\text{SCE} + \\text{SCR}"), "; el ", M("\\overline{R}{}^2"), " penaliza regresores.")),
       c(paste0(M("\\hat{\\boldsymbol\\beta} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'\\mathbf{Y}"), ": in Excel, with MMULT, MINVERSE and TRANSPOSE."),
         "Assumptions 1 to 5: Gauss-Markov. Number 6, normality, for testing.",
         paste0(M("\\hat{\\beta}_j"), ": marginal effect, with units and ceteris paribus."),
         paste0("Changing units moves the coefficient, not the ", M("t"), " or the ", M("R^2"), "."),
         "Log-log: elasticity. Log-lin: semi-elasticity.",
         paste0(M("\\text{TSS} = \\text{ESS} + \\text{RSS}"), "; the ", M("\\overline{R}{}^2"), " penalises regressors."))),
    tr("Excel de clase del tema 2, scripts de R del tema 2 y app «Estimación MCO». Explicación completa en el capítulo 2 del manual.",
       "Chapter 2 class Excel workbook, Chapter 2 R scripts and the «OLS estimation» app. Full explanation in Chapter 2 of the book."),
    tema)

  guardar_pptx(x, "T02_mEC_MCO.pptx", "T02_mEC_OLS.pptx")
}
