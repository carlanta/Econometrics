# ══════════════════════════════════════════════════════════════════
# Capítulo 03 · Propiedades de los estimadores MCO · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap03_pptx.R
# Contenido y numeración: 03-propiedades-estimadores-mco.qmd (secciones 3.1 a 3.5). Modelos progresivos de los casos
# A, B y C como en el manual; simulación de 1.000 muestras con la misma semilla; errores estándar a mano con DS_mini
# (el modelo simple del tema 2) para enlazar con el cálculo matricial.
# ══════════════════════════════════════════════════════════════════

# ── Ayudantes propios de este tema (no están en los comunes) ──────
pm <- function(m, d = 4) {
  m <- as.matrix(m)
  f <- if (is.character(m)) m else matrix(numtex(m, d), nrow(m))
  paste0("\\begin{pmatrix}", paste(apply(f, 1, paste, collapse = " & "), collapse = " \\\\ "), "\\end{pmatrix}")
}

for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 3", "Chapter 3")
  SCT <- tr("\\text{SCT}", "\\text{TSS}"); SCR <- tr("\\text{SCR}", "\\text{RSS}"); ES <- tr("\\text{ES}", "\\text{SE}")

  # ── Modelos progresivos (apartados 3.1 y 3.5) ──
  ma <- list(lm(ventas ~ precio_propio, data = caso_a),
             lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a),
             lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a),
             lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit + promocion + temporada, data = caso_a))
  mb <- list(lm(cifra_negocio ~ trabajadores, data = caso_b),
             lm(cifra_negocio ~ trabajadores + inversion, data = caso_b),
             lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b))
  mc <- list(lm(consumo ~ pib, data = caso_c),
             lm(consumo ~ pib + tipo_interes, data = caso_c),
             lm(consumo ~ pib + tipo_interes + inflacion, data = caso_c))
  r2  <- function(m) summary(m)$r.squared
  r2a <- function(m) summary(m)$adj.r.squared
  n_a <- nrow(caso_a); R2_m2 <- r2(ma[[2]])

  # ── Simulación de insesgadez (apartado 3.3.5: misma semilla y diseño que el manual) ──
  set.seed(2024)
  est <- replicate(1000, { xs <- rnorm(30, 5, 2); ys <- 2 + 3 * xs + rnorm(30, 0, 1); coef(lm(ys ~ xs))[2] })

  # ── Errores estándar a mano con DS_mini (modelo simple del tema 2) ──
  m_s <- lm(ventas ~ precio_propio, data = ds_mini)
  X <- cbind(1, ds_mini$precio_propio); XtXi <- solve(t(X) %*% X)
  scr_s <- sum(resid(m_s)^2); gl_s <- nrow(ds_mini) - 1 - 1; s2 <- scr_s / gl_s
  V <- s2 * XtXi; se <- sqrt(diag(V))
  stopifnot(all(abs(se - summary(m_s)$coefficients[, 2]) < 1e-10))

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 3, tr("Propiedades de los estimadores MCO", "Properties of the OLS estimators"),
    tr("¿Por qué fiarnos de MCO y no de cualquier otra recta?", "Why trust OLS rather than any other line?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = sprintf("3.%d", 1:5),
    titulo = tr(c(paste0("El problema del ", M("R^2"), ": ¿más variables es siempre mejor?"), "El coeficiente de determinación ajustado",
                  "Propiedades de los estimadores MCO", "Varianza del término de error y errores estándar", "Aplicación a los casos prácticos"),
                c(paste0("The problem with ", M("R^2"), ": are more variables always better?"), "The adjusted coefficient of determination",
                  "Properties of the OLS estimators", "Error variance and standard errors", "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Comparar modelos", paste0("Usar el ", M("\\overline{R}{}^2"), " para decidir si una variable nueva compensa el grado de libertad que cuesta.")),
       c("Compare models", paste0("Use the ", M("\\overline{R}{}^2"), " to decide whether a new variable is worth the degree of freedom it costs."))),
    tr(c("Entender ELIO", "Qué garantiza el teorema de Gauss-Markov: lineal, insesgado y de varianza mínima."),
       c("Understand BLUE", "What the Gauss-Markov theorem guarantees: linear, unbiased and minimum variance.")),
    tr(c("Medir la precisión", paste0("Estimar ", M("\\sigma^2"), " y obtener los errores estándar con la diagonal de ", M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}"), ".")),
       c("Measure precision", paste0("Estimate ", M("\\sigma^2"), " and obtain the standard errors from the diagonal of ", M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}"), ".")))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Comparar modelos", "Comparing models"),
    tr(c(paste0("El ", M("R^2"), " nunca baja"), paste0("El ", M("\\overline{R}{}^2"), " penaliza"), "Grados de libertad"),
       c(paste0("The ", M("R^2"), " never falls"), paste0("The ", M("\\overline{R}{}^2"), " penalises"), "Degrees of freedom")))

  # 5 · 3.1 El problema del R²
  x <- diapo_contenido(x, tr("3.1 · ¿Más variables es siempre mejor?", "3.1 · Are more variables always better?"),
    tr(paste0("El ", M("R^2"), " sube aunque la variable sobre"), paste0("The ", M("R^2"), " rises even for a useless variable")), tema)
  dr <- data.frame(m = rep(1:4, 2), v = c(sapply(ma, r2), sapply(ma, r2a)),
                   s = factor(rep(tr(c("R2", "R2 ajustado"), c("R2", "Adjusted R2")), each = 4), levels = tr(c("R2", "R2 ajustado"), c("R2", "Adjusted R2"))))
  g_r2 <- ggplot(dr, aes(m, v, colour = s, linetype = s)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    geom_text(data = dr[1:4, ], aes(label = num(v, 3)), vjust = -1.1, size = 3.8, show.legend = FALSE) +
    geom_text(data = dr[5:8, ], aes(label = num(v, 3)), vjust = 2.0, size = 3.8, show.legend = FALSE) +
    scale_colour_manual(values = c(COL$navy, COL$oro), name = NULL) + scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    scale_x_continuous(breaks = 1:4, labels = tr(c("M1\n1 var.", "M2\n2 var.", "M3\n3 var.", "M4\n5 var."), c("M1\n1 var.", "M2\n2 vars", "M3\n3 vars", "M4\n5 vars"))) +
    scale_y_continuous(labels = function(v) num(v, 2), limits = c(0.2, 0.95)) +
    labs(x = NULL, y = NULL) + tema_cdea(13)
  x <- imagen_ggplot(x, g_r2, 0.6, 1.95, 6.9, 4.85)
  x <- vinetas(x, tr(
    c(paste0("Caso A, modelos progresivos: el ", M("R^2"), " pasa de ", num(r2(ma[[1]]), 3), " a ", num(r2(ma[[4]]), 3), " y nunca baja."),
      "MCO siempre puede dar a la variable nueva un coeficiente que reduzca algo la SCR.",
      paste0("El gran salto es la publicidad (M1 a M2): el ", M("\\overline{R}{}^2"), " pasa de ", num(r2a(ma[[1]]), 3), " a ", num(r2a(ma[[2]]), 3), "."),
      paste0("Para comparar modelos con distinto ", M("k"), ", el ", M("R^2"), " no sirve.")),
    c(paste0("Case A, progressive models: the ", M("R^2"), " goes from ", num(r2(ma[[1]]), 3), " to ", num(r2(ma[[4]]), 3), " and never falls."),
      "OLS can always give the new variable a coefficient that cuts the RSS a little.",
      paste0("The big jump is advertising (M1 to M2): the ", M("\\overline{R}{}^2"), " goes from ", num(r2a(ma[[1]]), 3), " to ", num(r2a(ma[[2]]), 3), "."),
      paste0("To compare models with different ", M("k"), ", the ", M("R^2"), " is no use."))),
    7.8, 2.05, 4.95, 4.75, size = 16)

  # 6 · 3.2 R² ajustado
  x <- diapo_contenido(x, tr("3.2 · El coeficiente de determinación ajustado", "3.2 · The adjusted coefficient of determination"),
    tr(paste0("El ", M("\\overline{R}{}^2"), " cobra cada grado de libertad"), paste0("The ", M("\\overline{R}{}^2"), " charges for each degree of freedom")), tema)
  x <- formula_caja(x, paste0("\\overline{R}{}^2 = 1 - \\frac{", SCR, "/(n-k-1)}{", SCT, "/(n-1)} = 1 - \\frac{n-1}{n-k-1}\\,(1 - R^2)"),
                    0.6, 1.95, 12.1, 1.25, size = 24)
  x <- vinetas(x, tr(
    c(paste0("Si la variable nueva no reduce la SCR lo bastante para compensar el grado de libertad perdido, el ", M("\\overline{R}{}^2"), " baja."),
      paste0(M("\\overline{R}{}^2 \\leq R^2"), " siempre; puede ser negativo si el modelo explica muy poco."),
      paste0(M("R^2 - \\overline{R}{}^2 = (1 - R^2)\\,\\frac{k}{n-k-1}"), ": crece con regresores poco útiles.")),
    c(paste0("If the new variable does not cut the RSS enough to offset the lost degree of freedom, the ", M("\\overline{R}{}^2"), " falls."),
      paste0(M("\\overline{R}{}^2 \\leq R^2"), " always; it can be negative if the model explains very little."),
      paste0(M("R^2 - \\overline{R}{}^2 = (1 - R^2)\\,\\frac{k}{n-k-1}"), ": it grows with weak regressors."))),
    0.6, 3.45, 6.9, 3.35, size = 16)
  x <- caja_dato(x, 7.9, 3.45, 4.83, 3.25, tr("Ejercicio 3.1 · Caso A, M2", "Exercise 3.1 · Case A, M2"),
                 M(paste0("\\overline{R}{}^2 = 1 - \\frac{", n_a - 1, "}{", n_a - 3, "}(1 - ", numtex(R2_m2, 4), ") = ", numtex(1 - (n_a - 1) / (n_a - 3) * (1 - R2_m2), 4))),
                 tr(paste0("Con ", M(paste0("n = ", n_a)), " y ", M("k = 2"), ". Coincide con summary() en R."),
                    paste0("With ", M(paste0("n = ", n_a)), " and ", M("k = 2"), ". It matches summary() in R.")), ID$navy, size_valor = 16)

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Gauss-Markov: MCO es ELIO", "Gauss-Markov: OLS is BLUE"),
    tr(c("Lineal, insesgado, óptimo", "Insesgadez y eficiencia", "Consistencia", "Simulación de 1.000 muestras"),
       c("Linear, unbiased, best", "Unbiasedness and efficiency", "Consistency", "Simulation of 1,000 samples")))

  # 8 · 3.3.1 Gauss-Markov
  x <- diapo_contenido(x, tr("3.3 · El teorema de Gauss-Markov", "3.3 · The Gauss-Markov theorem"),
    tr("Con los supuestos 1 a 5, MCO es el mejor", "Under assumptions 1 to 5, OLS is the best"), tema)
  x <- poner_texto(x, parrafo(tr("Linealidad, muestreo aleatorio, no colinealidad perfecta, media condicional cero y homocedasticidad (tema 2): el estimador MCO es ELIO, en inglés BLUE.",
                                 "Linearity, random sampling, no perfect collinearity, zero conditional mean and homoskedasticity (chapter 2): the OLS estimator is BLUE."),
                              17, ID$gris), 0.6, 1.9, 12.1, 0.9)
  elio <- tr(list(c("E · Estimador", "Una regla que convierte la muestra en una cifra para el parámetro poblacional.", "\\hat{\\beta}_j \\rightarrow \\beta_j"),
                  c("L · Lineal", paste0("Combinación lineal de ", M("\\mathbf{Y}"), ", con ", M("\\mathbf{C} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'"), "."), "\\boldsymbol{\\hat{\\beta}} = \\mathbf{C}\\mathbf{Y}"),
                  c("I · Insesgado", "Sin error sistemático: en promedio acierta.", "E(\\hat{\\beta}_j) = \\beta_j"),
                  c("O · Óptimo", "La menor varianza entre los lineales e insesgados.", "\\text{Var}(\\hat{\\beta}_j) \\leq \\text{Var}(\\tilde{\\beta}_j)")),
             list(c("B · Best", "The smallest variance among linear unbiased estimators.", "\\text{Var}(\\hat{\\beta}_j) \\leq \\text{Var}(\\tilde{\\beta}_j)"),
                  c("L · Linear", paste0("A linear combination of ", M("\\mathbf{Y}"), ", with ", M("\\mathbf{C} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'"), "."), "\\boldsymbol{\\hat{\\beta}} = \\mathbf{C}\\mathbf{Y}"),
                  c("U · Unbiased", "No systematic error: right on average.", "E(\\hat{\\beta}_j) = \\beta_j"),
                  c("E · Estimator", "A rule that turns the sample into a figure for the population parameter.", "\\hat{\\beta}_j \\rightarrow \\beta_j")))
  for (k in 1:4) {
    left <- 0.6 + (k - 1) * 3.1
    x <- caja_dato(x, left, 2.95, 2.85, 2.45, elio[[k]][1], M(elio[[k]][3]), elio[[k]][2], if (k %in% c(1, 4)) ID$oro else ID$navy, size_valor = 18)
  }
  x <- caja_texto(x, 0.6, 5.6, 12.13, 1.15, tr("Si falla un supuesto", "If an assumption fails"),
                  tr("Con heterocedasticidad o autocorrelación MCO sigue siendo insesgado, pero deja de ser óptimo (temas 9 y 10). Si se omite una variable relevante, deja de ser insesgado (tema 6).",
                     "With heteroskedasticity or autocorrelation OLS is still unbiased but no longer best (chapters 9 and 10). If a relevant variable is omitted, it is no longer unbiased (chapter 6)."),
                  oscura = TRUE, size = 14)

  # 9 · 3.3.2–3.3.5 Insesgadez, eficiencia, consistencia
  x <- diapo_contenido(x, tr("3.3 · Insesgadez, eficiencia y consistencia", "3.3 · Unbiasedness, efficiency and consistency"),
    tr("Mil muestras: MCO acierta en promedio", "A thousand samples: OLS is right on average"), tema)
  g_sim <- ggplot(data.frame(b = est), aes(b)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = COL$oro_cl, colour = "white") +
    geom_vline(xintercept = 3, colour = COL$navy, linewidth = 1.2, linetype = "dashed") +
    geom_vline(xintercept = mean(est), colour = COL$oro, linewidth = 1) +
    annotate("text", x = 2.985, y = Inf, vjust = 1.5, hjust = 1, label = tr("Valor real: 3", "True value: 3"), colour = COL$navy, size = 4.2, fontface = "bold") +
    annotate("text", x = mean(est) + 0.02, y = Inf, vjust = 3.2, hjust = 0, label = paste0(tr("Media: ", "Mean: "), num(mean(est), 4)), colour = "#8A6D1E", size = 4.2, fontface = "bold") +
    scale_x_continuous(labels = function(v) num(v, 1)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Estimaciones de b1 en 1.000 muestras", "Estimates of b1 in 1,000 samples"), y = tr("Densidad", "Density")) + tema_cdea(13)
  x <- imagen_ggplot(x, g_sim, 0.6, 1.95, 6.6, 4.85)
  x <- vinetas(x, tr(
    c(paste0(M("Y = 2 + 3X + u"), ", 1.000 muestras de 30: la media de los ", M("\\hat{\\beta}_1"), " es ", num(mean(est), 4), ". Insesgadez."),
      paste0("Eficiencia: poca dispersión. ", M("\\text{Var}(\\boldsymbol{\\hat{\\beta}}) = \\sigma^2(\\mathbf{X}'\\mathbf{X})^{-1}"), "."),
      paste0("Consistencia: ", M("\\hat{\\beta}_j \\xrightarrow{p} \\beta_j"), " cuando ", M("n \\to \\infty"), "; con más datos, la campana se estrecha.")),
    c(paste0(M("Y = 2 + 3X + u"), ", 1,000 samples of 30: the mean of the ", M("\\hat{\\beta}_1"), " is ", num(mean(est), 4), ". Unbiasedness."),
      paste0("Efficiency: little spread. ", M("\\text{Var}(\\boldsymbol{\\hat{\\beta}}) = \\sigma^2(\\mathbf{X}'\\mathbf{X})^{-1}"), "."),
      paste0("Consistency: ", M("\\hat{\\beta}_j \\xrightarrow{p} \\beta_j"), " as ", M("n \\to \\infty"), "; with more data, the bell narrows."))),
    7.5, 2.05, 5.25, 4.75, size = 16)

  # 10 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Precisión: varianza y errores estándar", "Precision: variance and standard errors"),
    tr(c(paste0("Estimar ", M("\\sigma^2")), paste0("La diagonal de ", M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}")), "De qué depende el error estándar"),
       c(paste0("Estimating ", M("\\sigma^2")), paste0("The diagonal of ", M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}")), "What the standard error depends on")))

  # 11 · 3.4.1 Estimación de sigma²
  x <- diapo_contenido(x, tr("3.4 · Estimación de la varianza del error", "3.4 · Estimating the error variance"),
    tr("Dividir por los grados de libertad", "Dividing by the degrees of freedom"), tema)
  x <- formula_caja(x, paste0("\\hat{\\sigma}^2 = \\frac{", SCR, "}{n-k-1} = \\frac{\\sum_{i=1}^{n} \\hat{u}_i^2}{n-k-1} = \\frac{\\hat{\\mathbf{u}}'\\hat{\\mathbf{u}}}{n-k-1}"),
                    0.6, 1.95, 12.1, 1.3, size = 24)
  x <- vinetas(x, tr(
    c(paste0(M("n-k-1"), ": observaciones menos parámetros estimados (", M("k"), " pendientes y la constante)."),
      "Dividir por los grados de libertad, y no por n, hace insesgado al estimador.",
      paste0(M("\\hat{\\sigma}"), " es el error típico de la regresión: en R, «Residual standard error»; en Gretl, «D.T. de la regresión».")),
    c(paste0(M("n-k-1"), ": observations minus estimated parameters (", M("k"), " slopes and the constant)."),
      "Dividing by the degrees of freedom, not by n, makes the estimator unbiased.",
      paste0(M("\\hat{\\sigma}"), " is the regression standard error: «Residual standard error» in R, «S.E. of regression» in Gretl."))),
    0.6, 3.5, 6.9, 3.3, size = 16)
  x <- caja_dato(x, 7.9, 3.5, 4.83, 3.2, tr("DS_mini · modelo simple", "DS_mini · simple model"),
                 M(paste0("\\hat{\\sigma}^2 = \\frac{", numtex(scr_s, 4), "}{10 - 1 - 1} = ", numtex(s2, 5))),
                 paste0(M(paste0("\\hat{\\sigma} = ", numtex(sqrt(s2), 4))), tr(" MM € de ventas, el mismo valor que da lm().", " € million of sales, the same value lm() gives.")),
                 ID$navy, size_valor = 22)

  # 12 · 3.4.2 Errores estándar con la diagonal
  x <- diapo_contenido(x, tr("3.4 · Errores estándar de los coeficientes", "3.4 · Standard errors of the coefficients"),
    tr("Los errores estándar salen de la diagonal", "Standard errors come from the diagonal"), tema)
  x <- formula_caja(x, paste0(ES, "(\\hat{\\beta}_j) = \\sqrt{\\hat{\\sigma}^2 \\, [(\\mathbf{X}'\\mathbf{X})^{-1}]_{jj}}"), 0.6, 1.95, 12.1, 1.0, size = 26)
  x <- formula_caja(x, paste0("\\widehat{\\text{Var}}(\\boldsymbol{\\hat{\\beta}}) = \\hat{\\sigma}^2 (\\mathbf{X}'\\mathbf{X})^{-1} = ", numtex(s2, 5), " \\cdot ",
                              pm(XtXi, 4), " = ", pm(V, 4)), 0.6, 3.15, 12.1, 1.35, size = 22)
  x <- formula_caja(x, paste0(ES, "(\\hat{\\beta}_0) = \\sqrt{", numtex(V[1, 1], 4), "} = ", numtex(se[1], 4), " \\qquad ",
                              ES, "(\\hat{\\beta}_1) = \\sqrt{", numtex(V[2, 2], 4), "} = ", numtex(se[2], 4)), 0.6, 4.7, 12.1, 1.0, size = 24)
  x <- poner_texto(x, parrafo(tr("DS_mini, modelo simple del tema 2: los mismos errores estándar que lm() y Gretl. Fuera de la diagonal, las covarianzas.",
                                 "DS_mini, simple model from chapter 2: the same standard errors as lm() and Gretl. Off the diagonal, the covariances."),
                              16, ID$navy, bold = TRUE), 0.6, 5.95, 12.1, 0.8)

  # 13 · ¿De qué depende el error estándar?
  x <- diapo_contenido(x, tr("3.4 · ¿De qué depende el error estándar?", "3.4 · What does the standard error depend on?"),
    tr("Cuatro palancas de la precisión", "Four levers of precision"), tema)
  x <- formula_caja(x, "\\text{Var}(\\hat{\\beta}_j) = \\frac{\\sigma^2}{(n-1)\\,S_{X_j}^2\\,(1 - R_j^2)}", 0.6, 1.95, 12.1, 1.25, size = 26)
  palancas <- tr(list(c("Ruido del modelo", "\\sigma^2 \\uparrow \\;\\Rightarrow\\; \\text{ES} \\uparrow", "Si el modelo ajusta mal, todo es menos preciso."),
                      c("Tamaño muestral", "n \\uparrow \\;\\Rightarrow\\; \\text{ES} \\downarrow", "Más observaciones, más información."),
                      c(paste0("Variación de ", M("X_j")), "S_{X_j}^2 \\uparrow \\;\\Rightarrow\\; \\text{ES} \\downarrow", paste0("Si ", M("X_j"), " apenas varía, cuesta ver su efecto.")),
                      c("Colinealidad", "R_j^2 \\uparrow \\;\\Rightarrow\\; \\text{ES} \\uparrow", paste0("Si ", M("X_j"), " se parece a las demás, su efecto se confunde."))),
                 list(c("Model noise", "\\sigma^2 \\uparrow \\;\\Rightarrow\\; \\text{SE} \\uparrow", "If the model fits poorly, everything is less precise."),
                      c("Sample size", "n \\uparrow \\;\\Rightarrow\\; \\text{SE} \\downarrow", "More observations, more information."),
                      c(paste0("Variation in ", M("X_j")), "S_{X_j}^2 \\uparrow \\;\\Rightarrow\\; \\text{SE} \\downarrow", paste0("If ", M("X_j"), " barely moves, its effect is hard to see.")),
                      c("Collinearity", "R_j^2 \\uparrow \\;\\Rightarrow\\; \\text{SE} \\uparrow", paste0("If ", M("X_j"), " looks like the others, its effect gets blurred."))))
  for (k in 1:4) x <- caja_dato(x, 0.6 + (k - 1) * 3.1, 3.4, 2.85, 2.6, palancas[[k]][1], M(palancas[[k]][2]), palancas[[k]][3],
                                if (k %in% c(1, 4)) ID$oro else ID$navy, size_valor = 20)
  x <- poner_texto(x, parrafo(paste0(M("R_j^2"), tr(": el de la regresión de ", ": that of the regression of "), M("X_j"),
                                     tr(" sobre las demás explicativas. Colinealidad perfecta: ", " on the other regressors. Perfect collinearity: "), M("R_j^2 = 1"),
                                     tr(" y la varianza se dispara.", " and the variance explodes.")), 15, ID$gris, italic = TRUE), 0.6, 6.15, 12.1, 0.6)

  # 14 · Gretl → R
  x <- diapo_contenido(x, tr("3.4 · Gretl → R", "3.4 · Gretl → R"), tr("Dónde están estas cifras en Gretl y en R", "Where to find these figures in Gretl and R"), tema)
  gr <- data.frame(
    op = tr(c("R2 ajustado", "Error típico de la regresión", "Matriz de varianzas y covarianzas", "Errores estándar", "Comparar modelos"),
            c("Adjusted R2", "Regression standard error", "Variance-covariance matrix", "Standard errors", "Compare models")),
    gretl = tr(c("Ventana del modelo: «R-cuadrado corregido»", "Ventana del modelo: «D.T. de la regresión»", "Análisis → Matriz de covarianzas de los coeficientes",
                 "Columna «Desv. típica»", "Criterios de información en la ventana del modelo"),
               c("Model window: «Adjusted R-squared»", "Model window: «S.E. of regression»", "Analysis → Coefficient covariance matrix",
                 "«Std. Error» column", "Information criteria in the model window")),
    r = c("summary(m)$adj.r.squared", "sigma(m)", "vcov(m)", "sqrt(diag(vcov(m)))", "AIC(m1, m2)"))
  x <- poner_tabla(x, tabla_pptx(gr, tr(c("Qué", "Gretl", "R"), c("What", "Gretl", "R")), size = 13, mono = 3, texto = 1:2), 0.6, 1.95, max_ancho = 12.1)
  x <- caja_texto(x, 0.6, 4.88, 12.13, 1.22, tr("Y a mano, en Excel", "And by hand, in Excel"),
                  tr(paste0(M("\\hat{\\sigma}^2"), " es =SUMA.CUADRADOS(residuos) dividido entre ", M("n-k-1"), "; la matriz, ", M("\\hat{\\sigma}^2"), " por =MINVERSA(XX), y cada error estándar, =RAIZ() de un elemento de su diagonal."),
                     paste0(M("\\hat{\\sigma}^2"), " is =SUMSQ(residuals) divided by ", M("n-k-1"), "; the matrix, ", M("\\hat{\\sigma}^2"), " times =MINVERSE(XX), and each standard error, =SQRT() of one diagonal element.")),
                  size = 15)
  x <- poner_texto(x, parrafo(paste0(tr("En R, ", "In R, "), M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}"), tr(" es vcov(m): su diagonal da las varianzas y su raíz, los errores estándar.",
                                                                                                                     " is vcov(m): its diagonal gives the variances and their square root the standard errors.")),
                              16, ID$navy, bold = TRUE), 0.6, 6.2, 12.1, 0.6)

  # 15 · 3.5 Casos prácticos
  x <- diapo_contenido(x, tr("3.5 · Aplicación a los casos prácticos", "3.5 · Application to the case studies"),
    tr("Modelos progresivos en los tres casos", "Progressive models in the three cases"), tema)
  fila <- function(caso, nombre, m) data.frame(c = caso, m = nombre, k = length(coef(m)) - 1L, r2 = num(r2(m), 4), r2a = num(r2a(m), 4), s = num(summary(m)$sigma, 2))
  tb <- rbind(fila("A", tr("M1: precio", "M1: price"), ma[[1]]), fila("A", tr("M2: + publicidad", "M2: + advertising"), ma[[2]]),
              fila("A", tr("M3: + precio sust.", "M3: + subst. price"), ma[[3]]), fila("A", tr("M4: + promoción y temporada", "M4: + promotion and season"), ma[[4]]),
              fila("B", tr("M1: trabajadores", "M1: workers"), mb[[1]]), fila("B", tr("M2: + inversión", "M2: + investment"), mb[[2]]),
              fila("B", tr("M3: + superficie", "M3: + land area"), mb[[3]]),
              fila("C", tr("M1: PIB", "M1: GDP"), mc[[1]]), fila("C", tr("M2: + tipo de interés", "M2: + interest rate"), mc[[2]]),
              fila("C", tr("M3: + inflación", "M3: + inflation"), mc[[3]]))
  x <- poner_tabla(x, tabla_pptx(tb, tr(c("Caso", "Modelo", "k", "R2", "R2 ajustado", "Error típico"), c("Case", "Model", "k", "R2", "Adjusted R2", "Std. error")),
                                 size = 12, texto = 1:2), 0.6, 1.95, max_ancho = 7.4)
  x <- vinetas(x, tr(
    c(paste0("A: la publicidad dispara el ", M("\\overline{R}{}^2"), "; promoción y temporada también suman."),
      paste0("B: el ", M("\\overline{R}{}^2"), " baja al añadir inversión y superficie: no compensan."),
      paste0("C: ", M("R^2"), " cercano a 1 con solo el PIB; en series temporales puede ser espurio (temas 6 y 10).")),
    c(paste0("A: advertising lifts the ", M("\\overline{R}{}^2"), "; promotion and season also help."),
      paste0("B: the ", M("\\overline{R}{}^2"), " falls when investment and land area are added: not worth it."),
      paste0("C: ", M("R^2"), " close to 1 with GDP alone; in time series it may be spurious (chapters 6 and 10).")) ),
    8.3, 2.05, 4.45, 4.75, size = 15)

  # 16 · Cierre
  x <- diapo_cierre(x,
    tr(c(paste0("El ", M("R^2"), " nunca baja al añadir variables; el ", M("\\overline{R}{}^2"), " sí puede."),
         paste0("Compara modelos con distinto ", M("k"), " con el ", M("\\overline{R}{}^2"), ", no con el ", M("R^2"), "."),
         "Con los supuestos 1 a 5, MCO es ELIO: lineal, insesgado y óptimo.",
         paste0(M(paste0("\\hat{\\sigma}^2 = ", SCR, "/(n-k-1)")), ": varianza del error insesgada."),
         paste0(M("\\text{ES}(\\hat{\\beta}_j)"), ": raíz de la diagonal de ", M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}"), "."),
         paste0("Más ", M("n"), " y más variación de ", M("X"), ", más precisión; la colinealidad la resta.")),
       c(paste0("The ", M("R^2"), " never falls when variables are added; the ", M("\\overline{R}{}^2"), " can."),
         paste0("Compare models with different ", M("k"), " using the ", M("\\overline{R}{}^2"), ", not the ", M("R^2"), "."),
         "Under assumptions 1 to 5, OLS is BLUE: best, linear and unbiased.",
         paste0(M(paste0("\\hat{\\sigma}^2 = ", SCR, "/(n-k-1)")), ": an unbiased error variance."),
         paste0(M("\\text{SE}(\\hat{\\beta}_j)"), ": square root of the diagonal of ", M("\\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}"), "."),
         paste0("More ", M("n"), " and more variation in ", M("X"), ", more precision; collinearity takes it away."))),
    tr("Excel de clase del tema 3, scripts de R del tema 3 y app «Propiedades MCO». Explicación completa en el capítulo 3 del manual.",
       "Chapter 3 class Excel workbook, Chapter 3 R scripts and the «OLS properties» app. Full explanation in Chapter 3 of the book."),
    tema)

  guardar_pptx(x, "T03_mEC_Propiedades_MCO.pptx", "T03_mEC_OLS_Properties.pptx")
}
