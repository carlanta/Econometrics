# ══════════════════════════════════════════════════════════════════
# Capítulo 09 · Heterocedasticidad y MCO robusta · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap09_pptx.R
# Contenido y numeración: 09-heterocedasticidad.qmd (9.1 a 9.5.3). Modelos idénticos a los del capítulo:
#   A: ventas ~ precio_propio + gasto_publicidad
#   B: cifra_negocio ~ trabajadores + inversion
#   C: consumo ~ pib + tipo_interes
# Breusch-Pagan CLÁSICO, el que Carlos enseña en Gretl (Contrastes → Heterocedasticidad → Breusch-Pagan):
# lmtest::bptest(m, studentize = FALSE). Sin esa opción, bptest() calcula la variante de Koenker y no coincide con Gretl.
# Errores HC1 con sandwich::vcovHC().
# ══════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({ library(lmtest); library(sandwich) })
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 9", "Chapter 9")

  # ── Modelos y contrastes del capítulo ────────────────────────────
  ma <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
  mb <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
  mc <- lm(consumo ~ pib + tipo_interes, data = caso_c)
  bp <- list(A = bptest(ma, studentize = FALSE), B = bptest(mb, studentize = FALSE), C = bptest(mc, studentize = FALSE))
  # A mano (caso B): regresión auxiliar de û²/σ̃², con σ̃² = SCR/n, y BP = SCE_aux / 2
  g_b <- residuals(mb)^2 / (sum(residuals(mb)^2) / nrow(caso_b))
  aux_b <- lm(g_b ~ trabajadores + inversion, data = caso_b)
  sce_aux_b <- sum((fitted(aux_b) - mean(g_b))^2)
  stopifnot(abs(sce_aux_b / 2 - unname(bp$B$statistic)) < 1e-6)
  std_b <- coeftest(mb); rob_b <- coeftest(mb, vcov = vcovHC(mb, type = "HC1"))
  std_c <- coeftest(mc); rob_c <- coeftest(mc, vcov = vcovHC(mc, type = "HC1"))
  decide <- function(p) if (p < 0.05) tr("Heterocedasticidad", "Heteroskedasticity") else tr("Homocedasticidad", "Homoskedasticity")

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 9, tr("Heterocedasticidad y MCO robusta", "Heteroskedasticity and robust OLS"),
    tr("¿Se equivoca el modelo lo mismo con una empresa pequeña que con una grande?",
       "Does the model miss by the same amount for a small firm as for a large one?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("9.1", "9.2", "9.3", "9.4", "9.5"),
    titulo = tr(c("Repaso del supuesto de homocedasticidad", "Detección gráfica", "Contraste de Breusch-Pagan",
                  "Estimación MCO robusta", "Aplicación a los casos prácticos"),
                c("The homoskedasticity assumption revisited", "Graphical detection", "The Breusch-Pagan test",
                  "Robust OLS estimation", "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Entender el problema", "Qué deja de cumplirse y qué sigue valiendo de MCO cuando la varianza no es constante."),
       c("Understand the problem", "What stops holding and what still works in OLS when the variance is not constant.")),
    tr(c("Detectarlo", "El gráfico de residuos al cuadrado y el contraste de Breusch-Pagan."),
       c("Detect it", "The plot of squared residuals and the Breusch-Pagan test.")),
    tr(c("Corregir la inferencia", "Errores estándar robustos (HC): mismos coeficientes, contrastes fiables."),
       c("Fix the inference", "Robust (HC) standard errors: same coefficients, reliable tests."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Varianza constante", "Constant variance"),
    tr(c("El supuesto de homocedasticidad", "Qué falla y qué no", "Por qué importa en la práctica"),
       c("The homoskedasticity assumption", "What fails and what does not", "Why it matters in practice")))

  # 5 · 9.1 Concepto
  x <- diapo_contenido(x, tr("9.1 · Repaso del supuesto de homocedasticidad", "9.1 · The homoskedasticity assumption"),
    tr("La dispersión del error no debe cambiar", "The spread of the error must not change"), tema)
  x <- formula_caja(x, "\\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2 \\quad \\forall\\, i", 0.6, 2.0, 5.2, 0.9, size = 24)
  x <- formula_caja(x, "\\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma_i^2", 0.6, 3.1, 5.2, 0.9, size = 24)
  x <- vinetas(x, tr(
    c("Arriba, homocedasticidad: la misma varianza para todas las observaciones.",
      "Abajo, heterocedasticidad: cada observación tiene la suya.",
      "Típico en corte transversal: las empresas grandes varían más."),
    c("Top, homoskedasticity: the same variance for every observation.",
      "Bottom, heteroskedasticity: each observation has its own.",
      "Typical in cross-sections: large firms vary more.")),
    0.6, 4.3, 5.2, 2.5, size = 16)
  set.seed(2024)
  n_sim <- 120; xs <- runif(n_sim, 1, 10)
  pan <- tr(c("Homocedasticidad", "Heterocedasticidad"), c("Homoskedasticity", "Heteroskedasticity"))
  df_sim <- data.frame(x = rep(xs, 2), u = c(rnorm(n_sim, 0, 1.6), rnorm(n_sim, 0, 0.32 * xs)),
                       p = factor(rep(pan, each = n_sim), levels = pan))
  g_sim <- ggplot(df_sim, aes(x, u)) +
    geom_hline(yintercept = 0, colour = COL$gris, linetype = "dashed") +
    geom_point(colour = COL$navy, size = 1.8, alpha = 0.8) +
    facet_wrap(~ p) +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Variable explicativa X", "Explanatory variable X"), y = tr("Error", "Error")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_sim, 6.2, 1.95, 6.6, 4.85)

  # 6 · 9.1 Consecuencias
  x <- diapo_contenido(x, tr("9.1 · Consecuencias sobre MCO", "9.1 · Consequences for OLS"),
    tr("Los coeficientes valen; los errores estándar, no", "The coefficients hold; the standard errors do not"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 2.4, tr("Insesgadez y consistencia", "Unbiasedness and consistency"), tr("Se mantienen", "They hold"),
    paste0(tr("Sigue valiendo ", "It still holds that "), M("E(u_i \\mid \\mathbf{X}) = 0"), "."), ID$navy, size_valor = 26)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 2.4, tr("Eficiencia", "Efficiency"), tr("Se pierde", "It is lost"),
    tr("MCO deja de ser ELIO: ya no tiene la varianza mínima (Gauss-Markov).", "OLS is no longer BLUE: it no longer has minimum variance (Gauss-Markov)."), ID$oro, size_valor = 26)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 2.4, tr("Errores estándar", "Standard errors"), tr("Incorrectos", "Wrong"),
    tr("Los contrastes t y F y los intervalos de confianza dejan de ser fiables.", "t and F tests and confidence intervals stop being reliable."), ID$gris_med, size_valor = 26)
  x <- formula_caja(x, "\\text{Var}(\\hat{\\boldsymbol\\beta} \\mid \\mathbf{X}) = \\sigma^2 (\\mathbf{X}'\\mathbf{X})^{-1}", 0.6, 4.75, 6.0, 1.0, size = 24)
  x <- poner_texto(x, parrafo(tr("La fórmula de siempre solo es válida con varianza constante. Con heterocedasticidad, el programa calcula mal los errores estándar.",
                                 "The usual formula is valid only with constant variance. With heteroskedasticity, the software gets the standard errors wrong."),
                              16, ID$navy, bold = TRUE), 6.9, 4.75, 5.8, 1.2)
  x <- poner_texto(x, parrafo(tr("Peligro: con errores estándar demasiado pequeños, se declaran significativas variables que no lo son.",
                                 "Danger: with standard errors that are too small, variables are declared significant when they are not."),
                              16, ID$gris, italic = TRUE), 0.6, 6.0, 12.1, 0.8)

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Detectar", "Detect"),
    tr(c("El gráfico de residuos", "El contraste de Breusch-Pagan", "Los casos A, B y C"),
       c("The residual plot", "The Breusch-Pagan test", "Cases A, B and C")))

  # 8 · 9.2 Detección gráfica
  x <- diapo_contenido(x, tr("9.2 · Detección gráfica", "9.2 · Graphical detection"),
    tr("El embudo de los residuos al cuadrado", "The funnel of the squared residuals"), tema)
  casos <- tr(c("Caso A (demanda)", "Caso B (cifra de negocio)"), c("Case A (demand)", "Case B (turnover)"))
  df_r <- rbind(data.frame(f = fitted(ma), r2 = residuals(ma)^2, caso = casos[1]),
                data.frame(f = fitted(mb), r2 = residuals(mb)^2, caso = casos[2]))
  df_r$caso <- factor(df_r$caso, levels = casos)
  g_r <- ggplot(df_r, aes(f, r2)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = COL$oro, linewidth = 1.2, linetype = "dashed") +
    geom_point(colour = COL$navy, size = 2.2, alpha = 0.85) +
    facet_wrap(~ caso, scales = "free") +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Valores ajustados", "Fitted values"), y = tr("Residuos al cuadrado", "Squared residuals")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_r, 0.6, 1.95, 8.0, 4.85)
  x <- vinetas(x, tr(
    c(paste0("Se dibuja ", M("\\hat{u}_i^2"), " frente a ", M("\\hat{Y}_i"), " o frente a cada ", M("X_j"), "."),
      "Nube sin forma: nada que temer (caso A).",
      "Dispersión que crece con el tamaño: embudo (caso B).",
      "El gráfico orienta; la decisión la da el contraste."),
    c(paste0("Plot ", M("\\hat{u}_i^2"), " against ", M("\\hat{Y}_i"), " or against each ", M("X_j"), "."),
      "A shapeless cloud: nothing to worry about (case A).",
      "Spread growing with size: a funnel (case B).",
      "The plot guides; the test decides.")),
    8.9, 2.1, 3.9, 4.7, size = 16)

  # 9 · 9.3 Breusch-Pagan
  x <- diapo_contenido(x, tr("9.3 · Contraste de Breusch-Pagan", "9.3 · The Breusch-Pagan test"),
    tr(paste0("¿Explican las ", M("X"), " la varianza del error?"), paste0("Do the ", M("X"), " explain the error variance?")), tema)
  x <- formula_caja(x, tr("H_0: \\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2 \\qquad H_1: \\text{Var}(u_i \\mid \\mathbf{X}) \\neq \\sigma^2",
                          "H_0: \\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2 \\qquad H_1: \\text{Var}(u_i \\mid \\mathbf{X}) \\neq \\sigma^2"),
                    0.6, 2.0, 12.1, 0.9, size = 24)
  x <- formula_caja(x, "\\frac{\\hat{u}_i^2}{\\tilde{\\sigma}^2} = \\delta_0 + \\delta_1 X_{1i} + \\dots + \\delta_k X_{ki} + v_i", 0.6, 3.1, 6.6, 1.05, size = 22)
  x <- formula_caja(x, tr("BP = \\frac{\\text{SCE}_{\\text{aux}}}{2} \\sim \\chi^2_k", "BP = \\frac{\\text{ESS}_{\\text{aux}}}{2} \\sim \\chi^2_k"), 7.5, 3.1, 5.2, 1.05, size = 22)
  x <- vinetas(x, tr(
    c(paste0("1. Se estima el modelo por MCO y se guardan los residuos ", M("\\hat{u}_i"), "."),
      paste0("2. Regresión auxiliar de ", M("\\hat{u}_i^2/\\tilde{\\sigma}^2"), ", con ", M("\\tilde{\\sigma}^2 = \\text{SCR}/n"), ", sobre las explicativas."),
      paste0("3. Si el p-valor es menor que ", M("\\alpha"), ", se rechaza la homocedasticidad.")),
    c(paste0("1. Estimate the model by OLS and keep the residuals ", M("\\hat{u}_i"), "."),
      paste0("2. Auxiliary regression of ", M("\\hat{u}_i^2/\\tilde{\\sigma}^2"), ", with ", M("\\tilde{\\sigma}^2 = \\text{RSS}/n"), ", on the regressors."),
      paste0("3. If the p-value is below ", M("\\alpha"), ", homoskedasticity is rejected."))),
    0.6, 4.35, 7.4, 2.4, size = 17)
  x <- caja_dato(x, 8.4, 4.3, 4.3, 2.45, tr("Caso B, a mano", "Case B, by hand"),
    M(paste0("BP = ", numtex(sce_aux_b, 3), "/2 = ", numtex(sce_aux_b / 2, 2))),
    tr(sprintf("La mitad de la suma de cuadrados explicada de la regresión auxiliar, con 2 g. l.: p-valor = %s. Es lo que da Gretl.", num(bp$B$p.value, 4)),
       sprintf("Half the explained sum of squares of the auxiliary regression, with 2 d.f.: p-value = %s. It is what Gretl reports.", num(bp$B$p.value, 4))),
    ID$navy, size_valor = 22)

  # 10 · 9.3 Resultados de los tres casos
  x <- diapo_contenido(x, tr("9.3 · Breusch-Pagan en los tres casos", "9.3 · Breusch-Pagan in the three cases"),
    tr("A pasa; B y C no", "A passes; B and C do not"), tema)
  tab_bp <- data.frame(
    caso = tr(c("A (demanda)", "B (cifra de negocio)", "C (consumo trimestral)"), c("A (demand)", "B (turnover)", "C (quarterly consumption)")),
    bp = vapply(bp, function(b) num(b$statistic, 3), ""),
    gl = vapply(bp, function(b) sprintf("%d", as.integer(b$parameter)), ""),
    p = vapply(bp, function(b) num(b$p.value, 4), ""),
    d = vapply(bp, function(b) decide(b$p.value), ""), stringsAsFactors = FALSE)
  x <- poner_tabla(x, tabla_pptx(tab_bp, tr(c("Caso", "Estadístico BP", "g. l.", "p-valor", "Decisión (5 %)"),
                                            c("Case", "BP statistic", "d.f.", "p-value", "Decision (5 %)")), size = 16, texto = c(1, 5)), 0.6, 2.1)
  x <- vinetas(x, tr(
    c(sprintf("A: p-valor = %s. No se rechaza: los errores estándar de siempre valen.", num(bp$A$p.value, 3)),
      sprintf("B: p-valor = %s, se rechaza incluso al 1 %%: heterocedasticidad natural por tamaño.", num(bp$B$p.value, 3)),
      sprintf("C: p-valor = %s. En series, la varianza también puede cambiar con el tiempo.", num(bp$C$p.value, 4))),
    c(sprintf("A: p-value = %s. Not rejected: the usual standard errors are fine.", num(bp$A$p.value, 3)),
      sprintf("B: p-value = %s, rejected even at 1 %%: natural heteroskedasticity due to size.", num(bp$B$p.value, 3)),
      sprintf("C: p-value = %s. In time series the variance may also change over time.", num(bp$C$p.value, 4)))),
    0.6, 4.4, 12.1, 2.4, size = 17)

  # 11 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Corregir", "Fix"),
    tr(c("Errores estándar robustos (HC)", "Caso B: estándar frente a robusto", "Caso C: también en series"),
       c("Robust (HC) standard errors", "Case B: standard versus robust", "Case C: in time series too")))

  # 12 · 9.4 MCO robusta
  x <- diapo_contenido(x, tr("9.4 · Estimación MCO robusta", "9.4 · Robust OLS estimation"),
    tr("Mismos coeficientes, otra matriz de varianzas", "Same coefficients, another variance matrix"), tema)
  x <- formula_caja(x, "\\widehat{\\text{Var}}(\\hat{\\boldsymbol\\beta}) = (\\mathbf{X}'\\mathbf{X})^{-1} \\left( \\sum_{i=1}^{n} \\hat{u}_i^2 \\, \\mathbf{x}_i \\mathbf{x}_i' \\right) (\\mathbf{X}'\\mathbf{X})^{-1}",
                    0.6, 2.0, 12.1, 1.2, size = 24)
  x <- vinetas(x, tr(
    c("White (HC0): cada observación aporta su propio residuo al cuadrado.",
      paste0("HC1 (Gretl y el manual): HC0 multiplicado por ", M("\\frac{n}{n-k-1}"), " para muestras pequeñas."),
      "No hace falta conocer la forma de la heterocedasticidad.",
      "Los coeficientes no cambian: solo cambia la inferencia."),
    c("White (HC0): each observation contributes its own squared residual.",
      paste0("HC1 (Gretl and the book): HC0 multiplied by ", M("\\frac{n}{n-k-1}"), " for small samples."),
      "There is no need to know the form of the heteroskedasticity.",
      "The coefficients do not change: only the inference does.")),
    0.6, 3.5, 7.6, 3.3, size = 17)
  x <- caja_texto(x, 8.6, 3.5, 4.1, 2.6, tr("Matriz sándwich", "Sandwich matrix"),
    tr(paste0("Dos «panes», ", M("(\\mathbf{X}'\\mathbf{X})^{-1}"), ", y en medio la «carne»: la varianza observada de cada error. Con homocedasticidad, se parece a la fórmula de siempre."),
       paste0("Two «slices of bread», ", M("(\\mathbf{X}'\\mathbf{X})^{-1}"), ", and in the middle the «filling»: the observed variance of each error. Under homoskedasticity it resembles the usual formula.")),
    oscura = TRUE, size = 15)

  # 13 · 9.5.2 Caso B
  x <- diapo_contenido(x, tr("9.5.2 · Caso B: heterocedasticidad natural", "9.5.2 · Case B: natural heteroskedasticity"),
    tr("Errores estándar convencionales y robustos", "Conventional and robust standard errors"), tema)
  tab_b <- data.frame(
    v = tr(c("Constante", "Trabajadores", "Inversión"), c("Constant", "Workers", "Investment")),
    b = num(std_b[, 1], 4), se = num(std_b[, 2], 4), p = vapply(std_b[, 4], pval, ""),
    ser = num(rob_b[, 2], 4), pr = vapply(rob_b[, 4], pval, ""), stringsAsFactors = FALSE)
  x <- poner_tabla(x, tabla_pptx(tab_b, tr(c("Variable", "Coef.", "ES conv.", "p conv.", "ES robusto", "p robusto"),
                                           c("Variable", "Coef.", "Conv. SE", "Conv. p", "Robust SE", "Robust p")), size = 16), 0.6, 2.1, 8.0)
  x <- vinetas(x, tr(
    c("Los coeficientes son idénticos en las dos columnas.",
      sprintf("Trabajadores: el error estándar sube de %s a %s y el p-valor, de %s a %s.",
              num(std_b[2, 2], 4), num(rob_b[2, 2], 4), num(std_b[2, 4], 3), num(rob_b[2, 4], 3))),
    c("The coefficients are identical in both columns.",
      sprintf("Workers: the standard error rises from %s to %s and the p-value from %s to %s.",
              num(std_b[2, 2], 4), num(rob_b[2, 2], 4), num(std_b[2, 4], 3), num(rob_b[2, 4], 3)))),
    0.6, 3.9, 8.0, 2.5, size = 17)
  x <- caja_dato(x, 9.0, 4.95, 3.7, 1.8, tr("p-valor de trabajadores", "p-value of workers"),
    M(paste0(numtex(std_b[2, 4], 3), " \\rightarrow ", numtex(rob_b[2, 4], 3))), NULL, ID$oro, size_valor = 24)
  x <- caja_texto(x, 9.0, 2.1, 3.7, 2.65, tr("Conclusión", "Conclusion"),
    tr(if (rob_b[2, 4] < 0.05) "Los trabajadores siguen siendo significativos al 5 %, ahora con un error estándar fiable. La inversión no lo es con ninguno de los dos."
       else "Con errores robustos, los trabajadores dejan de ser significativos al 5 %.",
       if (rob_b[2, 4] < 0.05) "Workers are still significant at 5 %, now with a reliable standard error. Investment is not significant with either."
       else "With robust errors, workers are no longer significant at 5 %."),
    oscura = TRUE, size = 15)

  # 14 · 9.5.3 Caso C
  x <- diapo_contenido(x, tr("9.5.3 · Caso C: comprobar heterocedasticidad", "9.5.3 · Case C: checking for heteroskedasticity"),
    tr("En series temporales la varianza también cambia", "In time series the variance changes too"), tema)
  anio <- substr(caso_c$trimestre, 1, 4)
  cortes <- which(substr(caso_c$trimestre, 6, 7) == "Q1"); cortes <- cortes[seq(1, length(cortes), by = 2)]
  df_c <- data.frame(t = seq_len(nrow(caso_c)), r2 = residuals(mc)^2 / 1e6)
  g_c <- ggplot(df_c, aes(t, r2)) +
    geom_col(fill = COL$navy, width = 0.75) +
    scale_x_continuous(breaks = cortes, labels = anio[cortes]) +
    scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = NULL, y = tr("Residuo al cuadrado (millones)", "Squared residual (millions)")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_c, 0.6, 1.95, 7.4, 4.85)
  tab_c <- data.frame(v = tr(c("PIB", "Tipo de interés"), c("GDP", "Interest rate")),
                      se = c(num(std_c[2, 2], 4), num(std_c[3, 2], 2)), ser = c(num(rob_c[2, 2], 4), num(rob_c[3, 2], 2)),
                      stringsAsFactors = FALSE)
  x <- caja_dato(x, 8.4, 2.05, 4.3, 1.5, tr("Breusch-Pagan", "Breusch-Pagan"),
    M(paste0("BP = ", numtex(bp$C$statistic, 2), "\\quad p = ", numtex(bp$C$p.value, 4))), NULL, ID$oro, size_valor = 22)
  x <- poner_tabla(x, tabla_pptx(tab_c, tr(c("Variable", "ES conv.", "ES robusto"), c("Variable", "Conv. SE", "Robust SE")), size = 14), 8.4, 3.85, 4.3)
  x <- poner_texto(x, parrafo(tr("La PMC sigue siendo muy significativa con errores robustos; el tipo de interés no lo es con ninguno.",
                                 "The MPC remains highly significant with robust errors; the interest rate is not significant with either."), 15, ID$navy, bold = TRUE),
                   8.4, 5.4, 4.3, 1.3)

  # 15 · Gretl → R
  x <- diapo_contenido(x, tr("Gretl → R", "Gretl → R"), tr("La heterocedasticidad en Gretl y en R", "Heteroskedasticity in Gretl and in R"), tema)
  sw <- data.frame(
    op = tr(c("Gráfico de residuos", "Contraste de Breusch-Pagan", "Contraste de White", "MCO con errores robustos"),
            c("Residual plot", "Breusch-Pagan test", "White test", "OLS with robust errors")),
    gretl = tr(c("Modelo: Gráficos → Gráfico de residuos", "Modelo: Contrastes → Heterocedasticidad → Breusch-Pagan",
                 "Modelo: Contrastes → Heterocedasticidad → White", "Modelo → MCO → Desviaciones típicas robustas"),
               c("Model: Graphs → Residual plot", "Model: Tests → Heteroskedasticity → Breusch-Pagan",
                 "Model: Tests → Heteroskedasticity → White", "Model → OLS → Robust standard errors")),
    r = c("plot(fitted(m), resid(m)^2)", "bptest(m, studentize = FALSE)", "bptest(m, ~ x1 * x2 + I(x1^2) + I(x2^2))",
          "coeftest(m, vcov = vcovHC(m, type = \"HC1\"))"))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 13, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c("En R, studentize = FALSE da el mismo valor que Gretl. Sin esa opción, bptest() calcula la variante de Koenker, que da otro número.",
      "Excel de clase: regresión auxiliar y estadístico BP paso a paso."),
    c("In R, studentize = FALSE gives the same value as Gretl. Without it, bptest() computes Koenker's variant, which gives a different number.",
      "Class Excel file: auxiliary regression and the BP statistic step by step.")),
    0.6, 4.8, 12.1, 1.7, size = 16)

  # 16 · Vuelta a los casos
  x <- diapo_contenido(x, tr("9.5 · Vuelta a los casos", "9.5 · Back to the cases"),
    tr("Un diagnóstico para cada caso", "A diagnosis for each case"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 3.0, tr("A · Demanda", "A · Demand"), tr("Homocedástico", "Homoskedastic"),
    tr(sprintf("BP: p-valor = %s. Los errores estándar convencionales valen; los robustos darían casi lo mismo.", num(bp$A$p.value, 3)),
       sprintf("BP: p-value = %s. The conventional standard errors are valid; robust ones would give almost the same.", num(bp$A$p.value, 3))),
    ID$navy, size_valor = 24)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 3.0, tr("B · Cifra de negocio", "B · Turnover"), tr("Heterocedástico", "Heteroskedastic"),
    tr(sprintf("BP: p-valor = %s. Embudo por tamaño: errores HC1. Los trabajadores siguen siendo significativos.", num(bp$B$p.value, 3)),
       sprintf("BP: p-value = %s. A size funnel: HC1 errors. Workers are still significant.", num(bp$B$p.value, 3))),
    ID$oro, size_valor = 24)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 3.0, tr("C · Consumo", "C · Consumption"), tr("Heterocedástico", "Heteroskedastic"),
    tr(sprintf("BP: p-valor = %s. Errores robustos; en el tema 10 veremos que además hay autocorrelación.", num(bp$C$p.value, 4)),
       sprintf("BP: p-value = %s. Robust errors; chapter 10 will show there is autocorrelation as well.", num(bp$C$p.value, 4))),
    ID$gris_med, size_valor = 24)
  x <- caja_texto(x, 0.6, 5.3, 12.1, 1.45, tr("Regla práctica", "Rule of thumb"),
    tr("En corte transversal, estimar con errores robustos es una práctica prudente: si no hay heterocedasticidad, apenas cambian.",
       "In cross-sections, estimating with robust errors is a prudent practice: if there is no heteroskedasticity, they barely change."),
    oscura = TRUE, size = 16)

  # 17 · Cierre
  x <- diapo_cierre(x,
    tr(c(paste0("Homocedasticidad: ", M("\\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2"), " para todo ", M("i"), "."),
         "Con heterocedasticidad, MCO sigue insesgado pero deja de ser eficiente.",
         "Los errores estándar convencionales fallan: t, F e intervalos no son fiables.",
         paste0("Se detecta con el gráfico de ", M("\\hat{u}_i^2"), " y con el contraste de Breusch-Pagan."),
         "Se corrige con errores robustos HC1: mismos coeficientes, otra inferencia.",
         "A es homocedástico; B y C, heterocedásticos."),
       c(paste0("Homoskedasticity: ", M("\\text{Var}(u_i \\mid \\mathbf{X}) = \\sigma^2"), " for every ", M("i"), "."),
         "With heteroskedasticity, OLS stays unbiased but is no longer efficient.",
         "Conventional standard errors fail: t, F and intervals are not reliable.",
         paste0("It is detected with the plot of ", M("\\hat{u}_i^2"), " and the Breusch-Pagan test."),
         "It is fixed with robust HC1 errors: same coefficients, different inference.",
         "A is homoskedastic; B and C are heteroskedastic.")),
    tr("Excel de clase del tema 9, scripts de R del tema 9 y app «Heterocedasticidad». Explicación completa en el capítulo 9 del manual.",
       "Chapter 9 class Excel file, Chapter 9 R scripts and the «Heteroskedasticity» app. Full explanation in Chapter 9 of the book."),
    tema)

  guardar_pptx(x, "T09_mEC_Heterocedasticidad.pptx", "T09_mEC_Heteroskedasticity.pptx")
}
