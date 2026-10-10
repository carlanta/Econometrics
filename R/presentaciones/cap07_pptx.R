# ══════════════════════════════════════════════════════════════════
# Capítulo 07 · Predicción y análisis de residuos · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap07_pptx.R
# Contenido y numeración: 07-prediccion-residuos.qmd (secciones 7.1 a 7.4). Modelos como en el capítulo:
#   Caso A: ventas ~ precio_propio + gasto_publicidad (precio 2,50 / 3,00 / 3,50 €, publicidad 1,20 MM€)
#   Caso B: cifra_negocio ~ trabajadores + inversion      Caso C: consumo ~ pib + tipo_interes (tipo al 2 %)
# El manual no trata leverage ni distancia de Cook: no se incluyen. Titulares de UNA línea (≈ 45 caracteres).
# ══════════════════════════════════════════════════════════════════
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  # Espacio duro entre número y unidad, signo menos tipográfico y guion que no parte en «p-valor»
  # (envuelve runs_texto de los comunes sin modificarlos)
  runs_texto_base <- runs_texto
  runs_texto <- function(texto, ...) {
    texto <- gsub("([0-9]) (%|€|MM€|M€|EUR)", "\\1\u00a0\\2", texto)
    texto <- gsub("(^|[ (\\[;:])-([0-9])", "\\1\u2212\\2", texto, perl = TRUE)
    texto <- gsub("p-val", "p\u2011val", texto, fixed = TRUE)
    runs_texto_base(texto, ...)
  }
  tema <- tr("Tema 7", "Chapter 7")

  # ── Modelos y cifras del capítulo ──────────────────────────────────
  m_a <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
  m_b <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
  m_c <- lm(consumo ~ pib + tipo_interes, data = caso_c)
  b <- coef(m_a); s_a <- summary(m_a)$sigma; gl <- df.residual(m_a); tc <- qt(0.975, gl)
  perf_a <- data.frame(precio_propio = c(2.50, 3.00, 3.50), gasto_publicidad = 1.20)
  pc_a <- predict(m_a, perf_a, interval = "confidence"); pp_a <- predict(m_a, perf_a, interval = "prediction")
  # Paso a paso en el perfil central (precio 3 €, publicidad 1,2 MM€)
  X <- model.matrix(m_a); x0 <- c(1, 3.00, 1.20)
  h0 <- drop(t(x0) %*% solve(crossprod(X)) %*% x0)
  y0 <- sum(x0 * b); es_m <- s_a * sqrt(h0); es_i <- s_a * sqrt(1 + h0)
  # Caso C
  perf_c <- data.frame(pib = c(280000, 300000, 320000), tipo_interes = 2)
  pc_c <- predict(m_c, perf_c, interval = "confidence"); pp_c <- predict(m_c, perf_c, interval = "prediction")
  # Caso B
  perf_b <- data.frame(trabajadores = c(20, 50, 100, 150), inversion = c(0.5, 1, 2, 3))
  pp_b <- predict(m_b, perf_b, interval = "prediction")
  p200 <- predict(m_b, data.frame(trabajadores = 200, inversion = 5), interval = "prediction")

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 7, tr("Predicción y análisis de residuos", "Prediction and residual analysis"),
    tr("Si el precio sube a 3,50 €, ¿cuánto venderemos… y con qué margen de error?",
       "If the price rises to €3.50, how much will we sell… and with what margin of error?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("7.1", "7.2", "7.3", "7.4"),
    titulo = tr(c("Predicción puntual", "Intervalos de predicción: para la media y para un individuo", "Análisis de residuos", "Aplicación a los casos prácticos"),
                c("Point prediction", "Prediction intervals: for the mean and for an individual", "Residual analysis", "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Predecir", "Sustituir un escenario de las explicativas en el modelo estimado: la predicción puntual."),
       c("Predict", "Plug a scenario of the explanatory variables into the estimated model: the point prediction.")),
    tr(c("Medir la incertidumbre", "Intervalo para la media y para un individuo: por qué el segundo es siempre más ancho."),
       c("Measure the uncertainty", "Interval for the mean and for an individual: why the second is always wider.")),
    tr(c("Revisar los residuos", "Residuos frente a ajustados y gráfico Q-Q: lo que delata un modelo mal especificado."),
       c("Check the residuals", "Residuals against fitted values and the Q-Q plot: what gives a misspecified model away."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Predecir con el modelo", "Predicting with the model"),
    tr(c("Un escenario, una predicción", "La media o un individuo", "El caso A a 3 €"),
       c("One scenario, one prediction", "The mean or an individual", "Case A at €3")))

  # 5 · 7.1 Concepto
  x <- diapo_contenido(x, tr("7.1 · Predicción puntual", "7.1 · Point prediction"),
    tr("Sustituir el escenario en el modelo estimado", "Plugging the scenario into the estimated model"), tema)
  x <- formula_caja(x, "\\hat{Y}_0 = \\mathbf{x}_0'\\hat{\\boldsymbol{\\beta}} = \\hat{\\beta}_0 + \\hat{\\beta}_1 x_{01} + \\cdots + \\hat{\\beta}_k x_{0k}", 0.6, 2.05, 12.1, 1.0, size = 28)
  x <- poner_texto(x, parrafo(tr(paste0(M("\\mathbf{x}_0 = (1,\\, x_{01},\\, \\ldots,\\, x_{0k})'"), ": los valores de las explicativas en el escenario que queremos predecir."),
                                 paste0(M("\\mathbf{x}_0 = (1,\\, x_{01},\\, \\ldots,\\, x_{0k})'"), ": the values of the explanatory variables in the scenario to predict.")),
                              17, ID$gris), 0.6, 3.2, 12.1, 0.6)
  x <- caja_texto(x, 0.6, 4.0, 5.9, 1.85, tr("La media condicionada", "The conditional mean"),
    tr(paste0(M("E(Y \\mid \\mathbf{X} = \\mathbf{x}_0)"), ": ¿cuánto venden, en promedio, todos los meses con ese precio y esa publicidad?"),
       paste0(M("E(Y \\mid \\mathbf{X} = \\mathbf{x}_0)"), ": how much is sold, on average, in all the months with that price and that advertising?")), size = 17)
  x <- caja_texto(x, 6.8, 4.0, 5.9, 1.85, tr("Un valor individual", "An individual value"),
    tr(paste0(M("Y_0"), ": ¿cuánto venderemos el próximo mes concreto con ese precio y esa publicidad?"),
       paste0(M("Y_0"), ": how much will we sell next month, specifically, with that price and that advertising?")), oscura = TRUE, size = 17)
  x <- poner_texto(x, parrafo(tr("La misma predicción puntual para las dos preguntas; distinta incertidumbre.",
                                 "The same point prediction for both questions; different uncertainty."), 18, ID$navy, bold = TRUE), 0.6, 6.1, 12.1, 0.5)

  # 6 · 7.1 Caso A
  x <- diapo_contenido(x, tr("7.1 · Predicción puntual en el caso A", "7.1 · Point prediction in case A"),
    tr("Precio de 3 € y publicidad de 1,2 MM€", "A price of €3 and advertising of €1.2m"), tema)
  x <- formula_caja(x, paste0("\\hat{Y}_0 = ", numtex(b[1], 4), " ", numtex(b[2], 4), " \\times 3 + ", numtex(b[3], 4), " \\times 1", tr("{,}", "."), "2 = ", numtex(y0, 2)),
                    0.6, 2.05, 12.1, 1.05, size = 28)
  x <- caja_dato(x, 0.6, 3.45, 3.85, 2.0, tr("Predicción", "Prediction"), paste0(num(y0, 2), tr(" MM€", " €m")),
                 tr("Ventas esperadas en ese escenario.", "Expected sales in that scenario."), ID$navy)
  x <- caja_dato(x, 4.75, 3.45, 3.85, 2.0, tr("Precio a 2,50 €", "Price at €2.50"), paste0(num(pc_a[1, 1], 2), tr(" MM€", " €m")),
                 tr("Medio euro menos de precio.", "Half a euro cheaper."), ID$oro)
  x <- caja_dato(x, 8.9, 3.45, 3.85, 2.0, tr("Precio a 3,50 €", "Price at €3.50"), paste0(num(pc_a[3, 1], 2), tr(" MM€", " €m")),
                 tr("Medio euro más de precio.", "Half a euro dearer."), ID$gris_med)
  x <- poner_texto(x, parrafo(tr(paste0("Cada 0,50 € de precio cambia las ventas en ", num(abs(b[2]) / 2, 2), " MM€: es ", M("0{,}5 \\cdot \\hat{\\beta}_1"), ". Pero ¿con qué margen?"),
                                 paste0("Each €0.50 of price changes sales by €", num(abs(b[2]) / 2, 2), "m: that is ", M("0.5 \\cdot \\hat{\\beta}_1"), ". But with what margin?")),
                              18, ID$navy, bold = TRUE), 0.6, 5.8, 12.1, 0.6)

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Intervalos de predicción", "Prediction intervals"),
    tr(c("Para la media: dónde está la recta", "Para un individuo: dónde cae un punto", "Por qué el individual es más ancho"),
       c("For the mean: where the line is", "For an individual: where a point falls", "Why the individual one is wider")))

  # 8 · 7.2 Fórmulas
  x <- diapo_contenido(x, tr("7.2 · Intervalos de predicción", "7.2 · Prediction intervals"),
    tr("Un 1 bajo la raíz marca la diferencia", "A 1 under the square root makes the difference"), tema)
  x <- poner_texto(x, parrafo(tr("Intervalo de confianza para la media", "Confidence interval for the mean"), 15, ID$gris, bold = TRUE), 0.6, 1.95, 7.6, 0.35)
  x <- formula_caja(x, "\\hat{Y}_0 \\pm t_{n-k-1,\\,\\alpha/2} \\cdot \\hat{\\sigma}\\sqrt{\\mathbf{x}_0'(\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{x}_0}", 0.6, 2.3, 7.6, 1.15, size = 26)
  x <- poner_texto(x, parrafo(tr("Intervalo de predicción para un individuo", "Prediction interval for an individual"), 15, ID$gris, bold = TRUE), 0.6, 3.65, 7.6, 0.35)
  x <- formula_caja(x, "\\hat{Y}_0 \\pm t_{n-k-1,\\,\\alpha/2} \\cdot \\hat{\\sigma}\\sqrt{1 + \\mathbf{x}_0'(\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{x}_0}", 0.6, 4.0, 7.6, 1.15, size = 26)
  x <- vinetas(x, tr(
    c("Media: solo la incertidumbre sobre la posición de la recta.",
      paste0("Individuo: además, el error propio de esa observación, ", M("u_0"), ".")),
    c("Mean: only the uncertainty about where the line is.",
      paste0("Individual: also the observation's own error, ", M("u_0"), "."))),
    0.6, 5.4, 7.6, 1.4, size = 16)
  x <- caja_texto(x, 8.6, 2.3, 4.1, 3.9, tr("Siempre más ancho", "Always wider"),
    tr(paste0("Aunque conociéramos la recta sin error, cada observación seguiría fluctuando a su alrededor. Por eso el intervalo para un individuo es siempre más ancho que el de la media. Y los dos se ensanchan cuando ", M("\\mathbf{x}_0"), " se aleja del centro de los datos."),
       paste0("Even if we knew the line without error, each observation would still fluctuate around it. That is why the interval for an individual is always wider than the one for the mean. And both widen as ", M("\\mathbf{x}_0"), " moves away from the centre of the data.")),
    oscura = TRUE, size = 16)

  # 9 · 7.2 Paso a paso
  x <- diapo_contenido(x, tr("7.2 · Cálculo paso a paso", "7.2 · Step-by-step calculation"),
    tr("Los dos intervalos del caso A a 3 €", "The two intervals for case A at €3"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 2.0, tr("Ingredientes", "Ingredients"),
                 M(paste0("\\hat{\\sigma} = ", numtex(s_a, 4))),
                 paste0(M(paste0("\\mathbf{x}_0'(\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{x}_0 = ", numtex(h0, 4))), tr(";  ", ";  "), M(paste0("t_{", gl, "} = ", numtex(tc, 3)))),
                 ID$navy, size_valor = 26)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 2.0, tr("Error estándar: media", "Standard error: mean"),
                 M(paste0(numtex(s_a, 4), "\\sqrt{", numtex(h0, 4), "} = ", numtex(es_m, 4))), NULL, ID$oro, size_valor = 17)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 2.0, tr("Error estándar: individuo", "Standard error: individual"),
                 M(paste0(numtex(s_a, 4), "\\sqrt{1 + ", numtex(h0, 4), "} = ", numtex(es_i, 4))), NULL, ID$gris_med, size_valor = 17)
  x <- formula_caja(x, paste0("\\text{", tr("Media", "Mean"), "}: \\ ", numtex(y0, 2), " \\pm ", numtex(tc, 3), " \\times ", numtex(es_m, 4), " = [", numtex(pc_a[2, 2], 2), ",\\ ", numtex(pc_a[2, 3], 2), "]"),
                    0.6, 4.35, 12.1, 0.95, size = 26)
  x <- formula_caja(x, paste0("\\text{", tr("Individuo", "Individual"), "}: \\ ", numtex(y0, 2), " \\pm ", numtex(tc, 3), " \\times ", numtex(es_i, 4), " = [", numtex(pp_a[2, 2], 2), ",\\ ", numtex(pp_a[2, 3], 2), "]"),
                    0.6, 5.45, 12.1, 0.95, size = 26)
  x <- poner_texto(x, parrafo(tr(sprintf("El intervalo individual es %s veces más ancho que el de la media.", num((pp_a[2, 3] - pp_a[2, 2]) / (pc_a[2, 3] - pc_a[2, 2]), 1)),
                                 sprintf("The individual interval is %s times wider than the one for the mean.", num((pp_a[2, 3] - pp_a[2, 2]) / (pc_a[2, 3] - pc_a[2, 2]), 1))),
                              15, ID$gris, italic = TRUE, align = "center"), 0.6, 6.45, 12.1, 0.4)

  # 10 · 7.2 Bandas
  x <- diapo_contenido(x, tr("7.2 · Las dos bandas en el caso A", "7.2 · The two bands in case A"),
    tr("La banda individual siempre envuelve a la media", "The individual band always wraps the mean one"), tema)
  rej <- data.frame(precio_propio = seq(min(caso_a$precio_propio), max(caso_a$precio_propio), length.out = 80), gasto_publicidad = 1.20)
  bc <- predict(m_a, rej, interval = "confidence"); bp <- predict(m_a, rej, interval = "prediction")
  bandas <- data.frame(p = rej$precio_propio, fit = bc[, 1], cl = bc[, 2], cu = bc[, 3], pl = bp[, 2], pu = bp[, 3])
  niv_b <- tr(c("Predicción para un individuo", "Confianza para la media"), c("Prediction for an individual", "Confidence for the mean"))
  g_b <- ggplot(bandas, aes(p)) +
    geom_ribbon(aes(ymin = pl, ymax = pu, fill = niv_b[1]), alpha = 0.55) +
    geom_ribbon(aes(ymin = cl, ymax = cu, fill = niv_b[2]), alpha = 0.9) +
    geom_point(data = caso_a, aes(precio_propio, ventas), colour = COL$navy, size = 2, alpha = 0.55) +
    geom_line(aes(y = fit), colour = COL$navy, linewidth = 1.2) +
    scale_fill_manual(values = setNames(c(COL$oro_cl, COL$oro), niv_b), breaks = niv_b, name = NULL) +
    scale_x_continuous(labels = function(v) num(v, 2)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Precio propio (€), publicidad fija en 1,2 MM€", "Own price (€), advertising fixed at €1.2m"), y = tr("Ventas (MM€)", "Sales (€m)")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_b, 0.6, 1.95, 7.7, 4.85)
  x <- vinetas(x, tr(
    c("Línea: predicción puntual.", "Banda oscura: intervalo al 95 % para la media.", "Banda clara: intervalo al 95 % para un mes concreto.",
      "Las dos se abren en los extremos: lejos del centro de los datos se predice peor."),
    c("Line: point prediction.", "Dark band: 95 % interval for the mean.", "Light band: 95 % interval for a specific month.",
      "Both open up at the ends: far from the centre of the data, prediction is worse.")),
    8.6, 2.2, 4.2, 4.6, size = 16)

  # 11 · 7.2 Tabla caso A
  x <- diapo_contenido(x, tr("7.2 · Intervalos en el caso A", "7.2 · Intervals in case A"),
    tr("Tres precios, dos intervalos para cada uno", "Three prices, two intervals for each"), tema)
  tab_a <- data.frame(p = perf_a$precio_propio, fit = pc_a[, 1], cl = pc_a[, 2], cu = pc_a[, 3], pl = pp_a[, 2], pu = pp_a[, 3])
  ft <- tabla_pptx(tab_a, tr(c("Precio (€)", "Predicción", "Media: inf.", "Media: sup.", "Individuo: inf.", "Individuo: sup."),
                             c("Price (€)", "Prediction", "Mean: lower", "Mean: upper", "Individual: lower", "Individual: upper")), digits = 2, size = 17, texto = 1L)
  x <- poner_tabla(x, ft, 0.6, 2.1)
  x <- caja_dato(x, 0.6, 4.2, 3.85, 2.45, tr("Media a 3 €", "Mean at €3"), paste0(num(pc_a[2, 3] - pc_a[2, 2], 2), tr(" MM€", " €m")),
                 tr("Anchura del intervalo para la media.", "Width of the interval for the mean."), ID$oro, size_valor = 28)
  x <- caja_dato(x, 4.75, 4.2, 3.85, 2.45, tr("Individuo a 3 €", "Individual at €3"), paste0(num(pp_a[2, 3] - pp_a[2, 2], 2), tr(" MM€", " €m")),
                 tr("Un mes concreto puede quedar lejos de la media.", "A specific month may land far from the mean."), ID$navy, size_valor = 28)
  x <- caja_dato(x, 8.9, 4.2, 3.85, 2.45, tr("Media a 3,50 €", "Mean at €3.50"), paste0(num(pc_a[3, 3] - pc_a[3, 2], 2), tr(" MM€", " €m")),
                 tr("Más ancho que a 3 €: el precio de 3,50 € está cerca del borde de los datos.", "Wider than at €3: the €3.50 price is near the edge of the data."), ID$gris_med, size_valor = 28)

  # 12 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Analizar los residuos", "Analysing the residuals"),
    tr(c("Residuos frente a ajustados", "El gráfico Q-Q", "De Gretl a R", "Los tres casos del manual"),
       c("Residuals against fitted values", "The Q-Q plot", "From Gretl to R", "The three cases of the book")))

  # 13 · 7.3 Patrones
  x <- diapo_contenido(x, tr("7.3 · Gráfico de residuos frente a ajustados", "7.3 · Residuals against fitted values"),
    tr("Lo que debe verse y lo que no", "What should be seen and what should not"), tema)
  set.seed(2024)
  xs <- runif(120, 1, 10)
  pats <- tr(c("Aleatorio: bien", "Embudo: heterocedasticidad", "Curva: forma funcional"),
             c("Random: fine", "Funnel: heteroskedasticity", "Curve: functional form"))
  sim_r <- function(y, et) { m <- lm(y ~ xs); data.frame(fit = fitted(m), res = residuals(m), tipo = et) }
  df_p <- rbind(sim_r(2 + xs + rnorm(120, 0, 1), pats[1]),
                sim_r(2 + xs + rnorm(120, 0, 0.25 * xs), pats[2]),
                sim_r(0.3 * (xs - 5.5)^2 + xs + rnorm(120, 0, 0.8), pats[3]))
  df_p$tipo <- factor(df_p$tipo, levels = pats)
  g_p <- ggplot(df_p, aes(fit, res)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = COL$gris) +
    geom_point(aes(colour = tipo), size = 1.8, alpha = 0.8, show.legend = FALSE) +
    scale_colour_manual(values = setNames(c(COL$navy, COL$oro, COL$navy2), pats)) +
    facet_wrap(~ tipo, scales = "free") +
    labs(x = tr("Valores ajustados", "Fitted values"), y = tr("Residuos", "Residuals")) +
    tema_cdea(13) + theme(axis.text = element_blank())
  x <- imagen_ggplot(x, g_p, 0.6, 1.95, 12.1, 3.55)
  x <- vinetas(x, tr(
    c(paste0("Si se cumplen los supuestos, los residuos ", M("\\hat{u}_i = Y_i - \\hat{Y}_i"), " se dispersan al azar en torno a cero."),
      "Embudo: la varianza crece con el ajustado, heterocedasticidad (tema 9). Curva: falta una variable o la forma funcional no es lineal (tema 6)."),
    c(paste0("If the assumptions hold, the residuals ", M("\\hat{u}_i = Y_i - \\hat{Y}_i"), " scatter randomly around zero."),
      "Funnel: the variance grows with the fitted value, heteroskedasticity (chapter 9). Curve: a variable is missing or the functional form is not linear (chapter 6).")),
    0.6, 5.6, 12.1, 1.3, size = 16)

  # 14 · 7.3 Caso A
  x <- diapo_contenido(x, tr("7.3 · Diagnóstico de residuos del caso A", "7.3 · Residual diagnostics for case A"),
    tr("Sin patrones y cerca de la normal", "No patterns and close to normal"), tema)
  df_r <- data.frame(fit = fitted(m_a), res = residuals(m_a))
  g_r1 <- ggplot(df_r, aes(fit, res)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = COL$gris) +
    geom_point(colour = COL$navy, size = 2.6, alpha = 0.85) +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 1)) +
    labs(title = tr("Residuos frente a ajustados", "Residuals against fitted values"), x = tr("Valores ajustados (MM€)", "Fitted values (€m)"), y = tr("Residuos", "Residuals")) +
    tema_cdea(13) + theme(plot.title = element_text(face = "bold", colour = COL$navy, size = 14))
  g_r2 <- ggplot(df_r, aes(sample = res)) +
    stat_qq_line(colour = COL$oro, linewidth = 1.1) +
    stat_qq(colour = COL$navy, size = 2.6, alpha = 0.85) +
    scale_x_continuous(labels = function(v) num(v, 1)) + scale_y_continuous(labels = function(v) num(v, 1)) +
    labs(title = tr("Gráfico Q-Q normal", "Normal Q-Q plot"), x = tr("Cuantiles teóricos", "Theoretical quantiles"), y = tr("Cuantiles de los residuos", "Residual quantiles")) +
    tema_cdea(13) + theme(plot.title = element_text(face = "bold", colour = COL$navy, size = 14))
  x <- imagen_ggplot(x, g_r1, 0.6, 1.95, 4.6, 4.0)
  x <- imagen_ggplot(x, g_r2, 5.35, 1.95, 4.6, 4.0)
  x <- vinetas(x, tr(
    c("Izquierda: nube sin embudo ni curva; homocedasticidad y linealidad razonables.",
      "Derecha: los puntos siguen la diagonal; residuos aproximadamente normales."),
    c("Left: a cloud with no funnel or curve; homoskedasticity and linearity look reasonable.",
      "Right: the points follow the diagonal; residuals are approximately normal.")),
    10.2, 2.1, 2.7, 4.6, size = 15)
  x <- poner_texto(x, parrafo(tr(sprintf("Modelo del caso A con precio y publicidad · %d observaciones.", nrow(caso_a)),
                                 sprintf("Case A model with price and advertising · %d observations.", nrow(caso_a))),
                              14, ID$gris, italic = TRUE), 0.6, 6.1, 9.4, 0.5)

  # 15 · Gretl → R
  x <- diapo_contenido(x, tr("De Gretl a R", "From Gretl to R"),
    tr("Predicciones y residuos con Gretl y con R", "Predictions and residuals with Gretl and R"), tema)
  sw <- data.frame(
    op = tr(c("Añadir el escenario a predecir", "Predicción e intervalos", "Guardar residuos y ajustados", "Residuos frente a ajustados", "Gráfico Q-Q"),
            c("Add the scenario to predict", "Prediction and intervals", "Save residuals and fitted values", "Residuals against fitted values", "Q-Q plot")),
    gretl = tr(c("Datos → Añadir observaciones", "Análisis → Predicciones", "Guardar → Residuos / Valores estimados", "Gráficos → Gráfico de residuos", "Variable → Gráfico Q-Q normal"),
               c("Data → Add observations", "Analysis → Forecasts", "Save → Residuals / Fitted values", "Graphs → Residual plot", "Variable → Normal Q-Q plot")),
    r = tr(c("nuevo <- data.frame(...)", "predict(modelo, nuevo, interval = \"prediction\")", "residuals(modelo); fitted(modelo)", "plot(modelo, which = 1)", "qqnorm(residuals(modelo))"),
           c("new <- data.frame(...)", "predict(model, new, interval = \"prediction\")", "residuals(model); fitted(model)", "plot(model, which = 1)", "qqnorm(residuals(model))")))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 15, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c("En R, interval = \"confidence\" da el intervalo para la media e interval = \"prediction\" el de un individuo.",
      paste0("El Excel de clase calcula los dos intervalos a mano con la matriz ", M("(\\mathbf{X}'\\mathbf{X})^{-1}"), ".")),
    c("In R, interval = \"confidence\" gives the interval for the mean and interval = \"prediction\" the one for an individual.",
      paste0("The class Excel computes both intervals by hand with the ", M("(\\mathbf{X}'\\mathbf{X})^{-1}"), " matrix."))),
    0.6, 4.65, 12.1, 1.9, size = 17)

  # 16 · 7.4 Casos B y C
  x <- diapo_contenido(x, tr("7.4 · Aplicación a los casos B y C", "7.4 · Application to cases B and C"),
    tr("Consumo con precisión; empresas, con cautela", "Consumption with precision; firms, with caution"), tema)
  tab_c <- data.frame(pib = as.integer(perf_c$pib / 1000), fit = pc_c[, 1] / 1000, cl = pc_c[, 2] / 1000, cu = pc_c[, 3] / 1000, pl = pp_c[, 2] / 1000, pu = pp_c[, 3] / 1000)
  x <- poner_texto(x, parrafo(tr("C · Consumo trimestral (miles de MM€, tipo de interés del 2 %)", "C · Quarterly consumption (€bn, interest rate at 2 %)"), 16, ID$navy, bold = TRUE), 0.6, 1.95, 7.6, 0.4)
  ft_c <- tabla_pptx(tab_c, tr(c("PIB", "Predicción", "Media: inf.", "Media: sup.", "Indiv.: inf.", "Indiv.: sup."),
                               c("GDP", "Prediction", "Mean: lower", "Mean: upper", "Indiv.: lower", "Indiv.: upper")), digits = 2, size = 14, texto = 1L)
  x <- poner_tabla(x, ft_c, 0.6, 2.4, max_ancho = 7.5)
  x <- poner_texto(x, parrafo(tr(sprintf("Con un PIB de 300 miles de MM€, el intervalo individual es %s veces más ancho que el de la media: el error propio de cada trimestre pesa mucho más que la incertidumbre de la recta.",
                                         num((pp_c[2, 3] - pp_c[2, 2]) / (pc_c[2, 3] - pc_c[2, 2]), 1)),
                                 sprintf("With GDP of €300bn, the individual interval is %s times wider than the one for the mean: each quarter's own error weighs far more than the uncertainty of the line.",
                                         num((pp_c[2, 3] - pp_c[2, 2]) / (pc_c[2, 3] - pc_c[2, 2]), 1))),
                              16, ID$texto), 0.6, 4.25, 7.6, 1.5)
  x <- caja_texto(x, 8.6, 1.95, 4.1, 4.2, tr("B · Cifra de negocio", "B · Turnover"),
    tr(paste0("Con 20 trabajadores y 0,5 M€ de inversión, el intervalo individual va de ", num(pp_b[1, 2], 1), " a ", num(pp_b[1, 3], 1),
              " M€: incluye cifras negativas, imposibles. Los intervalos son muy amplios y crecen con el tamaño, un indicio de heterocedasticidad (tema 9). Predecir con 200 trabajadores, ",
              "por encima del máximo observado (", max(caso_b$trabajadores), "), es extrapolar."),
       paste0("With 20 workers and €0.5m of investment, the individual interval runs from ", num(pp_b[1, 2], 1), " to ", num(pp_b[1, 3], 1),
              " million euros: it includes negative, impossible values. The intervals are very wide and grow with size, a hint of heteroskedasticity (chapter 9). Predicting with 200 workers, ",
              "above the observed maximum (", max(caso_b$trabajadores), "), is extrapolating.")),
    oscura = TRUE, size = 15)

  # 17 · Cierre
  x <- diapo_cierre(x,
    tr(c(paste0("Predicción puntual: ", M("\\hat{Y}_0 = \\mathbf{x}_0'\\hat{\\beta}"), ", la misma para la media y para un individuo."),
         "Intervalo para la media: dónde está la recta.",
         paste0("Para un individuo: añade el error ", M("u_0"), "; siempre más ancho."),
         "Los dos se abren lejos del centro de los datos: cuidado al extrapolar.",
         "Residuos frente a ajustados: sin embudo ni curva.",
         "Gráfico Q-Q: puntos sobre la diagonal si hay normalidad."),
       c(paste0("Point prediction: ", M("\\hat{Y}_0 = \\mathbf{x}_0'\\hat{\\beta}"), ", the same for the mean and for an individual."),
         "Interval for the mean: where the line is.",
         paste0("For an individual: adds the error ", M("u_0"), "; always wider."),
         "Both widen far from the centre of the data: beware of extrapolation.",
         "Residuals against fitted values: no funnel and no curve.",
         "Q-Q plot: points on the diagonal under normality.")),
    tr("Excel de clase del tema 7, scripts de R del tema 7 y app «Predicción y residuos». Explicación completa en el capítulo 7 del manual.",
       "Chapter 7 class Excel, Chapter 7 R scripts and the «Prediction and residuals» app. Full explanation in Chapter 7 of the book."),
    tema)

  guardar_pptx(x, "T07_mEC_Prediccion_Residuos.pptx", "T07_mEC_Prediction_Residuals.pptx")
}
