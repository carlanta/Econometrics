# ══════════════════════════════════════════════════════════════════
# Capítulo 10 · Autocorrelación serial · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap10_pptx.R
# Contenido y numeración: 10-autocorrelacion.qmd (10.1 a 10.4.2). Modelo idéntico al del capítulo:
#   C: consumo ~ pib + tipo_interes   (Durbin-Watson con lmtest::dwtest, H1: rho > 0)
# Inferencia robusta HAC (Newey-West, núcleo de Bartlett, L = floor(0,75·n^(1/3)) retardos, sin preblanqueo) con sandwich.
# ══════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({ library(lmtest); library(sandwich) })
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 10", "Chapter 10")

  # ── Modelo y contrastes del capítulo ─────────────────────────────
  mc  <- lm(consumo ~ pib + tipo_interes, data = caso_c)
  mc1 <- lm(consumo ~ pib, data = caso_c)
  dw  <- dwtest(mc); dw1 <- dwtest(mc1)
  res <- residuals(mc); n <- length(res)
  num_dw <- sum(diff(res)^2); den_dw <- sum(res^2)
  rho_dw <- 1 - dw$statistic[[1]] / 2
  rho_cor <- cor(res[-n], res[-1])
  bg <- bgtest(mc, order = 4)
  lag_nw <- floor(0.75 * n^(1 / 3))
  ct_std <- coeftest(mc)
  ct_hc  <- coeftest(mc, vcov = vcovHC(mc, type = "HC1"))
  ct_hac <- coeftest(mc, vcov = NeweyWest(mc, lag = lag_nw, prewhite = FALSE))
  anio <- substr(caso_c$trimestre, 1, 4)
  cortes <- which(substr(caso_c$trimestre, 6, 7) == "Q1"); cortes <- cortes[seq(1, length(cortes), by = 2)]

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 10, tr("Autocorrelación serial", "Serial autocorrelation"),
    tr("¿Arrastra el consumo de este trimestre la sorpresa del anterior?",
       "Does this quarter's consumption carry over last quarter's surprise?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("10.1", "10.2", "10.3", "10.3.1", "10.4", "10.5"),
    titulo = tr(c("Concepto de autocorrelación", "Consecuencias de la autocorrelación", "Contraste de Durbin-Watson",
                  "Aplicación al caso C", "Inferencia robusta a la autocorrelación: errores HAC", "Aplicación a los casos prácticos"),
                c("The autocorrelation concept", "Consequences of autocorrelation", "The Durbin-Watson test",
                  "Application to case C", "Inference robust to autocorrelation: HAC errors", "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Entender el concepto", paste0("Errores que se parecen a los del periodo anterior: el esquema AR(1) y el coeficiente ", M("\\rho"), ".")),
       c("Understand the concept", paste0("Errors that resemble those of the previous period: the AR(1) scheme and the coefficient ", M("\\rho"), "."))),
    tr(c("Detectarla", "Gráfico de residuos en el tiempo y contraste de Durbin-Watson con el caso C."),
       c("Detect it", "Plot of residuals over time and the Durbin-Watson test with case C.")),
    tr(c("Corregir la inferencia", "Errores estándar HAC (Newey-West): mismos coeficientes, contrastes fiables."),
       c("Fix the inference", "HAC (Newey-West) standard errors: same coefficients, reliable tests."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Errores con memoria", "Errors with memory"),
    tr(c("El supuesto de no autocorrelación", "El esquema AR(1)", "Consecuencias sobre MCO"),
       c("The no-autocorrelation assumption", "The AR(1) scheme", "Consequences for OLS")))

  # 5 · 10.1 Concepto
  x <- diapo_contenido(x, tr("10.1 · Concepto de autocorrelación", "10.1 · The autocorrelation concept"),
    tr("Cada error recuerda al anterior", "Each error remembers the previous one"), tema)
  x <- formula_caja(x, tr("\\text{Cov}(u_t, u_s \\mid \\mathbf{X}) \\neq 0 \\quad \\text{para algún } t \\neq s",
                          "\\text{Cov}(u_t, u_s \\mid \\mathbf{X}) \\neq 0 \\quad \\text{for some } t \\neq s"), 0.6, 2.0, 5.4, 0.9, size = 22)
  x <- formula_caja(x, "u_t = \\rho\\, u_{t-1} + \\varepsilon_t, \\qquad |\\rho| < 1", 0.6, 3.1, 5.4, 0.9, size = 22)
  x <- vinetas(x, tr(
    c(paste0(M("\\rho > 0"), ": un error positivo suele ir seguido de otro positivo; rachas."),
      paste0(M("\\rho < 0"), ": los errores alternan de signo; zigzag."),
      paste0(M("\\varepsilon_t"), ": ruido blanco, sin memoria.")),
    c(paste0(M("\\rho > 0"), ": a positive error tends to be followed by another; runs."),
      paste0(M("\\rho < 0"), ": the errors alternate in sign; zigzag."),
      paste0(M("\\varepsilon_t"), ": white noise, with no memory."))),
    0.6, 4.3, 5.4, 2.5, size = 16)
  set.seed(2024)
  ar1 <- function(rho, T = 60) { e <- rnorm(T); u <- numeric(T); u[1] <- e[1]; for (t in 2:T) u[t] <- rho * u[t - 1] + e[t]; u }
  etq <- tr(c('rho == "0,8"', 'rho == "0"', 'rho == "-0,8"'), c('rho == "0.8"', 'rho == "0"', 'rho == "-0.8"'))
  df_ar <- data.frame(t = rep(1:60, 3), u = c(ar1(0.8), ar1(0), ar1(-0.8)), p = factor(rep(etq, each = 60), levels = etq))
  g_ar <- ggplot(df_ar, aes(t, u)) +
    geom_hline(yintercept = 0, colour = COL$gris, linetype = "dashed") +
    geom_line(colour = COL$navy, linewidth = 0.7) + geom_point(colour = COL$oro, size = 1.1) +
    facet_wrap(~ p, ncol = 1, labeller = label_parsed) +
    scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Periodo", "Period"), y = tr("Error simulado", "Simulated error")) +
    tema_cdea(12)
  x <- imagen_ggplot(x, g_ar, 6.4, 1.95, 6.4, 4.85)

  # 6 · 10.1 Dónde aparece
  x <- diapo_contenido(x, tr("10.1 · Dónde aparece", "10.1 · Where it appears"),
    tr("Un problema de las series temporales", "A time-series problem"), tema)
  x <- vinetas(x, tr(
    c("Inercia: los hábitos de consumo cambian despacio.",
      "Variables omitidas que también tienen inercia.",
      "Forma funcional equivocada: los residuos siguen la curva que falta.",
      "Datos suavizados o interpolados (medias móviles, trimestralizaciones)."),
    c("Inertia: consumption habits change slowly.",
      "Omitted variables that also have inertia.",
      "Wrong functional form: the residuals follow the missing curve.",
      "Smoothed or interpolated data (moving averages, quarterly splits).")),
    0.6, 2.1, 7.2, 4.6, size = 18)
  x <- caja_texto(x, 8.3, 2.1, 4.4, 3.4, tr("Casos A y B: no aplica", "Cases A and B: not applicable"),
    tr("En corte transversal no hay «observación anterior»: la empresa 5 no va después de la 4. Si se reordenan los datos, el DW cambia. La autocorrelación serial es cosa de datos temporales: el caso C.",
       "In a cross-section there is no «previous observation»: firm 5 does not come after firm 4. Reordering the data changes the DW. Serial autocorrelation belongs to time data: case C."),
    oscura = TRUE, size = 16)

  # 7 · 10.2 Consecuencias
  x <- diapo_contenido(x, tr("10.2 · Consecuencias de la autocorrelación", "10.2 · Consequences of autocorrelation"),
    tr("Las mismas que la heterocedasticidad", "The same as heteroskedasticity"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 2.4, tr("Insesgadez y consistencia", "Unbiasedness and consistency"), tr("Se mantienen", "They hold"),
    tr("Los coeficientes MCO siguen centrados en el verdadero valor.", "The OLS coefficients are still centred on the true value."), ID$navy, size_valor = 26)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 2.4, tr("Eficiencia", "Efficiency"), tr("Se pierde", "It is lost"),
    tr("Falla Gauss-Markov: MCO ya no es ELIO.", "Gauss-Markov fails: OLS is no longer BLUE."), ID$oro, size_valor = 26)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 2.4, tr("Errores estándar", "Standard errors"), tr("Incorrectos", "Wrong"),
    tr(paste0("Con ", M("\\rho > 0"), " suelen salir demasiado pequeños."), paste0("With ", M("\\rho > 0"), " they usually come out too small.")), ID$gris_med, size_valor = 26)
  x <- vinetas(x, tr(
    c(paste0("Estadísticos ", M("t"), " artificialmente grandes y p-valores demasiado pequeños."),
      paste0("Falsos rechazos de ", M("H_0"), ": variables «significativas» que no lo son."),
      "Intervalos de confianza y de predicción (tema 7) demasiado estrechos."),
    c(paste0("Artificially large ", M("t"), " statistics and p-values that are too small."),
      paste0("False rejections of ", M("H_0"), ": «significant» variables that are not."),
      "Confidence and prediction intervals (chapter 7) that are too narrow.")),
    0.6, 4.75, 12.1, 2.0, size = 17)

  # 8 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Detectar y corregir", "Detect and fix"),
    tr(c("El contraste de Durbin-Watson", "El caso C, trimestre a trimestre", "Errores estándar HAC"),
       c("The Durbin-Watson test", "Case C, quarter by quarter", "HAC standard errors")))

  # 9 · 10.3 Durbin-Watson
  x <- diapo_contenido(x, tr("10.3 · Contraste de Durbin-Watson", "10.3 · The Durbin-Watson test"),
    tr("Un número entre 0 y 4 que se lee junto al 2", "A number between 0 and 4, read against 2"), tema)
  x <- formula_caja(x, "DW = \\frac{\\sum_{t=2}^{n}(\\hat{u}_t - \\hat{u}_{t-1})^2}{\\sum_{t=1}^{n}\\hat{u}_t^2} \\approx 2(1 - \\hat{\\rho})",
                    0.6, 2.0, 6.6, 1.6, size = 24)
  x <- vinetas(x, tr(
    c(paste0(M("H_0: \\rho = 0"), " frente a ", M("H_1: \\rho > 0"), "."),
      paste0("Solo detecta el esquema AR(1) y exige constante y ningún ", M("Y_{t-1}"), " entre las explicativas."),
      "Gretl lo da en la salida de MCO con series temporales."),
    c(paste0(M("H_0: \\rho = 0"), " against ", M("H_1: \\rho > 0"), "."),
      paste0("It only detects the AR(1) scheme and requires a constant and no ", M("Y_{t-1}"), " among the regressors."),
      "Gretl reports it in the OLS output for time series.")),
    7.5, 2.0, 5.2, 2.0, size = 15)
  zonas <- data.frame(x0 = c(0, 1.5, 2.5), x1 = c(1.5, 2.5, 4), z = factor(1:3))
  rot <- tr(c("Autocorrelación positiva", "Sin autocorrelación", "Autocorrelación negativa"),
            c("Positive autocorrelation", "No autocorrelation", "Negative autocorrelation"))
  g_dw <- ggplot() +
    geom_rect(data = zonas, aes(xmin = x0, xmax = x1, ymin = 0, ymax = 1, fill = z), colour = "white", linewidth = 1.5) +
    scale_fill_manual(values = c(COL$oro_cl, COL$linea, COL$oro_cl), guide = "none") +
    annotate("text", x = c(0.75, 2, 3.25), y = -0.25, label = rot, colour = COL$navy, size = 4.6, fontface = "bold") +
    annotate("segment", x = dw$statistic, xend = dw$statistic, y = 0, yend = 1.15, colour = COL$navy, linewidth = 1.6) +
    annotate("point", x = dw$statistic, y = 0.5, colour = COL$navy, size = 4) +
    annotate("text", x = dw$statistic + 0.05, y = 1.32, hjust = 0, label = paste0(tr("Caso C: DW = ", "Case C: DW = "), num(dw$statistic, 2)),
             colour = COL$navy, size = 4.8, fontface = "bold") +
    scale_x_continuous(breaks = 0:4, labels = function(v) num(v, 0), limits = c(-0.05, 4.05)) +
    scale_y_continuous(limits = c(-0.45, 1.45)) +
    labs(x = "DW", y = NULL) +
    tema_cdea(14) + theme(axis.text.y = element_blank(), panel.grid = element_blank())
  x <- imagen_ggplot(x, g_dw, 0.6, 4.0, 12.1, 2.8)

  # 10 · 10.3.1 Caso C: gráficos
  x <- diapo_contenido(x, tr("10.3.1 · Aplicación al caso C", "10.3.1 · Application to case C"),
    tr("Rachas de residuos del mismo signo", "Runs of residuals with the same sign"), tema)
  df_t <- data.frame(t = seq_len(n), r = res, s = factor(ifelse(res >= 0, "+", "-"), levels = c("+", "-")))
  g_t <- ggplot(df_t, aes(t, r)) +
    geom_hline(yintercept = 0, colour = COL$gris, linetype = "dashed") +
    geom_line(colour = COL$navy, linewidth = 0.8) +
    geom_point(aes(colour = s), size = 2.4, show.legend = FALSE) +
    scale_colour_manual(values = c(COL$navy, COL$oro)) +
    scale_x_continuous(breaks = cortes, labels = anio[cortes]) +
    scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = NULL, y = tr("Residuo (MM€)", "Residual (€m)"), title = tr("Residuos en el tiempo", "Residuals over time")) +
    tema_cdea(13) + theme(plot.title = element_text(face = "bold", colour = COL$navy, size = 14))
  df_l <- data.frame(a = res[-n], b = res[-1])
  g_l <- ggplot(df_l, aes(a, b)) +
    geom_hline(yintercept = 0, colour = COL$gris, linetype = "dotted") + geom_vline(xintercept = 0, colour = COL$gris, linetype = "dotted") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = COL$oro, linewidth = 1.2) +
    geom_point(colour = COL$navy, size = 2.3, alpha = 0.85) +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Residuo en t", "Residual at t"), y = tr("Residuo en t+1", "Residual at t+1"),
         title = tr("Residuo en t frente a t+1", "Residual at t against t+1")) +
    tema_cdea(13) + theme(plot.title = element_text(face = "bold", colour = COL$navy, size = 14))
  x <- imagen_ggplot(x, g_t, 0.6, 1.95, 6.9, 4.1)
  x <- imagen_ggplot(x, g_l, 7.7, 1.95, 5.1, 4.1)
  x <- poner_texto(x, parrafo(paste0(tr("Modelo del capítulo: ", "Chapter model: "),
                                     M(tr("\\hat{C}_t = \\hat{\\beta}_0 + \\hat{\\beta}_1 \\,\\text{PIB}_t + \\hat{\\beta}_2 \\,\\text{tipo}_t",
                                          "\\hat{C}_t = \\hat{\\beta}_0 + \\hat{\\beta}_1 \\,\\text{GDP}_t + \\hat{\\beta}_2 \\,\\text{rate}_t")),
                                     tr(". Los residuos mantienen el signo varios trimestres seguidos: autocorrelación positiva.",
                                        ". The residuals keep their sign for several quarters in a row: positive autocorrelation.")),
                              16, ID$navy, bold = TRUE), 0.6, 6.15, 12.1, 0.7)

  # 11 · 10.3.1 Caso C: el estadístico
  x <- diapo_contenido(x, tr("10.3.1 · Durbin-Watson en el caso C", "10.3.1 · Durbin-Watson in case C"),
    tr("Lejos de 2: autocorrelación positiva", "Far from 2: positive autocorrelation"), tema)
  x <- formula_caja(x, paste0("DW = \\frac{", numtex(num_dw, 0), "}{", numtex(den_dw, 0), "} = ", numtex(dw$statistic, 4)),
                    0.6, 2.0, 7.4, 1.4, size = 26)
  x <- caja_dato(x, 8.4, 2.0, 4.3, 2.35, tr("p-valor del contraste", "Test p-value"), tr("< 0,001", "< 0.001"),
    paste0(tr("Se rechaza ", ""), M("H_0: \\rho = 0"), tr(".", " is rejected.")), ID$navy, size_valor = 28)
  x <- caja_dato(x, 0.6, 3.7, 3.55, 1.95, tr(paste0(M("\\hat{\\rho}"), " a partir del DW"), paste0(M("\\hat{\\rho}"), " from the DW")),
    M(paste0("1 - DW/2 = ", numtex(rho_dw, 3))), NULL, ID$oro, size_valor = 24)
  x <- caja_dato(x, 4.45, 3.7, 3.55, 1.95, tr("Correlación de residuos", "Residual correlation"),
    M(paste0("\\text{cor}(\\hat{u}_t, \\hat{u}_{t-1}) = ", numtex(rho_cor, 3))), NULL, ID$oro, size_valor = 20)
  x <- caja_texto(x, 8.4, 4.6, 4.3, 2.1, tr("Sin el tipo de interés", "Without the interest rate"),
    tr(sprintf("%s con solo el PIB: la autocorrelación no se debe a omitir el tipo; es inercia del consumo.", M(paste0("DW = ", numtex(dw1$statistic, 4)))),
       sprintf("%s with GDP only: the autocorrelation is not due to omitting the rate; it is consumption inertia.", M(paste0("DW = ", numtex(dw1$statistic, 4))))),
    oscura = TRUE, size = 14)
  x <- poner_texto(x, parrafo(tr(paste0("Las dos estimaciones de ", M("\\rho"), " coinciden: en torno a ", num(min(rho_dw, rho_cor), 2), "–", num(max(rho_dw, rho_cor), 2), "."),
                                 paste0("The two estimates of ", M("\\rho"), " agree: around ", num(min(rho_dw, rho_cor), 2), "–", num(max(rho_dw, rho_cor), 2), ".")),
                              15, ID$gris, italic = TRUE), 0.6, 5.9, 7.4, 0.6)

  # 12 · 10.4 HAC: concepto
  x <- diapo_contenido(x, tr("10.4 · Inferencia robusta: errores HAC", "10.4 · Robust inference: HAC errors"),
    tr("Newey-West: robustos a las dos cosas", "Newey-West: robust to both problems"), tema)
  x <- formula_caja(x, "\\widehat{\\text{Var}}_{\\text{HAC}}(\\hat{\\boldsymbol\\beta}) = (\\mathbf{X}'\\mathbf{X})^{-1} \\left[ \\sum_{t} \\hat{u}_t^2 \\mathbf{x}_t \\mathbf{x}_t' + \\sum_{l=1}^{L} w_l \\sum_{t} \\hat{u}_t \\hat{u}_{t-l} (\\mathbf{x}_t \\mathbf{x}_{t-l}' + \\mathbf{x}_{t-l} \\mathbf{x}_t') \\right] (\\mathbf{X}'\\mathbf{X})^{-1}",
                    0.6, 2.0, 12.1, 1.3, size = 20)
  x <- vinetas(x, tr(
    c("HAC: robusta a la heterocedasticidad y a la autocorrelación.",
      "El primer sumando es el de White (tema 9); el segundo añade las covarianzas entre errores cercanos.",
      paste0(M("w_l = 1 - \\frac{l}{L+1}"), ": pesos de Bartlett, que bajan con la distancia ", M("l"), "."),
      paste0("Caso C: ", M(paste0("L = ", lag_nw)), " retardos, ", M("L \\approx 0{,}75\\, n^{1/3}"), " con ", M(paste0("n = ", n)), ".")),
    c("HAC: robust to heteroskedasticity and autocorrelation.",
      "The first term is White's (chapter 9); the second adds the covariances between nearby errors.",
      paste0(M("w_l = 1 - \\frac{l}{L+1}"), ": Bartlett weights, falling with the distance ", M("l"), "."),
      paste0("Case C: ", M(paste0("L = ", lag_nw)), " lags, ", M("L \\approx 0.75\\, n^{1/3}"), " with ", M(paste0("n = ", n)), "."))),
    0.6, 3.6, 7.9, 3.2, size = 16)
  x <- caja_texto(x, 8.9, 3.6, 3.8, 2.5, tr("Qué cambia", "What changes"),
    tr("Como con HC: los coeficientes no se tocan. Solo cambian los errores estándar, las t, los p-valores y los intervalos.",
       "As with HC: the coefficients are untouched. Only the standard errors, t statistics, p-values and intervals change."),
    oscura = TRUE, size = 15)

  # 13 · 10.4.2 Caso C: comparación
  x <- diapo_contenido(x, tr("10.4 · Caso C: tres errores estándar", "10.4 · Case C: three standard errors"),
    tr("Convencional, HC1 y HAC en la función de consumo", "Conventional, HC1 and HAC in the consumption function"), tema)
  fila <- function(j, d) c(num(ct_std[j, 2], d), num(ct_hc[j, 2], d), num(ct_hac[j, 2], d))
  tab <- data.frame(
    v = tr(c("Constante", "PIB (PMC)", "Tipo de interés"), c("Constant", "GDP (MPC)", "Interest rate")),
    b = c(num(coef(mc)[1], 0), num(coef(mc)[2], 4), num(coef(mc)[3], 1)),
    s1 = c(num(ct_std[1, 2], 0), num(ct_std[2, 2], 4), num(ct_std[3, 2], 1)),
    s2 = c(num(ct_hc[1, 2], 0), num(ct_hc[2, 2], 4), num(ct_hc[3, 2], 1)),
    s3 = c(num(ct_hac[1, 2], 0), num(ct_hac[2, 2], 4), num(ct_hac[3, 2], 1)),
    p3 = vapply(ct_hac[, 4], pval, ""), stringsAsFactors = FALSE)
  x <- poner_tabla(x, tabla_pptx(tab, tr(c("Variable", "Coef.", "ES conv.", "ES HC1", "ES HAC", "p HAC"),
                                         c("Variable", "Coef.", "Conv. SE", "HC1 SE", "HAC SE", "HAC p")), size = 16), 0.6, 2.1)
  x <- vinetas(x, tr(
    c(sprintf("El error estándar de la PMC pasa de %s a %s con HAC: un %s mayor.",
              num(ct_std[2, 2], 4), num(ct_hac[2, 2], 4), pct(ct_hac[2, 2] / ct_std[2, 2] - 1, 0)),
      sprintf("La PMC sigue siendo muy significativa (t = %s); el tipo de interés no lo es (p-valor = %s).",
              num(ct_hac[2, 3], 1), num(ct_hac[3, 4], 3))),
    c(sprintf("The MPC standard error goes from %s to %s with HAC: %s larger.",
              num(ct_std[2, 2], 4), num(ct_hac[2, 2], 4), pct(ct_hac[2, 2] / ct_std[2, 2] - 1, 0)),
      sprintf("The MPC is still highly significant (t = %s); the interest rate is not (p-value = %s).",
              num(ct_hac[2, 3], 1), num(ct_hac[3, 4], 3)))),
    0.6, 4.2, 12.1, 1.6, size = 17)
  x <- caja_texto(x, 0.6, 5.65, 12.1, 1.1, tr("Lección", "Lesson"),
    tr("Con autocorrelación positiva, los errores convencionales prometen una precisión que los datos no tienen.",
       "With positive autocorrelation, conventional errors promise a precision the data do not have."),
    oscura = TRUE, size = 15)

  # 14 · Gretl → R
  x <- diapo_contenido(x, tr("Gretl → R", "Gretl → R"), tr("La autocorrelación en Gretl y en R", "Autocorrelation in Gretl and in R"), tema)
  sw <- data.frame(
    op = tr(c("Residuos en el tiempo", "Durbin-Watson", "Breusch-Godfrey (orden 4)", "MCO con errores HAC"),
            c("Residuals over time", "Durbin-Watson", "Breusch-Godfrey (order 4)", "OLS with HAC errors")),
    gretl = tr(c("Modelo: Gráficos → Gráfico de residuos → Contra el tiempo", "Salida de MCO; p-valor en Contrastes → Durbin-Watson",
                 "Modelo: Contrastes → Autocorrelación", "Modelo → MCO → Desviaciones típicas robustas"),
               c("Model: Graphs → Residual plot → Against time", "OLS output; p-value in Tests → Durbin-Watson",
                 "Model: Tests → Autocorrelation", "Model → OLS → Robust standard errors")),
    r = c("plot(resid(m), type = \"b\")", "lmtest::dwtest(m)", "lmtest::bgtest(m, order = 4)",
          sprintf("coeftest(m, vcov = NeweyWest(m, lag = %d))", lag_nw)))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 13, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c(sprintf("Breusch-Godfrey admite retardos de %s y órdenes mayores: en el caso C, p-valor = %s.", M("Y"), num(bg$p.value, 4)),
      "Con datos de series temporales, Gretl calcula errores HAC al marcar «robustas»."),
    c(sprintf("Breusch-Godfrey allows lags of %s and higher orders: in case C, p-value = %s.", M("Y"), num(bg$p.value, 4)),
      "With time-series data, Gretl computes HAC errors when «robust» is ticked.")),
    0.6, 4.3, 12.1, 1.8, size = 16)

  # 15 · Vuelta a los casos
  x <- diapo_contenido(x, tr("10.5 · Vuelta a los casos", "10.5 · Back to the cases"),
    tr("Solo el caso C tiene memoria", "Only case C has memory"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 2.75, tr("A · Demanda", "A · Demand"), tr("No aplica", "Not applicable"),
    tr("Corte transversal: no hay orden temporal.", "Cross-section: there is no time order."), ID$gris_med, size_valor = 26)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 2.75, tr("B · Cifra de negocio", "B · Turnover"), tr("No aplica", "Not applicable"),
    tr("Corte transversal: su problema era la heterocedasticidad (tema 9).", "Cross-section: its problem was heteroskedasticity (chapter 9)."), ID$gris_med, size_valor = 26)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 2.75, tr("C · Consumo", "C · Consumption"), M(paste0("DW = ", numtex(dw$statistic, 2))),
    tr("Autocorrelación positiva y heterocedasticidad: errores HAC.", "Positive autocorrelation and heteroskedasticity: HAC errors."), ID$oro, size_valor = 26)
  x <- caja_texto(x, 0.6, 5.1, 12.1, 1.65, tr("Y las predicciones del tema 7", "And the forecasts of chapter 7"),
    tr("Las predicciones puntuales del caso C siguen siendo válidas, pero sus intervalos eran demasiado estrechos: la incertidumbre real es mayor.",
       "The point forecasts of case C remain valid, but their intervals were too narrow: the real uncertainty is larger."),
    oscura = TRUE, size = 16)

  # 16 · Cierre
  x <- diapo_cierre(x,
    tr(c(paste0("Autocorrelación: ", M("\\text{Cov}(u_t, u_s) \\neq 0"), "; típica de las series temporales."),
         paste0("Esquema AR(1): ", M("u_t = \\rho\\, u_{t-1} + \\varepsilon_t"), "."),
         "MCO sigue insesgado, pero los errores estándar convencionales fallan.",
         paste0(M("DW \\approx 2(1 - \\hat{\\rho})"), ": cerca de 2, bien; cerca de 0, autocorrelación positiva."),
         sprintf("Caso C: %s, autocorrelación positiva clara.", M(paste0("DW = ", numtex(dw$statistic, 2)))),
         "Errores HAC (Newey-West): mismos coeficientes, inferencia fiable."),
       c(paste0("Autocorrelation: ", M("\\text{Cov}(u_t, u_s) \\neq 0"), "; typical of time series."),
         paste0("AR(1) scheme: ", M("u_t = \\rho\\, u_{t-1} + \\varepsilon_t"), "."),
         "OLS stays unbiased, but the conventional standard errors fail.",
         paste0(M("DW \\approx 2(1 - \\hat{\\rho})"), ": near 2, fine; near 0, positive autocorrelation."),
         sprintf("Case C: %s, clear positive autocorrelation.", M(paste0("DW = ", numtex(dw$statistic, 2)))),
         "HAC (Newey-West) errors: same coefficients, reliable inference.")),
    tr("Excel de clase del tema 10, scripts de R del tema 10 y app «Autocorrelación». Explicación completa en el capítulo 10 del manual.",
       "Chapter 10 class Excel file, Chapter 10 R scripts and the «Autocorrelation» app. Full explanation in Chapter 10 of the book."),
    tema)

  guardar_pptx(x, "T10_mEC_Autocorrelacion.pptx", "T10_mEC_Autocorrelation.pptx")
}
