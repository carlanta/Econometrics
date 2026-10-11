# ══════════════════════════════════════════════════════════════════
# Capítulo 05 · Intervalos de confianza en el MRLG · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap05_pptx.R
# Contenido y numeración: 05-intervalos-confianza.qmd (secciones 5.1 a 5.5). Modelos como en el capítulo:
#   Caso A: ventas ~ precio_propio + gasto_publicidad + precio_sustit (β1 precio, β2 publicidad, β3 sustitutivo)
#   Caso B: cifra_negocio ~ trabajadores + inversion + superficie
#   Caso C: consumo ~ pib + tipo_interes
# Titulares de UNA línea (≈ 45 caracteres). Cifras calculadas aquí con los datos de data/.
# ══════════════════════════════════════════════════════════════════
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  # Espacio duro entre número y unidad en todo el texto de las diapositivas (envuelve runs_texto de los comunes
  # sin modificarlos: parrafo() y vinetas() la buscan en el entorno global)
  runs_texto_base <- runs_texto
  # (y signo menos tipográfico, que no parte la línea, en los números negativos; guion que no parte en «p-valor»)
  runs_texto <- function(texto, ...) {
    texto <- gsub("([0-9]) (%|€|MM€|M€|EUR)", "\\1\u00a0\\2", texto)
    texto <- gsub("(^|[ (\\[;:])-([0-9])", "\\1\u2212\\2", texto, perl = TRUE)
    texto <- gsub("p-val", "p\u2011val", texto, fixed = TRUE)
    runs_texto_base(texto, ...)
  }
  tema <- tr("Tema 5", "Chapter 5")
  ES <- tr("\\text{ES}", "\\text{SE}")
  IC <- tr("\\text{IC}", "\\text{CI}")

  # ── Modelos y cifras del capítulo ──────────────────────────────────
  m_a <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
  m_b <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)
  m_c <- lm(consumo ~ pib + tipo_interes, data = caso_c)
  gl_a <- df.residual(m_a); tc_a <- qt(0.975, gl_a)
  b_a <- coef(m_a); se_a <- sqrt(diag(vcov(m_a)))
  ic_a <- lapply(c(0.90, 0.95, 0.99), function(l) confint(m_a, level = l))
  ic_b <- confint(m_b); ic_c <- confint(m_c)
  # Combinaciones lineales (sección 5.3 y ejercicio 5.5)
  comb <- function(cv) {
    est <- sum(cv * b_a); se <- sqrt(drop(t(cv) %*% vcov(m_a) %*% cv))
    c(est = est, se = se, li = est - tc_a * se, ls = est + tc_a * se)
  }
  cl1 <- comb(c(0, 1, 0.5, 0))   # β1 + 0,5·β2
  cl2 <- comb(c(0, 1, 1, 0))     # β1 + β2: como β1 < 0, compara las magnitudes (β2 − β1 sale positiva siempre)

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 5, tr("Intervalos de confianza en el MRLG", "Confidence intervals in the MLRM"),
    tr("¿Entre qué valores está, de verdad, el efecto del precio sobre las ventas?",
       "Between which values does the effect of price on sales really lie?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("5.1", "5.2", "5.3", "5.4", "5.5"),
    titulo = tr(c("Concepto de intervalo de confianza", "Intervalo para un coeficiente individual",
                  "Intervalo para una combinación lineal de coeficientes", "Visualización: intervalos y significatividad",
                  "Aplicación a los casos prácticos"),
                c("The concept of a confidence interval", "Interval for an individual coefficient",
                  "Interval for a linear combination of coefficients", "Visualisation: intervals and significance",
                  "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Interpretar bien", "Qué significa el 95 %: una propiedad del método al repetir el muestreo, no de un intervalo concreto."),
       c("Interpret correctly", "What 95 % means: a property of the method under repeated sampling, not of one particular interval.")),
    tr(c("Construir el intervalo", "Coeficiente, valor crítico de la t y error estándar; también para una combinación lineal."),
       c("Build the interval", "Coefficient, critical t value and standard error; also for a linear combination.")),
    tr(c("Contrastar con él", "Si el valor de la hipótesis nula queda fuera del intervalo, se rechaza: el mismo veredicto que la t."),
       c("Test with it", "If the null hypothesis value falls outside the interval, it is rejected: the same verdict as the t test."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Qué es un intervalo de confianza", "What a confidence interval is"),
    tr(c("Un rango de valores plausibles", "Lo aleatorio es el intervalo", "Dualidad con los contrastes"),
       c("A range of plausible values", "The interval is what is random", "Duality with hypothesis tests")))

  # 5 · 5.1 Definición
  x <- diapo_contenido(x, tr("5.1 · Concepto de intervalo de confianza", "5.1 · The concept of a confidence interval"),
    tr(paste0("Un rango de valores plausibles para ", M("\\beta_j")), paste0("A range of plausible values for ", M("\\beta_j"))), tema)
  x <- formula_caja(x, "P(L \\leq \\beta_j \\leq U) = 1 - \\alpha", 0.6, 2.05, 6.2, 1.0, size = 28)
  x <- vinetas(x, tr(
    c(paste0(M("L"), " y ", M("U"), " se calculan con la muestra: cambian de una muestra a otra."),
      paste0(M("\\beta_j"), " es fijo, aunque desconocido."),
      paste0("Niveles habituales: 90 %, 95 % y 99 %, es decir, ", M("\\alpha"), " de 0,10, 0,05 y 0,01."),
      "Más rico que un «sí o no»: da magnitud y precisión del efecto."),
    c(paste0(M("L"), " and ", M("U"), " are computed from the sample: they change from one sample to another."),
      paste0(M("\\beta_j"), " is fixed, although unknown."),
      paste0("Usual levels: 90 %, 95 % and 99 %, that is, ", M("\\alpha"), " of 0.10, 0.05 and 0.01."),
      "Richer than a «yes or no»: it gives the size and the precision of the effect.")),
    0.6, 3.35, 6.4, 3.4, size = 17)
  x <- caja_texto(x, 7.4, 2.05, 5.3, 3.9, tr("Interpretación correcta", "Correct interpretation"),
    tr(paste0("El 95 % NO es la probabilidad de que ", M("\\beta_j"), " esté dentro de este intervalo concreto: ", M("\\beta_j"), " no es aleatorio. Significa que, si repitiéramos el muestreo muchas veces, el 95 % de los intervalos así construidos contendrían el verdadero ", M("\\beta_j"), "."),
       paste0("The 95 % is NOT the probability that ", M("\\beta_j"), " lies inside this particular interval: ", M("\\beta_j"), " is not random. It means that, if we repeated the sampling many times, 95 % of the intervals built this way would contain the true ", M("\\beta_j"), ".")),
    oscura = TRUE, size = 17)

  # 6 · 5.1 Simulación del muestreo repetido
  set.seed(2024)
  B <- 40; b_true <- -1.8
  sim <- do.call(rbind, lapply(seq_len(B), function(s) {
    p <- runif(35, 1.8, 3.5); y <- 11 + b_true * p + rnorm(35, 0, 1.2)
    ci <- confint(lm(y ~ p))[2, ]
    data.frame(muestra = s, li = ci[1], ls = ci[2], b = coef(lm(y ~ p))[2])
  }))
  sim$dentro <- sim$li <= b_true & sim$ls >= b_true
  n_dentro <- sum(sim$dentro)
  niv_sim <- tr(c("Contiene el verdadero valor", "No lo contiene"), c("Contains the true value", "Does not contain it"))
  sim$estado <- factor(ifelse(sim$dentro, niv_sim[1], niv_sim[2]), levels = niv_sim)
  x <- diapo_contenido(x, tr("5.1 · Interpretación del nivel de confianza", "5.1 · Interpreting the confidence level"),
    tr("El 95 % es del método, no del intervalo", "The 95 % belongs to the method, not the interval"), tema)
  g_sim <- ggplot(sim, aes(y = muestra)) +
    geom_vline(xintercept = b_true, colour = COL$gris, linetype = "dashed", linewidth = 0.8) +
    geom_segment(aes(x = li, xend = ls, yend = muestra, colour = estado), linewidth = 1.1) +
    geom_point(aes(x = b, colour = estado), size = 1.8) +
    scale_colour_manual(values = setNames(c(COL$navy, COL$oro), niv_sim), name = NULL) +
    labs(x = tr("Intervalo al 95 % para la pendiente", "95 % interval for the slope"), y = tr("Muestra", "Sample")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_sim, 0.6, 1.95, 7.3, 4.85)
  x <- vinetas(x, tr(
    c(sprintf("%d muestras simuladas de 35 observaciones con la misma recta verdadera (%s).", B, paste0(M(paste0("\\beta_1 = ", numtex(b_true, 1))))),
      sprintf("%d de los %d intervalos contienen el verdadero valor; %d no lo contienen.", n_dentro, B, B - n_dentro),
      "Con una sola muestra no sabemos si la nuestra es de las que aciertan: confiamos en el procedimiento."),
    c(sprintf("%d simulated samples of 35 observations with the same true line (%s).", B, paste0(M(paste0("\\beta_1 = ", numtex(b_true, 1))))),
      sprintf("%d of the %d intervals contain the true value; %d do not.", n_dentro, B, B - n_dentro),
      "With a single sample we cannot know whether ours is one of the hits: we trust the procedure.")),
    8.3, 2.2, 4.5, 4.6, size = 17)

  # 7 · 5.1 Dualidad con los contrastes
  x <- diapo_contenido(x, tr("5.1 · Relación con los contrastes de hipótesis", "5.1 · Link with hypothesis tests"),
    tr("Contrastar con el intervalo: ¿está el 0?", "Testing with the interval: is 0 inside?"), tema)
  x <- formula_caja(x, paste0("H_0: \\beta_j = \\beta_j^{*} \\qquad H_1: \\beta_j \\neq \\beta_j^{*}"), 0.6, 2.05, 12.1, 0.95, size = 26)
  x <- caja_texto(x, 0.6, 3.3, 5.9, 1.85, tr("Valor dentro del intervalo", "Value inside the interval"),
    tr(paste0(M("\\beta_j^{*} \\in [L,\\ U]"), ": no se rechaza ", M("H_0"), ". Si ", M("\\beta_j^{*} = 0"), " está dentro, la variable no es significativa al nivel ", M("\\alpha"), "."),
       paste0(M("\\beta_j^{*} \\in [L,\\ U]"), ": ", M("H_0"), " is not rejected. If ", M("\\beta_j^{*} = 0"), " is inside, the variable is not significant at level ", M("\\alpha"), ".")),
    size = 17)
  x <- caja_texto(x, 6.8, 3.3, 5.9, 1.85, tr("Valor fuera del intervalo", "Value outside the interval"),
    tr(paste0(M("\\beta_j^{*} \\notin [L,\\ U]"), ": se rechaza ", M("H_0"), ". Si el 0 queda fuera, la variable es significativa al nivel ", M("\\alpha"), "."),
       paste0(M("\\beta_j^{*} \\notin [L,\\ U]"), ": ", M("H_0"), " is rejected. If 0 lies outside, the variable is significant at level ", M("\\alpha"), ".")),
    oscura = TRUE, size = 17)
  x <- poner_texto(x, parrafo(tr(paste0("El intervalo al ", M("1-\\alpha"), " y el contraste ", M("t"), " bilateral al nivel ", M("\\alpha"), " dan siempre el mismo veredicto: es una propiedad matemática, no una coincidencia."),
                                 paste0("The ", M("1-\\alpha"), " interval and the two-sided ", M("t"), " test at level ", M("\\alpha"), " always give the same verdict: it is a mathematical property, not a coincidence.")),
                              17, ID$navy, bold = TRUE), 0.6, 5.5, 12.1, 0.9)

  # 8 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Construir el intervalo", "Building the interval"),
    tr(c(paste0("Coeficiente ", M("\\pm\\, t"), " · error estándar"), "Paso a paso con el caso A", "Tres niveles de confianza", "Combinaciones lineales"),
       c(paste0("Coefficient ", M("\\pm\\, t"), " · standard error"), "Step by step with case A", "Three confidence levels", "Linear combinations")))

  # 9 · 5.2 Fórmula
  x <- diapo_contenido(x, tr("5.2 · Intervalo para un coeficiente individual", "5.2 · Interval for an individual coefficient"),
    tr("Estimación ± valor crítico × error estándar", "Estimate ± critical value × standard error"), tema)
  x <- formula_caja(x, paste0("\\hat{\\beta}_j \\pm t_{n-k-1,\\,\\alpha/2} \\cdot ", ES, "(\\hat{\\beta}_j)"), 0.6, 2.05, 12.1, 1.05, size = 30)
  anchos <- tr(list(c("Nivel de confianza", "A más confianza, mayor valor crítico y un intervalo más ancho."),
                    c("Error estándar", "A más dispersión del estimador, un intervalo más ancho y menos preciso."),
                    c("Tamaño muestral", "Con más observaciones, menor error estándar y más grados de libertad: más estrecho.")),
               list(c("Confidence level", "More confidence means a larger critical value and a wider interval."),
                    c("Standard error", "A more dispersed estimator gives a wider, less precise interval."),
                    c("Sample size", "More observations: smaller standard error and more degrees of freedom, so narrower.")))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 3.45, 3.85, 2.3, anchos[[k]][1], anchos[[k]][2], oscura = (k == 2), size = 16)
  x <- poner_texto(x, parrafo(tr(paste0(M("t_{n-k-1,\\,\\alpha/2}"), ": valor crítico de la t de Student con ", M("n-k-1"), " grados de libertad, el mismo del contraste bilateral del tema 4."),
                                 paste0(M("t_{n-k-1,\\,\\alpha/2}"), ": critical value of Student's t with ", M("n-k-1"), " degrees of freedom, the same as in the two-sided test of chapter 4.")),
                              16, ID$gris), 0.6, 6.0, 12.1, 0.8)

  # 10 · 5.2 Paso a paso
  x <- diapo_contenido(x, tr("5.2 · Ejemplo detallado: caso A", "5.2 · Worked example: case A"),
    tr("El intervalo del precio, paso a paso", "The price interval, step by step"), tema)
  x <- formula_caja(x, paste0(IC, "_{95\\%}(\\beta_1) = ", numtex(b_a[2], 4), " \\pm ", numtex(tc_a, 3), " \\times ", numtex(se_a[2], 4),
                              " = [", numtex(ic_a[[2]][2, 1], 4), ",\\ ", numtex(ic_a[[2]][2, 2], 4), "]"),
                    0.6, 2.05, 12.1, 1.05, size = 26)
  t_a <- summary(m_a)$coefficients[2, 3]; p_a <- summary(m_a)$coefficients[2, 4]
  x <- caja_dato(x, 0.6, 3.4, 3.85, 2.55, tr("Estimación", "Estimate"), M(paste0("\\hat{\\beta}_1 = ", numtex(b_a[2], 4))),
                 tr(paste0("Error estándar ", num(se_a[2], 4), "; ", sprintf("%d", nrow(caso_a)), " observaciones y 3 regresores."),
                    paste0("Standard error ", num(se_a[2], 4), "; ", sprintf("%d", nrow(caso_a)), " observations and 3 regressors.")), ID$navy)
  x <- caja_dato(x, 4.75, 3.4, 3.85, 2.55, tr("Valor crítico", "Critical value"), M(paste0("t_{", gl_a, ",\\,0", tr("{,}", "."), "025} = ", numtex(tc_a, 3))),
                 tr(sprintf("%d − 3 − 1 = %d grados de libertad, 2,5 %% en cada cola.", nrow(caso_a), gl_a),
                    sprintf("%d − 3 − 1 = %d degrees of freedom, 2.5 %% in each tail.", nrow(caso_a), gl_a)), ID$oro, size_valor = 26)
  x <- caja_dato(x, 8.9, 3.4, 3.85, 2.55, tr("Conclusión", "Conclusion"), M(paste0("0 \\notin ", IC, "_{95\\%}")),
                 tr(paste0("Ambos límites son negativos: se rechaza ", M("H_0: \\beta_1 = 0"), " al 5 %."),
                    paste0("Both limits are negative: ", M("H_0: \\beta_1 = 0"), " is rejected at 5 %.")), ID$gris_med, size_valor = 26)
  x <- poner_texto(x, list(
    parrafo(tr(paste0("Lectura económica: subir el precio 1 € resta entre ", num(abs(ic_a[[2]][2, 2]), 2), " y ", num(abs(ic_a[[2]][2, 1]), 2), " MM€ de ventas, con un 95 % de confianza."),
               paste0("Economic reading: raising the price by €1 cuts sales by between €", num(abs(ic_a[[2]][2, 2]), 2), "m and €", num(abs(ic_a[[2]][2, 1]), 2), "m, with 95 % confidence.")),
            17, ID$navy, bold = TRUE),
    parrafo(tr(paste0("Mismo veredicto que el contraste del tema 4: ", M(paste0("t = ", numtex(t_a, 2))), ", p-valor ", pval(p_a), "."),
               paste0("Same verdict as the chapter 4 test: ", M(paste0("t = ", numtex(t_a, 2))), ", p-value ", pval(p_a), ".")),
            16, ID$gris)), 0.6, 6.05, 12.1, 0.85)

  # 11 · 5.2 Tres niveles
  x <- diapo_contenido(x, tr("5.2 · Aplicación al caso A", "5.2 · Application to case A"),
    tr("Más confianza, intervalos más anchos", "More confidence, wider intervals"), tema)
  vars_a <- tr(c("Precio propio", "Publicidad", "Precio sustitutivo"), c("Own price", "Advertising", "Substitute price"))
  niveles <- c("90 %", "95 %", "99 %")
  tab <- do.call(rbind, lapply(1:3, function(v) do.call(rbind, lapply(1:3, function(l) {
    li <- ic_a[[l]][v + 1, 1]; ls <- ic_a[[l]][v + 1, 2]
    data.frame(var = vars_a[v], niv = niveles[l], coef = unname(b_a[v + 1]), li = li, ls = ls,
               cero = if (li <= 0 && ls >= 0) tr("Sí", "Yes") else "No")
  }))))
  ft <- tabla_pptx(tab, tr(c("Variable", "Nivel", "Coef.", "Lím. inf.", "Lím. sup.", "¿Contiene 0?"),
                           c("Variable", "Level", "Coef.", "Lower", "Upper", "Contains 0?")), digits = 4, size = 13, texto = 1:2)
  ft <- flextable::align(ft, j = 6, align = "center", part = "all")
  ft <- flextable::bold(ft, i = which(tab$cero != "No"), part = "body")
  ft <- flextable::color(ft, i = which(tab$cero != "No"), color = ID$oro, part = "body")
  x <- poner_tabla(x, ft, 0.6, 2.0, max_ancho = 7.7)
  x <- caja_texto(x, 8.7, 2.0, 4.0, 3.3, tr("Lectura", "Reading"),
    tr(sprintf("Precio y publicidad excluyen el 0 a los tres niveles. El precio del sustitutivo es significativo al 10 %% y al 5 %%, pero su intervalo al 99 %% ya incluye el 0: no lo es al 1 %%."),
       sprintf("Price and advertising exclude 0 at all three levels. The substitute price is significant at 10 %% and at 5 %%, but its 99 %% interval already includes 0: it is not significant at 1 %%.")),
    oscura = TRUE, size = 16)

  # 12 · 5.3 Combinación lineal
  x <- diapo_contenido(x, tr("5.3 · Intervalo para una combinación lineal", "5.3 · Interval for a linear combination"),
    tr("Efectos conjuntos: hace falta la covarianza", "Joint effects: the covariance is needed"), tema)
  x <- formula_caja(x, paste0("\\mathbf{c}'\\hat{\\boldsymbol{\\beta}} \\pm t_{n-k-1,\\,\\alpha/2} \\cdot \\sqrt{\\mathbf{c}'\\,\\widehat{\\text{Var}}(\\hat{\\boldsymbol{\\beta}})\\,\\mathbf{c}}",
                              "\\qquad \\widehat{\\text{Var}}(\\hat{\\boldsymbol{\\beta}}) = \\hat{\\sigma}^2(\\mathbf{X}'\\mathbf{X})^{-1}"),
                    0.6, 2.05, 12.1, 1.1, size = 26)
  x <- caja_dato(x, 0.6, 3.45, 5.9, 2.7, M(paste0("\\beta_1 + 0", tr("{,}", "."), "5\\,\\beta_2")),
                 M(paste0("[", numtex(cl1["li"], 2), ",\\ ", numtex(cl1["ls"], 2), "]")),
                 tr(paste0("Subir el precio 1 € y la publicidad 0,5 MM€: estimación ", num(cl1["est"], 4), ", error estándar ", num(cl1["se"], 4),
                           ". El 0 está dentro: los dos efectos se compensan."),
                    paste0("Raise the price by €1 and advertising by €0.5m: estimate ", num(cl1["est"], 4), ", standard error ", num(cl1["se"], 4),
                           ". 0 is inside: the two effects offset each other.")), ID$navy, size_valor = 26)
  x <- caja_dato(x, 6.8, 3.45, 5.9, 2.7, M("\\beta_1 + \\beta_2"),
                 M(paste0("[", numtex(cl2["li"], 2), ",\\ ", numtex(cl2["ls"], 2), "]")),
                 tr(paste0(M("\\mathbf{c}' = (0,\\,1,\\,1,\\,0)"), ": estimación ", num(cl2["est"], 4), ", error estándar ", num(cl2["se"], 4),
                           ". Como el precio resta, todo positivo: la publicidad pesa más, en valor absoluto, que el precio."),
                    paste0(M("\\mathbf{c}' = (0,\\,1,\\,1,\\,0)"), ": estimate ", num(cl2["est"], 4), ", standard error ", num(cl2["se"], 4),
                           ". Price enters with a minus sign, so all positive: advertising weighs more than price in absolute value.")), ID$oro, size_valor = 26)

  # 13 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Ver los intervalos y aplicarlos", "Seeing and applying intervals"),
    tr(c("El gráfico de intervalos", "De Gretl a R", "Los tres casos del manual"),
       c("The coefficient interval plot", "From Gretl to R", "The three cases of the book")))

  # 14 · 5.4 Gráfico de intervalos
  x <- diapo_contenido(x, tr("5.4 · Visualización de los intervalos", "5.4 · Visualising the intervals"),
    tr("Si el intervalo no cruza el 0, es significativa", "If the interval does not cross 0, it is significant"), tema)
  niv_g <- c("99 %", "95 %", "90 %")
  df_g <- do.call(rbind, lapply(3:1, function(l) data.frame(var = factor(vars_a, levels = rev(vars_a)), coef = unname(b_a[2:4]),
                                                          li = ic_a[[l]][2:4, 1], ls = ic_a[[l]][2:4, 2],
                                                          nivel = factor(c("90 %", "95 %", "99 %")[l], levels = niv_g))))
  g_ic <- ggplot(df_g, aes(y = var)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = COL$gris, linewidth = 0.8) +
    geom_segment(aes(x = li, xend = ls, yend = var, colour = nivel, linewidth = nivel)) +
    geom_point(data = df_g[df_g$nivel == "95 %", ], aes(x = coef), colour = COL$navy, size = 4) +
    scale_colour_manual(values = c("99 %" = "#C9D0DA", "95 %" = COL$oro_cl, "90 %" = COL$oro), name = tr("Nivel", "Level")) +
    scale_linewidth_manual(values = c("99 %" = 9, "95 %" = 5.5, "90 %" = 2.2), name = tr("Nivel", "Level")) +
    scale_x_continuous(labels = function(v) num(v, 1)) +
    labs(x = tr("Coeficiente estimado (caso A)", "Estimated coefficient (case A)"), y = NULL) +
    tema_cdea(14) + theme(panel.grid.major.y = element_blank())
  x <- imagen_ggplot(x, g_ic, 0.6, 1.95, 7.8, 4.85)
  x <- vinetas(x, tr(
    c("Punto: estimación. Barras: intervalos al 90 %, 95 % y 99 %.",
      "Lectura inmediata de magnitud, precisión y significatividad.",
      "El precio del sustitutivo roza el 0 solo con el intervalo al 99 %."),
    c("Dot: estimate. Bars: 90 %, 95 % and 99 % intervals.",
      "An immediate reading of size, precision and significance.",
      "The substitute price only touches 0 with the 99 % interval.")),
    8.7, 2.2, 4.1, 4.5, size = 17)

  # 15 · Gretl → R
  x <- diapo_contenido(x, tr("De Gretl a R", "From Gretl to R"),
    tr("Intervalos con Gretl y con R", "Intervals with Gretl and with R"), tema)
  sw <- data.frame(
    op = tr(c("Intervalos de los coeficientes", "Cambiar el nivel de confianza", "Combinación lineal", "Matriz de varianzas-covarianzas"),
            c("Coefficient intervals", "Change the confidence level", "Linear combination", "Variance-covariance matrix")),
    gretl = tr(c("Análisis → Intervalos de confianza para los coeficientes", "En esa ventana, botón α (o Herramientas → Preferencias)", "Contrastes → Restricciones lineales", "Análisis → Matriz de covarianzas de los coeficientes"),
               c("Analysis → Confidence intervals for coefficients", "In that window, the α button (or Tools → Preferences)", "Tests → Linear restrictions", "Analysis → Coefficient covariance matrix")),
    r = tr(c("confint(modelo)", "confint(modelo, level = 0.90)", "car::linearHypothesis()", "vcov(modelo)"),
           c("confint(model)", "confint(model, level = 0.90)", "car::linearHypothesis()", "vcov(model)")))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 15, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c(paste0("A mano: ", M("\\mathbf{c}'\\hat{\\beta}"), " con coef(modelo) y su error estándar con sqrt(t(c) %*% vcov(modelo) %*% c)."),
      "El Excel de clase repite el cálculo con matrices para ver de dónde sale cada número."),
    c(paste0("By hand: ", M("\\mathbf{c}'\\hat{\\beta}"), " with coef(model) and its standard error with sqrt(t(c) %*% vcov(model) %*% c)."),
      "The class Excel repeats the calculation with matrices to show where each number comes from.")),
    0.6, 4.2, 12.1, 1.8, size = 17)

  # 16 · 5.5 Casos
  x <- diapo_contenido(x, tr("5.5 · Aplicación a los casos prácticos", "5.5 · Application to the case studies"),
    tr("Los tres casos del manual, con intervalos", "The three cases of the book, with intervals"), tema)
  casos <- tr(list(
      c("A · Demanda", paste0("Precio: [", num(ic_a[[2]][2, 1], 2), "; ", num(ic_a[[2]][2, 2], 2), "]. Publicidad: [", num(ic_a[[2]][3, 1], 2), "; ", num(ic_a[[2]][3, 2], 2),
                              "]. Ninguno contiene el 0: efectos significativos y estimados con precisión.")),
      c("B · Cifra de negocio", paste0("Trabajadores: [", num(ic_b[2, 1], 2), "; ", num(ic_b[2, 2], 2), "]. Inversión: [", num(ic_b[3, 1], 2), "; ", num(ic_b[3, 2], 2),
                                       "]. Los tres intervalos contienen el 0 y son muy anchos: estimaciones imprecisas.")),
      c("C · Consumo", paste0("PMC: [", num(ic_c[2, 1], 4), "; ", num(ic_c[2, 2], 4), "]. Positiva, menor que 1 y estrecha: respalda a Keynes. El tipo de interés contiene el 0."))),
    list(
      c("A · Demand", paste0("Price: [", num(ic_a[[2]][2, 1], 2), "; ", num(ic_a[[2]][2, 2], 2), "]. Advertising: [", num(ic_a[[2]][3, 1], 2), "; ", num(ic_a[[2]][3, 2], 2),
                             "]. Neither contains 0: significant effects, precisely estimated.")),
      c("B · Turnover", paste0("Workers: [", num(ic_b[2, 1], 2), "; ", num(ic_b[2, 2], 2), "]. Investment: [", num(ic_b[3, 1], 2), "; ", num(ic_b[3, 2], 2),
                               "]. All three intervals contain 0 and are very wide: imprecise estimates.")),
      c("C · Consumption", paste0("MPC: [", num(ic_c[2, 1], 4), "; ", num(ic_c[2, 2], 4), "]. Positive, below 1 and narrow: it supports Keynes. The interest rate interval contains 0."))))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 2.1, 3.85, 3.0, casos[[k]][1], casos[[k]][2], oscura = (k == 3), size = 17)
  x <- poner_texto(x, parrafo(tr("Intervalos al 95 %. En el caso B, el tamaño de la empresa mueve a la vez trabajadores, inversión y superficie (tema 4).",
                                 "95 % intervals. In case B, firm size moves workers, investment and land area together (chapter 4)."),
                              17, ID$navy, bold = TRUE), 0.6, 5.5, 12.1, 0.9)

  # 17 · Cierre
  x <- diapo_cierre(x,
    tr(c("Un intervalo da magnitud y precisión, no solo un «sí o no».",
         paste0(M(paste0("\\hat{\\beta}_j \\pm t_{n-k-1,\\,\\alpha/2} \\cdot ", ES, "(\\hat{\\beta}_j)")), ": la fórmula de siempre."),
         paste0("El 95 % es del método: no es la probabilidad de que ", M("\\beta_j"), " esté dentro."),
         paste0("Si el 0 queda fuera, la variable es significativa al nivel ", M("\\alpha"), "."),
         "Más confianza o más dispersión, intervalos más anchos.",
         paste0("Para combinaciones lineales, ", M("\\mathbf{c}'\\hat{\\beta}"), " y la matriz de covarianzas.")),
       c("An interval gives size and precision, not just a «yes or no».",
         paste0(M(paste0("\\hat{\\beta}_j \\pm t_{n-k-1,\\,\\alpha/2} \\cdot ", ES, "(\\hat{\\beta}_j)")), ": the formula to remember."),
         paste0("The 95 % belongs to the method: it is not the probability that ", M("\\beta_j"), " is inside."),
         paste0("If 0 lies outside, the variable is significant at level ", M("\\alpha"), "."),
         "More confidence or more dispersion means wider intervals.",
         paste0("For linear combinations, ", M("\\mathbf{c}'\\hat{\\beta}"), " and the covariance matrix."))),
    tr("Excel de clase del tema 5, scripts de R del tema 5 y app «Intervalos de confianza». Explicación completa en el capítulo 5 del manual.",
       "Chapter 5 class Excel, Chapter 5 R scripts and the «Confidence intervals» app. Full explanation in Chapter 5 of the book."),
    tema)

  guardar_pptx(x, "T05_mEC_Intervalos.pptx", "T05_mEC_Confidence_Intervals.pptx")
}
