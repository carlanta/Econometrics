# ══════════════════════════════════════════════════════════════════
# Capítulo 06 · Problemas de especificación · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap06_pptx.R
# Contenido y numeración: 06-problemas-especificacion.qmd (secciones 6.1 a 6.6). Modelos como en el capítulo:
#   Omisión (6.3): ventas ~ precio_propio + gasto_publicidad frente a ventas ~ precio_propio (caso A)
#   Casos (6.6): A con 1–4 regresores; B trabajadores (+ inversión, + superficie); C pib (+ tipo de interés, + inflación)
#   Variables de más (6.2): se ilustra con el caso B de 6.6.2, donde inversión y superficie no aportan nada (p ≈ 0,97)
# Titulares de UNA línea (≈ 45 caracteres). Cifras calculadas aquí con los datos de data/.
# ══════════════════════════════════════════════════════════════════
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  # Espacio duro entre número y unidad en todo el texto (envuelve runs_texto de los comunes sin modificarlos)
  runs_texto_base <- runs_texto
  # (y signo menos tipográfico, que no parte la línea, en los números negativos; guion que no parte en «p-valor»)
  runs_texto <- function(texto, ...) {
    texto <- gsub("([0-9]) (%|€|MM€|M€|EUR)", "\\1\u00a0\\2", texto)
    texto <- gsub("(^|[ (\\[;:])-([0-9])", "\\1\u2212\\2", texto, perl = TRUE)
    texto <- gsub("p-val", "p\u2011val", texto, fixed = TRUE)
    runs_texto_base(texto, ...)
  }
  tema <- tr("Tema 6", "Chapter 6")
  ES <- tr("\\text{ES}", "\\text{SE}")

  # ── Modelos y cifras del capítulo ──────────────────────────────────
  r2a <- function(m) summary(m)$adj.r.squared
  cf  <- function(m, v) summary(m)$coefficients[v, ]
  # Caso A: omisión de la publicidad (6.3) y cuatro especificaciones (6.6.1)
  a1 <- lm(ventas ~ precio_propio, data = caso_a)
  a2 <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
  a3 <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
  a4 <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit + temporada, data = caso_a)
  aux <- lm(gasto_publicidad ~ precio_propio, data = caso_a)
  b2_a <- coef(a2)[["gasto_publicidad"]]; d21 <- coef(aux)[["precio_propio"]]
  sesgo_teo <- b2_a * d21; sesgo_obs <- coef(a1)[["precio_propio"]] - coef(a2)[["precio_propio"]]
  cor_pp <- cor(caso_a$precio_propio, caso_a$gasto_publicidad)
  # Caso B: trabajadores con regresores de más (6.6.2)
  b1 <- lm(cifra_negocio ~ trabajadores, data = caso_b)
  b2 <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
  b3 <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)
  mods_b <- list(b1, b2, b3)
  se_b <- sapply(mods_b, function(m) cf(m, "trabajadores")[2])
  p_b  <- sapply(mods_b, function(m) cf(m, "trabajadores")[4])
  p_inv <- cf(b3, "inversion")[4]; p_sup <- cf(b3, "superficie")[4]
  # Caso C: PMC con y sin tipo de interés e inflación (6.6.3)
  c1 <- lm(consumo ~ pib, data = caso_c)
  c2 <- lm(consumo ~ pib + tipo_interes, data = caso_c)
  c3 <- lm(consumo ~ pib + tipo_interes + inflacion, data = caso_c)
  cor_pi <- cor(caso_c$pib, caso_c$tipo_interes); sesgo_c <- coef(c1)[["pib"]] - coef(c2)[["pib"]]

  # Intervalos de un coeficiente en varios modelos, para los gráficos
  graf_ic <- function(mods, v, etiquetas, xlab, colores) {
    d <- do.call(rbind, lapply(seq_along(mods), function(k) {
      ci <- confint(mods[[k]])[v, ]
      data.frame(mod = etiquetas[k], b = coef(mods[[k]])[[v]], li = ci[1], ls = ci[2])
    }))
    d$mod <- factor(d$mod, levels = rev(etiquetas))
    ggplot(d, aes(y = mod)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = COL$gris, linewidth = 0.8) +
      geom_segment(aes(x = li, xend = ls, yend = mod, colour = mod), linewidth = 5, alpha = 0.45) +
      geom_point(aes(x = b, colour = mod), size = 4.5) +
      geom_text(aes(x = b, label = num(b, 4)), vjust = -1.5, size = 4.6, colour = "#1B2430") +
      scale_colour_manual(values = setNames(colores, rev(etiquetas)), guide = "none") +
      scale_x_continuous(labels = function(z) num(z, 2)) +
      labs(x = xlab, y = NULL) + tema_cdea(14) + theme(panel.grid.major.y = element_blank())
  }

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 6, tr("Problemas de especificación", "Specification problems"),
    tr("¿Qué es peor: que en el modelo sobre una variable o que falte?",
       "What is worse: a variable too many in the model, or one missing?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("6.1", "6.2", "6.3", "6.4", "6.5", "6.6"),
    titulo = tr(c("Importancia de la correcta especificación", "Inclusión de variables irrelevantes", "Omisión de variables relevantes",
                  "Pasos para detectar problemas de especificación", "Variables no observadas y variables proxy", "Aplicación a los casos prácticos"),
                c("Why correct specification matters", "Including irrelevant variables", "Omitting relevant variables",
                  "Steps to detect specification problems", "Unobserved variables and proxy variables", "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Distinguir los dos errores", "Variables de más: estimadores insesgados pero menos precisos. Variables de menos: estimadores sesgados."),
       c("Tell the two errors apart", "Too many variables: unbiased but less precise estimators. Too few: biased estimators.")),
    tr(c("Medir el sesgo", "Su tamaño y su signo con la regresión auxiliar de la variable omitida sobre la incluida."),
       c("Measure the bias", "Its size and sign from the auxiliary regression of the omitted variable on the included one.")),
    tr(c("Diagnosticar el modelo", paste0("Contrastes, intervalos, ", M("\\bar{R}^2"), ", estabilidad de los coeficientes y residuos; proxies si falta un dato.")),
       c("Diagnose the model", paste0("Tests, intervals, ", M("\\bar{R}^2"), ", coefficient stability and residuals; proxies when data are missing.")))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Especificar bien: variables de más", "Specifying well: too many variables"),
    tr(c("Qué entra en el modelo", "Dos errores asimétricos", "Insesgado, pero menos preciso"),
       c("What goes into the model", "Two asymmetric errors", "Unbiased, but less precise")))

  # 5 · 6.1 Asimetría
  x <- diapo_contenido(x, tr("6.1 · Importancia de la correcta especificación", "6.1 · Why correct specification matters"),
    tr("Dos errores con consecuencias muy distintas", "Two errors with very different consequences"), tema)
  x <- caja_texto(x, 0.6, 2.05, 5.9, 3.1, tr("Incluir una variable irrelevante", "Including an irrelevant variable"),
    tr("Los estimadores siguen siendo insesgados, pero pierden eficiencia: más varianza, intervalos más anchos y contrastes menos potentes.",
       "Estimators remain unbiased but lose efficiency: more variance, wider intervals and less powerful tests."), size = 18)
  x <- caja_texto(x, 6.8, 2.05, 5.9, 3.1, tr("Omitir una variable relevante", "Omitting a relevant variable"),
    tr("Los estimadores pueden ser sesgados e inconsistentes: el error no desaparece con más datos y contamina las conclusiones sobre las demás variables.",
       "Estimators may be biased and inconsistent: the error does not vanish with more data and contaminates the conclusions on the other variables."),
    oscura = TRUE, size = 18)
  x <- poner_texto(x, list(
    parrafo(tr("Omitir es más grave que incluir de más.", "Omitting is more serious than including too much."), 22, ID$navy, bold = TRUE, font = "Georgia"),
    parrafo(tr("En la duda, mejor pecar por exceso; pero buscando siempre el modelo más parsimonioso que recoja los efectos relevantes.",
               "When in doubt, err on the side of inclusion; but always seek the most parsimonious model that captures the relevant effects."), 17, ID$gris)),
    0.6, 5.45, 12.1, 1.4)

  # 6 · 6.2 Teoría
  x <- diapo_contenido(x, tr("6.2 · Inclusión de variables irrelevantes", "6.2 · Including irrelevant variables"),
    tr("Una variable de más: sin sesgo, más varianza", "One variable too many: no bias, more variance"), tema)
  x <- poner_texto(x, parrafo(tr("Modelo verdadero", "True model"), 14, ID$gris, bold = TRUE), 0.6, 1.95, 6, 0.35)
  x <- formula_caja(x, "Y_i = \\beta_0 + \\beta_1 X_{1i} + u_i", 0.6, 2.3, 6.0, 0.9, size = 26)
  x <- poner_texto(x, parrafo(tr(paste0("Modelo estimado, con ", M("X_2"), " irrelevante (", M("\\beta_2 = 0"), ")"),
                                 paste0("Estimated model, with an irrelevant ", M("X_2"), " (", M("\\beta_2 = 0"), ")")), 14, ID$gris, bold = TRUE), 0.6, 3.4, 6.2, 0.35)
  x <- formula_caja(x, "Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + u_i", 0.6, 3.75, 6.0, 0.9, size = 26)
  x <- poner_texto(x, parrafo(tr("Por qué crece la varianza", "Why the variance grows"), 14, ID$gris, bold = TRUE), 0.6, 4.85, 6.2, 0.35)
  x <- formula_caja(x, "\\text{Var}(\\hat{\\beta}_j) = \\sigma^2 \\left[(\\mathbf{X}'\\mathbf{X})^{-1}\\right]_{jj}", 0.6, 5.2, 6.0, 0.9, size = 24)
  x <- poner_texto(x, parrafo(tr(paste0("Una columna más en ", M("\\mathbf{X}"), " correlacionada con ", M("X_1"), " agranda ese elemento."),
                                 paste0("An extra column in ", M("\\mathbf{X}"), " correlated with ", M("X_1"), " enlarges that element.")),
                              14, ID$gris, italic = TRUE), 0.6, 6.2, 6.2, 0.6)
  x <- vinetas(x, tr(
    c(paste0("Insesgadez: ", M("E(\\hat{\\beta}_1) = \\beta_1"), " y ", M("E(\\hat{\\beta}_2) = 0"), "."),
      paste0("Pérdida de eficiencia: ", M("\\text{Var}(\\hat{\\beta}_1)"), " aumenta y los errores estándar crecen."),
      paste0("Contrastes menos potentes: cuesta más rechazar ", M("H_0"), " aunque la variable sí importe."),
      paste0("Detección: ", M("t"), " no significativa, intervalo que contiene el 0 y ", M("\\bar{R}^2"), " que no mejora.")),
    c(paste0("Unbiasedness: ", M("E(\\hat{\\beta}_1) = \\beta_1"), " and ", M("E(\\hat{\\beta}_2) = 0"), "."),
      paste0("Loss of efficiency: ", M("\\text{Var}(\\hat{\\beta}_1)"), " rises and standard errors grow."),
      paste0("Less powerful tests: it is harder to reject ", M("H_0"), " even when the variable matters."),
      paste0("Detection: non-significant ", M("t"), ", an interval containing 0 and an ", M("\\bar{R}^2"), " that does not improve."))),
    7.1, 2.0, 5.7, 4.8, size = 17)

  # 7 · 6.2 Caso B
  et_b <- tr(c("Solo trabajadores", "+ inversión", "+ superficie"), c("Workers only", "+ investment", "+ land area"))
  x <- diapo_contenido(x, tr("6.2 · Variables irrelevantes en el caso B", "6.2 · Irrelevant variables in case B"),
    tr("Igual coeficiente, cinco veces más error", "Same coefficient, five times the error"), tema)
  g_b <- graf_ic(mods_b, "trabajadores", et_b, tr("Coeficiente de trabajadores e intervalo al 95 %", "Workers coefficient and 95 % interval"),
                 c(COL$oro, COL$navy2, COL$navy))
  x <- imagen_ggplot(x, g_b, 0.6, 1.95, 7.6, 4.85)
  x <- caja_dato(x, 8.6, 2.0, 4.1, 2.25, tr("Error estándar", "Standard error"),
                 paste0(num(se_b[1], 4), " → ", num(se_b[3], 4)),
                 tr(sprintf("Se multiplica por %s; el coeficiente apenas se mueve.", num(se_b[3] / se_b[1], 1)),
                    sprintf("Multiplied by %s; the coefficient barely moves.", num(se_b[3] / se_b[1], 1))), ID$navy, size_valor = 24)
  x <- caja_dato(x, 8.6, 4.45, 4.1, 2.3, tr("Contraste menos potente", "A less powerful test"),
                 paste0(pval(p_b[1]), " → ", num(p_b[3], 3)),
                 tr(paste0("p-valor de trabajadores: deja de ser significativa. Inversión y superficie, p-valor de ", num(min(p_inv, p_sup), 2), "; ", M("\\bar{R}^2"), " baja de ", num(r2a(b1), 3), " a ", num(r2a(b3), 3), "."),
                    paste0("Workers p-value: it stops being significant. Investment and land area, p-value of ", num(min(p_inv, p_sup), 2), "; ", M("\\bar{R}^2"), " falls from ", num(r2a(b1), 3), " to ", num(r2a(b3), 3), ".")),
                 ID$oro, size_valor = 24)

  # 8 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Variables de menos: el sesgo", "Too few variables: the bias"),
    tr(c("Sesgo e inconsistencia", "La dirección del sesgo", "El caso A sin publicidad"),
       c("Bias and inconsistency", "The direction of the bias", "Case A without advertising")))

  # 9 · 6.3 Teoría
  x <- diapo_contenido(x, tr("6.3 · Omisión de variables relevantes", "6.3 · Omitting relevant variables"),
    tr("Lo que falta acaba en el error, y sesga", "What is left out ends up in the error, and biases"), tema)
  x <- formula_caja(x, "Y_i = \\beta_0 + \\beta_1 X_{1i} + v_i, \\qquad v_i = \\beta_2 X_{2i} + u_i", 0.6, 2.05, 6.6, 0.95, size = 24)
  x <- formula_caja(x, "E(\\hat{\\beta}_1^{*}) = \\beta_1 + \\beta_2 \\cdot \\delta_{21}", 0.6, 3.2, 6.6, 0.95, size = 26)
  x <- vinetas(x, tr(
    c(paste0(M("\\delta_{21}"), ": pendiente de la regresión auxiliar de ", M("X_2"), " sobre ", M("X_1"), "."),
      "Inconsistente: el sesgo no desaparece al crecer la muestra.",
      paste0("Si ", M("X_2"), " no está correlacionada con ", M("X_1"), ", no hay sesgo en ", M("\\hat{\\beta}_1"), ".")),
    c(paste0(M("\\delta_{21}"), ": slope of the auxiliary regression of ", M("X_2"), " on ", M("X_1"), "."),
      "Inconsistent: the bias does not vanish as the sample grows.",
      paste0("If ", M("X_2"), " is uncorrelated with ", M("X_1"), ", there is no bias in ", M("\\hat{\\beta}_1"), "."))),
    0.6, 4.4, 6.6, 2.4, size = 16)
  dir <- data.frame(
    cor = tr(c("Positiva", "Negativa"), c("Positive", "Negative")),
    pos = tr(c("Positivo", "Negativo"), c("Positive", "Negative")),
    neg = tr(c("Negativo", "Positivo"), c("Negative", "Positive")))
  x <- poner_texto(x, parrafo(tr(paste0("Signo del sesgo según la correlación de ", M("X_1"), " y ", M("X_2")),
                                 paste0("Sign of the bias by the correlation of ", M("X_1"), " and ", M("X_2"))), 16, ID$navy, bold = TRUE), 7.6, 2.0, 5.2, 0.45)
  ft_dir <- tabla_pptx(dir, tr(c("Correlación", "X2 eleva Y", "X2 reduce Y"), c("Correlation", "X2 raises Y", "X2 lowers Y")), size = 16, texto = 1:3)
  ft_dir <- flextable::width(ft_dir, j = 1:3, width = c(1.7, 1.65, 1.65))
  x <- ph_with(x, value = ft_dir, location = ph_location(left = 7.6, top = 2.5))
  x <- caja_texto(x, 7.6, 4.3, 5.1, 2.0, tr("Regla", "Rule"),
    tr(paste0("Signo del sesgo = signo de ", M("\\beta_2"), " × signo de ", M("\\delta_{21}"), ". Sesgo positivo: el efecto se sobreestima; negativo: se infraestima."),
       paste0("Sign of the bias = sign of ", M("\\beta_2"), " × sign of ", M("\\delta_{21}"), ". Positive bias: the effect is overstated; negative: understated.")), oscura = TRUE, size = 17)

  # 10 · 6.3 Caso A
  et_a <- tr(c("Completo (precio + publicidad)", "Sin publicidad"), c("Full (price + advertising)", "Without advertising"))
  x <- diapo_contenido(x, tr("6.3 · Omisión en el caso A", "6.3 · Omission in case A"),
    tr("Sin la publicidad, el precio parece pesar más", "Without advertising, price seems to weigh more"), tema)
  g_a <- graf_ic(list(a2, a1), "precio_propio", et_a, tr("Coeficiente del precio propio e intervalo al 95 %", "Own-price coefficient and 95 % interval"),
                 c(COL$oro, COL$navy))
  x <- imagen_ggplot(x, g_a, 0.6, 1.95, 7.6, 4.85)
  x <- vinetas(x, tr(
    c(paste0("Coeficiente del precio: de ", num(coef(a2)[["precio_propio"]], 4), " a ", num(coef(a1)[["precio_propio"]], 4), "."),
      paste0("La publicidad sube las ventas (", M("\\hat{\\beta}_2 > 0"), ") y se mueve en contra del precio (correlación ", num(cor_pp, 3), ")."),
      paste0("El precio absorbe parte de su efecto: sesgo negativo, efecto exagerado."),
      paste0(M("\\bar{R}^2"), " cae de ", num(r2a(a2), 3), " a ", num(r2a(a1), 3), ".")),
    c(paste0("Price coefficient: from ", num(coef(a2)[["precio_propio"]], 4), " to ", num(coef(a1)[["precio_propio"]], 4), "."),
      paste0("Advertising raises sales (", M("\\hat{\\beta}_2 > 0"), ") and moves against price (correlation ", num(cor_pp, 3), ")."),
      paste0("Price absorbs part of its effect: negative bias, an exaggerated effect."),
      paste0(M("\\bar{R}^2"), " falls from ", num(r2a(a2), 3), " to ", num(r2a(a1), 3), "."))),
    8.6, 2.1, 4.2, 4.7, size = 16)

  # 11 · 6.3 Sesgo paso a paso
  x <- diapo_contenido(x, tr("6.3 · El sesgo, paso a paso", "6.3 · The bias, step by step"),
    tr("La fórmula del sesgo cuadra al céntimo", "The bias formula adds up exactly"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 2.2, tr("1 · Efecto de la omitida", "1 · Effect of the omitted"), M(paste0("\\hat{\\beta}_2 = ", numtex(b2_a, 4))),
                 tr("Publicidad en el modelo completo.", "Advertising in the full model."), ID$navy, size_valor = 26)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 2.2, tr("2 · Regresión auxiliar", "2 · Auxiliary regression"), M(paste0("\\hat{\\delta}_{21} = ", numtex(d21, 4))),
                 tr("Publicidad sobre precio.", "Advertising on price."), ID$navy2, size_valor = 26)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 2.2, tr("3 · Sesgo", "3 · Bias"), M(paste0("\\hat{\\beta}_2 \\cdot \\hat{\\delta}_{21} = ", numtex(sesgo_teo, 4))),
                 tr("Negativo: positivo × negativo.", "Negative: positive × negative."), ID$oro, size_valor = 24)
  x <- formula_caja(x, paste0(numtex(coef(a1)[["precio_propio"]], 4), " - (", numtex(coef(a2)[["precio_propio"]], 4), ") = ", numtex(sesgo_obs, 4)),
                    0.6, 4.55, 12.1, 0.95, size = 26)
  x <- poner_texto(x, parrafo(tr("Diferencia observada entre el modelo sin publicidad y el completo: idéntica al sesgo calculado.",
                                 "Observed difference between the model without advertising and the full one: identical to the computed bias."),
                              17, ID$gris, italic = TRUE, align = "center"), 0.6, 5.7, 12.1, 0.6)

  # 12 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Diagnosticar y corregir", "Diagnosing and correcting"),
    tr(c("Cinco pasos de diagnóstico", "Variables proxy", "De Gretl a R", "Los tres casos del manual"),
       c("Five diagnostic steps", "Proxy variables", "From Gretl to R", "The three cases of the book")))

  # 13 · 6.4 Cinco pasos
  x <- diapo_contenido(x, tr("6.4 · Pasos para detectar problemas", "6.4 · Steps to detect problems"),
    tr("Cinco pasos para revisar la especificación", "Five steps to review the specification"), tema)
  pasos <- tr(list(c("Modelo completo", "Todas las variables que sugiere la teoría."),
                   c("Significación", paste0("Contraste ", M("t"), " de cada variable (tema 4).")),
                   c("Comparar modelos", paste0(M("\\bar{R}^2"), " de los modelos anidados (tema 3).")),
                   c("Estabilidad", "Si un coeficiente cambia mucho al quitar otra variable, hay omisión."),
                   c("Residuos", "Patrones que delatan omisiones o forma funcional (tema 7).")),
              list(c("Full model", "All the variables suggested by theory."),
                   c("Significance", paste0(M("t"), " test of each variable (chapter 4).")),
                   c("Compare models", paste0(M("\\bar{R}^2"), " of the nested models (chapter 3).")),
                   c("Stability", "If a coefficient changes a lot when another variable is dropped, something is omitted."),
                   c("Residuals", "Patterns that reveal omissions or functional form (chapter 7).")))
  for (k in 1:5) {
    left <- 0.6 + (k - 1) * 2.47
    x <- rect(x, left, 2.15, 2.27, 3.35, if (k %in% c(3, 4)) ID$navy else ID$suave)
    x <- rect(x, left, 2.15, 2.27, 0.07, ID$oro)
    x <- poner_texto(x, list(parrafo(sprintf("%d", k), 34, ID$oro, bold = TRUE, font = "Georgia"),
                             parrafo(pasos[[k]][1], 17, if (k %in% c(3, 4)) ID$blanco else ID$navy, bold = TRUE),
                             parrafo(pasos[[k]][2], 16, if (k %in% c(3, 4)) ID$linea else ID$texto)),
                     left + 0.2, 2.35, 1.95, 3.1)
  }
  x <- poner_texto(x, parrafo(tr("Variable que no aporta: fuera, con contraste e intervalo. Variable que mueve a las demás: dentro.",
                                 "A variable that adds nothing: out, with test and interval. A variable that moves the others: in."),
                              18, ID$navy, bold = TRUE), 0.6, 5.85, 12.1, 0.8)

  # 14 · 6.5 Proxy
  x <- diapo_contenido(x, tr("6.5 · Variables no observadas y proxies", "6.5 · Unobserved variables and proxies"),
    tr("Cuando la variable relevante no se observa", "When the relevant variable is not observed"), tema)
  x <- vinetas(x, tr(
    c("Muchas omisiones no son un descuido: la variable no se puede medir.",
      "Variable proxy: una variable observable muy correlacionada con la no observada.",
      "Reduce el sesgo por omisión, pero no lo elimina: todo depende de la calidad de la proxy."),
    c("Many omissions are not carelessness: the variable cannot be measured.",
      "Proxy variable: an observable variable highly correlated with the unobserved one.",
      "It reduces omitted-variable bias but does not remove it: everything depends on the quality of the proxy.")),
    0.6, 2.05, 6.4, 3.0, size = 17)
  x <- caja_texto(x, 0.6, 5.0, 6.4, 1.75, tr("Ejemplos", "Examples"),
    tr("Habilidad innata del trabajador en una ecuación de salarios; alfabetización de la población aproximada por la proporción de universitarios.",
       "A worker's innate ability in a wage equation; population literacy approximated by the share of university graduates."), size = 15)
  x <- rect(x, 7.4, 2.05, 5.3, 4.7, ID$navy)
  cond <- tr(c("Está muy correlacionada con la variable no observada.",
               paste0("No tiene efecto directo sobre ", M("Y"), " más allá del que ya recoge la variable no observada.")),
             c("It is highly correlated with the unobserved variable.",
               paste0("It has no direct effect on ", M("Y"), " beyond the one already captured by the unobserved variable.")))
  x <- poner_texto(x, list(parrafo(tr("Una buena proxy cumple dos condiciones", "A good proxy meets two conditions"), 20, ID$oro, bold = TRUE, font = "Georgia", espacio = 14),
                           parrafo("1", 30, ID$oro, bold = TRUE, font = "Georgia", espacio = 2),
                           parrafo(cond[1], 18, ID$blanco, espacio = 14),
                           parrafo("2", 30, ID$oro, bold = TRUE, font = "Georgia", espacio = 2),
                           parrafo(cond[2], 18, ID$blanco)),
                   7.7, 2.3, 4.8, 4.3)

  # 15 · Gretl → R
  x <- diapo_contenido(x, tr("De Gretl a R", "From Gretl to R"),
    tr("Comparar especificaciones con Gretl y con R", "Comparing specifications with Gretl and R"), tema)
  sw <- data.frame(
    op = tr(c("Significatividad individual", "R² ajustado", "Quitar variables del modelo", "Añadir variables al modelo", "Regresión auxiliar"),
            c("Individual significance", "Adjusted R²", "Drop variables from the model", "Add variables to the model", "Auxiliary regression")),
    gretl = tr(c("Ventana de resultados MCO", "Ventana de resultados MCO", "Contrastes → Omitir variables", "Contrastes → Añadir variables", "Modelo → Mínimos cuadrados ordinarios"),
               c("OLS output window", "OLS output window", "Tests → Omit variables", "Tests → Add variables", "Model → Ordinary least squares")),
    r = tr(c("summary(modelo)", "summary(modelo)$adj.r.squared", "anova(m_restringido, m_completo)", "update(modelo, . ~ . + x3)", "lm(x2 ~ x1, data = datos)"),
           c("summary(model)", "summary(model)$adj.r.squared", "anova(m_restricted, m_full)", "update(model, . ~ . + x3)", "lm(x2 ~ x1, data = data)")))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 15, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c("Omitir y añadir variables en Gretl hacen el contraste F de los modelos anidados: el mismo que anova() en R.",
      "Sin Excel de clase en este tema: los cálculos son los de los temas 3 y 4."),
    c("Omitting and adding variables in Gretl run the F test of nested models: the same as anova() in R.",
      "No class Excel in this chapter: the calculations are those of chapters 3 and 4.")),
    0.6, 4.75, 12.1, 1.9, size = 17)

  # 16 · 6.6 Caso A
  x <- diapo_contenido(x, tr("6.6 · Caso A: demanda de un producto", "6.6 · Case A: product demand"),
    tr("La publicidad es la variable que no puede faltar", "Advertising is the variable that cannot be missing"), tema)
  mods_a <- list(a1, a2, a3, a4)
  tab_a <- data.frame(
    mod = tr(c("Solo precio", "+ publicidad", "+ precio sustitutivo", "+ temporada"), c("Price only", "+ advertising", "+ substitute price", "+ season")),
    b = sapply(mods_a, function(m) coef(m)[["precio_propio"]]),
    se = sapply(mods_a, function(m) cf(m, "precio_propio")[2]),
    r2 = sapply(mods_a, r2a))
  ft <- tabla_pptx(tab_a, tr(c("Modelo", "Coef. precio", "ES precio", "R² ajustado"), c("Model", "Price coef.", "Price SE", "Adjusted R²")), digits = 4, size = 18)
  x <- poner_tabla(x, ft, 0.6, 2.1, max_ancho = 7.4)
  x <- caja_texto(x, 8.3, 2.1, 4.4, 4.0, tr("Lectura", "Reading"),
    tr(paste0("El gran salto llega con la publicidad: el coeficiente pasa de ", num(coef(a1)[["precio_propio"]], 2), " a ", num(coef(a2)[["precio_propio"]], 2),
              " y el ", M("\\bar{R}^2"), " de ", num(r2a(a1), 2), " a ", num(r2a(a2), 2), ". El sustitutivo y la temporada lo mueven menos (", num(coef(a3)[["precio_propio"]], 2), " y ",
              num(coef(a4)[["precio_propio"]], 2), ") y mejoran algo el ajuste."),
       paste0("The big jump comes with advertising: the coefficient moves from ", num(coef(a1)[["precio_propio"]], 2), " to ", num(coef(a2)[["precio_propio"]], 2),
              " and the ", M("\\bar{R}^2"), " from ", num(r2a(a1), 2), " to ", num(r2a(a2), 2), ". The substitute and the season move it less (", num(coef(a3)[["precio_propio"]], 2), " and ",
              num(coef(a4)[["precio_propio"]], 2), ") and improve the fit somewhat.")),
    oscura = TRUE, size = 18)
  x <- poner_texto(x, parrafo(tr("Un cambio grande del coeficiente al añadir una variable indica que su omisión causaba sesgo.",
                                 "A large change in the coefficient when a variable is added signals that omitting it caused bias."),
                              18, ID$navy, italic = TRUE), 0.6, 4.9, 7.3, 1.2)

  # 17 · 6.6 Casos B y C
  x <- diapo_contenido(x, tr("6.6 · Casos B y C", "6.6 · Cases B and C"),
    tr("Coeficientes estables: sin señales de omisión", "Stable coefficients: no sign of omission"), tema)
  x <- caja_texto(x, 0.6, 2.05, 5.9, 2.9, tr("B · Cifra de negocio", "B · Turnover"),
    tr(paste0("Trabajadores: ", num(coef(b1)[["trabajadores"]], 4), ", ", num(coef(b2)[["trabajadores"]], 4), " y ", num(coef(b3)[["trabajadores"]], 4),
              " al añadir inversión y superficie. El ", M("\\bar{R}^2"), " baja de ", num(r2a(b1), 3), " a ", num(r2a(b3), 3), ": el modelo simple ya recoge la relación principal."),
       paste0("Workers: ", num(coef(b1)[["trabajadores"]], 4), ", ", num(coef(b2)[["trabajadores"]], 4), " and ", num(coef(b3)[["trabajadores"]], 4),
              " when investment and land area are added. The ", M("\\bar{R}^2"), " falls from ", num(r2a(b1), 3), " to ", num(r2a(b3), 3), ": the simple model already captures the main relationship.")),
    size = 17)
  x <- caja_texto(x, 6.8, 2.05, 5.9, 2.9, tr("C · Consumo", "C · Consumption"),
    tr(paste0("PMC: ", num(coef(c1)[["pib"]], 4), " solo con el PIB, ", num(coef(c2)[["pib"]], 4), " con el tipo de interés y ", num(coef(c3)[["pib"]], 4),
              " con la inflación. Sesgo de ", num(sesgo_c, 4), ", positivo como predice la regla: tipo de interés con efecto negativo y correlación con el PIB de ", num(cor_pi, 3), "."),
       paste0("MPC: ", num(coef(c1)[["pib"]], 4), " with GDP only, ", num(coef(c2)[["pib"]], 4), " with the interest rate and ", num(coef(c3)[["pib"]], 4),
              " with inflation. A bias of ", num(sesgo_c, 4), ", positive as the rule predicts: negative interest-rate effect and correlation with GDP of ", num(cor_pi, 3), ".")),
    oscura = TRUE, size = 17)
  x <- poner_texto(x, parrafo(tr("Un sesgo pequeño no obliga a quitar la variable: el tipo de interés recoge un canal económico relevante, el coste de oportunidad del ahorro.",
                                 "A small bias does not mean dropping the variable: the interest rate captures a relevant economic channel, the opportunity cost of saving."),
                              17, ID$navy, bold = TRUE), 0.6, 5.3, 12.1, 0.9)

  # 18 · Cierre
  x <- diapo_cierre(x,
    tr(c("Variable irrelevante: sin sesgo, pero más varianza y menos potencia.",
         paste0("Variable omitida: sesgo ", M("\\beta_2 \\cdot \\delta_{21}"), ", que no desaparece con más datos."),
         paste0("Signo del sesgo: signo de ", M("\\beta_2"), " por signo de la correlación."),
         "Omitir es más grave que incluir de más.",
         paste0("Diagnóstico: ", M("t"), ", intervalos, ", M("\\bar{R}^2"), ", estabilidad y residuos."),
         "Si la variable no se observa, una buena proxy reduce el sesgo."),
       c("Irrelevant variable: no bias, but more variance and less power.",
         paste0("Omitted variable: bias ", M("\\beta_2 \\cdot \\delta_{21}"), ", which does not vanish with more data."),
         paste0("Sign of the bias: sign of ", M("\\beta_2"), " times sign of the correlation."),
         "Omitting is more serious than including too much.",
         paste0("Diagnosis: ", M("t"), ", intervals, ", M("\\bar{R}^2"), ", stability and residuals."),
         "If the variable is not observed, a good proxy reduces the bias.")),
    tr("Scripts de R del tema 6 y app «Especificación». Explicación completa en el capítulo 6 del manual.",
       "Chapter 6 R scripts and the «Specification» app. Full explanation in Chapter 6 of the book."),
    tema)

  guardar_pptx(x, "T06_mEC_Especificacion.pptx", "T06_mEC_Specification.pptx")
}
