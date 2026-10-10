# ══════════════════════════════════════════════════════════════════
# Capítulo 04 · Contrastes de hipótesis en el MRLG · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap04_pptx.R
# Contenido y numeración: 04-contrastes-hipotesis.qmd (secciones 4.1 a 4.6). Caso A con precio, publicidad y precio
# del sustitutivo; Caso B con trabajadores, inversión y superficie; Caso C con PIB y tipo de interés (como el manual).
# Las restricciones lineales, el contraste F y la significación global (clase S06) no están en el manual: van como
# ampliación al final, con cifras calculadas aquí sobre los mismos modelos.
# ══════════════════════════════════════════════════════════════════

# ── Ayudantes propios de este tema (no están en los comunes) ──────
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
# p-valor en LaTeX: «< 0,001» o el número
ptex <- function(p, d = 4) if (p < 0.001) tr("p < 0{,}001", "p < 0.001") else paste0("p = ", numtex(p, d))

for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 4", "Chapter 4")
  ES <- tr("\\text{ES}", "\\text{SE}"); SCR <- tr("\\text{SCR}", "\\text{RSS}")
  SCR_R <- tr("\\text{SCR}_R", "\\text{RSS}_R"); SCR_NR <- tr("\\text{SCR}_{NR}", "\\text{RSS}_U")
  rechazo <- function(p, a = 0.05) if (p < a) tr("Rechazar H0", "Reject H0") else tr("No rechazar H0", "Do not reject H0")

  # ── Caso A (apartado 4.5): ventas ~ precio + publicidad + precio del sustitutivo ──
  ma <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a); sa <- summary(ma); ca <- sa$coefficients
  gl_a <- df.residual(ma); tc_a <- qt(0.975, gl_a); tc1_a <- qt(0.95, gl_a)
  # Ejercicio 4.1: unilateral derecho para la publicidad
  t_pub <- ca[3, 3]; p_pub1 <- pt(t_pub, gl_a, lower.tail = FALSE)
  # Restricción lineal (ampliación): H0: b_precio + b_sustit = 0 (homogeneidad de grado cero en precios)
  V <- vcov(ma); Rv <- c(0, 1, 0, 1)
  rb <- sum(Rv * coef(ma)); se_rb <- sqrt(drop(t(Rv) %*% V %*% Rv)); t_rb <- rb / se_rb; p_rb <- 2 * pt(-abs(t_rb), gl_a)
  # ── Caso B (apartado 4.6): cifra ~ trabajadores + inversión + superficie ──
  mb <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b); sb <- summary(mb); cb <- sb$coefficients
  mb_r <- lm(cifra_negocio ~ trabajadores, data = caso_b)
  scr_nr <- sum(resid(mb)^2); scr_r <- sum(resid(mb_r)^2); gl_b <- df.residual(mb)
  F_b2 <- ((scr_r - scr_nr) / 2) / (scr_nr / gl_b); p_b2 <- pf(F_b2, 2, gl_b, lower.tail = FALSE); Fc_b2 <- qf(0.95, 2, gl_b)
  cor_b <- cor(caso_b[, c("trabajadores", "inversion", "superficie")])
  # ── Caso C (apartado 4.6): consumo ~ PIB + tipo de interés ──
  mc <- lm(consumo ~ pib + tipo_interes, data = caso_c); sc <- summary(mc); cc <- sc$coefficients
  # Significación global
  Fglob <- function(s) { f <- s$fstatistic; c(F = unname(f[1]), k = unname(f[2]), gl = unname(f[3]), p = unname(pf(f[1], f[2], f[3], lower.tail = FALSE))) }
  fa <- Fglob(sa); fb <- Fglob(sb); fc <- Fglob(sc)

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 4, tr("Contrastes de hipótesis en el MRLG", "Hypothesis testing in the GLRM"),
    tr("¿Es real el efecto del precio sobre las ventas o fruto del azar?", "Is the effect of price on sales real, or just chance?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c(sprintf("4.%d", 1:5), "4.6–4.8", "4.9"),
    titulo = tr(c("Conceptos básicos", "Los tres pasos del contraste", paste0("El contraste ", M("t"), " para un coeficiente individual"),
                  "La regla del p-valor", "Significatividad individual: ¿es relevante la variable?",
                  paste0("Restricciones lineales, contraste ", M("F"), " y significatividad global"), "Aplicación a los casos prácticos"),
                c("Basic concepts", "The three steps of a test", paste0("The ", M("t"), " test for a single coefficient"),
                  "The p-value rule", "Individual significance: does the variable matter?",
                  paste0("Linear restrictions, the ", M("F"), " test and overall significance"), "Application to the case studies"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Plantear el contraste", "Hipótesis nula y alternativa, nivel de significación y los dos tipos de error."),
       c("Set up the test", "Null and alternative hypotheses, significance level and the two types of error.")),
    tr(c("Decidir con t y p-valor", "Contrastes de una y dos colas: valor crítico o p-valor, siempre en tres pasos."),
       c("Decide with t and p-value", "One- and two-tailed tests: critical value or p-value, always in three steps.")),
    tr(c("Ir más allá", "Restricciones lineales sobre varios coeficientes y el contraste F de significación global."),
       c("Go further", "Linear restrictions on several coefficients and the F test of overall significance."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("La lógica del contraste", "The logic of testing"),
    tr(c(paste0(M("H_0"), " frente a ", M("H_1")), "Nivel de significación y errores", "Tres pasos, siempre"),
       c(paste0(M("H_0"), " versus ", M("H_1")), "Significance level and errors", "Three steps, always")))

  # 5 · 4.1 Conceptos básicos
  x <- diapo_contenido(x, tr("4.1 · Conceptos básicos", "4.1 · Basic concepts"),
    tr("Dos hipótesis, dos maneras de equivocarse", "Two hypotheses, two ways to be wrong"), tema)
  x <- caja_texto(x, 0.6, 1.95, 5.6, 1.45, tr("Hipótesis nula", "Null hypothesis"),
                  paste0(M("H_0"), tr(": la que se pone a prueba; se da por cierta mientras los datos no digan lo contrario.",
                                      ": the one put to the test; taken as true unless the data say otherwise.")), size = 15)
  x <- caja_texto(x, 0.6, 3.55, 5.6, 1.45, tr("Hipótesis alternativa", "Alternative hypothesis"),
                  paste0(M("H_1"), tr(": la que se acepta si los datos rechazan ", ": the one accepted if the data reject "), M("H_0"), "."), size = 15)
  x <- formula_caja(x, "H_0: \\beta_j = 0 \\quad \\text{vs} \\quad H_1: \\beta_j \\neq 0", 0.6, 5.2, 5.6, 0.95, size = 22)
  x <- poner_texto(x, parrafo(tr("El contraste más habitual: significatividad individual.", "The most common test: individual significance."), 14, ID$gris, italic = TRUE),
                   0.6, 6.25, 5.6, 0.5)
  x <- tabla_eq(x, c("", paste0(M("H_0"), tr(" cierta", " true")), paste0(M("H_0"), tr(" falsa", " false"))),
    list(c(paste0(tr("Rechazar ", "Reject "), M("H_0")), paste0(tr("Error tipo I ", "Type I error "), M("(\\alpha)")), tr("Correcto", "Correct")),
         c(paste0(tr("No rechazar ", "Do not reject "), M("H_0")), tr("Correcto", "Correct"), tr("Error tipo II", "Type II error"))),
    6.6, 1.95, c(2.3, 1.9, 1.93), alto = 0.85, size = 15)
  x <- vinetas(x, tr(
    c(paste0(M("\\alpha"), ": probabilidad máxima de rechazar ", M("H_0"), " siendo cierta (falso positivo)."),
      paste0("Valores habituales: ", M("\\alpha = 0{,}10"), ", ", M("0{,}05"), " y ", M("0{,}01"), "; el 5 % es el convencional.")),
    c(paste0(M("\\alpha"), ": the maximum probability of rejecting a true ", M("H_0"), " (false positive)."),
      paste0("Usual values: ", M("\\alpha = 0.10"), ", ", M("0.05"), " and ", M("0.01"), "; 5 % is the convention."))),
    6.6, 4.35, 6.13, 2.45, size = 16)

  # 6 · 4.2 Tres pasos
  x <- diapo_contenido(x, tr("4.2 · Los tres pasos del contraste", "4.2 · The three steps of a test"),
    tr("Hipótesis, estadístico y decisión", "Hypotheses, statistic and decision"), tema)
  pasos <- tr(list(c("Plantear", "H_0 \\text{ y } H_1",paste0("Elegir también el nivel de significación ", M("\\alpha"), ".")),
                   c("Calcular", "t \\text{ o } F", paste0("Bajo ", M("H_0"), " el estadístico sigue una distribución conocida: ", M("t_{n-k-1}"), " o ", M("F"), ".")),
                   c("Decidir", "|t| > t_{c} \\;\\text{o}\\; p < \\alpha", paste0("Rechazar ", M("H_0"), " si el estadístico cae en la región de rechazo o si el p-valor es menor que ", M("\\alpha"), "."))),
              list(c("Set up", "H_0 \\text{ and } H_1",paste0("Also choose the significance level ", M("\\alpha"), ".")),
                   c("Compute", "t \\text{ or } F", paste0("Under ", M("H_0"), " the statistic follows a known distribution: ", M("t_{n-k-1}"), " or ", M("F"), ".")),
                   c("Decide", "|t| > t_{c} \\;\\text{or}\\; p < \\alpha", paste0("Reject ", M("H_0"), " if the statistic falls in the rejection region or the p-value is below ", M("\\alpha"), "."))))
  for (k in 1:3) {
    left <- 0.6 + (k - 1) * 4.15
    x <- rect(x, left, 2.0, 3.85, 4.75, if (k == 2) ID$navy else ID$suave)
    x <- poner_texto(x, list(parrafo(sprintf("%d", k), 40, ID$oro, bold = TRUE, font = "Georgia"),
                             parrafo(pasos[[k]][1], 22, if (k == 2) ID$blanco else ID$navy, bold = TRUE, font = "Georgia"),
                             parrafo(M(pasos[[k]][2]), 22, if (k == 2) ID$oro_cl else ID$navy, espacio = 14),
                             parrafo(pasos[[k]][3], 16, if (k == 2) ID$blanco else ID$texto)),
                     left + 0.3, 2.2, 3.3, 4.4)
  }

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("El contraste t y el p-valor", "The t test and the p-value"),
    tr(c("Una y dos colas", "Valores críticos", "La regla del p-valor", "El precio, paso a paso"),
       c("One and two tails", "Critical values", "The p-value rule", "Price, step by step")))

  # 8 · 4.3 Estadístico t y tipos de contraste
  x <- diapo_contenido(x, tr("4.3 · El contraste t para un coeficiente", "4.3 · The t test for a single coefficient"),
    tr(paste0("El estadístico ", M("t"), ": distancia en errores estándar"), paste0("The ", M("t"), " statistic: distance in standard errors")), tema)
  x <- formula_caja(x, paste0("t = \\frac{\\hat{\\beta}_j - \\beta_j^{*}}{", ES, "(\\hat{\\beta}_j)} = \\frac{\\hat{\\beta}_j - \\beta_j^{*}}{\\sqrt{\\hat{\\sigma}^2 [(\\mathbf{X}'\\mathbf{X})^{-1}]_{jj}}} \\sim t_{n-k-1}"),
                    0.6, 1.95, 12.1, 1.6, size = 24)
  tipos <- tr(list(c("Dos colas", "H_1: \\beta_j \\neq \\beta_j^{*}", paste0("Rechazar si ", M("|t| > t_{n-k-1,\\,\\alpha/2}"), ". El de significatividad, con ", M("\\beta_j^{*} = 0"), ".")),
                   c("Cola derecha", "H_1: \\beta_j > \\beta_j^{*}", paste0("Rechazar si ", M("t > t_{n-k-1,\\,\\alpha}"), ". Ojo: valor positivo de ", M("t"), ".")),
                   c("Cola izquierda", "H_1: \\beta_j < \\beta_j^{*}", paste0("Rechazar si ", M("t < -t_{n-k-1,\\,\\alpha}"), ". Ojo: valor negativo de ", M("t"), "."))),
              list(c("Two tails", "H_1: \\beta_j \\neq \\beta_j^{*}", paste0("Reject if ", M("|t| > t_{n-k-1,\\,\\alpha/2}"), ". Significance tests use ", M("\\beta_j^{*} = 0"), ".")),
                   c("Right tail", "H_1: \\beta_j > \\beta_j^{*}", paste0("Reject if ", M("t > t_{n-k-1,\\,\\alpha}"), ". Note: a positive ", M("t"), ".")),
                   c("Left tail", "H_1: \\beta_j < \\beta_j^{*}", paste0("Reject if ", M("t < -t_{n-k-1,\\,\\alpha}"), ". Note: a negative ", M("t"), "."))))
  for (k in 1:3) x <- caja_dato(x, 0.6 + (k - 1) * 4.15, 3.75, 3.85, 3.0, tipos[[k]][1], M(tipos[[k]][2]), tipos[[k]][3],
                                if (k == 1) ID$navy else ID$oro, size_valor = 22)

  # 9 · 4.3 Regiones de rechazo
  x <- diapo_contenido(x, tr("4.3 · Tipos de contraste", "4.3 · Types of test"),
    tr("Regiones de rechazo al 5 %", "Rejection regions at 5 %"), tema)
  gl_fig <- 30
  paneles <- tr(c("Dos colas", "Cola derecha", "Cola izquierda"), c("Two tails", "Right tail", "Left tail"))
  xs <- seq(-4, 4, length.out = 400)
  curva <- do.call(rbind, lapply(paneles, function(pn) data.frame(p = pn, x = xs, y = dt(xs, gl_fig))))
  curva$p <- factor(curva$p, levels = paneles)
  t2 <- qt(0.975, gl_fig); t1 <- qt(0.95, gl_fig)
  zona <- rbind(transform(subset(curva, p == paneles[1] & x >= t2), g = "a"), transform(subset(curva, p == paneles[1] & x <= -t2), g = "b"),
                transform(subset(curva, p == paneles[2] & x >= t1), g = "c"), transform(subset(curva, p == paneles[3] & x <= -t1), g = "d"))
  crit <- data.frame(p = factor(paneles[c(1, 1, 2, 3)], levels = paneles), x = c(-t2, t2, t1, -t1))
  g_reg <- ggplot(curva, aes(x, y)) +
    geom_ribbon(data = zona, aes(ymin = 0, ymax = y, group = g), fill = COL$oro, alpha = 0.85) +
    geom_line(colour = COL$navy, linewidth = 1.1) +
    geom_vline(data = crit, aes(xintercept = x), linetype = "dashed", colour = COL$gris) +
    geom_label(data = crit, aes(x = x, y = 0.22, label = num(x, 3)), colour = COL$navy, fill = "white", size = 4.2) +
    facet_wrap(~ p) +
    scale_x_continuous(breaks = -3:3, labels = function(v) num(v, 0)) +
    labs(x = tr("t con 30 grados de libertad", "t with 30 degrees of freedom"), y = NULL) +
    tema_cdea(14) + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  x <- imagen_ggplot(x, g_reg, 0.6, 1.95, 12.1, 3.7)
  x <- poner_texto(x, parrafo(paste0(tr("Área dorada: probabilidad de rechazar ", "Gold area: probability of rejecting a true "), M("H_0"),
                                     tr(" siendo cierta, ", ", "), M(tr("\\alpha = 0{,}05", "\\alpha = 0.05")), tr(". En dos colas se reparte: ", ". With two tails it is split: "),
                                     M("\\alpha/2"), tr(" en cada lado; por eso el valor crítico es mayor.", " on each side, so the critical value is larger.")),
                              16, ID$navy, bold = TRUE), 0.6, 5.85, 12.1, 0.9)

  # 10 · 4.4 El p-valor
  x <- diapo_contenido(x, tr("4.4 · La regla del p-valor", "4.4 · The p-value rule"),
    tr(paste0("El p-valor: cuánto sorprende el dato si ", M("H_0"), " es cierta"), paste0("The p-value: how surprising the data are under ", M("H_0"))), tema)
  t_s <- ca[4, 3]; p_s <- ca[4, 4]
  xs2 <- seq(-4.5, 4.5, length.out = 500); dd <- data.frame(x = xs2, y = dt(xs2, gl_a))
  cola <- rbind(transform(subset(dd, x >= abs(t_s)), g = "d"), transform(subset(dd, x <= -abs(t_s)), g = "i"))
  g_p <- ggplot(dd, aes(x, y)) +
    geom_ribbon(data = cola, aes(ymin = 0, ymax = y, group = g), fill = COL$oro, alpha = 0.9) +
    geom_line(colour = COL$navy, linewidth = 1.1) +
    geom_vline(xintercept = c(-1, 1) * abs(t_s), linetype = "dashed", colour = COL$navy) +
    annotate("text", x = abs(t_s) + 0.15, y = 0.12, hjust = 0, label = paste0("t = ", num(t_s, 3)), colour = COL$navy, size = 4.5, fontface = "bold") +
    annotate("text", x = 3.2, y = 0.045, label = paste0(tr("área = ", "area = "), num(p_s / 2, 4)), colour = "#8A6D1E", size = 4) +
    annotate("text", x = -3.2, y = 0.045, label = paste0(tr("área = ", "area = "), num(p_s / 2, 4)), colour = "#8A6D1E", size = 4) +
    scale_x_continuous(breaks = -4:4, labels = function(v) num(v, 0)) +
    labs(x = paste0(tr("t con ", "t with "), gl_a, tr(" grados de libertad", " degrees of freedom")), y = NULL) +
    tema_cdea(13) + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  x <- imagen_ggplot(x, g_p, 0.6, 1.95, 6.9, 4.1)
  x <- poner_texto(x, parrafo(paste0(tr("Caso A, precio del sustitutivo: ", "Case A, substitute price: "), M(paste0("t = ", numtex(t_s, 3))), ", ",
                                     M(paste0("p = 2 \\cdot P(T > |t|) = ", numtex(p_s, 4)))), 15, ID$gris, italic = TRUE), 0.6, 6.1, 6.9, 0.7)
  x <- tabla_eq(x, c(tr("p-valor", "p-value"), paste0(tr("Evidencia contra ", "Evidence against "), M("H_0"))),
    list(c(M(tr("p < 0{,}01", "p < 0.01")), tr("Muy fuerte (***)", "Very strong (***)")),
         c(M(tr("0{,}01 \\leq p < 0{,}05", "0.01 \\leq p < 0.05")), tr("Fuerte (**)", "Strong (**)")),
         c(M(tr("0{,}05 \\leq p < 0{,}10", "0.05 \\leq p < 0.10")), tr("Débil (*)", "Weak (*)")),
         c(M(tr("p \\geq 0{,}10", "p \\geq 0.10")), tr("Insuficiente: no se rechaza", "Not enough: do not reject"))),
    7.8, 1.95, c(1.95, 2.98), alto = 0.62, size = 14)
  x <- caja_texto(x, 7.8, 4.95, 4.93, 1.8, tr("Regla", "Rule"),
                  paste0(tr("Rechazar ", "Reject "), M("H_0"), tr(" si ", " if "), M("p < \\alpha"), tr(". Aquí, ", ". Here, "), M(paste0("p = ", numtex(p_s, 3))),
                         tr(": se rechaza al 5 % pero no al 1 %.", ": reject at 5 % but not at 1 %.")), oscura = TRUE, size = 15)

  # 11 · 4.5 Ejemplo paso a paso (precio, Caso A)
  x <- diapo_contenido(x, tr("4.5 · Significatividad individual", "4.5 · Individual significance"),
    tr("¿Influye el precio en las ventas? Paso a paso", "Does price affect sales? Step by step"), tema)
  ej <- tr(list(c("Paso 1 · Hipótesis", paste0("H_0: \\beta_1 = 0 \\quad H_1: \\beta_1 \\neq 0, \\quad \\alpha = 0{,}05")),
                c("Paso 2 · Estadístico", paste0("t = \\frac{", numtex(ca[2, 1], 4), "}{", numtex(ca[2, 2], 4), "} = ", numtex(ca[2, 3], 3))),
                c("Paso 3 · Decisión", paste0("|", numtex(ca[2, 3], 3), "| > t_{", gl_a, ",\\,0{,}025} = ", numtex(tc_a, 3), " \\;\\Rightarrow\\; \\text{se rechaza } H_0"))),
           list(c("Step 1 · Hypotheses", paste0("H_0: \\beta_1 = 0 \\quad H_1: \\beta_1 \\neq 0, \\quad \\alpha = 0.05")),
                c("Step 2 · Statistic", paste0("t = \\frac{", numtex(ca[2, 1], 4), "}{", numtex(ca[2, 2], 4), "} = ", numtex(ca[2, 3], 3))),
                c("Step 3 · Decision", paste0("|", numtex(ca[2, 3], 3), "| > t_{", gl_a, ",\\,0.025} = ", numtex(tc_a, 3), " \\;\\Rightarrow\\; \\text{reject } H_0"))))
  for (k in 1:3) {
    top <- 1.95 + (k - 1) * 1.25
    x <- poner_texto(x, parrafo(ej[[k]][1], 17, ID$oro, bold = TRUE, font = "Georgia"), 0.6, top + 0.33, 2.9, 0.6)
    x <- formula_caja(x, ej[[k]][2], 3.5, top, 9.23, 1.1, size = 22)
  }
  x <- caja_texto(x, 0.6, 5.75, 12.13, 1.05, tr("Conclusión", "Conclusion"),
                  tr(paste0("Con el p-valor (", M(paste0("p = ", numtex(ca[2, 4], 5))), ") se llega a lo mismo: cada euro más de precio resta ", num(abs(ca[2, 1]), 2), " MM € de ventas, y no por azar."),
                     paste0("The p-value (", M(paste0("p = ", numtex(ca[2, 4], 5))), ") gives the same answer: each extra euro of price cuts sales by €", num(abs(ca[2, 1]), 2), " million, and not by chance.")),
                  oscura = TRUE, size = 14)

  # 12 · 4.5 Tabla Caso A y una cola
  x <- diapo_contenido(x, tr("4.5 · Significatividad individual", "4.5 · Individual significance"),
    tr("Caso A: las tres variables cuentan", "Case A: all three variables matter"), tema)
  nom_a <- tr(c("Intercepto", "Precio propio", "Publicidad", "Precio sustit."), c("Intercept", "Own price", "Advertising", "Subst. price"))
  ta <- data.frame(v = nom_a, b = num(ca[, 1], 4), s = num(ca[, 2], 4), t = num(ca[, 3], 3),
                   p = ifelse(ca[, 4] < 1e-4, tr("< 0,0001", "< 0.0001"), num(ca[, 4], 4)),
                   d = vapply(ca[, 4], rechazo, ""))
  x <- poner_tabla(x, tabla_pptx(ta, tr(c("Variable", "Coef.", "Error est.", "t", "p-valor", "Decisión (5 %)"), c("Variable", "Coef.", "Std. error", "t", "p-value", "Decision (5 %)")),
                                 size = 15, texto = c(1, 6)), 0.6, 1.95, max_ancho = 12.1)
  x <- caja_texto(x, 0.6, 4.45, 5.9, 2.0, tr("Ejercicio 4.1 · una cola", "Exercise 4.1 · one tail"),
                  paste0(M(paste0("t = ", numtex(t_pub, 3), " > t_{", gl_a, ",\\,", tr("0{,}05", "0.05"), "} = ", numtex(tc1_a, 3))),
                         tr(": se rechaza ", ": reject "), M("H_0: \\beta_2 \\leq 0"), tr(" frente a ", " against "), M("H_1: \\beta_2 > 0"),
                         tr(" (publicidad) al 5 % y también al 1 %.", " (advertising) at 5 % and also at 1 %.")), size = 15)
  x <- caja_texto(x, 6.83, 4.45, 5.9, 2.0, tr("Lectura", "Reading"),
                  tr(paste0("Con ", gl_a, " grados de libertad, el valor crítico bilateral al 5 % es ", num(tc_a, 3), ". Precio, publicidad y precio del sustitutivo superan ese umbral."),
                     paste0("With ", gl_a, " degrees of freedom, the two-tailed 5 % critical value is ", num(tc_a, 3), ". Price, advertising and substitute price all clear it.")),
                  oscura = TRUE, size = 15)

  # 13 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Restricciones lineales y contraste F", "Linear restrictions and the F test"),
    tr(c(paste0("Una restricción: ", M("\\mathbf{R}\\boldsymbol{\\beta} = r")), "Varias restricciones: F", "Significación global"),
       c(paste0("One restriction: ", M("\\mathbf{R}\\boldsymbol{\\beta} = r")), "Several restrictions: F", "Overall significance")))

  # 14 · Una restricción lineal
  x <- diapo_contenido(x, tr("4.6 · Una restricción lineal", "4.6 · One linear restriction"),
    tr("Contrastar una combinación de coeficientes", "Testing a combination of coefficients"), tema)
  x <- formula_caja(x, "H_0: \\beta_1 + \\beta_3 = 0 \\;\\Leftrightarrow\\; \\mathbf{R}\\boldsymbol{\\beta} = r, \\quad \\mathbf{R} = \\begin{pmatrix} 0 & 1 & 0 & 1 \\end{pmatrix}, \\; r = 0",
                    0.6, 1.95, 12.1, 0.95, size = 22)
  x <- formula_caja(x, paste0("t = \\frac{\\mathbf{R}\\hat{\\boldsymbol{\\beta}} - r}{\\sqrt{\\mathbf{R}\\,\\widehat{\\text{Var}}(\\hat{\\boldsymbol{\\beta}})\\,\\mathbf{R}'}} = \\frac{",
                              numtex(coef(ma)[2], 4), " + ", numtex(coef(ma)[4], 4), "}{", numtex(se_rb, 4), "} = ", numtex(t_rb, 3), " \\sim t_{", gl_a, "}"),
                    0.6, 3.1, 12.1, 1.65, size = 22)
  x <- vinetas(x, tr(
    c(paste0("Caso A: ", M("\\beta_1"), " es el precio propio y ", M("\\beta_3"), " el del sustitutivo. ¿Si los dos suben un euro, las ventas no cambian? (homogeneidad en precios)."),
      paste0("El denominador usa la matriz de varianzas y covarianzas: ", M("\\text{Var}(\\hat{\\beta}_1) + \\text{Var}(\\hat{\\beta}_3) + 2\\,\\text{Cov}(\\hat{\\beta}_1, \\hat{\\beta}_3)"), "."),
      paste0(M(ptex(p_rb, 3)), ": ", if (p_rb < 0.05) "se rechaza la homogeneidad al 5 %." else "no se rechaza al 5 %: los datos son compatibles con la homogeneidad.")),
    c(paste0("Case A: ", M("\\beta_1"), " is own price and ", M("\\beta_3"), " the substitute price. If both rise by one euro, do sales stay put? (price homogeneity)."),
      paste0("The denominator uses the variance-covariance matrix: ", M("\\text{Var}(\\hat{\\beta}_1) + \\text{Var}(\\hat{\\beta}_3) + 2\\,\\text{Cov}(\\hat{\\beta}_1, \\hat{\\beta}_3)"), "."),
      paste0(M(ptex(p_rb, 3)), ": ", if (p_rb < 0.05) "homogeneity is rejected at 5 %." else "not rejected at 5 %: the data are compatible with homogeneity."))),
    0.6, 4.95, 12.1, 1.9, size = 16)

  # 15 · Varias restricciones: contraste F
  x <- diapo_contenido(x, tr("4.7 · Varias restricciones lineales", "4.7 · Several linear restrictions"),
    tr(paste0("Varias restricciones a la vez: el contraste ", M("F")), paste0("Several restrictions at once: the ", M("F"), " test")), tema)
  x <- formula_caja(x, paste0("F = \\frac{(", SCR_R, " - ", SCR_NR, ")/q}{", SCR_NR, "/(n-k-1)} \\sim F_{q,\\,n-k-1}"), 0.6, 1.95, 6.0, 1.4, size = 24)
  x <- formula_caja(x, "F = \\frac{(\\mathbf{R}\\hat{\\boldsymbol{\\beta}} - \\mathbf{r})'\\,[\\mathbf{R}\\,\\widehat{\\text{Var}}(\\hat{\\boldsymbol{\\beta}})\\,\\mathbf{R}']^{-1}(\\mathbf{R}\\hat{\\boldsymbol{\\beta}} - \\mathbf{r})}{q}",
                    6.8, 1.95, 5.93, 1.4, size = 19)
  x <- vinetas(x, tr(
    c(paste0(M("q"), ": número de restricciones. R: modelo restringido (con ", M("H_0"), " impuesta); NR: sin restringir."),
      paste0("Se rechaza ", M("H_0"), " si ", M("F > F_{q,\\,n-k-1,\\,\\alpha}"), ", o si el p-valor es menor que ", M("\\alpha"), ". Una sola cola.")),
    c(paste0(M("q"), ": number of restrictions. R: restricted model (", M("H_0"), " imposed); U: unrestricted."),
      paste0("Reject ", M("H_0"), " if ", M("F > F_{q,\\,n-k-1,\\,\\alpha}"), ", or if the p-value is below ", M("\\alpha"), ". One tail only."))),
    0.6, 3.55, 6.0, 3.25, size = 16)
  x <- caja_dato(x, 6.8, 3.55, 5.93, 3.2, tr("Caso B: ¿sobran inversión y superficie?", "Case B: are investment and land area redundant?"),
                 M(paste0("F = \\frac{(", numtex(scr_r, 2), " - ", numtex(scr_nr, 2), ")/2}{", numtex(scr_nr, 2), "/", gl_b, "} = ", numtex(F_b2, 4))),
                 paste0(M(paste0("F_{2,", gl_b, ",\\,", tr("0{,}05", "0.05"), "} = ", numtex(Fc_b2, 3))), " · ", M(ptex(p_b2, 3)),
                        tr(". No se rechaza: las dos sobran cuando ya están los trabajadores.", ". Not rejected: both are redundant once workers are in.")),
                 ID$navy, size_valor = 20)

  # 16 · Significación global
  x <- diapo_contenido(x, tr("4.8 · Significación global", "4.8 · Overall significance"),
    tr("¿Explica algo el modelo en su conjunto?", "Does the model explain anything at all?"), tema)
  x <- formula_caja(x, "H_0: \\beta_1 = \\beta_2 = \\cdots = \\beta_k = 0", 0.6, 1.95, 5.6, 1.15, size = 22)
  x <- formula_caja(x, "F = \\frac{R^2 / k}{(1 - R^2)/(n-k-1)} \\sim F_{k,\\,n-k-1}", 6.5, 1.95, 6.23, 1.15, size = 22)
  glob <- list(c("A", fa), c("B", fb), c("C", fc))
  txt_g <- tr(c(paste0("Precio, publicidad y sustitutivo, juntos, explican las ventas."),
                paste0("Ninguna variable es significativa por separado al 5 %, pero juntas sí: trabajadores, inversión y superficie están muy correlacionadas (entre ",
                       num(min(cor_b[upper.tri(cor_b)]), 2), " y ", num(max(cor_b[upper.tri(cor_b)]), 2), ")."),
                paste0("PIB y tipo de interés: ", M(paste0("R^2 = ", numtex(sc$r.squared, 4))), ".")),
              c(paste0("Price, advertising and substitute price jointly explain sales."),
                paste0("No variable is significant on its own at 5 %, but jointly they are: workers, investment and land area are highly correlated (between ",
                       num(min(cor_b[upper.tri(cor_b)]), 2), " and ", num(max(cor_b[upper.tri(cor_b)]), 2), ")."),
                paste0("GDP and interest rate: ", M(paste0("R^2 = ", numtex(sc$r.squared, 4))), ".")))
  for (k in 1:3) {
    f <- as.numeric(glob[[k]][-1]); names(f) <- c("F", "k", "gl", "p")
    x <- caja_dato(x, 0.6 + (k - 1) * 4.15, 3.3, 3.85, 3.45, paste0(tr("Caso ", "Case "), glob[[k]][1]),
                   M(paste0("F_{", f["k"], ",", f["gl"], "} = ", numtex(f["F"], 2))),
                   paste0(M(ptex(f["p"], 3)), ". ", txt_g[k]), if (k == 2) ID$oro else ID$navy, size_valor = 22)
  }

  # 17 · Gretl → R
  x <- diapo_contenido(x, tr("4.4 · Gretl → R", "4.4 · Gretl → R"), tr("Los contrastes en Gretl y en R", "Tests in Gretl and in R"), tema)
  gr <- data.frame(
    op = tr(c("Contraste t de cada coeficiente", "Valor crítico t", "p-valor de un t", "Restricciones lineales", "F de significación global", "Valor crítico F"),
            c("t test of each coefficient", "Critical t value", "p-value of a t", "Linear restrictions", "F of overall significance", "Critical F value")),
    gretl = tr(c("Ventana de resultados MCO", "Herramientas → Tablas estadísticas", "Herramientas → Buscador de valores p", "Contrastes → Restricciones lineales",
                 "Ventana de resultados MCO", "Herramientas → Tablas estadísticas"),
               c("OLS results window", "Tools → Statistical tables", "Tools → P-value finder", "Tests → Linear restrictions",
                 "OLS results window", "Tools → Statistical tables")),
    r = c("summary(m)", "qt(1 - alpha/2, gl)", "2 * pt(-abs(t), gl)", "car::linearHypothesis(m, ...)", "summary(m)$fstatistic", "qf(1 - alpha, q, gl)"))
  x <- poner_tabla(x, tabla_pptx(gr, tr(c("Qué", "Gretl", "R"), c("What", "Gretl", "R")), size = 14, mono = 3, texto = 1:2), 0.6, 1.95, max_ancho = 12.1)
  x <- poner_texto(x, parrafo(tr("Gretl y R dan las mismas columnas: coeficiente, error estándar, t y p-valor, con asteriscos de significatividad.",
                                 "Gretl and R give the same columns: coefficient, standard error, t and p-value, with significance stars."),
                              16, ID$navy, bold = TRUE), 0.6, 5.85, 12.1, 0.9)

  # 18 · 4.6 Casos prácticos
  x <- diapo_contenido(x, tr("4.9 · Aplicación a los casos prácticos", "4.9 · Application to the case studies"),
    tr("Qué variables cuentan en cada caso", "Which variables matter in each case"), tema)
  casos <- tr(list(
      c("A · Demanda", paste0("Precio, publicidad y precio del sustitutivo: las tres significativas al 5 %, con ", M("t"), " de ",
                              num(ca[2, 3], 2), ", ", num(ca[3, 3], 2), " y ", num(ca[4, 3], 2), ".")),
      c("B · Cifra de negocio", paste0("Trabajadores (", M(ptex(cb[2, 4], 3)), "), inversión y superficie: ninguna significativa al 5 % por separado. El ", M("F"), " conjunto sí lo es: colinealidad (tema 6).")),
      c("C · Consumo", paste0("PIB: ", M(paste0("t = ", numtex(cc[2, 3], 1))), ", ", M(ptex(cc[2, 4])), ". Tipo de interés: ", M(ptex(cc[3, 4], 3)),
                              ", no significativo. Cuidado con la autocorrelación (tema 10)."))),
    list(
      c("A · Demand", paste0("Price, advertising and substitute price: all three significant at 5 %, with ", M("t"), " of ",
                             num(ca[2, 3], 2), ", ", num(ca[3, 3], 2), " and ", num(ca[4, 3], 2), ".")),
      c("B · Turnover", paste0("Workers (", M(ptex(cb[2, 4], 3)), "), investment and land area: none significant at 5 % on its own. The joint ", M("F"), " is: collinearity (chapter 6).")),
      c("C · Consumption", paste0("GDP: ", M(paste0("t = ", numtex(cc[2, 3], 1))), ", ", M(ptex(cc[2, 4])), ". Interest rate: ", M(ptex(cc[3, 4], 3)),
                                  ", not significant. Beware of autocorrelation (chapter 10)."))))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 1.95, 3.85, 3.0, casos[[k]][1], casos[[k]][2], oscura = (k == 2), size = 15)
  x <- caja_texto(x, 0.6, 5.15, 12.13, 1.6, tr("Magnitud no es significatividad", "Size is not significance"),
                  tr("Un coeficiente grande puede no ser significativo si su error estándar es grande; uno pequeño, como la propensión a consumir, puede serlo muchísimo si se estima con precisión.",
                     "A large coefficient may not be significant if its standard error is large; a small one, like the propensity to consume, can be hugely significant if estimated precisely."),
                  size = 15)

  # 19 · Cierre
  x <- diapo_cierre(x,
    tr(c(paste0("Tres pasos: hipótesis, estadístico y decisión al nivel ", M("\\alpha"), "."),
         paste0(M("t = (\\hat{\\beta}_j - \\beta_j^{*})/\\text{ES}(\\hat{\\beta}_j)"), " con ", M("n-k-1"), " grados de libertad."),
         paste0("Dos colas: ", M("|t| > t_{\\alpha/2}"), ". Una cola: atención al signo de ", M("t"), "."),
         paste0("p-valor menor que ", M("\\alpha"), ": se rechaza ", M("H_0"), "."),
         paste0("Restricciones lineales: ", M("\\mathbf{R}\\boldsymbol{\\beta} = \\mathbf{r}"), "; varias a la vez, con ", M("F"), "."),
         paste0(M("F"), " global: ¿explica algo el modelo en su conjunto?")),
       c(paste0("Three steps: hypotheses, statistic and decision at level ", M("\\alpha"), "."),
         paste0(M("t = (\\hat{\\beta}_j - \\beta_j^{*})/\\text{SE}(\\hat{\\beta}_j)"), " with ", M("n-k-1"), " degrees of freedom."),
         paste0("Two tails: ", M("|t| > t_{\\alpha/2}"), ". One tail: watch the sign of ", M("t"), "."),
         paste0("p-value below ", M("\\alpha"), ": reject ", M("H_0"), "."),
         paste0("Linear restrictions: ", M("\\mathbf{R}\\boldsymbol{\\beta} = \\mathbf{r}"), "; several at once, with ", M("F"), "."),
         paste0("Overall ", M("F"), ": does the model explain anything at all?"))),
    tr("Scripts de R del tema 4 y app «Contrastes de hipótesis». Explicación completa en el capítulo 4 del manual.",
       "Chapter 4 R scripts and the «Hypothesis tests» app. Full explanation in Chapter 4 of the book."),
    tema)

  cat(sprintf("A: t precio %.3f  tc %.3f | sust t %.3f p %.4f | restr b1+b3=%.4f se %.4f t %.3f p %.4f\n", ca[2, 3], tc_a, t_s, p_s, rb, se_rb, t_rb, p_rb))
  cat(sprintf("B: F(2,%d)=%.4f p=%.4f Fc=%.3f | F global A %.2f B %.2f C %.2f\n", gl_b, F_b2, p_b2, Fc_b2, fa["F"], fb["F"], fc["F"]))
  guardar_pptx(x, "T04_mEC_Contrastes.pptx", "T04_mEC_Hypothesis_Tests.pptx")
}
