# ══════════════════════════════════════════════════════════════════
# Capítulo 08 · Variables cualitativas (ficticias) · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap08_pptx.R
# Contenido y numeración: 08-variables-cualitativas.qmd (8.1 a 8.3.3). Modelos idénticos a los del capítulo:
#   A: ventas ~ precio_propio + gasto_publicidad + promocion
#   B: cifra_negocio ~ trabajadores + inversion + pac + region
#   C: consumo ~ pib + tipo_interes + crisis
# Ampliaciones de clase (varias categorías, trampa, interacciones, F conjunto, series) con los mismos datos.
# ══════════════════════════════════════════════════════════════════
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 8", "Chapter 8")

  # ── Ayudantes propios de este tema ───────────────────────────────
  # Importe en millones con la convención de cada idioma (espacio duro)
  mill <- function(x, d = 2, ud = "MM€") tr(paste0(num(x, d), "\u00a0", ud), paste0("€", num(x, d), "\u00a0m"))
  mill_es <- mill
  mill <- function(x, d = 2, ud = "MM€") tr(mill_es(x, d, ud), paste0(if (x < 0) "-" else "", "€", num(abs(x), d), intToUtf8(160), "m"))
  # Cuadro de coeficientes con cifras ya formateadas
  tab_coef <- function(m, etiquetas, d = 4) {
    s <- summary(m)$coefficients
    data.frame(v = etiquetas, b = num(s[, 1], d), se = num(s[, 2], d), t = num(s[, 3], 2),
               p = vapply(s[, 4], pval, ""), stringsAsFactors = FALSE)
  }
  cab_coef <- tr(c("Variable", "Coef.", "Error est.", "t", "p-valor"), c("Variable", "Coef.", "Std. error", "t", "p-value"))
  # Ecuación estimada en LaTeX: nombres de variable en \text{}
  eq_est <- function(m, y, xs, d = 2) {
    b <- coef(m)
    s <- paste0("\\hat{", y, "} = ", numtex(b[[1]], d))
    for (j in seq_along(xs)) s <- paste0(s, if (b[[j + 1]] < 0) " - " else " + ", numtex(abs(b[[j + 1]]), d), "\\,\\text{", xs[j], "}")
    s
  }

  # ── Modelos del capítulo ─────────────────────────────────────────
  ma  <- lm(ventas ~ precio_propio + gasto_publicidad + promocion, data = caso_a)
  ma0 <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
  ma2 <- lm(ventas ~ precio_propio + gasto_publicidad + promocion + temporada, data = caso_a)
  mai <- lm(ventas ~ precio_propio + gasto_publicidad * promocion, data = caso_a)
  mb  <- lm(cifra_negocio ~ trabajadores + inversion + pac + region, data = caso_b)
  mb0 <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
  mc  <- lm(consumo ~ pib + tipo_interes + crisis, data = caso_c)
  mc0 <- lm(consumo ~ pib + tipo_interes, data = caso_c)
  sa <- summary(ma)$coefficients; sb <- summary(mb)$coefficients; sc <- summary(mc)$coefficients
  si <- summary(mai)$coefficients
  F_a <- anova(ma0, ma2); F_b <- anova(mb0, mb)
  ic_c <- confint(mc, "crisis")
  rango <- qr(cbind(1, caso_a$promocion, 1 - caso_a$promocion))$rank
  n_crisis <- sum(caso_c$crisis)

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 8, tr("Variables cualitativas: las ficticias", "Qualitative variables: dummies"),
    tr("¿Cuánto más se vende en promoción, con el mismo precio y la misma publicidad?",
       "How much more is sold on promotion, at the same price and advertising?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("8.1", "8.2", "8.3", "8.4", "8.4.1", "8.4.2", "8.4.3"),
    titulo = tr(c("Concepto de variable ficticia (dummy)", "Efecto sobre el término independiente",
                  "Interacciones con variables binarias", "Aplicación a los casos prácticos",
                  "Caso A: efecto de la promoción sobre las ventas", "Caso B: efecto de la PAC y de la región",
                  "Caso C: efecto de la crisis sobre el consumo"),
                c("The dummy variable concept", "Effect on the intercept",
                  "Interactions with binary variables", "Application to the case studies",
                  "Case A: effect of the promotion on sales", "Case B: effect of the CAP subsidy and the region",
                  "Case C: effect of the crisis on consumption"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Codificar lo cualitativo", "Ceros y unos: una ficticia por categoría, menos la de referencia."),
       c("Code what is qualitative", "Zeros and ones: one dummy per category, except the reference one.")),
    tr(c("Interpretar el desplazamiento", "El coeficiente de la ficticia es la diferencia con la referencia, ceteris paribus."),
       c("Interpret the shift", "The dummy coefficient is the difference from the reference, ceteris paribus.")),
    tr(c("Contrastar en los casos", "Contrastes t y F conjunto con la promoción, la PAC, la región y la crisis."),
       c("Test it in the cases", "t tests and the joint F with the promotion, the CAP subsidy, the region and the crisis."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("Una ficticia, dos rectas", "One dummy, two lines"),
    tr(c("Ceros y unos", "La categoría de referencia", "Desplazar el término independiente"),
       c("Zeros and ones", "The reference category", "Shifting the intercept")))

  # 5 · 8.1 Concepto
  x <- diapo_contenido(x, tr("8.1 · Concepto de variable ficticia", "8.1 · The dummy variable concept"),
    tr("Lo cualitativo, en ceros y unos", "What is qualitative, in zeros and ones"), tema)
  x <- formula_caja(x, tr("D_i = \\begin{cases} 1 & \\text{si } i \\text{ tiene la característica} \\\\ 0 & \\text{en caso contrario} \\end{cases}",
                          "D_i = \\begin{cases} 1 & \\text{if } i \\text{ has the attribute} \\\\ 0 & \\text{otherwise} \\end{cases}"),
                    0.6, 2.0, 7.0, 1.5, size = 24)
  x <- vinetas(x, tr(
    c("Promoción activa, ayuda de la PAC, región norte, trimestre de crisis…",
      paste0("El grupo con ", M("D = 0"), " es la categoría de referencia."),
      "El coeficiente de la ficticia mide la diferencia respecto a ese grupo."),
    c("Promotion on, CAP subsidy, northern region, crisis quarter…",
      paste0("The group with ", M("D = 0"), " is the reference category."),
      "The dummy coefficient measures the difference from that group.")),
    0.6, 3.8, 7.0, 3.0, size = 18)
  x <- caja_texto(x, 8.1, 2.0, 4.6, 3.6, tr("En los casos del manual", "In the cases of the book"),
    tr(sprintf("A: promoción (%d de %d observaciones) y temporada alta. B: PAC y región norte. C: %d trimestres de crisis de %d. Las referencias: sin promoción, sin PAC, sur y trimestre normal.",
               sum(caso_a$promocion), nrow(caso_a), n_crisis, nrow(caso_c)),
       sprintf("A: promotion (%d of %d observations) and high season. B: CAP subsidy and northern region. C: %d crisis quarters out of %d. The references: no promotion, no CAP, south and a normal quarter.",
               sum(caso_a$promocion), nrow(caso_a), n_crisis, nrow(caso_c))),
    oscura = TRUE, size = 17)

  # 6 · 8.2 Desplazamiento del término independiente
  x <- diapo_contenido(x, tr("8.2 · Efecto sobre el término independiente", "8.2 · Effect on the intercept"),
    tr("Misma pendiente, distinto punto de partida", "Same slope, different starting point"), tema)
  x <- formula_caja(x, "Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 D_i + u_i", 0.6, 2.0, 5.4, 0.9, size = 24)
  x <- vinetas(x, tr(
    c(paste0("Si ", M("D_i = 0"), ": término independiente ", M("\\beta_0"), "."),
      paste0("Si ", M("D_i = 1"), ": término independiente ", M("\\beta_0 + \\beta_2"), "."),
      paste0(M("\\beta_2"), ": diferencia entre grupos con ", M("X"), " constante."),
      paste0("La pendiente ", M("\\beta_1"), " es la misma para los dos grupos: rectas paralelas.")),
    c(paste0("If ", M("D_i = 0"), ": intercept ", M("\\beta_0"), "."),
      paste0("If ", M("D_i = 1"), ": intercept ", M("\\beta_0 + \\beta_2"), "."),
      paste0(M("\\beta_2"), ": gap between groups holding ", M("X"), " constant."),
      paste0("The slope ", M("\\beta_1"), " is the same for both groups: parallel lines."))),
    0.6, 3.15, 5.4, 3.6, size = 17)
  b <- coef(ma); pm <- mean(caso_a$precio_propio)
  grupos <- tr(c("Sin promoción", "Con promoción"), c("No promotion", "Promotion"))
  df_a <- data.frame(pub = caso_a$gasto_publicidad,
                     v = caso_a$ventas - b[["precio_propio"]] * (caso_a$precio_propio - pm),
                     g = factor(grupos[caso_a$promocion + 1], levels = grupos))
  rectas <- data.frame(g = factor(grupos, levels = grupos),
                       a = b[[1]] + b[["precio_propio"]] * pm + c(0, b[["promocion"]]), s = b[["gasto_publicidad"]])
  g_a <- ggplot(df_a, aes(pub, v, colour = g)) +
    geom_abline(data = rectas, aes(intercept = a, slope = s, colour = g), linewidth = 1.3, show.legend = FALSE) +
    geom_point(size = 2.6, alpha = 0.9) +
    scale_colour_manual(values = c(COL$navy, COL$oro), name = NULL) +
    scale_x_continuous(labels = function(v) num(v, 1)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Gasto en publicidad (MM€)", "Advertising spend (€m)"),
         y = tr("Ventas al precio medio (MM€)", "Sales at the mean price (€m)")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_a, 6.3, 1.95, 6.5, 4.85)

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("Más allá de una ficticia", "Beyond a single dummy"),
    tr(c("Varias categorías", "La trampa de las ficticias", "Interacciones", "Series temporales y F conjunto"),
       c("Several categories", "The dummy variable trap", "Interactions", "Time series and the joint F")))

  # 8 · 8.2 Varias categorías
  x <- diapo_contenido(x, tr("8.2 · Varias categorías", "8.2 · Several categories"),
    tr(paste0(M("m"), " categorías, ", M("m - 1"), " ficticias"), paste0(M("m"), " categories, ", M("m - 1"), " dummies")), tema)
  x <- formula_caja(x, "\\text{imp}_i = \\beta_0 + \\beta_1\\,\\text{trab}_i + \\beta_2\\,\\text{NIE}_i + \\beta_3\\,\\text{CUE}_i + \\beta_4\\,\\text{SEPU}_i + \\beta_5\\,\\text{RIA}_i + u_i",
                    0.6, 2.0, 12.1, 0.9, size = 22)
  cod <- data.frame(p = c("Nieva", "Cuéllar", "Sepúlveda", "Riaza", tr("Segovia (referencia)", "Segovia (reference)")),
                    NIE = c(1L, 0L, 0L, 0L, 0L), CUE = c(0L, 1L, 0L, 0L, 0L), SEPU = c(0L, 0L, 1L, 0L, 0L), RIA = c(0L, 0L, 0L, 1L, 0L))
  x <- poner_tabla(x, tabla_pptx(cod, c(tr("Partido judicial", "District"), "NIE", "CUE", "SEPU", "RIA"), size = 14), 0.6, 3.2, 5.6)
  x <- vinetas(x, tr(
    c("Cifra de negocio de empresas agrarias de cinco partidos judiciales de Segovia.",
      paste0(M("\\beta_0"), ": nivel de la referencia, la categoría que se deja fuera."),
      paste0(M("\\beta_5 > 0"), ": en Riaza se factura más que en Segovia, a igual plantilla."),
      "Cambiar la referencia cambia los coeficientes, no el modelo."),
    c("Turnover of farm firms in five judicial districts of Segovia.",
      paste0(M("\\beta_0"), ": level of the reference, the category left out."),
      paste0(M("\\beta_5 > 0"), ": firms in Riaza turn over more than in Segovia, for the same workforce."),
      "Changing the reference changes the coefficients, not the model.")),
    6.6, 3.2, 6.2, 3.6, size = 16)

  # 9 · 8.2 Trampa de las ficticias
  x <- diapo_contenido(x, tr("8.2 · La trampa de las ficticias", "8.2 · The dummy variable trap"),
    tr(paste0("Con todas las categorías, ", M("\\mathbf{X}"), " pierde rango"), paste0("With every category, ", M("\\mathbf{X}"), " loses rank")), tema)
  x <- formula_caja(x, "\\mathbf{X} = \\begin{pmatrix} 1 & 1 & 0 \\\\ 1 & 0 & 1 \\\\ 1 & 1 & 0 \\\\ \\vdots & \\vdots & \\vdots \\\\ 1 & 0 & 1 \\end{pmatrix}",
                    0.6, 2.0, 4.0, 2.9, size = 22)
  x <- poner_texto(x, parrafo(tr("Constante, promoción y «sin promoción»", "Constant, promotion and «no promotion»"), 14, ID$gris, italic = TRUE, align = "center"),
                   0.6, 5.0, 4.0, 0.4)
  x <- vinetas(x, tr(
    c(paste0("Las dos ficticias suman la columna de unos: ", M("D_i + (1 - D_i) = 1"), "."),
      paste0("Multicolinealidad exacta: ", M("\\mathbf{X}'\\mathbf{X}"), " no tiene inversa y MCO no se puede calcular."),
      paste0("En el caso A, ", M(paste0("\\text{rango}(\\mathbf{X}) = ", rango)), " con 3 columnas."),
      paste0("Regla: con constante, ", M("m - 1"), " ficticias; la omitida es la referencia.")),
    c(paste0("The two dummies add up to the column of ones: ", M("D_i + (1 - D_i) = 1"), "."),
      paste0("Exact multicollinearity: ", M("\\mathbf{X}'\\mathbf{X}"), " has no inverse and OLS cannot be computed."),
      paste0("In case A, ", M(paste0("\\text{rank}(\\mathbf{X}) = ", rango)), " with 3 columns."),
      paste0("Rule: with a constant, ", M("m - 1"), " dummies; the omitted one is the reference."))),
    5.0, 2.05, 7.7, 3.4, size = 16)
  x <- caja_texto(x, 5.0, 5.45, 7.7, 1.3, tr("Supuesto de rango completo", "Full-rank assumption"),
    paste0(tr("Se exige ", "It requires "), M(tr("\\text{rango}(\\mathbf{X}) = k + 1", "\\text{rank}(\\mathbf{X}) = k + 1")),
           tr(": ninguna columna puede ser combinación lineal exacta de las demás.", ": no column may be an exact linear combination of the others.")),
    size = 15)

  # 10 · 8.2 Interacciones
  x <- diapo_contenido(x, tr("8.3 · Interacciones", "8.3 · Interactions"),
    tr("Cuando la ficticia también cambia la pendiente", "When the dummy also changes the slope"), tema)
  x <- formula_caja(x, "Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 D_i + \\beta_3 (D_i \\cdot X_i) + u_i", 0.6, 2.0, 7.4, 0.9, size = 24)
  x <- vinetas(x, tr(
    c(paste0("Si ", M("D_i = 0"), ": ", M("Y_i = \\beta_0 + \\beta_1 X_i + u_i"), "."),
      paste0("Si ", M("D_i = 1"), ": ", M("Y_i = (\\beta_0 + \\beta_2) + (\\beta_1 + \\beta_3) X_i + u_i"), "."),
      paste0(M("\\beta_3"), ": cambio de la pendiente en el grupo ", M("D = 1"), "."),
      paste0("Si ", M("\\beta_3"), " no es significativo, las rectas paralelas bastan.")),
    c(paste0("If ", M("D_i = 0"), ": ", M("Y_i = \\beta_0 + \\beta_1 X_i + u_i"), "."),
      paste0("If ", M("D_i = 1"), ": ", M("Y_i = (\\beta_0 + \\beta_2) + (\\beta_1 + \\beta_3) X_i + u_i"), "."),
      paste0(M("\\beta_3"), ": change in the slope for the ", M("D = 1"), " group."),
      paste0("If ", M("\\beta_3"), " is not significant, parallel lines are enough."))),
    0.6, 3.2, 7.4, 3.6, size = 17)
  x <- caja_dato(x, 8.4, 2.0, 4.3, 2.75, tr("Caso A: promoción × publicidad", "Case A: promotion × advertising"),
    M(paste0("\\hat{\\beta}_3 = ", numtex(si["gasto_publicidad:promocion", 1], 2))),
    tr(sprintf("p-valor = %s: no significativo.", num(si["gasto_publicidad:promocion", 4], 3)),
       sprintf("p-value = %s: not significant.", num(si["gasto_publicidad:promocion", 4], 3))), ID$navy)
  x <- caja_texto(x, 8.4, 4.95, 4.3, 1.8, tr("Lectura", "Reading"),
    tr("La promoción no cambia el efecto de la publicidad: basta con desplazar el término independiente.",
       "The promotion does not change the effect of advertising: shifting the intercept is enough."),
    oscura = TRUE, size = 15)

  # 11 · 8.2 Ficticias en series temporales
  x <- diapo_contenido(x, tr("8.2 · Ficticias en series temporales", "8.2 · Dummies in time series"),
    tr("Periodos, cambios de ley y estaciones", "Periods, law changes and seasons"), tema)
  x <- formula_caja(x, "Y_t = \\beta_0 + \\beta_1 X_t + \\delta_2 Q2_t + \\delta_3 Q3_t + \\delta_4 Q4_t + u_t", 0.6, 2.0, 12.1, 0.9, size = 24)
  x <- vinetas(x, tr(
    c("De periodo: 1 en los trimestres de crisis (caso C).",
      "De escalón: 1 desde que entra en vigor una ley (ventas de tabaco).",
      "De calendario: 1 los viernes o vísperas de festivo (efecto fin de semana).",
      paste0("Estacionales: tres ficticias trimestrales con constante; ", M("Q1"), " es la referencia.")),
    c("Period: 1 in the crisis quarters (case C).",
      "Step: 1 from the date a law comes into force (tobacco sales).",
      "Calendar: 1 on Fridays or the eve of holidays (weekend effect).",
      paste0("Seasonal: three quarterly dummies with a constant; ", M("Q1"), " is the reference."))),
    0.6, 3.2, 7.6, 3.6, size = 17)
  x <- caja_texto(x, 8.6, 3.2, 4.1, 2.4, tr("Cuatro estaciones", "Four seasons"),
    tr("Con constante, solo tres ficticias trimestrales. Con las cuatro, de nuevo la trampa: suman la columna de unos.",
       "With a constant, only three quarterly dummies. With all four, the trap again: they add up to the column of ones."),
    oscura = TRUE, size = 16)

  # 12 · 8.2 F conjunto
  x <- diapo_contenido(x, tr("8.2 · Contraste F conjunto", "8.2 · Joint F test"),
    tr("¿Aportan algo las ficticias en conjunto?", "Do the dummies add anything jointly?"), tema)
  x <- formula_caja(x, tr("F = \\frac{(\\text{SCR}_R - \\text{SCR}_{NR})/q}{\\text{SCR}_{NR}/(n-k-1)}", "F = \\frac{(\\text{RSS}_R - \\text{RSS}_{UR})/q}{\\text{RSS}_{UR}/(n-k-1)}"),
                    0.6, 2.0, 5.6, 1.5, size = 24)
  x <- vinetas(x, tr(
    c(paste0(M("H_0"), ": todas las ficticias del grupo valen cero."),
      paste0(M("q"), ": número de ficticias; ", M("R"), ": modelo sin ellas; ", M("NR"), ": modelo completo."),
      paste0("Se rechaza ", M("H_0"), " si el p-valor es menor que ", M("\\alpha"), ".")),
    c(paste0(M("H_0"), ": all the dummies in the group are zero."),
      paste0(M("q"), ": number of dummies; ", M("R"), ": model without them; ", M("UR"), ": full model."),
      paste0(M("H_0"), " is rejected if the p-value is below ", M("\\alpha"), "."))),
    0.6, 3.75, 5.6, 3.0, size = 16)
  x <- caja_texto(x, 6.6, 5.05, 6.1, 1.7, tr("Referencia y significatividad", "Reference and significance"),
    tr("Una t significativa dice que esa categoría difiere de la referencia, no que «importe». Si cambia la referencia, cambian las t; el F conjunto, no.",
       "A significant t says that category differs from the reference, not that it «matters». If the reference changes, the t tests change; the joint F does not."),
    oscura = TRUE, size = 14)
  x <- caja_dato(x, 6.6, 2.0, 2.95, 2.85, tr("A · promoción y temporada", "A · promotion and season"),
    M(paste0("F = ", numtex(F_a$F[2], 2))),
    tr(sprintf("p-valor = %s. Se rechaza %s: aportan.", pval(F_a$`Pr(>F)`[2]), M("H_0")),
       sprintf("p-value = %s. %s is rejected: they matter.", pval(F_a$`Pr(>F)`[2]), M("H_0"))), ID$navy)
  x <- caja_dato(x, 9.75, 2.0, 2.95, 2.85, tr("B · PAC y región", "B · CAP and region"),
    M(paste0("F = ", numtex(F_b$F[2], 2))),
    tr(sprintf("p-valor = %s. No se rechaza %s: no aportan.", pval(F_b$`Pr(>F)`[2]), M("H_0")),
       sprintf("p-value = %s. %s is not rejected: they add nothing.", pval(F_b$`Pr(>F)`[2]), M("H_0"))), ID$oro)

  # 13 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Los casos del manual", "The cases of the book"),
    tr(c("A: la promoción", "B: la PAC y la región", "C: la crisis"),
       c("A: the promotion", "B: the CAP subsidy and the region", "C: the crisis")))

  # 14 · 8.3.1 Caso A
  x <- diapo_contenido(x, tr("8.4.1 · Caso A: la promoción", "8.4.1 · Case A: the promotion"),
    tr("La promoción suma ventas a igual precio", "The promotion adds sales at the same price"), tema)
  x <- poner_tabla(x, tabla_pptx(tab_coef(ma, tr(c("Constante", "Precio propio", "Publicidad", "Promoción (ficticia)"),
                                                c("Constant", "Own price", "Advertising", "Promotion (dummy)"))), cab_coef, size = 15), 0.6, 2.05, 7.4)
  x <- formula_caja(x, tr(eq_est(ma, "Y", c("precio", "publi", "promo")), eq_est(ma, "Y", c("price", "adv", "promo"))),
                    0.6, 4.55, 7.4, 0.9, size = 20)
  x <- poner_texto(x, parrafo(paste0(M("Y"), tr(": ventas (MM€) · ", ": sales (€m) · "), M("R^2"), " = ", num(summary(ma)$r.squared, 4)), 15, ID$gris),
                   0.6, 5.6, 7.4, 0.5)
  x <- caja_dato(x, 8.4, 2.05, 4.3, 2.3, tr("Efecto de la promoción", "Effect of the promotion"),
    M(paste0("\\hat{\\beta}_3 = ", numtex(sa["promocion", 1], 4))),
    tr(sprintf("p-valor = %s: significativo al 5 %%.", num(sa["promocion", 4], 4)),
       sprintf("p-value = %s: significant at 5 %%.", num(sa["promocion", 4], 4))), ID$navy)
  x <- caja_texto(x, 8.4, 4.55, 4.3, 2.2, tr("Lectura", "Reading"),
    tr(sprintf("Con el mismo precio y la misma publicidad, se venden %s más en promoción: el término independiente pasa de %s a %s.",
               mill(sa["promocion", 1]), num(sa[1, 1], 2), num(sa[1, 1] + sa["promocion", 1], 2)),
       sprintf("At the same price and advertising, sales are %s higher on promotion: the intercept moves from %s to %s.",
               mill(sa["promocion", 1]), num(sa[1, 1], 2), num(sa[1, 1] + sa["promocion", 1], 2))),
    oscura = TRUE, size = 15)

  # 15 · 8.3.2 Caso B
  x <- diapo_contenido(x, tr("8.4.2 · Caso B: la PAC y la región", "8.4.2 · Case B: the CAP subsidy and the region"),
    tr("Dos ficticias que no marcan diferencias", "Two dummies that make no difference"), tema)
  x <- poner_tabla(x, tabla_pptx(tab_coef(mb, tr(c("Constante", "Trabajadores", "Inversión", "PAC (ficticia)", "Región norte (ficticia)"),
                                                c("Constant", "Workers", "Investment", "CAP (dummy)", "Northern region (dummy)"))), cab_coef, size = 15), 0.6, 2.05, 7.4)
  x <- vinetas(x, tr(
    c(sprintf("PAC: %s más de cifra de negocio, pero p-valor = %s.", mill(sb["pac", 1], 2, "M€"), num(sb["pac", 4], 3)),
      sprintf("Norte frente a sur: %s más, p-valor = %s.", mill(sb["region", 1], 2, "M€"), num(sb["region", 4], 3))),
    c(sprintf("CAP: %s more turnover, but p-value = %s.", mill(sb["pac", 1], 2, "M€"), num(sb["pac", 4], 3)),
      sprintf("North versus south: %s more, p-value = %s.", mill(sb["region", 1], 2, "M€"), num(sb["region", 4], 3)))),
    0.6, 4.8, 7.4, 1.8, size = 16)
  x <- caja_dato(x, 8.4, 5.0, 4.3, 1.75, tr("F conjunto · PAC y región", "Joint F · CAP and region"),
    M(paste0("F = ", numtex(F_b$F[2], 2), tr("\\quad p = ", "\\quad p = "), numtex(F_b$`Pr(>F)`[2], 3))), NULL, ID$oro, size_valor = 24)
  x <- caja_texto(x, 8.4, 2.05, 4.3, 2.75, tr("Lectura", "Reading"),
    tr("A igual plantilla e inversión, ni la PAC ni la región se distinguen de cero. Solo los trabajadores son significativos al 5 %.",
       "For the same workforce and investment, neither the CAP subsidy nor the region differs from zero. Only workers are significant at 5 %."),
    oscura = TRUE, size = 15)

  # 16 · 8.3.3 Caso C
  x <- diapo_contenido(x, tr("8.4.3 · Caso C: la crisis", "8.4.3 · Case C: the crisis"),
    tr("En crisis, el consumo autónomo cae", "In a crisis, autonomous consumption falls"), tema)
  anios <- substr(caso_c$trimestre, 1, 4)
  df_c <- data.frame(t = seq_len(nrow(caso_c)), r = residuals(mc0),
                     g = factor(tr(c("Normal", "Crisis"), c("Normal", "Crisis"))[caso_c$crisis + 1],
                                levels = tr(c("Normal", "Crisis"), c("Normal", "Crisis"))))
  cortes <- which(substr(caso_c$trimestre, 6, 7) == "Q1")
  cortes <- cortes[seq(1, length(cortes), by = 2)]
  g_c <- ggplot(df_c, aes(t, r)) +
    annotate("rect", xmin = min(df_c$t[caso_c$crisis == 1]) - 0.5, xmax = max(df_c$t[caso_c$crisis == 1]) + 0.5,
             ymin = -Inf, ymax = Inf, fill = COL$oro_cl, alpha = 0.5) +
    geom_hline(yintercept = 0, colour = COL$gris, linetype = "dashed") +
    geom_line(colour = COL$navy, linewidth = 0.8) +
    geom_point(aes(colour = g), size = 2.6) +
    scale_colour_manual(values = c(COL$navy, COL$oro), name = NULL) +
    scale_x_continuous(breaks = cortes, labels = anios[cortes]) +
    scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = NULL, y = tr("Residuo sin la ficticia (MM€)", "Residual without the dummy (€m)")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_c, 0.6, 1.95, 7.4, 4.85)
  x <- caja_dato(x, 8.4, 2.05, 4.3, 3.0, tr("Efecto de la crisis", "Effect of the crisis"),
    M(paste0("\\hat{\\beta}_3 = ", numtex(sc["crisis", 1], 0))),
    tr(sprintf("Con el mismo PIB y tipo de interés, el consumo es %s menor en crisis. IC al 95 %%: [%s; %s].",
               mill(-sc["crisis", 1], 0), num(ic_c[1], 0), num(ic_c[2], 0)),
       sprintf("For the same GDP and interest rate, consumption is %s lower in the crisis. 95 %% CI: [%s; %s].",
               mill(-sc["crisis", 1], 0), num(ic_c[1], 0), num(ic_c[2], 0))), ID$oro)
  x <- caja_texto(x, 8.4, 5.25, 4.3, 1.5, tr("Lectura del gráfico", "Reading the chart"),
    tr("Sin la ficticia, el modelo sobrestima el consumo en todos los trimestres de crisis.",
       "Without the dummy, the model overestimates consumption in every crisis quarter."),
    oscura = TRUE, size = 14)

  # 17 · Gretl → R
  x <- diapo_contenido(x, tr("Gretl → R", "Gretl → R"), tr("Las ficticias en Gretl y en R", "Dummies in Gretl and in R"), tema)
  sw <- data.frame(
    op = tr(c("Crear ficticias de una variable discreta", "Ficticias estacionales", "Estimar con ficticia", "Interacción", "F conjunto de las ficticias"),
            c("Dummies from a discrete variable", "Seasonal dummies", "Estimate with a dummy", "Interaction", "Joint F of the dummies")),
    gretl = tr(c("Añadir → Variables ficticias", "Añadir → Variables ficticias periódicas", "Modelo → Mínimos cuadrados ordinarios",
                 "Añadir → Definir nueva variable (D*X)", "Contrastes → Omitir variables"),
               c("Add → Dummies for selected discrete variables", "Add → Periodic dummies", "Model → Ordinary least squares",
                 "Add → Define new variable (D*X)", "Tests → Omit variables")),
    r = c("factor(region)", "factor(trimestre)", "lm(y ~ x + d, data = datos)", "lm(y ~ x * d, data = datos)", "anova(m_restr, m_compl)"))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 14, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c(paste0("En R, factor() crea las ", M("m - 1"), " ficticias y deja fuera la primera categoría como referencia."),
      paste0("Excel de clase: estimación con ficticias por matrices y comprobación del rango de ", M("\\mathbf{X}"), ".")),
    c(paste0("In R, factor() creates the ", M("m - 1"), " dummies and leaves the first category out as the reference."),
      paste0("Class Excel file: estimation with dummies using matrices, and a check of the rank of ", M("\\mathbf{X}"), "."))),
    0.6, 5.35, 12.1, 1.5, size = 16)

  # 18 · Vuelta a los casos
  x <- diapo_contenido(x, tr("8.4 · Vuelta a los casos", "8.4 · Back to the cases"),
    tr("Tres ficticias, tres respuestas", "Three dummies, three answers"), tema)
  x <- caja_dato(x, 0.6, 2.05, 3.85, 2.75, tr("A · Promoción", "A · Promotion"), paste0("+", mill(sa["promocion", 1])),
    tr(sprintf("Más ventas con igual precio y publicidad. Significativa (p-valor = %s).", num(sa["promocion", 4], 3)),
       sprintf("More sales at the same price and advertising. Significant (p-value = %s).", num(sa["promocion", 4], 3))), ID$navy, size_valor = 28)
  x <- caja_dato(x, 4.75, 2.05, 3.85, 2.75, tr("B · PAC y región", "B · CAP and region"), tr("No significativas", "Not significant"),
    tr(sprintf("F conjunto = %s (p-valor = %s). La plantilla explica la cifra de negocio; la PAC y la región, no.", num(F_b$F[2], 2), num(F_b$`Pr(>F)`[2], 3)),
       sprintf("Joint F = %s (p-value = %s). The workforce explains turnover; the CAP subsidy and the region do not.", num(F_b$F[2], 2), num(F_b$`Pr(>F)`[2], 3))),
    ID$gris_med, size_valor = 26)
  x <- caja_dato(x, 8.9, 2.05, 3.85, 2.75, tr("C · Crisis", "C · Crisis"), mill(sc["crisis", 1], 0),
    tr(sprintf("Menos consumo autónomo en los %d trimestres de crisis, con PIB y tipo constantes. Significativa.", n_crisis),
       sprintf("Less autonomous consumption in the %d crisis quarters, holding GDP and the rate constant. Significant.", n_crisis)), ID$oro, size_valor = 26)
  x <- formula_caja(x, "\\hat{Y} = (\\hat{\\beta}_0 + \\hat{\\beta}_D\\,D) + \\hat{\\beta}_1 X_1 + \\dots + \\hat{\\beta}_k X_k", 0.6, 5.0, 12.1, 0.85, size = 22)
  x <- poner_texto(x, parrafo(tr(paste0("Siempre la misma pregunta: cuánto se desplaza ", M("Y"), " en el grupo ", M("D = 1"), " frente a la referencia."),
                                 paste0("Always the same question: how far ", M("Y"), " shifts in the ", M("D = 1"), " group against the reference.")),
                              16, ID$navy, bold = TRUE), 0.6, 6.0, 12.1, 0.8)

  # 19 · Cierre
  x <- diapo_cierre(x,
    tr(c("Una ficticia vale 1 si se da la característica y 0 si no.",
         paste0(M("\\beta_2"), " desplaza el término independiente: rectas paralelas."),
         paste0(M("m"), " categorías, ", M("m - 1"), " ficticias: si no, la trampa (rango incompleto)."),
         "Con interacción, la ficticia cambia también la pendiente.",
         "Las t comparan con la referencia; el F conjunto juzga el grupo.",
         "Promoción y crisis importan; la PAC y la región, no."),
       c("A dummy is 1 if the attribute is present and 0 otherwise.",
         paste0(M("\\beta_2"), " shifts the intercept: parallel lines."),
         paste0(M("m"), " categories, ", M("m - 1"), " dummies: otherwise, the trap (rank deficiency)."),
         "With an interaction, the dummy also changes the slope.",
         "t tests compare with the reference; the joint F judges the group.",
         "The promotion and the crisis matter; the CAP subsidy and the region do not.")),
    tr("Excel de clase del tema 8, scripts de R del tema 8 y app «Variables ficticias». Explicación completa en el capítulo 8 del manual.",
       "Chapter 8 class Excel file, Chapter 8 R scripts and the «Dummy variables» app. Full explanation in Chapter 8 of the book."),
    tema)

  guardar_pptx(x, "T08_mEC_Cualitativas.pptx", "T08_mEC_Qualitative_Variables.pptx")
}
