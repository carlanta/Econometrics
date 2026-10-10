# ══════════════════════════════════════════════════════════════════
# Capítulo 01 · Introducción a la econometría · presentación con identidad CdeA (ES y EN)
# Ejecutar desde la raíz del proyecto:  Rscript R/presentaciones/cap01_pptx.R
# Contenido y numeración: 01-introduccion-econometria.qmd (secciones 1.1 a 1.5). Figuras con los mismos datos que
# el manual, en color corporativo. Titulares de UNA línea (≈ 45 caracteres).
# ══════════════════════════════════════════════════════════════════
for (L in c("es", "en")) {
  assign("idioma", L, envir = globalenv())
  source("R/presentaciones/econ_comun.R")
  source("R/presentaciones/pptx_helpers.R")
  tema <- tr("Tema 1", "Chapter 1")

  # Datos de las figuras del manual (misma semilla que el capítulo)
  set.seed(2024)
  n <- 30
  renta <- seq(1000, 4000, length.out = n)
  c_det <- 200 + 0.75 * renta
  c_eco <- 200 + 0.75 * renta + rnorm(n, 0, 150)
  niveles <- tr(c("Modelo matemático", "Modelo econométrico"),
                c("Mathematical model", "Econometric model"))
  df_mod <- data.frame(renta = rep(renta, 2), consumo = c(c_det, c_eco), tipo = factor(rep(niveles, each = n), levels = niveles))
  set.seed(2024)
  pib_sim <- seq(100000, 420000, length.out = 40) + rnorm(40, 0, 8000)
  cons_sim <- 2564.26 + 0.7572 * pib_sim + rnorm(40, 0, 5000)
  df_cons <- data.frame(pib = pib_sim / 1000, consumo = cons_sim / 1000)

  x <- nueva_presentacion()

  # 1 · Portada
  x <- diapo_portada(x, 1, tr("Introducción a la econometría", "Introduction to econometrics"),
    tr("¿Qué parte de cada euro adicional de renta se gastan los hogares?",
       "How much of each extra euro of income do households spend?"))

  # 2 · Índice
  x <- diapo_indice(x, data.frame(
    num = c("1.1", "1.2", "1.3", "1.4", "1.5"),
    titulo = tr(c("¿Qué es la econometría?", "El método econométrico: seis pasos", "Tipos de datos económicos",
                  "Interpretación de resultados: el análisis marginal", "Software econométrico: Excel, Gretl y R"),
                c("What is econometrics?", "The econometric method: six steps", "Types of economic data",
                  "Interpreting results: marginal analysis", "Econometric software: Excel, Gretl and R"))), tema)

  # 3 · Objetivos
  x <- diapo_objetivos(x, list(
    tr(c("Saber qué es", "Teoría económica, matemáticas y estadística aplicadas a datos reales: estimar, contrastar y predecir."),
       c("Know what it is", "Economic theory, mathematics and statistics applied to real data: estimate, test and forecast.")),
    tr(c("Seguir el método", "Los seis pasos, de la teoría a la predicción, con la función de consumo de Keynes."),
       c("Follow the method", "The six steps, from theory to forecasting, with Keynes's consumption function.")),
    tr(c("Reconocer los datos", "Corte transversal, series temporales, datos fusionados y panel, y los tres casos del manual."),
       c("Recognise the data", "Cross-section, time series, pooled and panel data, and the three cases of the book."))), tema)

  # 4 · Bloque 1
  x <- diapo_bloque(x, 1, tr("¿Qué es la econometría?", "What is econometrics?"),
    tr(c("Medición económica", "Estimar, contrastar y predecir", "El origen de la «regresión»"),
       c("Economic measurement", "Estimate, test and forecast", "Where «regression» comes from")))

  # 5 · 1.1 Definición
  x <- diapo_contenido(x, tr("1.1 · ¿Qué es la econometría?", "1.1 · What is econometrics?"),
    tr("Medir la economía con datos", "Measuring the economy with data"), tema)
  x <- poner_texto(x, parrafo(tr("«La ciencia social en la que la teoría económica, las matemáticas y la inferencia estadística se aplican al análisis de los fenómenos económicos» (Goldberger).",
                                 "«The social science in which economic theory, mathematics and statistical inference are applied to the analysis of economic phenomena» (Goldberger)."),
                              19, ID$gris, italic = TRUE), 0.6, 1.95, 12.1, 1.0)
  tres <- tr(list(c("Estimar", "¿Cuánto aumenta el consumo cuando la renta crece un euro?"),
                  c("Contrastar", "¿Es de verdad positiva y menor que uno la propensión a consumir?"),
                  c("Predecir", "¿Cuánto consumirán los hogares si el PIB crece un 2 %?")),
             list(c("Estimate", "How much does consumption rise when income grows by one euro?"),
                  c("Test", "Is the propensity to consume really positive and below one?"),
                  c("Forecast", "How much will households consume if GDP grows by 2 %?")))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 3.25, 3.85, 2.3, tres[[k]][1], tres[[k]][2], oscura = (k == 2), size = 17)
  x <- poner_texto(x, parrafo(tr("Sin teoría, el análisis no tiene dirección; sin matemáticas, no se formaliza; sin datos, la teoría es especulación.",
                                 "Without theory, analysis has no direction; without mathematics, nothing is formalised; without data, theory is speculation."),
                              17, ID$navy, bold = TRUE), 0.6, 5.85, 12.1, 0.9)

  # 6 · 1.1 Regresión
  x <- diapo_contenido(x, tr("1.1 · El origen de la «regresión»", "1.1 · Where «regression» comes from"),
    tr("Galton y la regresión hacia la media", "Galton and regression towards the mean"), tema)
  x <- vinetas(x, tr(
    c("Francis Galton, finales del siglo XIX: los hijos de padres muy altos tienden a ser más bajos que sus padres.",
      "Los valores extremos «regresan» hacia la media de la población.",
      "Hoy, una regresión explica cómo cambia la media de Y según los valores de X.",
      "El significado ha evolucionado; el nombre se ha quedado por tradición."),
    c("Francis Galton, late 19th century: the children of very tall parents tend to be shorter than their parents.",
      "Extreme values «regress» towards the population mean.",
      "Today, a regression explains how the mean of Y changes with the values of X.",
      "The meaning has evolved; the name has stayed by tradition.")),
    0.6, 2.1, 7.4, 4.6, size = 18)
  x <- caja_texto(x, 8.4, 2.2, 4.3, 4.3, tr("El econometrista", "The econometrician"),
    tr("Usa modelos, estadística y datos para analizar la economía y predecir. Su arte: supuestos específicos y realistas que aprovechen bien los datos disponibles.",
       "Uses models, statistics and data to analyse the economy and forecast. The art: specific, realistic assumptions that make the most of the data available."),
    oscura = TRUE, size = 16)

  # 7 · Bloque 2
  x <- diapo_bloque(x, 2, tr("El método econométrico", "The econometric method"),
    tr(c("Seis pasos, de la teoría a la predicción", "Del modelo matemático al econométrico", "La función de consumo de Keynes"),
       c("Six steps, from theory to forecasting", "From the mathematical to the econometric model", "Keynes's consumption function")))

  # 8 · 1.2 Seis pasos
  x <- diapo_contenido(x, tr("1.2 · El método econométrico", "1.2 · The econometric method"),
    tr("Seis pasos y una vuelta atrás si hace falta", "Six steps, and back again if needed"), tema)
  pasos <- tr(c("Teoría económica", "Especificación del modelo", "Datos", "Estimación MCO", "Contrastes e interpretación", "Predicción"),
              c("Economic theory", "Model specification", "Data", "OLS estimation", "Tests and interpretation", "Forecasting"))
  for (k in 1:6) {
    left <- 0.6 + (k - 1) * 2.05
    x <- rect(x, left, 2.3, 1.85, 1.9, if (k %in% c(4, 5)) ID$navy else ID$suave)
    x <- poner_texto(x, list(parrafo(sprintf("%d", k), 30, ID$oro, bold = TRUE, font = "Georgia", align = "center"),
                             parrafo(pasos[k], 15, if (k %in% c(4, 5)) ID$blanco else ID$navy, bold = TRUE, align = "center")),
                     left + 0.1, 2.4, 1.65, 1.7)
  }
  x <- rect(x, 2.65 + 0.9, 4.45, 8.2 - 2.65, 0.03, ID$oro)
  x <- poner_texto(x, parrafo(tr("↺  Si los resultados no convencen, se vuelve a especificar el modelo.",
                                 "↺  If the results are not convincing, the model is specified again."),
                              16, ID$gris, italic = TRUE, align = "center"), 0.6, 4.6, 12.1, 0.5)
  x <- vinetas(x, tr(
    c(paste0("Siempre se parte de una teoría: Keynes, ", M("0 < \\mathit{PMC} < 1"), "."), "Los temas 2 a 10 del curso desarrollan los pasos 4, 5 y 6."),
    c(paste0("We always start from a theory: Keynes, ", M("0 < \\mathit{MPC} < 1"), "."), "Chapters 2 to 10 of the course develop steps 4, 5 and 6.")),
    0.6, 5.3, 12.1, 1.5, size = 17)

  # 9 · 1.2 Modelo matemático y econométrico
  x <- diapo_contenido(x, tr("1.2 · Especificación del modelo", "1.2 · Model specification"),
    tr("Del modelo exacto al modelo con error", "From the exact model to the model with error"), tema)
  x <- formula_caja(x, "C = a + b \\cdot Y^{d}", 0.6, 2.0, 4.0, 0.9, size = 26)
  x <- formula_caja(x, "Y = \\beta_0 + \\beta_1 X + u", 0.6, 3.1, 4.0, 0.9, size = 26)
  x <- vinetas(x, tr(
    c(paste0(M("Y"), ": lo que queremos explicar (consumo)."), paste0(M("X"), ": lo que explica (renta)."),
      paste0(M("\\beta_1"), ": la propensión marginal a consumir."), paste0(M("u"), ": todo lo demás que influye y no medimos.")),
    c(paste0(M("Y"), ": what we want to explain (consumption)."), paste0(M("X"), ": what explains it (income)."),
      paste0(M("\\beta_1"), ": the marginal propensity to consume."), paste0(M("u"), ": everything else that matters and we do not measure."))),
    0.6, 4.25, 4.2, 2.6, size = 15)
  g_mod <- ggplot(df_mod, aes(renta, consumo)) +
    geom_line(data = data.frame(renta = renta, consumo = c_det), colour = COL$oro, linewidth = 1.3) +
    geom_point(colour = COL$navy, size = 2.2, alpha = 0.85) +
    facet_wrap(~ tipo) +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("Renta disponible (€)", "Disposable income (€)"), y = tr("Consumo (€)", "Consumption (€)")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_mod, 5.0, 1.95, 7.8, 4.85)

  # 10 · 1.2 Estimación e interpretación
  x <- diapo_contenido(x, tr("1.2 · Estimación e interpretación", "1.2 · Estimation and interpretation"),
    tr("¿Tenía razón Keynes?", "Was Keynes right?"), tema)
  x <- formula_caja(x, tr("\\hat{Y}_t = 2.564{,}26 + 0{,}7572 \\cdot X_t", "\\hat{Y}_t = 2{,}564.26 + 0.7572 \\cdot X_t"), 0.6, 2.05, 12.1, 1.0, size = 30)
  x <- caja_dato(x, 0.6, 3.35, 3.85, 3.2, tr("Propensión marginal", "Marginal propensity"), M(paste0("\\hat{\\beta}_1 = ", tr("0{,}7572", "0.7572"))),
                 tr("De cada euro adicional de renta, el consumo sube unos 0,76 €.", "Of each extra euro of income, consumption rises by about €0.76."), ID$navy)
  x <- caja_dato(x, 4.75, 3.35, 3.85, 3.2, tr("Coherente con la teoría", "Consistent with theory"), M("0 < \\hat{\\beta}_1 < 1"),
                 tr("Positiva y menor que uno, como predijo Keynes.", "Positive and below one, as Keynes predicted."), ID$oro)
  x <- caja_dato(x, 8.9, 3.35, 3.85, 3.2,tr("Pero falta el rigor", "But rigour is missing"), tr("Temas 4 y 5", "Chapters 4–5"),
                 tr("Contrastes e intervalos dirán si el resultado es estadísticamente sólido.", "Tests and intervals will tell whether the result is statistically sound."), ID$gris_med, size_valor = 26)

  # 11 · Bloque 3
  x <- diapo_bloque(x, 3, tr("Datos y análisis marginal", "Data and marginal analysis"),
    tr(c("Cuatro tipos de datos", "Los tres casos del manual", "Ceteris paribus", "Excel, Gretl y R"),
       c("Four types of data", "The three cases of the book", "Ceteris paribus", "Excel, Gretl and R")))

  # 12 · 1.3 Tipos de datos
  x <- diapo_contenido(x, tr("1.3 · Tipos de datos económicos", "1.3 · Types of economic data"),
    tr("De dónde viene la variación", "Where the variation comes from"), tema)
  tipos <- data.frame(
    t = tr(c("Corte transversal", "Series temporales", "Datos fusionados", "Datos de panel"), c("Cross-section", "Time series", "Pooled cross-sections", "Panel data")),
    v = tr(c("Entre unidades, en un momento", "En el tiempo, una unidad", "Entre unidades y periodos", "Mismas unidades en el tiempo"),
           c("Across units, at one moment", "Over time, one unit", "Across units and periods", "Same units over time")),
    e = tr(c("Encuesta de hogares de 2023", "PIB trimestral de España", "Encuestas de 2020 y 2023", "Empresas cotizadas, 2015–2024"),
           c("2023 household survey", "Spain's quarterly GDP", "2020 and 2023 surveys", "Listed firms, 2015–2024")))
  x <- poner_tabla(x, tabla_pptx(tipos, tr(c("Tipo", "Variación", "Ejemplo"), c("Type", "Variation", "Example")), size = 16, texto = 1:3), 0.6, 2.1)
  x <- poner_texto(x, parrafo(tr("En series temporales las observaciones seguidas se parecen entre sí: es la autocorrelación del tema 10.",
                                 "In time series, consecutive observations resemble each other: this is the autocorrelation of chapter 10."),
                              16, ID$gris, italic = TRUE), 0.6, 5.6, 12.1, 0.8)

  # 13 · 1.3 Casos del manual
  x <- diapo_contenido(x, tr("1.3 · Los casos del manual", "1.3 · The cases of the book"),
    tr("Tres casos que acompañan todo el curso", "Three cases that run through the course"), tema)
  casos <- tr(list(c("A · Demanda de un producto", sprintf("Corte transversal · %d observaciones. Ventas según precio propio, precio del sustitutivo, publicidad, promoción y temporada.", nrow(caso_a))),
                   c("B · Cifra de negocio sectorial", sprintf("Corte transversal · %d empresas agrarias. Cifra de negocio según trabajadores, inversión, superficie, PAC y región.", nrow(caso_b))),
                   c("C · Consumo trimestral", sprintf("Serie temporal · %d trimestres (%s a %s). Consumo según PIB, tipo de interés, inflación y crisis.", nrow(caso_c), caso_c$trimestre[1], caso_c$trimestre[nrow(caso_c)]))),
              list(c("A · Product demand", sprintf("Cross-section · %d observations. Sales by own price, substitute price, advertising, promotion and season.", nrow(caso_a))),
                   c("B · Sector turnover", sprintf("Cross-section · %d farm firms. Turnover by workers, investment, land area, CAP subsidy and region.", nrow(caso_b))),
                   c("C · Quarterly consumption", sprintf("Time series · %d quarters (%s to %s). Consumption by GDP, interest rate, inflation and crisis.", nrow(caso_c), caso_c$trimestre[1], caso_c$trimestre[nrow(caso_c)]))))
  for (k in 1:3) x <- caja_texto(x, 0.6 + (k - 1) * 4.15, 2.15, 3.85, 3.6, casos[[k]][1], casos[[k]][2], oscura = (k == 2), size = 16)
  x <- poner_texto(x, parrafo(tr("Datos en data/ de los scripts de cada tema; la app del tema 1 los muestra.",
                                 "Data in the data/ folder of each chapter's scripts; the chapter 1 app shows them."),
                              16, ID$navy, bold = TRUE), 0.6, 6.0, 12.1, 0.7)

  # 14 · 1.4 Análisis marginal
  x <- diapo_contenido(x, tr("1.4 · El análisis marginal", "1.4 · Marginal analysis"),
    tr("Un euro más de renta, 0,76 euros más de consumo", "One more euro of income, €0.76 more consumption"), tema)
  g_cons <- ggplot(df_cons, aes(pib, consumo)) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = COL$oro, fill = COL$oro_cl, linewidth = 1.3) +
    geom_point(colour = COL$navy, size = 2.3, alpha = 0.85) +
    scale_x_continuous(labels = function(v) num(v, 0)) + scale_y_continuous(labels = function(v) num(v, 0)) +
    labs(x = tr("PIB (miles de millones de €)", "GDP (€ billion)"), y = tr("Consumo privado (miles de millones de €)", "Private consumption (€ billion)")) +
    tema_cdea(13)
  x <- imagen_ggplot(x, g_cons, 0.6, 2.0, 7.6, 4.8)
  x <- vinetas(x, tr(
    c(paste0(M("\\hat{\\beta}_1"), ": cambio esperado en ", M("Y"), " si ", M("X"), " sube una unidad, con todo lo demás constante (ceteris paribus)."),
      "La pendiente de la recta es la propensión marginal a consumir.",
      paste0(M("\\hat{\\beta}_0 = 2.564{,}26"), ": consumo autónomo; su lectura literal puede no tener sentido.")),
    c(paste0(M("\\hat{\\beta}_1"), ": expected change in ", M("Y"), " when ", M("X"), " rises by one unit, everything else held constant (ceteris paribus)."),
      "The slope of the line is the marginal propensity to consume.",
      paste0(M("\\hat{\\beta}_0 = 2{,}564.26"), ": autonomous consumption; its literal reading may make no sense."))),
    8.5, 2.2, 4.3, 4.6, size = 16)

  # 15 · 1.5 Software
  x <- diapo_contenido(x, tr("1.5 · Software econométrico", "1.5 · Econometric software"),
    tr("A mano, con Gretl y con R", "By hand, with Gretl and with R"), tema)
  sw <- data.frame(
    op = tr(c("Importar datos", "Estadísticos descriptivos", "Gráfico de dispersión", "Estimar por MCO"),
            c("Import data", "Descriptive statistics", "Scatter plot", "OLS estimation")),
    gretl = tr(c("Archivo → Abrir datos → Usuario", "Variable → Estadísticos descriptivos", "Variable → Gráfico X-Y", "Modelo → Mínimos cuadrados ordinarios"),
               c("File → Open data → User file", "Variable → Summary statistics", "Variable → X-Y scatter", "Model → Ordinary least squares")),
    r = c("readxl::read_excel(\"f.xlsx\")", "summary(datos)", "plot(datos$x, datos$y)", "lm(y ~ x, data = datos)"))
  x <- poner_tabla(x, tabla_pptx(sw, tr(c("Operación", "Gretl", "R"), c("Task", "Gretl", "R")), size = 15, mono = 3, texto = 1:2), 0.6, 2.1)
  x <- vinetas(x, tr(
    c("Excel: el cálculo a mano con matrices, para entender lo que hace el programa (parte manual del examen).",
      "Gretl: el programa del examen. R: los scripts y las apps del curso."),
    c("Excel: the calculation by hand with matrices, to understand what the software does (hand part of the exam).",
      "Gretl: the exam software. R: the course scripts and apps.")),
    0.6, 5.1, 12.1, 1.7, size = 16)

  # 16 · Cierre
  x <- diapo_cierre(x,
    tr(c("Econometría: teoría, matemáticas y estadística sobre datos reales.",
         "Seis pasos: teoría, modelo, datos, estimación, contraste y predicción.",
         paste0("El error ", M("u"), " recoge todo lo que influye y no medimos."),
         "Corte transversal, series temporales, fusionados y panel.",
         paste0(M("\\hat{\\beta}_1"), " es un efecto marginal ceteris paribus."),
         "Excel para entender, Gretl para el examen, R para practicar."),
       c("Econometrics: theory, mathematics and statistics on real data.",
         "Six steps: theory, model, data, estimation, testing and forecasting.",
         paste0("The error ", M("u"), " captures everything that matters and we do not measure."),
         "Cross-section, time series, pooled and panel data.",
         paste0(M("\\hat{\\beta}_1"), " is a ceteris paribus marginal effect."),
         "Excel to understand, Gretl for the exam, R to practise.")),
    tr("Scripts de R del tema 1 y app «Tipos de datos económicos». Explicación completa en el capítulo 1 del manual.",
       "Chapter 1 R scripts and the «Types of economic data» app. Full explanation in Chapter 1 of the book."),
    tema)

  guardar_pptx(x, "T01_mEC_Introduccion.pptx", "T01_mEC_Introduction.pptx")
}
