# ══════════════════════════════════════════════════════════════════
# Web de la asignatura · plan semanal (fuente única, en inglés y en español a la vez)
#   Cada texto que ven los alumnos se escribe como TT("inglés", "español"): la web usa el inglés en / y la traducción
#   en /es/ (diccionario ES_PLAN del motor). Las notas docentes (último campo de b()) van solo en español.
#   Calendario PROVISIONAL: la programación semanal del curso 2025-26 (materiales/ del autor) llevada al segundo
#   cuatrimestre de 2026-27, desde el lunes 1 de marzo de 2027: 15 semanas de clase y la semana 16 de exámenes,
#   con parada de Semana Santa (22–28 de marzo de 2027).
#   Clases: una clase en directo por semana, de 60 minutos, y de 90 en las semanas 1, 6, 7, 8, 11 y 14
#   (presentación, repaso, resolución y lanzamiento de actividades y orientaciones de examen). La semana 11 tiene
#   dos clases. Días PROVISIONALES: lunes; la segunda clase de la semana 11, jueves.
#   Cuando se conozcan el calendario y el horario reales, se cambian aquí (fechas, DIAS) y en CURSO$dias.
#   Material: códigos que resuelve web/build_web.R
#     book:NN  slides:NN  clase:NN  script:NN  app:NN  act:AN  ev:SN  form
# ══════════════════════════════════════════════════════════════════

ES_PLAN <- character()
TT <- function(en, es) {
  if (!is.na(ES_PLAN[en]) && ES_PLAN[[en]] != es) stop("«", en, "» tiene dos traducciones: «", ES_PLAN[[en]], "» y «", es, "»")
  ES_PLAN[[en]] <<- es
  en
}

b <- function(ini, fin, actividad, detalle = "", nota = "") tibble::tibble(ini = ini, fin = fin, actividad = actividad, detalle = detalle, nota = nota)

DIAS <- c(clase = 0L, larga = 0L)
CODIGOS <- c(clase = "C", larga = "L")       # letra de cada tipo de clase en los identificadores de bloque del motor
DURACION <- c(clase = 60L, larga = 90L)
sesion <- function(tipo, titulo, bloques, materiales = character(), preparar = character(), dia = DIAS[[tipo]]) {
  bl <- dplyr::bind_rows(bloques)
  if (bl$ini[1] != 0 || any(bl$ini[-1] != bl$fin[-nrow(bl)]) || bl$fin[nrow(bl)] != DURACION[[tipo]])
    stop("Bloques no contiguos o que no suman ", DURACION[[tipo]], " min: ", titulo)
  list(tipo = tipo, codigo = CODIGOS[[tipo]], titulo = titulo, dia = as.integer(dia), bloques = bl, materiales = materiales, preparar = preparar)
}
semana <- function(n, inicio, bloque, capitulos, titulo, resumen, sesiones, hitos = character(), aviso = "") {
  list(n = n, inicio = as.Date(inicio), bloque = bloque, capitulos = capitulos, titulo = titulo, resumen = resumen,
       sesiones = sesiones, hitos = hitos, aviso = aviso)
}

# Bloques que se repiten
repaso <- function(ini, fin, detalle = TT("Two questions on last week's chapter.", "Dos preguntas sobre el tema de la semana anterior.")) b(ini, fin, TT("Recap", "Repaso"), detalle)
cierre <- function(ini, fin, detalle = TT("What to read and practise before the next class.", "Qué leer y practicar antes de la próxima clase.")) b(ini, fin, TT("Wrap-up", "Cierre"), detalle)
expo <- function(ini, fin, detalle, nota = "") b(ini, fin, TT("Lecture", "Exposición"), detalle, nota)
problema <- function(ini, fin, detalle, nota = "") b(ini, fin, TT("Problem of the day", "Problema del día"), detalle, nota)
excel <- function(ini, fin, detalle, nota = "") b(ini, fin, TT("Guided practice in Excel", "Práctica guiada en Excel"), detalle, nota)
gretl <- function(ini, fin, detalle, nota = "Quien no tenga Gretl puede seguir con el script de R del tema.") b(ini, fin, TT("Practice with Gretl or R", "Práctica con Gretl o R"), detalle, nota)
app <- function(ini, fin, detalle) b(ini, fin, TT("App of the chapter", "App del tema"), detalle, "Proyectar la app y dejar que los alumnos cambien los parámetros.")
LEER <- function(n) TT(sprintf("Read chapter %d of the book.", n), sprintf("Lee el capítulo %d del manual.", n))
PORTATIL <- TT("Bring a laptop with Excel and Gretl (or RStudio).", "Trae un portátil con Excel y Gretl (o RStudio).")

B1 <- TT("I · Foundations of the linear regression model", "I · Fundamentos del modelo de regresión lineal")
B2 <- TT("II · Evaluation and analysis", "II · Evaluación y análisis")
B3 <- TT("III · Extending the linear regression model", "III · Avanzando en el modelo de regresión lineal")
CAP <- function(n) TT(sprintf("Chapter %d", n), sprintf("Tema %d", n))

plan_semanas <- function() list(

  semana(1, "2027-03-01", B1, CAP(1), TT("Introduction to econometrics", "Introducción a la econometría"),
    TT("How the course works; what econometrics is, the econometric method and the types of economic data.",
       "Cómo funciona la asignatura; qué es la econometría, el método econométrico y los tipos de datos económicos."),
    list(sesion("larga", TT("Course introduction and types of economic data", "Presentación de la asignatura y tipos de datos económicos"), list(
      b(0, 20, TT("Course introduction", "Presentación de la asignatura"),
        TT("Who teaches the course, what it covers, how it is assessed (final exam 60 %, which must be passed, and continuous assessment 40 %) and where to find the materials.",
           "Quién imparte la asignatura, de qué trata, cómo se evalúa (examen final 60 %, que hay que aprobar, y evaluación continua 40 %) y dónde están los materiales."),
        "Enseñar la web y explicar que los temas se publican según avanzamos. Presentación del profesor con el Tema 0 (PPTX en tu zona). El usuario y la clave del servidor RStudio se dan en clase, nunca en la web."),
      expo(20, 45, TT("What econometrics is and the six steps of the econometric method: theory, model, data, estimation, testing and forecasting.",
                      "Qué es la econometría y los seis pasos del método econométrico: teoría, modelo, datos, estimación, contraste y predicción.")),
      problema(45, 75, TT("What kinds of data can we use to model an economic reality? Cross-section, time series and panel data with the three cases of the book.",
                          "¿Qué tipos de datos podemos utilizar para modelizar una realidad económica? Sección cruzada, series temporales y datos de panel con los tres casos del manual."),
               "Casos del manual: A (demanda de un producto), B (cifra de negocio sectorial) y C (consumo agregado trimestral)."),
      app(75, 85, TT("Types of economic data: explore the three cases of the book.", "Tipos de datos económicos: explora los tres casos del manual.")),
      cierre(85, 90)),
      c("slides:01", "book:01", "app:01", "script:01"), c(PORTATIL))),
    hitos = TT("Course introduction", "Presentación de la asignatura")),

  semana(2, "2027-03-08", B1, CAP(2), TT("The OLS method", "El método MCO"),
    TT("The general linear regression model, its basic assumptions and ordinary least squares estimation in matrix form.",
       "El modelo de regresión lineal general, sus supuestos básicos y la estimación por mínimos cuadrados ordinarios en forma matricial."),
    list(sesion("clase", TT("How do we obtain an econometric model?", "¿Cómo obtenemos un modelo econométrico?"), list(
      repaso(0, 5, TT("The six steps of the econometric method and the types of data.", "Los seis pasos del método econométrico y los tipos de datos.")),
      expo(5, 25, TT("The general linear regression model, its basic assumptions and OLS estimation in matrix form.",
                     "El modelo de regresión lineal general, sus supuestos básicos y la estimación MCO en forma matricial.")),
      excel(25, 50, TT("OLS by hand: the X and Y matrices, (X'X)⁻¹ and X'Y with MMULT, MINVERSE and TRANSPOSE (sales of a chocolate drink).",
                       "MCO a mano: las matrices X e Y, (X'X)⁻¹ y X'Y con MMULT, MINVERSA y TRANSPONER (ventas de un cacao soluble)."),
            "Ficheros T02_Matrices y T02_Modelo_Ventas_ColaCao. Insistir en la columna de unos."),
      b(50, 57, TT("Check with R", "Comprobación con R"), TT("The same estimate with lm() in the chapter 2 script.", "La misma estimación con lm() en el script del tema 2.")),
      cierre(57, 60)),
      c("slides:02", "book:02", "clase:02", "script:02"), c(PORTATIL)))),

  semana(3, "2027-03-15", B1, CAP(2), TT("Interpretation and goodness of fit", "Interpretación y bondad del ajuste"),
    TT("Marginal effects, changes in units of measurement, non-linear functional forms, R² and adjusted R².",
       "Efectos marginales, cambios de unidades de medida, formas funcionales no lineales, R² y R² corregido."),
    list(sesion("clase", TT("How do we judge how good a model is?", "¿Cómo valoramos lo bueno que es un modelo?"), list(
      repaso(0, 5),
      expo(5, 25, TT("Marginal effects, changes in units of measurement and logarithms to capture non-linear relationships.",
                     "Efectos marginales, cambios de unidades de medida y logaritmos para recoger relaciones no lineales.")),
      problema(25, 45, TT("How do we judge how good a model is? Total, explained and residual sums of squares, R² and adjusted R².",
                          "¿Cómo valoramos lo bueno que es un modelo? Sumas de cuadrados total, explicada y residual, R² y R² corregido.")),
      excel(45, 55, TT("Goodness of fit and change of units in Excel.", "Bondad del ajuste y cambio de unidades en Excel."), "Ficheros T02_Bondad_Ajuste y T02_Cambio_Unidades."),
      cierre(55, 60, LEER(2))),
      c("slides:02", "clase:02", "app:02"), c(PORTATIL)))),

  semana(4, "2027-03-29", B1, CAP(3), TT("Properties of the OLS estimators", "Propiedades de los estimadores MCO"),
    TT("The Gauss–Markov theorem, the variance of the error and of the estimators, and standard errors. Activity 1 is launched.",
       "El teorema de Gauss-Markov, la varianza del error y de los estimadores y los errores estándar. Se lanza la actividad 1."),
    list(sesion("clase", TT("What characterises the estimators of a model?", "¿Qué caracteriza a los estimadores de un modelo?"), list(
      repaso(0, 5),
      expo(5, 30, TT("Gauss–Markov theorem: linearity, unbiasedness and efficiency; variance of the error, variance–covariance matrix of the estimators and standard errors.",
                     "Teorema de Gauss-Markov: linealidad, insesgadez y eficiencia; varianza del error, matriz de varianzas y covarianzas de los estimadores y errores estándar.")),
      excel(30, 45, TT("Standard errors of the coefficients with the manual OLS tool.", "Errores estándar de los coeficientes con la herramienta de MCO manual.")),
      b(45, 57, TT("Activity 1 launch", "Lanzamiento de la actividad 1"),
        TT("A1 · OLS estimation by hand in Excel: tasks, answer sheet, rubric and deadline.", "A1 · estimación MCO a mano con Excel: tareas, hoja de respuestas, rúbrica y fecha de entrega."),
        "Liberar A1 antes de la clase. Recordar: mínimo 4 decimales y fórmulas visibles; no vale solo el output de un programa."),
      cierre(57, 60)),
      c("slides:03", "book:03", "clase:03", "app:03", "act:A1"), c(PORTATIL))),
    hitos = TT("A1 launch", "Lanzamiento de A1")),

  semana(5, "2027-04-05", B2, CAP(4), TT("Hypothesis testing", "Contrastes de hipótesis"),
    TT("Basic concepts of hypothesis testing and the t test for a single coefficient.",
       "Conceptos básicos de los contrastes de hipótesis y el contraste t para un único coeficiente."),
    list(sesion("clase", TT("How do we know whether a variable is relevant in the model?", "¿Cómo sabemos si una variable es relevante en el modelo?"), list(
      repaso(0, 5),
      expo(5, 30, TT("Null and alternative hypotheses, significance level, one- and two-tailed tests and the t statistic.",
                     "Hipótesis nula y alternativa, nivel de significación, contrastes de una y dos colas y el estadístico t.")),
      problema(30, 50, TT("How do we know whether a variable is relevant in the model? Individual significance with the t statistic and the critical value.",
                          "¿Cómo sabemos si una variable es relevante en el modelo? Significación individual con el estadístico t y el valor crítico.")),
      gretl(50, 57, TT("t statistics and p-values of case A with the chapter 4 script.", "Estadísticos t y p-valores del caso A con el script del tema 4.")),
      cierre(57, 60)),
      c("slides:04", "book:04", "script:04"), c(LEER(3), PORTATIL)))),

  semana(6, "2027-04-12", B2, CAP(4), TT("Joint significance", "Significación conjunta"),
    TT("Test statistic and p-value, tests of linear restrictions and the overall significance F test. Activity 1 is due.",
       "Estadístico y p-valor, contrastes de restricciones lineales y contraste F de significación global. Se entrega la actividad 1."),
    list(sesion("larga", TT("Review session and joint significance", "Sesión de repaso y significación conjunta"), list(
      b(0, 15, TT("Review session", "Sesión de repaso"), TT("Questions on chapters 1 to 4 and on Activity 1, due tomorrow.", "Dudas de los temas 1 a 4 y de la actividad 1, que se entrega mañana."),
        "A1 vence el martes 13 de abril a las 23:59."),
      expo(15, 40, TT("Test statistic versus p-value; tests of one linear restriction (t) and of several restrictions (F).",
                      "Estadístico frente a p-valor; contrastes de una restricción lineal (t) y de varias restricciones (F).")),
      problema(40, 70, TT("Can we assess the relevance of all the variables together? Overall significance F test with the sums of squares.",
                          "¿Es posible evaluar la relevancia de todas las variables en conjunto? Contraste F de significación global con las sumas de cuadrados.")),
      app(70, 85, TT("Hypothesis testing: rejection regions, significance level and p-value.", "Contrastes de hipótesis: regiones de rechazo, nivel de significación y p-valor.")),
      cierre(85, 90)),
      c("slides:04", "app:04", "script:04"), c(PORTATIL))),
    hitos = TT("A1 deadline · Tuesday 13 April", "Entrega de A1 · martes 13 de abril")),

  semana(7, "2027-04-19", B2, CAP(5), TT("Confidence intervals", "Intervalos de confianza"),
    TT("Solution of Activity 1; confidence intervals for one coefficient and for a linear combination of coefficients. Activity 2 is launched.",
       "Resolución de la actividad 1; intervalos de confianza para un coeficiente y para una combinación lineal de coeficientes. Se lanza la actividad 2."),
    list(sesion("larga", TT("Activity 1 solution, confidence intervals and Activity 2", "Resolución de la actividad 1, intervalos de confianza y actividad 2"), list(
      b(0, 25, TT("Activity 1 solution", "Resolución de la actividad 1"),
        TT("Worked solution of A1: matrices, coefficients, R², change of units and error variance.", "Solución comentada de A1: matrices, coeficientes, R², cambio de unidades y varianza del error."),
        "Liberar solutions-A1 al acabar la clase."),
      expo(25, 50, TT("Confidence intervals for one coefficient and for a linear combination; using intervals to test hypotheses.",
                      "Intervalos de confianza para un coeficiente y para una combinación lineal; uso de los intervalos para contrastar hipótesis.")),
      problema(50, 75, TT("How can we use confidence intervals to learn about the model? The interval for a coefficient at 90 % and 95 %.",
                          "¿Cómo podemos aplicar los intervalos de confianza para obtener información del modelo? El intervalo de un coeficiente al 90 % y al 95 %.")),
      b(75, 87, TT("Activity 2 launch", "Lanzamiento de la actividad 2"),
        TT("A2 · modelling and hypothesis testing with Gretl or R: tasks, answer sheet, rubric and deadline.", "A2 · modelización y contrastes con Gretl o R: tareas, hoja de respuestas, rúbrica y fecha de entrega."),
        "Liberar A2 antes de la clase. Recordar: outputs copiados como texto, sin capturas."),
      cierre(87, 90)),
      c("slides:05", "book:05", "act:A1", "act:A2"), c(LEER(4)))),
    hitos = c(TT("A1 solution", "Resolución de A1"), TT("A2 launch", "Lanzamiento de A2"))),

  semana(8, "2027-04-26", B2, CAP(5), TT("Exam guidance and Gretl", "Orientaciones de examen y Gretl"),
    TT("How the final exam works, a practice exam case by hand and the first session with Gretl.",
       "Cómo es el examen final, un supuesto orientativo a mano y la primera sesión con Gretl."),
    list(sesion("larga", TT("Exam guidance and confidence intervals with Gretl", "Orientaciones de examen e intervalos de confianza con Gretl"), list(
      b(0, 30, TT("Exam guidance", "Orientaciones para el examen"),
        TT("Exam format: a hand-calculation exercise and a Gretl exercise, 5 points each. The order of work and the practice exam case.",
           "Formato del examen: un ejercicio de cálculo manual y otro con Gretl, de 5 puntos cada uno. El orden de trabajo y el supuesto orientativo."),
        "Supuesto orientativo S0: liberar el enunciado al empezar y el solucionario al terminar."),
      excel(30, 55, TT("The practice exam case by hand: matrices, coefficients, sums of squares, R², intervals and tests.",
                       "El supuesto orientativo a mano: matrices, coeficientes, sumas de cuadrados, R², intervalos y contrastes.")),
      gretl(55, 80, TT("First Gretl session: importing an Excel file, OLS estimation and confidence intervals with the cigarette sales data.",
                       "Primera sesión con Gretl: importar un Excel, estimar por MCO y calcular intervalos de confianza con los datos de venta de cigarrillos.")),
      cierre(80, 90)),
      c("slides:05", "clase:05", "ev:S0", "script:05", "app:05"), c(PORTATIL))),
    hitos = TT("Exam guidance", "Orientaciones para el examen")),

  semana(9, "2027-05-03", B2, CAP(6), TT("Specification problems", "Problemas de especificación"),
    TT("Including irrelevant variables, omitting relevant ones, how to detect specification problems and proxy variables.",
       "Inclusión de variables irrelevantes, omisión de variables relevantes, cómo detectar problemas de especificación y variables proxy."),
    list(sesion("clase", TT("Omitted and irrelevant variables", "Variables omitidas e irrelevantes"), list(
      repaso(0, 5),
      expo(5, 30, TT("Specification errors: irrelevant variables (less precision), omitted relevant variables (bias) and unobservable variables.",
                     "Errores de especificación: variables irrelevantes (menos precisión), variables relevantes omitidas (sesgo) y variables no observables.")),
      problema(30, 50, TT("How do we detect the omission of relevant variables in a model? And the inclusion of irrelevant ones?",
                          "¿Cómo detectamos la omisión de variables relevantes en un modelo? ¿Y la inclusión de variables irrelevantes?")),
      app(50, 57, TT("Omitted-variable bias: what happens to the coefficients when a variable is left out.", "Sesgo por omisión: qué les pasa a los coeficientes cuando se deja fuera una variable.")),
      cierre(57, 60)),
      c("slides:06", "book:06", "script:06", "app:06"), c(LEER(5))))),

  semana(10, "2027-05-10", B2, CAP(7), TT("Prediction and residual analysis", "Predicción y análisis de residuos"),
    TT("Point prediction, prediction intervals for the mean and for an individual value, and residual analysis. Activity 2 is due.",
       "Predicción puntual, intervalos de predicción para el valor medio y para un valor individual y análisis de residuos. Se entrega la actividad 2."),
    list(sesion("clase", TT("How do we use the regression model to make decisions?", "¿Cómo utilizamos el modelo de regresión para la toma de decisiones?"), list(
      repaso(0, 5),
      expo(5, 30, TT("Point prediction, interval for the mean and for an individual value; residual analysis, leverage and Cook's distance.",
                     "Predicción puntual, intervalo para el valor medio y para un valor individual; análisis de residuos, leverage y distancia de Cook.")),
      problema(30, 50, TT("How do we use the regression model to make decisions? A sales forecast with its prediction interval.",
                          "¿Cómo utilizamos el modelo de regresión para la toma de decisiones? Una previsión de ventas con su intervalo de predicción.")),
      excel(50, 57, TT("Leverage and Cook's distance in Excel.", "Leverage y distancia de Cook en Excel."), "A2 vence el martes 11 de mayo a las 23:59."),
      cierre(57, 60)),
      c("slides:07", "book:07", "clase:07", "app:07", "script:07"), c(PORTATIL))),
    hitos = TT("A2 deadline · Tuesday 11 May", "Entrega de A2 · martes 11 de mayo")),

  semana(11, "2027-05-17", B3, CAP(8), TT("Qualitative variables", "Variables cualitativas"),
    TT("Solution of Activity 2; binary variables and models with one type of qualitative information. Activity 3 is launched.",
       "Resolución de la actividad 2; variables binarias y modelos con un único tipo de información cualitativa. Se lanza la actividad 3."),
    list(
      sesion("larga", TT("Activity 2 solution", "Resolución de la actividad 2"), list(
        b(0, 40, TT("Activity 2 solution", "Resolución de la actividad 2"),
          TT("Worked solution of A2: full model, p-values, confidence intervals, improved model, F test and recommendation.",
             "Solución comentada de A2: modelo completo, p-valores, intervalos de confianza, modelo mejorado, contraste F y recomendación."),
          "Liberar solutions-A2 al acabar la clase."),
        b(40, 70, TT("Common mistakes", "Errores frecuentes"), TT("The most common mistakes in Activities 1 and 2 and how to avoid them in the exam.",
                                                                  "Los errores más frecuentes de las actividades 1 y 2 y cómo evitarlos en el examen.")),
        gretl(70, 85, TT("Re-estimating the A2 model and reading the Gretl output line by line.", "Volver a estimar el modelo de A2 y leer la salida de Gretl línea a línea.")),
        cierre(85, 90)),
        c("act:A2", "script:05"), c(PORTATIL)),
      sesion("clase", TT("How do we include qualitative information in a model?", "¿Cómo incluimos información cualitativa en un modelo?"), list(
        repaso(0, 5),
        expo(5, 30, TT("Binary variables: how to create them, models with a single qualitative feature and the shift in the intercept.",
                       "Variables binarias: cómo se crean, modelos con una única característica cualitativa y el desplazamiento del término independiente.")),
        problema(30, 47, TT("How do we include qualitative information in a model? Did a law change cigarette sales?",
                            "¿Cómo incluimos información cualitativa en un modelo? ¿Cambió una ley las ventas de cigarrillos?")),
        b(47, 57, TT("Activity 3 launch", "Lanzamiento de la actividad 3"),
          TT("A3 · full econometric analysis with diagnostics, in groups: tasks, group control sheet, rubric and deadline.",
             "A3 · análisis econométrico completo con diagnóstico, en grupo: tareas, hoja de control del grupo, rúbrica y fecha de entrega."),
          "Liberar A3 antes de la clase y explicar el protocolo de las actividades en grupo."),
        cierre(57, 60)),
        c("slides:08", "book:08", "clase:08", "act:A3"), c(LEER(8)), dia = 3L)),
    hitos = c(TT("A2 solution", "Resolución de A2"), TT("A3 launch", "Lanzamiento de A3"))),

  semana(12, "2027-05-24", B3, CAP(8), TT("Several qualitative variables", "Varias variables cualitativas"),
    TT("Several categories and the reference category, the dummy variable trap, interactions and dummies in time series.",
       "Varias categorías y la categoría de referencia, la trampa de las ficticias, interacciones y ficticias en series temporales."),
    list(sesion("clase", TT("What difference does qualitative information make in a model?", "¿Qué repercusión tiene la inclusión de variables cualitativas en un modelo?"), list(
      repaso(0, 5),
      expo(5, 30, TT("Several categories and the reference category; the dummy variable trap and full rank; interactions; dummies in time series.",
                     "Varias categorías y la categoría de referencia; la trampa de las ficticias y el rango completo; interacciones; ficticias en series temporales.")),
      problema(30, 50, TT("What difference do qualitative variables make? Turnover by district and the weekend effect on a share.",
                          "¿Qué repercusión tienen las variables cualitativas? La cifra de negocio por partido judicial y el efecto fin de semana en una acción.")),
      gretl(50, 57, TT("Dummy variables in Gretl and the joint F test.", "Variables ficticias en Gretl y el contraste F conjunto.")),
      cierre(57, 60)),
      c("slides:08", "clase:08", "app:08", "script:08"), c(PORTATIL)))),

  semana(13, "2027-05-31", B3, CAP(9), TT("Heteroskedasticity", "Heterocedasticidad"),
    TT("The concept of heteroskedasticity, its consequences for OLS, how to detect it and robust inference. Activity 3 is due.",
       "El concepto de heterocedasticidad, sus consecuencias sobre MCO, cómo detectarla e inferencia robusta. Se entrega la actividad 3."),
    list(sesion("clase", TT("Is a model with heteroskedasticity valid?", "¿Es válido un modelo que presente heterocedasticidad?"), list(
      repaso(0, 5),
      expo(5, 30, TT("Heteroskedasticity: concept, consequences for OLS, residual plots, Breusch–Pagan and White tests, robust standard errors.",
                     "Heterocedasticidad: concepto, consecuencias sobre MCO, gráficos de residuos, contrastes de Breusch-Pagan y de White y errores estándar robustos.")),
      problema(30, 50, TT("Is a model with heteroskedasticity valid? The Breusch–Pagan test and robust inference in Gretl.",
                          "¿Es válido un modelo que presente heterocedasticidad? El contraste de Breusch-Pagan y la inferencia robusta en Gretl."),
               "A3 vence el martes 1 de junio a las 23:59."),
      app(50, 57, TT("Heteroskedasticity: how the residuals and the standard errors change.", "Heterocedasticidad: cómo cambian los residuos y los errores estándar.")),
      cierre(57, 60)),
      c("slides:09", "book:09", "clase:09", "app:09", "script:09"), c(PORTATIL))),
    hitos = TT("A3 deadline · Tuesday 1 June", "Entrega de A3 · martes 1 de junio")),

  semana(14, "2027-06-07", B3, CAP(10), TT("Serial autocorrelation", "Autocorrelación serial"),
    TT("Solution of Activity 3; the concept of autocorrelation, its consequences, the Durbin–Watson test and robust inference.",
       "Resolución de la actividad 3; el concepto de autocorrelación, sus consecuencias, el contraste de Durbin-Watson e inferencia robusta."),
    list(sesion("larga", TT("Activity 3 solution and autocorrelation", "Resolución de la actividad 3 y autocorrelación"), list(
      b(0, 30, TT("Activity 3 solution", "Resolución de la actividad 3"),
        TT("Worked solution of A3: descriptive analysis, estimation, tests, diagnostics and the final model.",
           "Solución comentada de A3: análisis descriptivo, estimación, contrastes, diagnóstico y modelo final."),
        "Liberar solutions-A3 al acabar la clase."),
      expo(30, 55, TT("Serial correlation: concept, consequences for OLS, the Durbin–Watson test and inference robust to autocorrelation (HAC).",
                      "Correlación serial: concepto, consecuencias sobre MCO, contraste de Durbin-Watson e inferencia robusta a la autocorrelación (HAC).")),
      problema(55, 80, TT("Can we use a model with autocorrelation to forecast? Durbin–Watson in Gretl with the cigarette data.",
                          "¿Podemos utilizar para predecir un modelo que presenta autocorrelación? Durbin-Watson en Gretl con los datos de cigarrillos.")),
      app(80, 87, TT("Autocorrelation: patterns in the residuals and the Durbin–Watson statistic.", "Autocorrelación: patrones en los residuos y el estadístico de Durbin-Watson.")),
      cierre(87, 90)),
      c("slides:10", "book:10", "clase:10", "app:10", "script:10", "act:A3"), c(PORTATIL))),
    hitos = TT("A3 solution", "Resolución de A3")),

  semana(15, "2027-06-14", B3, TT("Chapters 1–10", "Temas 1–10"), TT("Review", "Repaso"),
    TT("Review of the course and a mock exam solved together.", "Repaso de la asignatura y un simulacro de examen resuelto en clase."),
    list(sesion("clase", TT("Review session and mock exam", "Sesión de repaso y simulacro de examen"), list(
      b(0, 10, TT("Exam format", "Formato del examen"), TT("The two exercises of the exam, the tables of critical values and how to organise the time.",
                                                              "Los dos ejercicios del examen, las tablas de valores críticos y cómo organizar el tiempo.")),
      b(10, 50, TT("Mock exam", "Simulacro de examen"), TT("Mock exam: the hand-calculation part and the Gretl part, solved together.",
                                                           "Simulacro: la parte de cálculo manual y la parte de Gretl, resueltas juntos."),
        "Simulacro S1: liberar el enunciado al empezar y el solucionario al terminar."),
      b(50, 57, TT("Questions", "Dudas"), TT("Last questions on the whole course.", "Últimas dudas de toda la asignatura.")),
      cierre(57, 60, TT("Good luck in the final exam.", "Suerte en el examen final."))),
      c("ev:S1"), c(PORTATIL))),
    hitos = TT("Mock exam", "Simulacro de examen"))
)

EXAMEN_FINAL <- list(fechas = TT("Exam week, 21–25 June 2027 (date to be confirmed)", "Semana de exámenes, 21–25 de junio de 2027 (fecha por confirmar)"),
                     texto = TT("Final exam · chapters 1–10 (a grade of at least 5.0 is required)", "Examen final · temas 1–10 (hace falta al menos un 5,0)"))
