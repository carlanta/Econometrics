# ══════════════════════════════════════════════════════════════════
# Vídeos del curso y podcasts de repaso · Econometría (modelo: mFinEconomics/web/videos.R)
# El motor los pinta solo si este fichero define videos_sesion() (plan del profesor) y videos_alumno() (alumnos).
#
# Vídeos propios: los hacemos nosotros con el manual delante (13_Videos en el equipo local, crear.ps1 de cada
# carpeta), en español y en inglés. Cada vídeo tiene las dos versiones en la misma carpeta del servidor de webs:
#   /srv/webs/econometrics/videos/video-sNNX/<id>_ES.mp4 y <id>_EN.mp4   (NN = semana, X = C clase o L larga)
# La web de cada idioma enseña su versión y un botón pequeño con la del otro idioma.
# Los ficheros NO pasan por git ni por el publicador: se suben a mano (tar por puerta, webs:webs, 644) y nginx los
# sirve desde videos/publicos/, donde liberar.py pone un enlace solo cuando se libera la clave de la sesión.
#
# `listo = FALSE`: el vídeo está registrado pero todavía no se ha montado o subido. Los alumnos no lo ven y en el
# plan del profesor sale «en preparación». Al subirlo (las dos versiones), se cambia a TRUE y se vuelve a publicar.
# Duración: la de la locución de cada idioma (montaje.log), redondeada al minuto; el inglés es algo más largo.
# ══════════════════════════════════════════════════════════════════

v <- function(id, titulo_en, titulo_es, min_en, min_es, para, listo = TRUE)
  tibble(id = id, titulo_en = titulo_en, titulo_es = titulo_es,
         fichero_en = paste0(id, "_EN.mp4"), fichero_es = paste0(id, "_ES.mp4"),
         min_en = sprintf("%d min", min_en), min_es = sprintf("%d min", min_es), para = para, listo = listo)

PROPIOS <- list(

  # Tema 1 (semana 1, clase larga)
  "s01L" = bind_rows(
    v("T01_00_bienvenida", "Welcome to Econometrics", "Bienvenida a Econometría", 3, 3, "Qué vas a aprender y cómo vas a trabajar."),
    v("T01_01_que_es", "What is econometrics?", "¿Qué es la econometría?", 3, 2, "Medir la economía con datos."),
    v("T01_02_metodo", "The econometric method", "El método econométrico", 4, 3, "Seis pasos, de la teoría a la predicción."),
    v("T01_03_tipos_datos", "Types of economic data", "Tipos de datos económicos", 5, 3, "¿De dónde viene la variación?"),
    v("T01_04_analisis_marginal", "Marginal analysis", "El análisis marginal", 4, 3, "Cómo se lee un coeficiente, y con qué se trabaja.")),

  # Tema 2: el modelo y MCO a mano en la semana 2; interpretación y bondad del ajuste en la 3
  "s02C" = bind_rows(
    v("T02_01_mrlg", "The general linear regression model", "El modelo de regresión lineal general", 5, 4, "Una ecuación para n observaciones, y seis supuestos."),
    v("T02_02_mco_matrices_1", "OLS by hand (I): from the data to X'X and X'Y", "MCO a mano (I): de los datos a X'X y X'Y", 4, 3, "La fórmula del examen, paso a paso con DS_mini."),
    v("T02_03_mco_matrices_2", "OLS by hand (II): the inverse and the coefficients", "MCO a mano (II): la inversa y los coeficientes", 4, 3, "Pasos 3 y 4 con DS_mini, y la comprobación.")),

  "s03C" = bind_rows(
    v("T02_04_interpretacion", "Interpreting the coefficients", "Interpretar los coeficientes", 4, 3, "Número, unidades y ceteris paribus; y los logaritmos."),
    v("T02_05_bondad_ajuste", "Goodness of fit: R² and the standard error", "Bondad de ajuste: el R² y el error típico", 3, 3, "Cuánta variación de Y explica el modelo."),
    v("T02_06_casos", "Back to the cases", "Vuelta a los casos", 3, 2, "Los tres casos del curso, estimados por MCO.")),

  # Tema 3 (semana 4)
  "s04C" = bind_rows(
    v("T03_01_r2_ajustado", "The adjusted R²", "El R² ajustado", 4, 3, "¿Más variables es siempre mejor?"),
    v("T03_02_gauss_markov", "Gauss-Markov: OLS is BLUE", "Gauss-Markov: MCO es ELIO", 3, 3, "¿Por qué fiarnos de MCO y no de cualquier otra recta?"),
    v("T03_03_errores_estandar", "Error variance and standard errors", "Varianza del error y errores estándar", 4, 3, "La precisión de cada coeficiente, con DS_mini."),
    v("T03_04_casos", "Back to the cases", "Vuelta a los casos", 4, 3, "Modelos progresivos en los tres casos.")),

  # Tema 4: el contraste t en la semana 5; restricciones, F y significación global en la 6
  "s05C" = bind_rows(
    v("T04_01_logica", "The logic of a test", "La lógica del contraste", 3, 2, "¿Es real el efecto, o fruto del azar?"),
    v("T04_02_contraste_t", "The t test and the p-value", "El contraste t y el p-valor", 4, 3, "Una y dos colas, y el precio paso a paso.")),

  "s06L" = bind_rows(
    v("T04_03_restriccion_lineal", "One linear restriction", "Una restricción lineal", 3, 3, "Contrastar una combinación de coeficientes con el t."),
    v("T04_04_contraste_F", "Several restrictions: the F test", "Varias restricciones: el contraste F", 3, 3, "Comparar el modelo restringido con el completo."),
    v("T04_05_significatividad_global", "Overall significance", "Significatividad global", 3, 2, "¿Explica algo el modelo en su conjunto?"),
    v("T04_06_casos", "Back to the cases", "Vuelta a los casos", 4, 3, "Qué variables cuentan en cada caso.")),

  # Tema 5: la teoría en la semana 7; los casos con Gretl en la 8
  "s07L" = bind_rows(
    v("T05_01_concepto", "What a confidence interval is", "Qué es un intervalo de confianza", 3, 2, "Un rango de valores plausibles, y cómo se lee."),
    v("T05_02_calculo", "Building the interval", "Construir el intervalo", 4, 3, "Estimación, valor crítico y error estándar."),
    v("T05_03_dualidad", "Testing with the interval", "Contrastar con el intervalo", 3, 2, "¿Está el valor de la hipótesis nula dentro o fuera?"),
    v("T05_04_combinacion", "Interval for a linear combination", "Intervalo para una combinación lineal", 3, 2, "Cuando interesa el efecto conjunto de dos coeficientes.")),

  "s08L" = bind_rows(
    v("T05_05_casos", "Back to the cases", "Vuelta a los casos", 3, 2, "Los intervalos de confianza en los casos A, B y C.")),

  # Tema 6 (semana 9)
  "s09C" = bind_rows(
    v("T06_01_dos_errores", "Two specification errors", "Dos errores de especificación", 3, 2, "¿Qué es peor: que sobre una variable o que falte?"),
    v("T06_02_irrelevantes", "Irrelevant variables", "Variables de más", 3, 2, "Sin sesgo, pero con más varianza: el caso B."),
    v("T06_03_omitidas", "Omitted variables: the bias", "Variables de menos: el sesgo", 3, 3, "La fórmula del sesgo, su signo y el caso A sin publicidad."),
    v("T06_04_diagnostico", "Diagnosis and proxy variables", "Diagnóstico y variables proxy", 3, 2, "Cinco pasos para revisar la especificación."),
    v("T06_05_casos", "Back to the cases", "Vuelta a los casos", 3, 3, "La especificación de los casos A, B y C.")),

  # Tema 7 (semana 10)
  "s10C" = bind_rows(
    v("T07_01_puntual", "Point prediction", "La predicción puntual", 3, 2, "Sustituir un escenario en el modelo estimado."),
    v("T07_02_intervalos", "Intervals for the mean and for an individual", "Intervalos para la media y para un individuo", 3, 2, "Un 1 bajo la raíz marca la diferencia."),
    v("T07_03_residuos", "Residual analysis", "Análisis de residuos", 3, 2, "Lo que delata un modelo mal especificado."),
    v("T07_04_casos", "Back to the cases", "Vuelta a los casos", 3, 3, "Predecir en los casos A, B y C.")),

  # Tema 8: las ficticias en la segunda clase de la semana 11; el resto en la 12
  "s11C" = bind_rows(
    v("T08_01_ficticias", "Dummy variables", "Las variables ficticias", 3, 2, "Lo cualitativo, en ceros y unos.")),

  "s12C" = bind_rows(
    v("T08_02_trampa", "Several categories and the dummy variable trap", "Varias categorías y la trampa de las ficticias", 3, 2, "m categorías, m − 1 ficticias."),
    v("T08_03_interacciones", "Interactions with dummy variables", "Interacciones con variables ficticias", 3, 3, "Cuando la ficticia también cambia la pendiente."),
    v("T08_04_casos", "Back to the cases", "Vuelta a los casos", 4, 3, "Tres ficticias, tres respuestas.")),

  # Tema 9 (semana 13)
  "s13C" = bind_rows(
    v("T09_01_homocedasticidad", "Homoskedasticity and graphical detection", "Homocedasticidad y detección gráfica", 4, 3, "Qué falla cuando la varianza cambia, y cómo se ve."),
    v("T09_02_breusch_pagan", "The Breusch-Pagan test", "El contraste de Breusch-Pagan", 3, 3, "¿Explican las X la varianza del error?"),
    v("T09_03_robusta", "Robust OLS estimation", "Estimación MCO robusta", 3, 2, "Mismos coeficientes, otra matriz de varianzas."),
    v("T09_04_casos", "Back to the cases", "Vuelta a los casos", 3, 2, "Un diagnóstico para cada caso.")),

  # Tema 10 (semana 14, clase larga)
  "s14L" = bind_rows(
    v("T10_01_concepto", "The concept of autocorrelation", "El concepto de autocorrelación", 3, 2, "Errores que recuerdan al anterior."),
    v("T10_02_durbin_watson", "Consequences and the Durbin-Watson test", "Consecuencias y contraste de Durbin-Watson", 3, 2, "Un número entre 0 y 4 que se lee junto al 2."),
    v("T10_03_dw_caso_c", "Durbin-Watson in case C", "Durbin-Watson en el caso C", 3, 2, "Rachas de residuos del mismo signo."),
    v("T10_04_hac", "Robust inference: HAC errors", "Inferencia robusta: errores HAC", 4, 3, "Newey-West: robustos a las dos cosas."),
    v("T10_05_casos", "Back to the cases", "Vuelta a los casos", 3, 2, "Solo el caso C tiene memoria."))
)

# ── Podcasts de repaso (clave propia) ─────────────────────────────
# Uno por tema, en la última sesión del tema, con clave propia podcast-TNN y una entrada por idioma con la MISMA
# clave: un solo botón de Liberar suelta las dos versiones. Ficheros en /srv/webs/econometrics/videos/podcast-TNN/.
# `titulo_es` es el texto del botón en la web española y en la zona del profesor. Duración real redondeada.
e <- function(titulo, fichero, minutos, para, clave, tipo = "video", titulo_es = titulo)
  tibble(titulo, fichero, minutos, para, clave, tipo, titulo_es)
podcast <- function(t, f_en, m_en, f_es, m_es, cuando = sprintf("al cerrar el tema %d", t)) {
  k <- sprintf("podcast-T%02d", t)
  bind_rows(
    e("Podcast (English)", f_en, sprintf("%d min", m_en), sprintf("Repaso del tema %d en conversación, en inglés. Para escuchar %s.", t, cuando),
      k, "podcast", "Podcast (inglés)"),
    e("Podcast (Spanish)", f_es, sprintf("%d min", m_es), sprintf("El mismo repaso del tema %d, en español.", t),
      k, "podcast", "Podcast (español)"))
}

ESPECIALES <- list(
  "s01L" = podcast(1,  "T01_Introduction_EN.mp3", 12,              "T01_Introduccion_ES.mp3", 10),
  "s03C" = podcast(2,  "T02_OLS_EN.mp3", 13,                       "T02_MetodoMCO_ES.mp3", 12),
  "s04C" = podcast(3,  "T03_OLS_Properties_EN.mp3", 13,            "T03_PropiedadesMCO_ES.mp3", 11),
  "s06L" = podcast(4,  "T04_Hypothesis_Tests_EN.mp3", 13,          "T04_Contrastes_ES.mp3", 11),
  "s08L" = podcast(5,  "T05_Confidence_Intervals_EN.mp3", 13,      "T05_Intervalos_ES.mp3", 10),
  "s09C" = podcast(6,  "T06_Specification_EN.mp3", 13,             "T06_Especificacion_ES.mp3", 11),
  "s10C" = podcast(7,  "T07_Prediction_Residuals_EN.mp3", 12,      "T07_PrediccionResiduos_ES.mp3", 10),
  "s12C" = podcast(8,  "T08_Qualitative_Variables_EN.mp3", 12,     "T08_Cualitativas_ES.mp3", 10),
  "s13C" = podcast(9,  "T09_Heteroskedasticity_EN.mp3", 13,        "T09_Heterocedasticidad_ES.mp3", 11),
  "s14L" = podcast(10, "T10_Autocorrelation_EN.mp3", 13,           "T10_Autocorrelacion_ES.mp3", 11,
                   cuando = "al cerrar el curso y antes del examen final")
)

# ── Claves y sesiones ─────────────────────────────────────────────
sid_sesion <- function(s, x) sprintf("s%02d%s", s$n, codigo_sesion(x))
clave_video <- function(s, x) paste0("video-", sid_sesion(s, x))
propios_listos <- function(sid) { vs <- PROPIOS[[sid]]; if (is.null(vs)) NULL else vs[vs$listo, ] }
sesiones_con_video <- function(semanas) {
  out <- list()
  for (s in semanas) for (x in s$sesiones) {
    sid <- sid_sesion(s, x)
    if (!is.null(PROPIOS[[sid]]) || !is.null(ESPECIALES[[sid]])) out[[length(out) + 1]] <- list(s = s, x = x)
  }
  out
}

# Chips de los vídeos propios. La ruta lleva la clave porque el servicio enlaza por clave.
# `profesor = TRUE`: la ruta pasa a /profesor/videos/ (sirve la carpeta entera, liberada o no, para que el profesor
# los vea ANTES de soltarlos) y el botón lleva data-estado (profesor.js lo pinta de verde o rojo). Los que aún no
# están listos salen solo al profesor, como «en preparación».
chips_propios <- function(sid, clave, L = "en", con_nota = FALSE, profesor = FALSE) {
  vs <- PROPIOS[[sid]]
  if (is.null(vs)) return("")
  if (!profesor) vs <- vs[vs$listo, ]
  if (!nrow(vs)) return("")
  base <- if (profesor) "/profesor/videos" else "/videos"
  attr_est <- if (profesor) sprintf(' data-estado="%s"', h(clave)) else ""
  otro <- setdiff(IDIOMAS, L)
  paste(vapply(seq_len(nrow(vs)), function(j) {
    x <- vs[j, ]
    tit <- if (L == "es") x$titulo_es else x$titulo_en
    nota <- if (con_nota) paste0(" · ", h(x$para)) else ""
    if (!x$listo) return(sprintf('<span class="dl soon"><span class="dl-l">▶ %s</span><span class="dl-m">Vídeo del curso · en preparación%s</span></span>', h(tit), nota))
    f <- function(l) if (l == "es") x$fichero_es else x$fichero_en
    sprintf('<span class="pair"><a class="dl"%s href="%s/%s/%s"><span class="dl-l">▶ %s</span><span class="dl-m">%s · %s%s</span></a><a class="alt" href="%s/%s/%s" title="%s" lang="%s">%s</a></span>',
            attr_est, base, h(clave), h(f(L)), h(tit), tx(L, "Course video", "Vídeo del curso"),
            h(if (L == "es") x$min_es else x$min_en), nota,
            base, h(clave), h(f(otro)), h(NOMBRE_IDIOMA[[otro]]), otro, toupper(otro))
  }, ""), collapse = "")
}

# Chips de los podcasts (clave propia). Las dos versiones comparten clave y van en el mismo bloque liberable.
chips_especiales <- function(sid, con_nota = FALSE, profesor = FALSE, L = "en") {
  vs <- ESPECIALES[[sid]]
  if (is.null(vs)) return("")
  base <- if (profesor) "/profesor/videos" else "/videos"
  paste(vapply(unique(vs$clave), function(k) {
    v <- vs[vs$clave == k, ]
    podcast <- v$tipo[1] == "podcast"
    attr_est <- if (profesor) sprintf(' data-estado="%s"', h(k)) else ""
    chips <- paste(sprintf('<a class="dl"%s href="%s/%s/%s"><span class="dl-l">%s %s</span><span class="dl-m">%s%s</span></a>',
                           attr_est, base, h(k), h(v$fichero), if (podcast) "♪" else "▶",
                           h(if (L == "es") v$titulo_es else v$titulo),
                           sprintf(if (podcast) "Podcast · %s" else tx(L, "Course video · %s", "Vídeo del curso · %s"), h(v$minutos)),
                           if (con_nota) paste0(" · ", h(v$para)) else ""), collapse = "")
    bloque <- sprintf('<div class="dls">%s</div>', chips)
    if (profesor) sprintf('<p class="note">Clave propia <b>%s</b>: %s</p>%s', h(k),
                          if (podcast) "podcast de repaso, se libera aparte de los vídeos del tema (las dos versiones a la vez)."
                          else "sale con su documento, no con los vídeos del tema.", bloque)
    else liberable(k, bloque, "")
  }, ""), collapse = "")
}

# Podcasts de repaso: una fila en Liberar por clave, con las versiones que lleva
podcasts <- function() {
  vs <- bind_rows(lapply(names(ESPECIALES), function(sid) mutate(ESPECIALES[[sid]], sid = sid)))
  vs[vs$tipo == "podcast", ]
}

# Web de los alumnos: los vídeos de la sesión con la clave video-sNNX (solo los listos) y los podcasts con la suya
videos_alumno <- function(s, x, L) {
  sid <- sid_sesion(s, x)
  propios <- chips_propios(sid, clave_video(s, x), L = L)
  paste0(if (nzchar(propios)) liberable(clave_video(s, x), sprintf('<div class="dls">%s</div>', propios), "") else "",
         chips_especiales(sid, L = L))
}

# Plan del profesor (en español): todos los vídeos, con su nota, en verde o rojo según estén liberados
videos_sesion <- function(s, x) {
  sid <- sid_sesion(s, x)
  propios <- chips_propios(sid, clave_video(s, x), L = "es", con_nota = TRUE, profesor = TRUE)
  paste0(
    if (nzchar(propios)) paste0(
      sprintf('<p class="note">Vídeos del curso (español, con botón EN para la versión inglesa). Los alumnos los ven cuando liberas <b>%s</b> en <a href="/profesor/liberar.html">Liberar contenidos</a>. Tú puedes verlos siempre: en verde los liberados, en rojo los que siguen ocultos.</p>', h(clave_video(s, x))),
      '<div class="dls">', propios, '</div>') else "",
    chips_especiales(sid, con_nota = TRUE, profesor = TRUE, L = "es"))
}
