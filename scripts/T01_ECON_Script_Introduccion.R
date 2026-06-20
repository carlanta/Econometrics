# =============================================================================
# ECONOMETRÍA — Capítulo 1
# Script Teórico: Introducción a la Econometría
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(ggplot2); library(scales)

col_principal  <- "#2C3E7A"; col_secundario <- "#E8501A"
col_terciario  <- "#27AE60"; col_gris <- "#7F8C8D"; col_fondo <- "#F4F6F9"

cat("
=============================================================================
  ECONOMETRÍA — Tema 1: Introducción a la Econometría
=============================================================================

  En este script exploraremos los conceptos fundamentales de la econometría
  usando R como herramienta. Veremos:

    1. Qué es un modelo econométrico
    2. La diferencia entre modelo matemático y econométrico
    3. Tipos de datos económicos
    4. Un primer vistazo a la estimación MCO

=============================================================================
")

pausa()

# ── 1. MODELO MATEMÁTICO vs MODELO ECONOMÉTRICO ─────────────────────────────
cat("\n--- 1. MODELO MATEMÁTICO vs MODELO ECONOMÉTRICO ---\n")

cat("
  La teoría keynesiana dice:  C = a + b*Yd   con 0 < b < 1

  Esto es un modelo MATEMÁTICO: determinista, exacto.
  Pero la realidad no es exacta. Necesitamos un modelo ECONOMÉTRICO:

      Y = b0 + b1*X + u

  donde 'u' recoge todo lo que no podemos observar.
  Veamos la diferencia gráficamente...
")

set.seed(2024)
n <- 30
renta <- seq(1000, 4000, length.out = n)

# Modelo determinista
consumo_det <- 200 + 0.75 * renta

# Modelo econométrico (con error)
consumo_eco <- 200 + 0.75 * renta + rnorm(n, 0, 150)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(renta, consumo_det, pch = 16, col = col_principal,
     xlab = "Renta disponible (EUR)", ylab = "Consumo (EUR)",
     main = "Modelo MATEMATICO\n(determinista)")
abline(a = 200, b = 0.75, col = col_secundario, lwd = 2)

plot(renta, consumo_eco, pch = 16, col = col_principal,
     xlab = "Renta disponible (EUR)", ylab = "Consumo (EUR)",
     main = "Modelo ECONOMETRICO\n(con termino de error)")
abline(a = 200, b = 0.75, col = col_secundario, lwd = 2)

par(mfrow = c(1, 1))

cat("\n  En el modelo matemático, todos los puntos caen sobre la recta.")
cat("\n  En el modelo econométrico, los puntos se dispersan alrededor.")
cat("\n  La dispersión la genera el término de error u.\n")

pausa()

# ── 2. TIPOS DE DATOS ECONÓMICOS ─────────────────────────────────────────────
cat("\n--- 2. TIPOS DE DATOS ECONÓMICOS ---\n")

cat("
  Tipo 1: CORTE TRANSVERSAL — múltiples unidades, un momento del tiempo
  Tipo 2: SERIE TEMPORAL    — una unidad, múltiples momentos del tiempo
  Tipo 3: DATOS FUSIONADOS  — corte transversal en dos o más momentos
  Tipo 4: DATOS DE PANEL    — las mismas unidades a lo largo del tiempo
")

# Ejemplo de corte transversal
cat("\n  Ejemplo de CORTE TRANSVERSAL (hogares en 2023):\n")
hogares <- data.frame(
  hogar   = paste0("H", 1:6),
  renta   = c(1200, 1800, 2400, 3100, 2200, 1500),
  consumo = c(1050, 1520, 1980, 2450, 1850, 1280),
  miembros = c(2, 3, 4, 4, 3, 2)
)
print(hogares)

cat("\n  Observamos 6 hogares en el MISMO momento del tiempo.")
cat("\n  La variación es ENTRE unidades.\n")

# Ejemplo de serie temporal
cat("\n  Ejemplo de SERIE TEMPORAL (PIB trimestral):\n")
serie <- data.frame(
  trimestre = c("2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4", "2024-Q1", "2024-Q2"),
  pib       = c(350200, 358400, 362100, 355800, 361500, 370200),
  consumo   = c(198500, 203200, 205800, 201400, 204700, 210100)
)
print(serie)

cat("\n  Observamos UN país a lo largo del TIEMPO.")
cat("\n  La variación es A LO LARGO del tiempo.")
cat("\n  CUIDADO: las observaciones están correlacionadas (autocorrelación, Tema 10).\n")

pausa()

# ── 3. UN PRIMER VISTAZO A LA ESTIMACIÓN MCO ─────────────────────────────────
cat("\n--- 3. UN PRIMER VISTAZO A LA ESTIMACIÓN MCO ---\n")

cat("
  Con datos de consumo y PIB de España, estimamos la función de consumo.
  Usamos la función lm() de R — la estudiaremos a fondo en el Tema 2.
")

# Simular datos de consumo-PIB trimestrales
set.seed(2024)
n_trim <- 40
pib <- seq(100000, 420000, length.out = n_trim) + rnorm(n_trim, 0, 8000)
consumo <- 2564.26 + 0.7572 * pib + rnorm(n_trim, 0, 5000)

datos_macro <- data.frame(pib = pib, consumo = consumo)

# Estimar el modelo
modelo <- lm(consumo ~ pib, data = datos_macro)

cat("\n  Resultado de la estimación MCO:\n\n")
print(summary(modelo))

coefs <- coef(modelo)
cat(sprintf("\n  b0 (consumo autónomo)         = %12.2f\n", coefs[1]))
cat(sprintf("  b1 (propensión marginal)      = %12.4f\n", coefs[2]))
cat(sprintf("  R2 (bondad de ajuste)         = %12.4f\n", summary(modelo)$r.squared))

pausa()

# ── 4. INTERPRETACIÓN ECONÓMICA ──────────────────────────────────────────────
cat("\n--- 4. INTERPRETACIÓN ECONÓMICA ---\n")

cat(sprintf("
  La función de consumo estimada es:

      Consumo = %.2f + %.4f x PIB + u

  b1 = %.4f -> Por cada euro adicional de PIB, el consumo aumenta
    en %.2f céntimos. Es la PROPENSIÓN MARGINAL A CONSUMIR.

  Cumple la predicción de Keynes (0 < PMC < 1): 0 < %.4f < 1

  b0 = %.2f -> Es el consumo autónomo. No tiene interpretación
    económica directa en este contexto.

  R2 = %.4f -> El PIB explica el %.1f%% de la variabilidad del consumo.

  NOTA: Para afirmar todo esto con RIGOR ESTADÍSTICO necesitamos
  los contrastes de hipótesis (Tema 4) y los intervalos de confianza
  (Tema 5). De momento, es un primer vistazo.
",
  coefs[1], coefs[2], coefs[2], coefs[2] * 100,
  coefs[2], coefs[1],
  summary(modelo)$r.squared, summary(modelo)$r.squared * 100
))

# Gráfico final
plot(datos_macro$pib / 1000, datos_macro$consumo / 1000,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "PIB (miles de MM EUR)",
     ylab = "Consumo (miles de MM EUR)",
     main = "Funcion de consumo estimada - Espana")
abline(modelo, col = col_secundario, lwd = 2)

cat("\n=== FIN Script Tema 01 ===\n")
