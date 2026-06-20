# =============================================================================
# ECONOMETRÍA — Capítulo 1, Caso Práctico C
# CPC: Consumo agregado trimestral de España — Primer contacto
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl); library(ggplot2)

col_principal  <- "#2C3E7A"; col_secundario <- "#E8501A"
col_terciario  <- "#27AE60"; col_gris <- "#7F8C8D"; col_fondo <- "#F4F6F9"

caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO C — Consumo agregado trimestral de España
  Tema 1: Primer contacto con los datos
=============================================================================
")

# ── 1. CARGAR Y EXPLORAR LOS DATOS ──────────────────────────────────────────
cat("\n--- 1. CARGAR Y EXPLORAR LOS DATOS ---\n")

cat(sprintf("\n  Dataset cargado: %d trimestres\n", nrow(caso_c)))
cat(sprintf("  Periodo: %s a %s\n", caso_c$trimestre[1],
            caso_c$trimestre[nrow(caso_c)]))

cat("\n  Primeras observaciones:\n")
print(head(caso_c, 8))

cat("\n  TIPO DE DATOS: SERIE TEMPORAL")
cat("\n  Cada fila es un trimestre. Las observaciones están ORDENADAS en el tiempo.\n")

pausa()

# ── 2. ESTADÍSTICAS DESCRIPTIVAS ─────────────────────────────────────────────
cat("\n--- 2. ESTADÍSTICAS DESCRIPTIVAS ---\n")

cat("\n  Resumen de las variables:\n")
print(summary(caso_c[, c("consumo", "pib", "tipo_interes", "inflacion")]))

cat(sprintf("\n  Consumo medio:         %.0f MM EUR\n", mean(caso_c$consumo)))
cat(sprintf("  PIB medio:             %.0f MM EUR\n", mean(caso_c$pib)))
cat(sprintf("  Tipo interés medio:    %.2f%%\n", mean(caso_c$tipo_interes)))
cat(sprintf("  Trimestres de crisis:  %d de %d\n",
            sum(caso_c$crisis), nrow(caso_c)))

pausa()

# ── 3. VISUALIZACIÓN ─────────────────────────────────────────────────────────
cat("\n--- 3. VISUALIZACIÓN ---\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Consumo en el tiempo
plot(1:nrow(caso_c), caso_c$consumo / 1000, type = "l", lwd = 1.5,
     col = col_principal,
     xlab = "Trimestre", ylab = "Consumo (miles MM EUR)",
     main = "Consumo trimestral")
crisis_idx <- which(caso_c$crisis == 1)
if (length(crisis_idx) > 0) {
  rect(min(crisis_idx) - 0.5, par("usr")[3],
       max(crisis_idx) + 0.5, par("usr")[4],
       col = rgb(0, 0, 0, 0.1), border = NA)
  text(mean(crisis_idx), par("usr")[4] * 0.95, "Crisis",
       cex = 0.7, col = col_gris)
}

# PIB en el tiempo
plot(1:nrow(caso_c), caso_c$pib / 1000, type = "l", lwd = 1.5,
     col = col_principal,
     xlab = "Trimestre", ylab = "PIB (miles MM EUR)",
     main = "PIB trimestral")

# Consumo vs PIB (dispersión)
plot(caso_c$pib / 1000, caso_c$consumo / 1000,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "PIB (miles MM EUR)", ylab = "Consumo (miles MM EUR)",
     main = "Consumo vs PIB")
abline(lm(I(consumo/1000) ~ I(pib/1000), data = caso_c),
       col = col_secundario, lwd = 2)

# Tipo de interés
plot(1:nrow(caso_c), caso_c$tipo_interes, type = "l", lwd = 1.5,
     col = col_principal,
     xlab = "Trimestre", ylab = "Tipo interes (%)",
     main = "Tipo de interes")

par(mfrow = c(1, 1))

cat("\n  Los gráficos muestran:")
cat("\n  - Consumo y PIB con TENDENCIA CRECIENTE (típico de series temporales)")
cat("\n  - La crisis genera una CAÍDA visible en el consumo")
cat("\n  - Relación POSITIVA y fuerte entre consumo y PIB")
cat("\n  - Las observaciones sucesivas están CORRELACIONADAS")
cat("\n    (autocorrelación — Tema 10)\n")

pausa()

# ── 4. UN PRIMER MODELO: FUNCIÓN DE CONSUMO KEYNESIANA ──────────────────────
cat("\n--- 4. UN PRIMER MODELO: FUNCIÓN DE CONSUMO KEYNESIANA ---\n")

cat("\n  Estimamos: Consumo = b0 + b1*PIB + u\n")
cat("  Según Keynes: 0 < b1 < 1 (propensión marginal a consumir)\n\n")

modelo_c <- lm(consumo ~ pib, data = caso_c)
print(summary(modelo_c))

coefs <- coef(modelo_c)
cat(sprintf("\n  b0 (consumo autónomo)              = %12.2f\n", coefs[1]))
cat(sprintf("  b1 (propensión marginal a consumir) = %12.4f\n", coefs[2]))

cat(sprintf("\n  Interpretación: por cada EUR adicional de PIB,\n"))
cat(sprintf("  el consumo aumenta en %.2f céntimos.\n", coefs[2] * 100))

if (coefs[2] > 0 & coefs[2] < 1) {
  cat("\n  Cumple la predicción de Keynes: 0 < b1 < 1\n")
} else {
  cat("\n  ATENCIÓN: no cumple la predicción de Keynes\n")
}

cat("\n  NOTA: este modelo puede tener AUTOCORRELACIÓN en los residuos")
cat("\n  (Tema 10) y puede omitir variables relevantes (Tema 6).")
cat("\n  Lo iremos completando a lo largo del curso.\n")

cat("\n=== FIN Caso Práctico C — Tema 01 ===\n")
