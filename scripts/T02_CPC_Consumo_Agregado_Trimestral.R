# =============================================================================
# ECONOMETRÍA — Capítulo 2, Caso Práctico C
# CPC: Consumo agregado trimestral de España — Estimación MCO
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
  Tema 2: Estimación MCO — Función de consumo keynesiana
=============================================================================

  Objetivo: estimar la función de consumo keynesiana con MCO.
  Verificar si se cumple la predicción de Keynes: 0 < PMC < 1.

  Modelo simple:  Consumo = b0 + b1*PIB + u
  Modelo ampliado: Consumo = b0 + b1*PIB + b2*Tipo_interés + u

=============================================================================
")

pausa()

# ── 1. RECORDATORIO DE LOS DATOS ───────────────────────────────────────────
cat("\n--- 1. RECORDATORIO DE LOS DATOS ---\n")

cat(sprintf("\n  Dataset: %d trimestres (serie temporal)\n", nrow(caso_c)))
cat(sprintf("  Periodo: %s a %s\n", caso_c$trimestre[1],
            caso_c$trimestre[nrow(caso_c)]))

cat("\n  Resumen de las variables:\n")
print(summary(caso_c[, c("consumo", "pib", "tipo_interes", "inflacion")]))

# Correlaciones
cat("\n  Correlaciones con consumo:\n")
cat(sprintf("    PIB:           %.4f\n", cor(caso_c$consumo, caso_c$pib)))
cat(sprintf("    Tipo interés:  %.4f\n", cor(caso_c$consumo, caso_c$tipo_interes)))
cat(sprintf("    Inflación:     %.4f\n", cor(caso_c$consumo, caso_c$inflacion)))

pausa()

# ── 2. MODELO SIMPLE: CONSUMO ~ PIB ───────────────────────────────────────
cat("\n--- 2. MODELO SIMPLE: Consumo = b0 + b1*PIB + u ---\n\n")

mod_simple <- lm(consumo ~ pib, data = caso_c)
print(summary(mod_simple))

coefs_s <- coef(mod_simple)

cat(sprintf("\n  b0 (consumo autónomo)     = %12.2f\n", coefs_s[1]))
cat(sprintf("  b1 (PMC)                  = %12.4f\n", coefs_s[2]))
cat(sprintf("  R²                        = %12.4f\n", summary(mod_simple)$r.squared))

cat(sprintf("\n  Interpretación:"))
cat(sprintf("\n  Por cada EUR adicional de PIB, el consumo aumenta"))
cat(sprintf("\n  en %.2f céntimos. Es la PROPENSIÓN MARGINAL A CONSUMIR.\n", coefs_s[2] * 100))

if (coefs_s[2] > 0 & coefs_s[2] < 1) {
  cat("\n  ¿Cumple Keynes (0 < PMC < 1)? SÍ ✓\n")
} else {
  cat("\n  ¿Cumple Keynes (0 < PMC < 1)? NO ✗\n")
}

pausa()

# ── 3. MODELO AMPLIADO: CONSUMO ~ PIB + TIPO INTERÉS ──────────────────────
cat("\n--- 3. MODELO AMPLIADO: Consumo = b0 + b1*PIB + b2*Tipo_interés + u ---\n\n")

mod_ampliado <- lm(consumo ~ pib + tipo_interes, data = caso_c)
print(summary(mod_ampliado))

coefs_a <- coef(mod_ampliado)

cat(sprintf("\n  b0 (consumo autónomo)     = %12.2f\n", coefs_a[1]))
cat(sprintf("  b1 (PMC)                  = %12.4f\n", coefs_a[2]))
cat(sprintf("  b2 (tipo interés)         = %12.2f\n", coefs_a[3]))
cat(sprintf("  R²                        = %12.4f\n", summary(mod_ampliado)$r.squared))

cat(sprintf("\n  b2 < 0: al subir el tipo de interés, el consumo DISMINUYE."))
cat(sprintf("\n  Es coherente: el ahorro se vuelve más atractivo.\n"))

pausa()

# ── 4. BONDAD DE AJUSTE: COMPARACIÓN ─────────────────────────────────────
cat("\n--- 4. COMPARACIÓN DE BONDAD DE AJUSTE ---\n")

R2_simple   <- summary(mod_simple)$r.squared
R2_ampliado <- summary(mod_ampliado)$r.squared

cat(sprintf("\n  Modelo simple (PIB):             R² = %.4f\n", R2_simple))
cat(sprintf("  Modelo ampliado (PIB + interés): R² = %.4f\n", R2_ampliado))
cat(sprintf("\n  Al añadir el tipo de interés, R² sube de %.4f a %.4f.\n",
            R2_simple, R2_ampliado))
cat(sprintf("  PERO CUIDADO: R² SIEMPRE sube al añadir variables."))
cat(sprintf("\n  ¿Merece la pena añadirla? Lo veremos con R² ajustado (Tema 3).\n"))

pausa()

# ── 5. CÁLCULO MATRICIAL DE LA FUNCIÓN DE CONSUMO ────────────────────────
cat("\n--- 5. VERIFICACIÓN CON CÁLCULO MATRICIAL ---\n")

Y <- as.matrix(caso_c$consumo)
X <- cbind(1, caso_c$pib)

XtX     <- t(X) %*% X
XtX_inv <- solve(XtX)
XtY     <- t(X) %*% Y
beta_hat <- XtX_inv %*% XtY

cat("\n  Cálculo matricial: beta_hat = (X'X)^(-1) * X'Y\n")
cat(sprintf("  b0 = %.2f\n", beta_hat[1]))
cat(sprintf("  b1 = %.4f\n", beta_hat[2]))
cat(sprintf("\n  Con lm():  b0 = %.2f, b1 = %.4f\n", coefs_s[1], coefs_s[2]))
cat("  ¡Coincidencia exacta!\n")

pausa()

# ── 6. VISUALIZACIÓN ───────────────────────────────────────────────────
cat("\n--- 6. VISUALIZACIÓN ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Consumo vs PIB con recta
plot(caso_c$pib / 1000, caso_c$consumo / 1000,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "PIB (miles MM EUR)", ylab = "Consumo (miles MM EUR)",
     main = sprintf("Función de consumo (R² = %.3f)", R2_simple))
abline(mod_simple$coefficients[1] / 1000,
       mod_simple$coefficients[2],
       col = col_secundario, lwd = 2)

# Observado vs ajustado
plot(fitted(mod_simple) / 1000, caso_c$consumo / 1000,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Consumo ajustado (miles MM EUR)",
     ylab = "Consumo observado (miles MM EUR)",
     main = "Observado vs Ajustado")
abline(0, 1, col = col_terciario, lwd = 2, lty = 2)

par(mfrow = c(1, 1))

cat("\n  ADVERTENCIAS para series temporales:")
cat("\n  1. R² muy alto puede ser ESPURIO (ambas variables tienen tendencia).")
cat("\n  2. Los residuos sucesivos pueden estar CORRELACIONADOS.")
cat("\n     Esto es AUTOCORRELACIÓN — lo diagnosticaremos en el Tema 10.\n")

cat("\n=== FIN Caso Práctico C — Tema 02 ===\n")
