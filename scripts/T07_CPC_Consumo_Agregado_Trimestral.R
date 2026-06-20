# =============================================================================
# ECONOMETRÍA — Capítulo 7, Caso Práctico C
# CPC: Consumo trimestral — Predicción y residuos
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl)
caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO C — Consumo agregado trimestral de España
  Tema 7: Predicción y análisis de residuos
=============================================================================
")
pausa()

mod <- lm(consumo ~ pib + tipo_interes, data = caso_c)

# ── 1. PREDICCIÓN FUTURA ────────────────────────────────────────────────
cat("\n--- 1. PREDICCIÓN DE CONSUMO FUTURO ---\n\n")

perfiles <- data.frame(pib = c(280000, 300000, 320000), tipo_interes = c(2.0, 2.0, 2.0))
ic <- predict(mod, newdata = perfiles, interval = "confidence", level = 0.95)
ip <- predict(mod, newdata = perfiles, interval = "prediction", level = 0.95)

cat("  PIB (miles MM)  Pred.     IC 95%              IP 95%\n")
cat("  --------------  ------    ----------------    ----------------\n")
for (i in 1:3) {
  cat(sprintf("  %6.0f          %.0f    [%.0f, %.0f]    [%.0f, %.0f]\n",
              perfiles$pib[i]/1000, ic[i,1], ic[i,2], ic[i,3], ip[i,2], ip[i,3]))
}
pausa()

# ── 2. RESIDUOS SECUENCIALES (ANTICIPO AUTOCORRELACIÓN) ─────────────────
cat("\n--- 2. RESIDUOS SECUENCIALES ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(1:nrow(caso_c), residuals(mod), type = "b", pch = 16, col = "gray30",
     xlab = "Trimestre", ylab = "Residuos", main = "Residuos en el tiempo")
abline(h = 0, lty = 2)

plot(residuals(mod)[-length(residuals(mod))], residuals(mod)[-1],
     pch = 16, col = "gray30",
     xlab = "Residuo(t)", ylab = "Residuo(t+1)", main = "Residuo t vs t+1")
abline(h = 0, lty = 2); abline(v = 0, lty = 2)
par(mfrow = c(1, 1))

cat("\n  Si los residuos sucesivos se parecen entre sí,")
cat("\n  hay AUTOCORRELACIÓN → lo estudiaremos en el Tema 10.\n")

cat("\n=== FIN Caso Práctico C — Tema 07 ===\n")
