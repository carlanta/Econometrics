# =============================================================================
# ECONOMETRÍA — Capítulo 7
# Script Teórico: Predicción y análisis de residuos
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl)
caso_a <- read_excel("data/caso_a_demanda.xlsx")

cat("
=============================================================================
  ECONOMETRÍA — Tema 7: Predicción y análisis de residuos
=============================================================================
")
pausa()

mod <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)

# ── 1. PREDICCIÓN PUNTUAL ────────────────────────────────────────────────
cat("\n--- 1. PREDICCIÓN PUNTUAL ---\n")

nuevos <- data.frame(precio_propio = c(2.00, 2.50, 3.00, 3.50),
                     gasto_publicidad = rep(1.20, 4))

pred <- predict(mod, newdata = nuevos)
cat("\n  Predicciones para publicidad = 1.20 MM EUR:\n\n")
for (i in 1:4) cat(sprintf("  Precio = %.2f EUR -> Ventas = %.2f MM EUR\n",
                           nuevos$precio_propio[i], pred[i]))
pausa()

# ── 2. IC vs IP ─────────────────────────────────────────────────────────
cat("\n--- 2. INTERVALO DE CONFIANZA vs PREDICCIÓN ---\n")

ic <- predict(mod, newdata = nuevos, interval = "confidence", level = 0.95)
ip <- predict(mod, newdata = nuevos, interval = "prediction", level = 0.95)

cat("\n  Precio  Pred.    IC 95%              IP 95%\n")
cat("  ------  ------   ----------------    ----------------\n")
for (i in 1:4) {
  cat(sprintf("  %.2f    %.2f   [%.2f, %.2f]    [%.2f, %.2f]\n",
              nuevos$precio_propio[i], ic[i,1], ic[i,2], ic[i,3], ip[i,2], ip[i,3]))
}
cat("\n  El IP es SIEMPRE más ancho que el IC.\n")
pausa()

# ── 3. ANÁLISIS DE RESIDUOS ─────────────────────────────────────────────
cat("\n--- 3. ANÁLISIS DE RESIDUOS ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(fitted(mod), residuals(mod), pch = 16, col = "gray30",
     xlab = "Ajustados", ylab = "Residuos", main = "Residuos vs Ajustados")
abline(h = 0, lty = 2)

qqnorm(residuals(mod), pch = 16, col = "gray30", main = "Q-Q Plot")
qqline(residuals(mod), lty = 2)
par(mfrow = c(1, 1))

cat("\n  Si los residuos se dispersan sin patrón → modelo OK.")
cat("\n  Si hay embudo → heterocedasticidad (Tema 9).")
cat("\n  Si hay curva → problema de forma funcional.\n")

cat("\n=== FIN Script Tema 07 ===\n")
