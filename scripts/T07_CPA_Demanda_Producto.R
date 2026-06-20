# =============================================================================
# ECONOMETRÍA — Capítulo 7, Caso Práctico A
# CPA: Demanda — Predicción y análisis de residuos
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
  CASO PRÁCTICO A — Demanda de un producto de consumo
  Tema 7: Predicción y análisis de residuos
=============================================================================
")
pausa()

mod <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)

# ── 1. PREDICCIÓN PARA ESCENARIOS ────────────────────────────────────────
cat("\n--- 1. PREDICCIÓN PARA DISTINTOS ESCENARIOS ---\n\n")

perfiles <- data.frame(
  precio_propio = c(2.00, 2.50, 3.00, 3.50),
  gasto_publicidad = c(1.50, 1.20, 1.00, 0.80)
)

ip <- predict(mod, newdata = perfiles, interval = "prediction", level = 0.95)

cat("  Precio  Public.  Pred.    [IP 95%]\n")
cat("  ------  -------  ------   ----------------\n")
for (i in 1:4) {
  cat(sprintf("  %.2f    %.2f     %.2f   [%.2f, %.2f]\n",
              perfiles$precio_propio[i], perfiles$gasto_publicidad[i],
              ip[i,1], ip[i,2], ip[i,3]))
}
pausa()

# ── 2. DIAGNÓSTICO DE RESIDUOS ──────────────────────────────────────────
cat("\n--- 2. DIAGNÓSTICO DE RESIDUOS ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(fitted(mod), residuals(mod), pch = 16, col = "gray30",
     xlab = "Ajustados", ylab = "Residuos", main = "Residuos vs Ajustados")
abline(h = 0, lty = 2)
qqnorm(residuals(mod), pch = 16, col = "gray30", main = "Q-Q Plot")
qqline(residuals(mod), lty = 2)
par(mfrow = c(1, 1))

cat("\n  Sin patrón claro en residuos → modelo razonablemente OK.\n")

cat("\n=== FIN Caso Práctico A — Tema 07 ===\n")
