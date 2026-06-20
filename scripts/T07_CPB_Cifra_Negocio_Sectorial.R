# =============================================================================
# ECONOMETRÍA — Capítulo 7, Caso Práctico B
# CPB: Cifra de negocio — Predicción y residuos
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl)
caso_b <- read_excel("data/caso_b_cifra_negocio.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO B — Cifra de negocio y empleo sectorial
  Tema 7: Predicción y análisis de residuos
=============================================================================
")
pausa()

mod <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)

# ── 1. PREDICCIÓN POR PERFILES ──────────────────────────────────────────
cat("\n--- 1. PREDICCIÓN POR PERFILES DE EMPRESA ---\n\n")

perfiles <- data.frame(trabajadores = c(20, 50, 100), inversion = c(0.5, 1.0, 2.0))
ip <- predict(mod, newdata = perfiles, interval = "prediction", level = 0.95)

cat("  Trab.  Inv.   Pred.     [IP 95%]\n")
cat("  -----  -----  --------  ----------------\n")
for (i in 1:3) {
  cat(sprintf("  %3d    %.1f    %8.2f  [%.2f, %.2f]\n",
              perfiles$trabajadores[i], perfiles$inversion[i], ip[i,1], ip[i,2], ip[i,3]))
}
pausa()

# ── 2. RESIDUOS: ¿HETEROCEDASTICIDAD? ──────────────────────────────────
cat("\n--- 2. DIAGNÓSTICO DE RESIDUOS ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(fitted(mod), residuals(mod), pch = 16, col = "gray30",
     xlab = "Ajustados", ylab = "Residuos", main = "Residuos vs Ajustados")
abline(h = 0, lty = 2)
plot(caso_b$trabajadores, residuals(mod)^2, pch = 16, col = "gray30",
     xlab = "Trabajadores", ylab = "Residuos al cuadrado", main = "Residuos² vs Trabajadores")
par(mfrow = c(1, 1))

cat("\n  ¿Se observa embudo? Si los residuos crecen con los ajustados,")
cat("\n  hay indicios de HETEROCEDASTICIDAD (Tema 9).\n")

cat("\n=== FIN Caso Práctico B — Tema 07 ===\n")
