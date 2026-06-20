# =============================================================================
# ECONOMETRÍA — Capítulo 8
# Script Teórico: Variables cualitativas en el MRLG
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
  ECONOMETRÍA — Tema 8: Variables cualitativas (dummies)
=============================================================================
")
pausa()

# ── 1. MODELO SIN DUMMY ────────────────────────────────────────────────
cat("\n--- 1. MODELO SIN DUMMY ---\n\n")
mod_sin <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
print(summary(mod_sin)$coefficients)
pausa()

# ── 2. MODELO CON DUMMY PROMOCIÓN ──────────────────────────────────────
cat("\n--- 2. MODELO CON DUMMY PROMOCIÓN ---\n\n")
mod_con <- lm(ventas ~ precio_propio + gasto_publicidad + promocion, data = caso_a)
print(summary(mod_con)$coefficients)

b_promo <- coef(mod_con)[4]
cat(sprintf("\n  Efecto de la promoción: %.4f MM EUR\n", b_promo))
cat(sprintf("  Cuando hay promoción, las ventas son %.2f MM EUR %s.\n",
            abs(b_promo), ifelse(b_promo > 0, "MAYORES", "MENORES")))
pausa()

# ── 3. INTERPRETACIÓN GRÁFICA ─────────────────────────────────────────
cat("\n--- 3. INTERPRETACIÓN: DOS RECTAS PARALELAS ---\n")

plot(caso_a$precio_propio, caso_a$ventas, pch = ifelse(caso_a$promocion == 1, 17, 16),
     col = ifelse(caso_a$promocion == 1, "gray40", "black"),
     xlab = "Precio propio", ylab = "Ventas", main = "Efecto de la dummy promoción")
abline(a = coef(mod_con)[1], b = coef(mod_con)[2], lty = 1, lwd = 2)
abline(a = coef(mod_con)[1] + coef(mod_con)[4], b = coef(mod_con)[2], lty = 2, lwd = 2)
legend("topright", legend = c("Sin promoción (D=0)", "Con promoción (D=1)"),
       pch = c(16, 17), lty = c(1, 2), col = c("black", "gray40"), bty = "n")

cat("\n  La dummy DESPLAZA la recta hacia arriba (si b > 0) o abajo (si b < 0).")
cat("\n  La PENDIENTE (efecto del precio) es la misma en ambos grupos.\n")

cat("\n=== FIN Script Tema 08 ===\n")
