# =============================================================================
# ECONOMETRÍA — Capítulo 6, Caso Práctico A
# CPA: Demanda de un producto de consumo — Problemas de especificación
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
  Tema 6: Problemas de especificación
=============================================================================
")
pausa()

# ── 1. MODELO COMPLETO vs OMITIDO ────────────────────────────────────────
cat("\n--- 1. SESGO POR OMISIÓN DE PUBLICIDAD ---\n\n")

mod_comp <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
mod_omit <- lm(ventas ~ precio_propio, data = caso_a)

cat("  Modelo           Coef. precio   SE        R2 adj.\n")
cat("  ---------------  ------------  ------    --------\n")
cat(sprintf("  Con publicidad   %12.4f  %6.4f    %.4f\n",
            coef(mod_comp)[2], summary(mod_comp)$coefficients[2,2],
            summary(mod_comp)$adj.r.squared))
cat(sprintf("  Sin publicidad   %12.4f  %6.4f    %.4f\n",
            coef(mod_omit)[2], summary(mod_omit)$coefficients[2,2],
            summary(mod_omit)$adj.r.squared))

cat(sprintf("\n  Sesgo observado: %.4f\n", coef(mod_omit)[2] - coef(mod_comp)[2]))
cat("  Al omitir publicidad, el precio absorbe PARTE de su efecto.\n")

pausa()

# ── 2. INCLUSIÓN DE VARIABLE IRRELEVANTE ──────────────────────────────────
cat("\n--- 2. INCLUSIÓN DE TEMPORADA (¿IRRELEVANTE?) ---\n\n")

mod_temp <- lm(ventas ~ precio_propio + gasto_publicidad + temporada, data = caso_a)
s_temp <- summary(mod_temp)

cat(sprintf("  Coef. temporada = %.4f, p-valor = %.4f\n",
            coef(mod_temp)[4], s_temp$coefficients[4,4]))
cat(sprintf("  R2 adj. sin temporada: %.4f\n", summary(mod_comp)$adj.r.squared))
cat(sprintf("  R2 adj. con temporada: %.4f\n", s_temp$adj.r.squared))

if (s_temp$coefficients[4,4] > 0.05) {
  cat("\n  Temporada NO es significativa al 5% → probablemente irrelevante.\n")
} else {
  cat("\n  Temporada ES significativa al 5%.\n")
}

cat("\n=== FIN Caso Práctico A — Tema 06 ===\n")
