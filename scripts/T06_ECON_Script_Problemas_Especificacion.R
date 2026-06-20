# =============================================================================
# ECONOMETRÍA — Capítulo 6
# Script Teórico: Problemas de especificación
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
  ECONOMETRÍA — Tema 6: Problemas de especificación
=============================================================================

  En este script veremos:

    1. Inclusión de variables irrelevantes (consecuencias)
    2. Omisión de variables relevantes (sesgo)
    3. Cálculo de la dirección del sesgo
    4. Detección de problemas de especificación

=============================================================================
")

pausa()

# ── 1. INCLUSIÓN DE VARIABLE IRRELEVANTE ──────────────────────────────────
cat("\n--- 1. INCLUSIÓN DE VARIABLE IRRELEVANTE ---\n")

mod_correcto <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
caso_a$ruido <- rnorm(nrow(caso_a))
mod_ruido    <- lm(ventas ~ precio_propio + gasto_publicidad + ruido, data = caso_a)

cat("\n  Modelo correcto: ventas ~ precio + publicidad")
cat("\n  Modelo con ruido: ventas ~ precio + publicidad + ruido (variable aleatoria)\n\n")

s1 <- summary(mod_correcto); s2 <- summary(mod_ruido)

cat("                        Modelo correcto    Con ruido\n")
cat("  R² ajustado:          ", sprintf("%.4f", s1$adj.r.squared),
    "            ", sprintf("%.4f", s2$adj.r.squared), "\n")
cat("  Coef. precio:         ", sprintf("%.4f", coef(mod_correcto)[2]),
    "            ", sprintf("%.4f", coef(mod_ruido)[2]), "\n")
cat("  SE precio:            ", sprintf("%.4f", s1$coefficients[2,2]),
    "            ", sprintf("%.4f", s2$coefficients[2,2]), "\n")
cat("  p-valor ruido:        ", "—               ",
    sprintf("%.4f", s2$coefficients[4,4]), "\n")

cat("\n  CONCLUSIÓN:")
cat("\n  - Los coeficientes APENAS cambian (insesgadez preservada).")
cat("\n  - El SE del precio AUMENTA ligeramente (pérdida de eficiencia).")
cat("\n  - El R² ajustado NO mejora.")
cat("\n  - La variable ruido NO es significativa.\n")

pausa()

# ── 2. OMISIÓN DE VARIABLE RELEVANTE ─────────────────────────────────────
cat("\n--- 2. OMISIÓN DE VARIABLE RELEVANTE ---\n")

mod_completo <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
mod_omitido  <- lm(ventas ~ precio_propio, data = caso_a)

cat("\n  Modelo completo: ventas ~ precio + publicidad")
cat("\n  Modelo omitido:  ventas ~ precio (sin publicidad)\n\n")

cat("                        Completo          Sin publicidad\n")
cat("  Coef. precio:         ", sprintf("%.4f", coef(mod_completo)[2]),
    "            ", sprintf("%.4f", coef(mod_omitido)[2]), "\n")
cat("  R² ajustado:          ", sprintf("%.4f", summary(mod_completo)$adj.r.squared),
    "            ", sprintf("%.4f", summary(mod_omitido)$adj.r.squared), "\n")

sesgo_obs <- coef(mod_omitido)[2] - coef(mod_completo)[2]
cat(sprintf("\n  SESGO OBSERVADO: %.4f - (%.4f) = %.4f\n",
            coef(mod_omitido)[2], coef(mod_completo)[2], sesgo_obs))
cat("  El coeficiente del precio CAMBIA al omitir publicidad → SESGO.\n")

pausa()

# ── 3. DIRECCIÓN TEÓRICA DEL SESGO ──────────────────────────────────────
cat("\n--- 3. DIRECCIÓN TEÓRICA DEL SESGO ---\n")

b2 <- coef(mod_completo)[3]
reg_aux <- lm(gasto_publicidad ~ precio_propio, data = caso_a)
delta21 <- coef(reg_aux)[2]

sesgo_teorico <- b2 * delta21

cat(sprintf("\n  Fórmula: sesgo = b2 * delta21\n"))
cat(sprintf("  b2 (efecto publicidad) = %.4f\n", b2))
cat(sprintf("  delta21 (regresión auxiliar publicidad ~ precio) = %.4f\n", delta21))
cat(sprintf("  Sesgo teórico = %.4f × %.4f = %.4f\n", b2, delta21, sesgo_teorico))
cat(sprintf("  Sesgo observado = %.4f\n", sesgo_obs))
cat(sprintf("\n  ¡Coinciden! La fórmula del sesgo funciona.\n"))

pausa()

# ── 4. RESUMEN ──────────────────────────────────────────────────────────
cat("\n--- 4. RESUMEN ---\n")

cat("\n  INCLUIR variable irrelevante:")
cat("\n    → Coeficientes INSESGADOS pero MENOS PRECISOS")
cat("\n    → R² ajustado NO mejora")
cat("\n    → Variable NO significativa (t-test)")
cat("\n")
cat("\n  OMITIR variable relevante:")
cat("\n    → Coeficientes SESGADOS")
cat("\n    → Sesgo = b2 × delta21 (NO desaparece con más datos)")
cat("\n    → R² ajustado BAJA")
cat("\n    → Coeficientes INESTABLES")
cat("\n")
cat("\n  → EN CASO DE DUDA: mejor incluir de más que omitir.\n")

cat("\n=== FIN Script Tema 06 ===\n")
