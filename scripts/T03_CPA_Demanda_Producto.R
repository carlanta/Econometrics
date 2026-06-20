# =============================================================================
# ECONOMETRÍA — Capítulo 3, Caso Práctico A
# CPA: Demanda de un producto de consumo — Propiedades MCO
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
col_terciario  <- "#27AE60"; col_gris <- "#7F8C8D"

caso_a <- read_excel("data/caso_a_demanda.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO A — Demanda de un producto de consumo
  Tema 3: Propiedades MCO — R² vs R² ajustado
=============================================================================

  Objetivo: estimar modelos progresivos añadiendo variables y
  comparar R² con R² ajustado para decidir qué modelo es mejor.

=============================================================================
")

pausa()

# ── 1. MODELOS PROGRESIVOS ─────────────────────────────────────────────────
cat("\n--- 1. MODELOS PROGRESIVOS ---\n")

mod1 <- lm(ventas ~ precio_propio, data = caso_a)
mod2 <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
mod3 <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
mod4 <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit +
             promocion + temporada, data = caso_a)

cat("\n  M1: ventas ~ precio")
cat("\n  M2: ventas ~ precio + publicidad")
cat("\n  M3: M2 + precio sustitutivo")
cat("\n  M4: M3 + promocion + temporada\n")

pausa()

# ── 2. COMPARACIÓN R² vs R² AJUSTADO ──────────────────────────────────────
cat("\n--- 2. COMPARACIÓN R² vs R² AJUSTADO ---\n\n")

cat("  Modelo  k   R²       R² ajust.  Sigma    Mejora R² adj.\n")
cat("  ------  --  ------   ---------  ------   -------------\n")
modelos <- list(mod1, mod2, mod3, mod4)
nombres <- c("M1", "M2", "M3", "M4")
ks      <- c(1, 2, 3, 5)

for (i in 1:4) {
  s <- summary(modelos[[i]])
  mejora <- if (i == 1) "—" else sprintf("%+.4f", s$adj.r.squared - summary(modelos[[i-1]])$adj.r.squared)
  cat(sprintf("  %s      %d   %.4f   %.4f     %.4f   %s\n",
              nombres[i], ks[i], s$r.squared, s$adj.r.squared, s$sigma, mejora))
}

cat("\n  CONCLUSIÓN:")
cat("\n  - El salto grande está en M1 → M2 (publicidad es MUY relevante).")
cat("\n  - M3 y M4 aportan mejoras marginales en R² ajustado.")
cat("\n  - El modelo M2 (precio + publicidad) es probablemente suficiente.\n")

pausa()

# ── 3. ESTABILIDAD DE COEFICIENTES ────────────────────────────────────────
cat("\n--- 3. ESTABILIDAD DE COEFICIENTES ---\n\n")

cat("  Variable          M1        M2        M3        M4\n")
cat("  ----------------  --------  --------  --------  --------\n")
cat(sprintf("  Intercepto        %8.4f  %8.4f  %8.4f  %8.4f\n",
            coef(mod1)[1], coef(mod2)[1], coef(mod3)[1], coef(mod4)[1]))
cat(sprintf("  Precio propio     %8.4f  %8.4f  %8.4f  %8.4f\n",
            coef(mod1)[2], coef(mod2)[2], coef(mod3)[2], coef(mod4)[2]))

cat("\n  El coeficiente del precio cambia MUCHO de M1 a M2 (omisión de")
cat("\n  publicidad sesga el efecto precio), pero es ESTABLE de M2 en adelante.")
cat("\n  Esto sugiere que M2 ya captura bien el efecto.\n")

pausa()

# ── 4. ERRORES ESTÁNDAR ──────────────────────────────────────────────────
cat("\n--- 4. ERRORES ESTÁNDAR DE M2 ---\n\n")

s2 <- summary(mod2)
cat(sprintf("  Error estándar de la regresión (sigma): %.4f MM EUR\n\n", s2$sigma))
cat("  Coeficiente        Estimación  Error est.\n")
cat("  ----------------   ----------  ----------\n")
for (i in 1:nrow(s2$coefficients)) {
  cat(sprintf("  %-17s  %10.4f  %10.4f\n",
              rownames(s2$coefficients)[i],
              s2$coefficients[i, 1], s2$coefficients[i, 2]))
}

cat("\n  Los errores estándar son PEQUEÑOS respecto a los coeficientes,")
cat("\n  lo que indica estimaciones PRECISAS.\n")

cat("\n=== FIN Caso Práctico A — Tema 03 ===\n")
