# =============================================================================
# ECONOMETRÍA — Capítulo 3, Caso Práctico B
# CPB: Cifra de negocio y empleo sectorial — Propiedades MCO
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

caso_b <- read_excel("data/caso_b_cifra_negocio.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO B — Cifra de negocio y empleo sectorial
  Tema 3: Propiedades MCO — R² vs R² ajustado
=============================================================================
")

pausa()

# ── 1. MODELOS PROGRESIVOS ─────────────────────────────────────────────────
cat("\n--- 1. MODELOS PROGRESIVOS ---\n")

mod1 <- lm(cifra_negocio ~ trabajadores, data = caso_b)
mod2 <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
mod3 <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)

cat("\n  M1: cifra ~ trabajadores")
cat("\n  M2: M1 + inversion")
cat("\n  M3: M2 + superficie\n")

pausa()

# ── 2. COMPARACIÓN R² vs R² AJUSTADO ──────────────────────────────────────
cat("\n--- 2. COMPARACIÓN ---\n\n")

cat("  Modelo  k   R²       R² ajust.  Sigma\n")
cat("  ------  --  ------   ---------  ------\n")
for (m in list(list(mod1,"M1",1), list(mod2,"M2",2), list(mod3,"M3",3))) {
  s <- summary(m[[1]])
  cat(sprintf("  %s      %d   %.4f   %.4f     %.4f\n",
              m[[2]], m[[3]], s$r.squared, s$adj.r.squared, s$sigma))
}

cat("\n  El número de trabajadores ya captura casi toda la variabilidad.")
cat("\n  La inversión y la superficie aportan POCO adicional.\n")

pausa()

# ── 3. COEFICIENTES Y ERRORES ESTÁNDAR ───────────────────────────────────
cat("\n--- 3. COEFICIENTES DEL MODELO M3 (COMPLETO) ---\n\n")

print(summary(mod3))

cat("\n  Observa los errores estándar: si el error estándar es grande")
cat("\n  respecto al coeficiente, la estimación es IMPRECISA.\n")

cat("\n=== FIN Caso Práctico B — Tema 03 ===\n")
