# =============================================================================
# ECONOMETRÍA — Capítulo 3, Caso Práctico C
# CPC: Consumo agregado trimestral de España — Propiedades MCO
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

caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO C — Consumo agregado trimestral de España
  Tema 3: Propiedades MCO — R² vs R² ajustado
=============================================================================
")

pausa()

# ── 1. MODELOS PROGRESIVOS ─────────────────────────────────────────────────
cat("\n--- 1. MODELOS PROGRESIVOS ---\n")

mod1 <- lm(consumo ~ pib, data = caso_c)
mod2 <- lm(consumo ~ pib + tipo_interes, data = caso_c)
mod3 <- lm(consumo ~ pib + tipo_interes + inflacion, data = caso_c)

cat("\n  M1: consumo ~ PIB (función de consumo simple)")
cat("\n  M2: M1 + tipo de interés")
cat("\n  M3: M2 + inflación\n")

pausa()

# ── 2. COMPARACIÓN R² vs R² AJUSTADO ──────────────────────────────────────
cat("\n--- 2. COMPARACIÓN ---\n\n")

cat("  Modelo  k   R²       R² ajust.  Sigma\n")
cat("  ------  --  ------   ---------  ------\n")
for (m in list(list(mod1,"M1",1), list(mod2,"M2",2), list(mod3,"M3",3))) {
  s <- summary(m[[1]])
  cat(sprintf("  %s      %d   %.4f   %.4f     %.2f\n",
              m[[2]], m[[3]], s$r.squared, s$adj.r.squared, s$sigma))
}

pmc1 <- coef(mod1)[2]
pmc2 <- coef(mod2)[2]
pmc3 <- coef(mod3)[2]

cat("\n  Evolución de la PMC (propensión marginal a consumir):")
cat(sprintf("\n    M1: PMC = %.4f", pmc1))
cat(sprintf("\n    M2: PMC = %.4f", pmc2))
cat(sprintf("\n    M3: PMC = %.4f\n", pmc3))

cat("\n  La PMC es ESTABLE al añadir variables — buena señal.")
cat("\n  ADVERTENCIA: R² muy alto en series temporales puede ser espurio.\n")

pausa()

# ── 3. ERRORES ESTÁNDAR ──────────────────────────────────────────────────
cat("\n--- 3. ERRORES ESTÁNDAR DEL MODELO M2 ---\n\n")

s2 <- summary(mod2)
cat(sprintf("  Error estándar de la regresión: %.2f MM EUR\n\n", s2$sigma))
cat("  Coeficiente     Estimación     Error est.\n")
cat("  -------------   -----------    ----------\n")
for (i in 1:nrow(s2$coefficients)) {
  cat(sprintf("  %-14s  %11.4f    %10.4f\n",
              rownames(s2$coefficients)[i],
              s2$coefficients[i, 1], s2$coefficients[i, 2]))
}

cat("\n  El error estándar del PIB es PEQUEÑO respecto al coeficiente:")
cat(sprintf("\n  ratio = SE/coef = %.4f/%.4f = %.2f",
            s2$coefficients[2, 2], s2$coefficients[2, 1],
            s2$coefficients[2, 2] / s2$coefficients[2, 1]))
cat("\n  Un ratio bajo indica PRECISIÓN.\n")

cat("\n=== FIN Caso Práctico C — Tema 03 ===\n")
