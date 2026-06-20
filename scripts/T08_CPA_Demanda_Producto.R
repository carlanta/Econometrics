# =============================================================================
# ECONOMETRÍA — Capítulo 8, Caso Práctico A — Dummies
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
# =============================================================================
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl)
caso_a <- read_excel("data/caso_a_demanda.xlsx")
cat("\n=== CASO PRÁCTICO A — Tema 8: Dummies ===\n")
pausa()
mod <- lm(ventas ~ precio_propio + gasto_publicidad + promocion + temporada, data = caso_a)
s <- summary(mod)
cat("\n--- Modelo con promoción y temporada ---\n\n")
print(s$coefficients)
cat(sprintf("\n  Efecto promoción: %.4f MM EUR (p = %.4f)\n", coef(mod)[4], s$coefficients[4,4]))
cat(sprintf("  Efecto temporada: %.4f MM EUR (p = %.4f)\n", coef(mod)[5], s$coefficients[5,4]))
cat("\n=== FIN Caso Práctico A — Tema 08 ===\n")
