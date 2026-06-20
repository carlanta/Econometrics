# =============================================================================
# ECONOMETRÍA — Capítulo 8, Caso Práctico B — Dummies PAC y región
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
# =============================================================================
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl)
caso_b <- read_excel("data/caso_b_cifra_negocio.xlsx")
cat("\n=== CASO PRÁCTICO B — Tema 8: Dummies PAC y región ===\n")
pausa()
mod <- lm(cifra_negocio ~ trabajadores + inversion + pac + region, data = caso_b)
s <- summary(mod)
cat("\n--- Modelo con PAC y región ---\n\n")
print(s$coefficients)
cat(sprintf("\n  Efecto PAC: %.4f M EUR (p = %.4f)\n", coef(mod)[4], s$coefficients[4,4]))
cat(sprintf("  Efecto región norte: %.4f M EUR (p = %.4f)\n", coef(mod)[5], s$coefficients[5,4]))
cat("\n=== FIN Caso Práctico B — Tema 08 ===\n")
