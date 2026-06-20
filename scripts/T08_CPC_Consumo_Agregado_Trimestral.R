# =============================================================================
# ECONOMETRÍA — Capítulo 8, Caso Práctico C — Dummy crisis
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
# =============================================================================
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl)
caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")
cat("\n=== CASO PRÁCTICO C — Tema 8: Dummy crisis ===\n")
pausa()
mod <- lm(consumo ~ pib + tipo_interes + crisis, data = caso_c)
s <- summary(mod)
cat("\n--- Función de consumo con dummy crisis ---\n\n")
print(s$coefficients)
b_crisis <- coef(mod)[4]
cat(sprintf("\n  Efecto crisis: %.2f MM EUR\n", b_crisis))
cat(sprintf("  Durante la crisis, el consumo fue %.0f MM EUR %s.\n",
            abs(b_crisis), ifelse(b_crisis < 0, "MENOR", "MAYOR")))
cat("\n=== FIN Caso Práctico C — Tema 08 ===\n")
