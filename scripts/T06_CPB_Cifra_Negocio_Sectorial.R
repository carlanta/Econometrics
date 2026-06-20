# =============================================================================
# ECONOMETRÍA — Capítulo 6, Caso Práctico B
# CPB: Cifra de negocio — Problemas de especificación
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
  Tema 6: Problemas de especificación
=============================================================================
")
pausa()

mod1 <- lm(cifra_negocio ~ trabajadores, data = caso_b)
mod2 <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
mod3 <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)

cat("\n--- 1. ESTABILIDAD DEL COEFICIENTE DE TRABAJADORES ---\n\n")

cat("  Modelo               Coef. trab.   SE        R2 adj.\n")
cat("  -------------------  -----------  ------    --------\n")
cat(sprintf("  Solo trabajadores    %11.4f  %6.4f    %.4f\n",
            coef(mod1)[2], summary(mod1)$coefficients[2,2], summary(mod1)$adj.r.squared))
cat(sprintf("  + inversion          %11.4f  %6.4f    %.4f\n",
            coef(mod2)[2], summary(mod2)$coefficients[2,2], summary(mod2)$adj.r.squared))
cat(sprintf("  + superficie         %11.4f  %6.4f    %.4f\n",
            coef(mod3)[2], summary(mod3)$coefficients[2,2], summary(mod3)$adj.r.squared))

cat("\n  El coeficiente de trabajadores es ESTABLE → no hay sesgo grave.")
cat("\n  El R2 ajustado APENAS mejora → inversión y superficie son prescindibles.\n")

cat("\n=== FIN Caso Práctico B — Tema 06 ===\n")
