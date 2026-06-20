# =============================================================================
# ECONOMETRÍA — Capítulo 6, Caso Práctico C
# CPC: Consumo agregado trimestral — Problemas de especificación
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl)
caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO C — Consumo agregado trimestral de España
  Tema 6: Problemas de especificación — ¿Omitir tipo de interés?
=============================================================================
")
pausa()

mod1 <- lm(consumo ~ pib, data = caso_c)
mod2 <- lm(consumo ~ pib + tipo_interes, data = caso_c)
mod3 <- lm(consumo ~ pib + tipo_interes + inflacion, data = caso_c)

cat("\n--- 1. EVOLUCIÓN DE LA PMC AL AÑADIR VARIABLES ---\n\n")

cat("  Modelo               PMC         SE         R2 adj.\n")
cat("  -------------------  ----------  ---------  --------\n")
cat(sprintf("  Solo PIB             %10.4f  %9.4f  %.4f\n",
            coef(mod1)[2], summary(mod1)$coefficients[2,2], summary(mod1)$adj.r.squared))
cat(sprintf("  + tipo interes       %10.4f  %9.4f  %.4f\n",
            coef(mod2)[2], summary(mod2)$coefficients[2,2], summary(mod2)$adj.r.squared))
cat(sprintf("  + inflacion          %10.4f  %9.4f  %.4f\n",
            coef(mod3)[2], summary(mod3)$coefficients[2,2], summary(mod3)$adj.r.squared))

pausa()

cat("\n--- 2. CORRELACIÓN PIB-TIPO INTERÉS ---\n")

cor_pi <- cor(caso_c$pib, caso_c$tipo_interes)
cat(sprintf("\n  cor(PIB, tipo_interes) = %.4f\n", cor_pi))

sesgo_obs <- coef(mod1)[2] - coef(mod2)[2]
cat(sprintf("  Sesgo observado en PMC al omitir tipo interés: %.4f\n", sesgo_obs))

b2 <- coef(mod2)[3]
cat(sprintf("  b2 (tipo interés) = %.2f, signo: %s\n", b2,
            ifelse(b2 > 0, "positivo", "negativo")))
cat(sprintf("  correlación: %s\n", ifelse(cor_pi > 0, "positiva", "negativa")))
cat(sprintf("  Dirección esperada del sesgo: signo(b2)*signo(cor) = %s\n",
            ifelse(b2 * cor_pi > 0, "positivo", "negativo")))

cat("\n=== FIN Caso Práctico C — Tema 06 ===\n")
