# =============================================================================
# ECONOMETRÍA — Capítulo 5, Caso Práctico B
# CPB: Cifra de negocio y empleo sectorial — Intervalos de confianza
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
  Tema 5: Intervalos de confianza
=============================================================================
")

pausa()

mod <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)

# ── 1. IC AL 95% ─────────────────────────────────────────────────────────
cat("\n--- 1. INTERVALOS DE CONFIANZA AL 95% ---\n\n")

ic <- confint(mod, level = 0.95)
coefs <- coef(mod)

cat("  Variable       Coef.      [Lim. inf.  Lim. sup.]  Contiene 0?\n")
cat("  -------------  --------   ----------  ----------  -----------\n")
noms <- c("Intercepto", "Trabajadores", "Inversion", "Superficie")
for (i in 1:4) {
  cont <- ifelse(ic[i,1] <= 0 & ic[i,2] >= 0, "Si", "No")
  cat(sprintf("  %-13s %8.4f   %10.4f  %10.4f  %s\n",
              noms[i], coefs[i], ic[i,1], ic[i,2], cont))
}

cat("\n  Solo 'trabajadores' tiene IC que NO contiene 0.")
cat("\n  Inversión y superficie: IC MUY ANCHOS → estimaciones imprecisas.\n")

pausa()

# ── 2. INTERPRETACIÓN ECONÓMICA ──────────────────────────────────────────
cat("\n--- 2. INTERPRETACIÓN DEL IC DE TRABAJADORES ---\n")

cat(sprintf("\n  IC 95%% para el efecto de un trabajador adicional:"))
cat(sprintf("\n  [%.4f, %.4f] M EUR\n", ic[2,1], ic[2,2]))
cat(sprintf("\n  Con un 95%% de confianza, cada trabajador adicional"))
cat(sprintf("\n  aumenta la cifra de negocio entre %.4f y %.4f M EUR.\n",
            ic[2,1], ic[2,2]))

cat("\n=== FIN Caso Práctico B — Tema 05 ===\n")
