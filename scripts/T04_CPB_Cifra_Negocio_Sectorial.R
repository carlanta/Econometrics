# =============================================================================
# ECONOMETRÍA — Capítulo 4, Caso Práctico B
# CPB: Cifra de negocio y empleo sectorial — Contrastes de hipótesis
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
  Tema 4: Contrastes de hipótesis
=============================================================================
")

pausa()

mod <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)
s   <- summary(mod)
n <- nrow(caso_b); k <- 3; gl <- n - k - 1

# ── 1. SIGNIFICATIVIDAD INDIVIDUAL ────────────────────────────────────────
cat("\n--- 1. SIGNIFICATIVIDAD INDIVIDUAL ---\n\n")

cat("  Variable       Coef.      SE       t       p-valor  Decision (5%)\n")
cat("  -------------  --------  ------  ------  ---------  -------------\n")
for (i in 2:nrow(s$coefficients)) {
  dec <- ifelse(s$coefficients[i, 4] < 0.05, "Signif.", "No signif.")
  cat(sprintf("  %-13s %8.4f  %6.4f  %6.3f  %9.4f  %s\n",
              rownames(s$coefficients)[i],
              s$coefficients[i, 1], s$coefficients[i, 2],
              s$coefficients[i, 3], s$coefficients[i, 4], dec))
}

cat("\n  CONCLUSIÓN:")
cat("\n  Solo 'trabajadores' es significativo individualmente.")
cat("\n  Inversión y superficie NO son significativas al 5%.\n")

pausa()

# ── 2. ¿MÁS TRABAJADORES IMPLICA MÁS FACTURACIÓN? ────────────────────────
cat("\n--- 2. UNILATERAL: ¿MÁS TRABAJADORES = MÁS FACTURACIÓN? ---\n")

b1 <- coef(mod)[2]; se1 <- s$coefficients[2, 2]
t1 <- b1 / se1
p_uni <- pt(t1, gl, lower.tail = FALSE)

cat(sprintf("\n  H0: b1 <= 0  vs  H1: b1 > 0  (unilateral derecho)\n"))
cat(sprintf("  t = %.3f, p-valor (una cola) = %.6f\n", t1, p_uni))
cat(sprintf("  Decisión: %s\n",
            ifelse(p_uni < 0.05, "RECHAZAR H0 — más trabajadores SÍ implica más facturación",
                   "No rechazar H0")))

cat("\n=== FIN Caso Práctico B — Tema 04 ===\n")
