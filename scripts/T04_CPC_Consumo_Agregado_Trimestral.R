# =============================================================================
# ECONOMETRÍA — Capítulo 4, Caso Práctico C
# CPC: Consumo agregado trimestral de España — Contrastes de hipótesis
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
  Tema 4: Contrastes de hipótesis — PMC keynesiana
=============================================================================
")

pausa()

mod <- lm(consumo ~ pib + tipo_interes, data = caso_c)
s   <- summary(mod)
n <- nrow(caso_c); k <- 2; gl <- n - k - 1

# ── 1. SIGNIFICATIVIDAD INDIVIDUAL ────────────────────────────────────────
cat("\n--- 1. SIGNIFICATIVIDAD INDIVIDUAL ---\n\n")

cat("  Variable       Coef.         SE          t       p-valor  Decision\n")
cat("  -------------  ----------  ----------  ------  ---------  --------\n")
for (i in 1:nrow(s$coefficients)) {
  dec <- ifelse(s$coefficients[i, 4] < 0.05, "Signif.", "No signif.")
  cat(sprintf("  %-13s %10.4f  %10.4f  %6.3f  %9.6f  %s\n",
              rownames(s$coefficients)[i],
              s$coefficients[i, 1], s$coefficients[i, 2],
              s$coefficients[i, 3], s$coefficients[i, 4], dec))
}

pausa()

# ── 2. CONTRASTE KEYNESIANO: ¿PMC > 0? ───────────────────────────────────
cat("\n--- 2. ¿LA PMC ES POSITIVA? (contraste unilateral) ---\n")

pmc    <- coef(mod)[2]
se_pmc <- s$coefficients[2, 2]
t_pmc  <- pmc / se_pmc
p_uni  <- pt(t_pmc, gl, lower.tail = FALSE)

cat(sprintf("\n  H0: PMC <= 0  vs  H1: PMC > 0\n"))
cat(sprintf("  PMC estimada = %.4f\n", pmc))
cat(sprintf("  t = %.3f, p-valor (una cola) = %.8f\n", t_pmc, p_uni))
cat(sprintf("  Decisión: RECHAZAR H0 — La PMC es significativamente POSITIVA.\n"))

pausa()

# ── 3. CONTRASTE: ¿PMC = 0.80? ──────────────────────────────────────────
cat("\n--- 3. ¿LA PMC ES EXACTAMENTE 0.80? ---\n")

beta_star <- 0.80
t_h0 <- (pmc - beta_star) / se_pmc
p_h0 <- 2 * pt(abs(t_h0), gl, lower.tail = FALSE)

cat(sprintf("\n  H0: PMC = 0.80  vs  H1: PMC ≠ 0.80  (bilateral)\n"))
cat(sprintf("  t = (%.4f - 0.80) / %.4f = %.3f\n", pmc, se_pmc, t_h0))
cat(sprintf("  p-valor = %.4f\n", p_h0))

if (p_h0 < 0.05) {
  cat("  Decisión: Rechazar H0. La PMC difiere significativamente de 0.80.\n")
} else {
  cat("  Decisión: No rechazar H0. La PMC es compatible con un valor de 0.80.\n")
}

cat("\n  ADVERTENCIA: en series temporales, la autocorrelación puede")
cat("\n  invalidar estos contrastes. Lo veremos en el Tema 10.\n")

cat("\n=== FIN Caso Práctico C — Tema 04 ===\n")
