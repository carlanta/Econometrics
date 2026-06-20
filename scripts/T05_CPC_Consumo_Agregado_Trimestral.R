# =============================================================================
# ECONOMETRÍA — Capítulo 5, Caso Práctico C
# CPC: Consumo agregado trimestral de España — Intervalos de confianza
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
  Tema 5: Intervalos de confianza — PMC keynesiana
=============================================================================
")

pausa()

mod <- lm(consumo ~ pib + tipo_interes, data = caso_c)

# ── 1. IC AL 95% ─────────────────────────────────────────────────────────
cat("\n--- 1. INTERVALOS DE CONFIANZA AL 95% ---\n\n")

ic <- confint(mod, level = 0.95)
coefs <- coef(mod)

cat("  Variable       Coef.         [Lim. inf.     Lim. sup.]   Contiene 0?\n")
cat("  -------------  -----------   -----------    -----------   -----------\n")
noms <- c("Intercepto", "PIB (PMC)", "Tipo interes")
for (i in 1:3) {
  cont <- ifelse(ic[i,1] <= 0 & ic[i,2] >= 0, "Si", "No")
  cat(sprintf("  %-13s %11.4f   %11.4f    %11.4f   %s\n",
              noms[i], coefs[i], ic[i,1], ic[i,2], cont))
}

pausa()

# ── 2. ¿LA PMC ESTÁ ENTRE 0 Y 1? ────────────────────────────────────────
cat("\n--- 2. ¿CUMPLE KEYNES? IC DE LA PMC ---\n")

pmc_li <- ic[2, 1]
pmc_ls <- ic[2, 2]

cat(sprintf("\n  IC 95%% para la PMC: [%.4f, %.4f]\n", pmc_li, pmc_ls))
cat(sprintf("\n  ¿Está entre 0 y 1?"))

if (pmc_li > 0 & pmc_ls < 1) {
  cat(sprintf("\n  SÍ: todo el intervalo cae dentro de (0, 1)."))
  cat(sprintf("\n  Keynes se cumple con un 95%% de confianza.\n"))
} else {
  cat(sprintf("\n  Hay que verificar: el intervalo puede salirse de (0,1).\n"))
}

pausa()

# ── 3. DUALIDAD IC ↔ CONTRASTE ──────────────────────────────────────────
cat("\n--- 3. DUALIDAD IC ↔ CONTRASTE t ---\n")

s <- summary(mod)
cat(sprintf("\n  PMC: IC 95%% = [%.4f, %.4f]\n", pmc_li, pmc_ls))
cat(sprintf("  PMC: p-valor del contraste t = %.8f\n", s$coefficients[2, 4]))
cat(sprintf("\n  IC NO contiene 0 y p-valor < 0.05: COHERENTE.\n"))

cat("\n=== FIN Caso Práctico C — Tema 05 ===\n")
