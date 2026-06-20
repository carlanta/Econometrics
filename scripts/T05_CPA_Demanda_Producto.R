# =============================================================================
# ECONOMETRÍA — Capítulo 5, Caso Práctico A
# CPA: Demanda de un producto de consumo — Intervalos de confianza
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl)

caso_a <- read_excel("data/caso_a_demanda.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO A — Demanda de un producto de consumo
  Tema 5: Intervalos de confianza
=============================================================================
")

pausa()

mod <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)

# ── 1. IC AL 95% ─────────────────────────────────────────────────────────
cat("\n--- 1. INTERVALOS DE CONFIANZA AL 95% ---\n\n")

ic <- confint(mod, level = 0.95)
coefs <- coef(mod)

cat("  Variable          Coef.      [Lim. inf.  Lim. sup.]  Contiene 0?\n")
cat("  ----------------  --------   ----------  ----------  -----------\n")
noms <- c("Intercepto", "Precio propio", "Publicidad", "Precio sustit.")
for (i in 1:4) {
  cont <- ifelse(ic[i,1] <= 0 & ic[i,2] >= 0, "Si", "No")
  cat(sprintf("  %-17s %8.4f   %10.4f  %10.4f  %s\n",
              noms[i], coefs[i], ic[i,1], ic[i,2], cont))
}

cat("\n  Variables con IC que NO contiene 0 → SIGNIFICATIVAS al 5%.\n")

pausa()

# ── 2. COMPARACIÓN DE NIVELES ────────────────────────────────────────────
cat("\n--- 2. IC DEL PRECIO A DISTINTOS NIVELES ---\n\n")

for (nivel in c(0.90, 0.95, 0.99)) {
  ic_n <- confint(mod, "precio_propio", level = nivel)
  ancho <- ic_n[2] - ic_n[1]
  cat(sprintf("  IC al %2.0f%%: [%8.4f, %8.4f]  ancho = %.4f\n",
              nivel * 100, ic_n[1], ic_n[2], ancho))
}

cat("\n  A mayor confianza, intervalo MÁS ANCHO.\n")

cat("\n=== FIN Caso Práctico A — Tema 05 ===\n")
