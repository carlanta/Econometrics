# =============================================================================
# ECONOMETRÍA — Capítulo 4, Caso Práctico A
# CPA: Demanda de un producto de consumo — Contrastes de hipótesis
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
  Tema 4: Contrastes de hipótesis
=============================================================================
")

pausa()

mod <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
s   <- summary(mod)
n <- nrow(caso_a); k <- 3; gl <- n - k - 1

# ── 1. SIGNIFICATIVIDAD DE CADA VARIABLE ──────────────────────────────────
cat("\n--- 1. SIGNIFICATIVIDAD INDIVIDUAL (bilateral, alpha = 0.05) ---\n\n")

cat("  Variable          Coef.      SE       t       p-valor  Decision\n")
cat("  ----------------  --------  ------  ------  ---------  --------\n")
for (i in 2:nrow(s$coefficients)) {
  dec <- ifelse(s$coefficients[i, 4] < 0.05, "Signif.", "No signif.")
  cat(sprintf("  %-17s %8.4f  %6.4f  %6.3f  %9.6f  %s\n",
              rownames(s$coefficients)[i],
              s$coefficients[i, 1], s$coefficients[i, 2],
              s$coefficients[i, 3], s$coefficients[i, 4], dec))
}

cat(sprintf("\n  Valor crítico bilateral t(0.025, %d) = ±%.3f\n", gl, qt(0.975, gl)))

pausa()

# ── 2. CONTRASTE UNILATERAL: PRECIO NEGATIVO ──────────────────────────────
cat("\n--- 2. UNILATERAL: ¿EL PRECIO REDUCE LAS VENTAS? ---\n")

b1 <- coef(mod)[2]; se1 <- s$coefficients[2, 2]
t1 <- b1 / se1
p_uni <- pt(t1, gl)

cat(sprintf("\n  H0: b1 >= 0  vs  H1: b1 < 0  (unilateral izquierdo)\n"))
cat(sprintf("  t = %.3f, p-valor (una cola) = %.6f\n", t1, p_uni))
cat(sprintf("  Decisión: %s al 5%%\n",
            ifelse(p_uni < 0.05, "RECHAZAR H0 — el precio SÍ reduce las ventas",
                   "No rechazar H0")))

pausa()

# ── 3. CONTRASTE UNILATERAL: PUBLICIDAD POSITIVA ─────────────────────────
cat("\n--- 3. UNILATERAL: ¿LA PUBLICIDAD AUMENTA LAS VENTAS? ---\n")

b2 <- coef(mod)[3]; se2 <- s$coefficients[3, 2]
t2 <- b2 / se2
p_uni2 <- pt(t2, gl, lower.tail = FALSE)

cat(sprintf("\n  H0: b2 <= 0  vs  H1: b2 > 0  (unilateral derecho)\n"))
cat(sprintf("  t = %.3f, p-valor (una cola) = %.6f\n", t2, p_uni2))
cat(sprintf("  Decisión: %s al 5%%\n",
            ifelse(p_uni2 < 0.05, "RECHAZAR H0 — la publicidad SÍ aumenta las ventas",
                   "No rechazar H0")))

cat("\n=== FIN Caso Práctico A — Tema 04 ===\n")
