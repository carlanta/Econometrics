# =============================================================================
# ECONOMETRÍA — Capítulo 5
# Script Teórico: Intervalos de confianza en el MRLG
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
  ECONOMETRÍA — Tema 5: Intervalos de confianza en el MRLG
=============================================================================

  En este script aprenderemos a:

    1. Construir intervalos de confianza con confint()
    2. Calcular un IC manualmente paso a paso
    3. Verificar la dualidad IC ↔ contraste t
    4. Construir IC para combinaciones lineales

=============================================================================
")

pausa()

# ── 1. MODELO DE REFERENCIA ───────────────────────────────────────────────
cat("\n--- 1. MODELO DE REFERENCIA ---\n")

mod <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
s   <- summary(mod)
n <- nrow(caso_a); k <- 3; gl <- n - k - 1

cat("\n  Ventas = b0 + b1*Precio + b2*Publicidad + b3*Precio_sustit + u\n")
cat(sprintf("  n = %d, k = %d, grados de libertad = %d\n", n, k, gl))

pausa()

# ── 2. IC CON confint() ──────────────────────────────────────────────────
cat("\n--- 2. INTERVALOS DE CONFIANZA CON confint() ---\n")

cat("\n  IC al 95%:\n")
print(round(confint(mod, level = 0.95), 4))

cat("\n  IC al 90%:\n")
print(round(confint(mod, level = 0.90), 4))

cat("\n  IC al 99%:\n")
print(round(confint(mod, level = 0.99), 4))

cat("\n  Observa: a mayor confianza, intervalos MÁS ANCHOS.\n")

pausa()

# ── 3. CÁLCULO MANUAL PASO A PASO ────────────────────────────────────────
cat("\n--- 3. CÁLCULO MANUAL: IC AL 95% PARA EL PRECIO ---\n")

b1    <- coef(mod)[2]
se_b1 <- s$coefficients[2, 2]
tc    <- qt(0.975, gl)

li <- b1 - tc * se_b1
ls <- b1 + tc * se_b1

cat(sprintf("\n  b1 = %.4f\n", b1))
cat(sprintf("  SE(b1) = %.4f\n", se_b1))
cat(sprintf("  t(0.025, %d) = %.3f\n\n", gl, tc))
cat(sprintf("  IC = b1 ± t * SE(b1)\n"))
cat(sprintf("     = %.4f ± %.3f × %.4f\n", b1, tc, se_b1))
cat(sprintf("     = [%.4f, %.4f]\n\n", li, ls))

cat(sprintf("  ¿Contiene el 0? %s\n",
            ifelse(li <= 0 & ls >= 0, "SÍ → No significativo",
                   "NO → SIGNIFICATIVO al 5%%")))

cat(sprintf("\n  Verificación con confint():\n"))
ic_auto <- confint(mod, level = 0.95)[2, ]
cat(sprintf("     [%.4f, %.4f]  ¡Idéntico!\n", ic_auto[1], ic_auto[2]))

pausa()

# ── 4. DUALIDAD IC ↔ CONTRASTE t ────────────────────────────────────────
cat("\n--- 4. DUALIDAD: IC Y CONTRASTE t ---\n")

cat("\n  Para el precio propio:\n")
cat(sprintf("  IC 95%%: [%.4f, %.4f] — NO contiene 0\n", li, ls))
cat(sprintf("  p-valor del contraste t: %.6f — MENOR que 0.05\n", s$coefficients[2, 4]))
cat("\n  Ambos dicen lo mismo: el precio ES significativo.\n")
cat("  Esta coherencia SIEMPRE se cumple (dualidad IC-contrastes).\n")

pausa()

# ── 5. IC PARA COMBINACIÓN LINEAL ────────────────────────────────────────
cat("\n--- 5. IC PARA COMBINACIÓN LINEAL ---\n")

cat("\n  Queremos el IC para b1 + 0.5*b2")
cat("\n  (efecto de subir precio 1 EUR y publicidad 0.5 MM EUR)\n\n")

c_vec <- c(0, 1, 0.5, 0)  # combinación: 0*b0 + 1*b1 + 0.5*b2 + 0*b3
est   <- sum(c_vec * coef(mod))
se_cl <- sqrt(t(c_vec) %*% vcov(mod) %*% c_vec)
li_cl <- est - tc * se_cl
ls_cl <- est + tc * se_cl

cat(sprintf("  Estimación: b1 + 0.5*b2 = %.4f + 0.5*%.4f = %.4f\n",
            coef(mod)[2], coef(mod)[3], est))
cat(sprintf("  SE de la combinación: %.4f\n", se_cl))
cat(sprintf("  IC 95%%: [%.4f, %.4f]\n", li_cl, ls_cl))

cat("\n=== FIN Script Tema 05 ===\n")
