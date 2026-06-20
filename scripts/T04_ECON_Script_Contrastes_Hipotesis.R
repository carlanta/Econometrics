# =============================================================================
# ECONOMETRÍA — Capítulo 4
# Script Teórico: Contrastes de hipótesis en el MRLG
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl); library(ggplot2)

caso_a <- read_excel("data/caso_a_demanda.xlsx")

cat("
=============================================================================
  ECONOMETRÍA — Tema 4: Contrastes de hipótesis en el MRLG
=============================================================================

  En este script aprenderemos a:

    1. Plantear contrastes de hipótesis sobre coeficientes
    2. Calcular el estadístico t paso a paso
    3. Aplicar la regla del p-valor
    4. Distinguir bilateral vs unilateral

=============================================================================
")

pausa()

# ── 1. ESTIMACIÓN DEL MODELO ───────────────────────────────────────────────
cat("\n--- 1. MODELO DE REFERENCIA ---\n")

mod <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
s   <- summary(mod)

cat("\n  Modelo: Ventas = b0 + b1*Precio + b2*Publicidad + b3*Precio_sustit + u\n\n")
print(s$coefficients)

n  <- nrow(caso_a)
k  <- 3
gl <- n - k - 1
cat(sprintf("\n  n = %d, k = %d, grados de libertad = %d\n", n, k, gl))

pausa()

# ── 2. CONTRASTE BILATERAL: ¿ES SIGNIFICATIVO EL PRECIO? ──────────────────
cat("\n--- 2. CONTRASTE BILATERAL: ¿ES SIGNIFICATIVO EL PRECIO? ---\n")

b1    <- coef(mod)[2]
se_b1 <- s$coefficients[2, 2]
t_obs <- b1 / se_b1
p_bil <- 2 * pt(abs(t_obs), gl, lower.tail = FALSE)

cat(sprintf("\n  H0: b1 = 0  vs  H1: b1 ≠ 0  (bilateral, alpha = 0.05)\n"))
cat(sprintf("\n  PASO 1: Hipótesis planteadas\n"))
cat(sprintf("  PASO 2: t = b1 / SE(b1) = %.4f / %.4f = %.3f\n", b1, se_b1, t_obs))

t_crit <- qt(0.975, gl)
cat(sprintf("  PASO 3: Valor crítico t(0.025, %d) = ±%.3f\n", gl, t_crit))
cat(sprintf("           |t| = %.3f > %.3f? %s\n",
            abs(t_obs), t_crit, ifelse(abs(t_obs) > t_crit, "SÍ -> RECHAZAR H0", "NO")))
cat(sprintf("           p-valor = %.6f < 0.05? %s\n",
            p_bil, ifelse(p_bil < 0.05, "SÍ -> RECHAZAR H0", "NO")))

cat(sprintf("\n  CONCLUSIÓN: El precio propio ES significativo al 5%%.\n"))

pausa()

# ── 3. CONTRASTE UNILATERAL: ¿LA PUBLICIDAD TIENE EFECTO POSITIVO? ────────
cat("\n--- 3. CONTRASTE UNILATERAL DERECHO: PUBLICIDAD ---\n")

b2    <- coef(mod)[3]
se_b2 <- s$coefficients[3, 2]
t_obs2 <- b2 / se_b2
p_uni  <- pt(t_obs2, gl, lower.tail = FALSE)

cat(sprintf("\n  H0: b2 ≤ 0  vs  H1: b2 > 0  (unilateral derecho, alpha = 0.05)\n"))
cat(sprintf("\n  t = %.4f / %.4f = %.3f\n", b2, se_b2, t_obs2))

t_crit1 <- qt(0.95, gl)
cat(sprintf("  Valor crítico t(0.05, %d) = %.3f (una cola)\n", gl, t_crit1))
cat(sprintf("  t = %.3f > %.3f? %s\n",
            t_obs2, t_crit1, ifelse(t_obs2 > t_crit1, "SÍ -> RECHAZAR H0", "NO")))
cat(sprintf("  p-valor (una cola) = %.6f\n", p_uni))

cat(sprintf("\n  CONCLUSIÓN: La publicidad tiene un efecto POSITIVO significativo.\n"))

pausa()

# ── 4. CONTRASTE CON VALOR DISTINTO DE CERO ───────────────────────────────
cat("\n--- 4. CONTRASTE: ¿EL EFECTO DEL PRECIO ES -2? ---\n")

beta_star <- -2
t_h0 <- (b1 - beta_star) / se_b1
p_h0 <- 2 * pt(abs(t_h0), gl, lower.tail = FALSE)

cat(sprintf("\n  H0: b1 = -2  vs  H1: b1 ≠ -2  (bilateral, alpha = 0.05)\n"))
cat(sprintf("\n  t = (b1 - (-2)) / SE(b1) = (%.4f - (-2)) / %.4f = %.3f\n",
            b1, se_b1, t_h0))
cat(sprintf("  p-valor = %.4f\n", p_h0))

if (p_h0 < 0.05) {
  cat("  DECISIÓN: Rechazar H0. El efecto NO es exactamente -2.\n")
} else {
  cat("  DECISIÓN: No rechazar H0. No hay evidencia de que difiera de -2.\n")
}

pausa()

# ── 5. RESUMEN DE SIGNIFICATIVIDAD ────────────────────────────────────────
cat("\n--- 5. RESUMEN DE SIGNIFICATIVIDAD DEL MODELO ---\n\n")

cat("  Variable          Coef.      t       p-valor    Signif.\n")
cat("  ----------------  --------  ------  ---------  -------\n")
for (i in 1:nrow(s$coefficients)) {
  sig <- ifelse(s$coefficients[i, 4] < 0.001, "***",
         ifelse(s$coefficients[i, 4] < 0.01, "**",
         ifelse(s$coefficients[i, 4] < 0.05, "*",
         ifelse(s$coefficients[i, 4] < 0.10, ".", ""))))
  cat(sprintf("  %-17s %8.4f  %6.3f  %9.4f  %s\n",
              rownames(s$coefficients)[i],
              s$coefficients[i, 1], s$coefficients[i, 3],
              s$coefficients[i, 4], sig))
}

cat("\n  Código: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10\n")

cat("\n=== FIN Script Tema 04 ===\n")
