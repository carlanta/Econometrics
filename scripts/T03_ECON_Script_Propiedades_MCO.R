# =============================================================================
# ECONOMETRÍA — Capítulo 3
# Script Teórico: Propiedades de los estimadores MCO
# Autor: Carlos de Anta Puig
# =============================================================================
# INSTRUCCIONES: Abre primero mEconometria.Rproj en RStudio.
#                Ejecutar bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") {
  cat(msg); invisible(readline())
}

library(tidyverse); library(readxl); library(ggplot2)

col_principal  <- "#2C3E7A"; col_secundario <- "#E8501A"
col_terciario  <- "#27AE60"; col_gris <- "#7F8C8D"

caso_a <- read_excel("data/caso_a_demanda.xlsx")

cat("
=============================================================================
  ECONOMETRÍA — Tema 3: Propiedades de los estimadores MCO
=============================================================================

  En este script veremos:

    1. R² vs R² ajustado con modelos progresivos
    2. Simulación de las propiedades BLUE (insesgadez)
    3. Varianza del error y errores estándar

=============================================================================
")

pausa()

# ── 1. R² vs R² AJUSTADO ───────────────────────────────────────────────────
cat("\n--- 1. R² vs R² AJUSTADO: MODELOS PROGRESIVOS ---\n")

cat("\n  Estimamos 4 modelos añadiendo variables al Caso A:\n")

mod1 <- lm(ventas ~ precio_propio, data = caso_a)
mod2 <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
mod3 <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit, data = caso_a)
mod4 <- lm(ventas ~ precio_propio + gasto_publicidad + precio_sustit +
             promocion + temporada, data = caso_a)

cat("\n  Modelo  Variables  R²       R² ajust.  Sigma\n")
cat("  ------  ---------  ------   ---------  ------\n")
for (m in list(list(mod1,"M1",1), list(mod2,"M2",2),
               list(mod3,"M3",3), list(mod4,"M4",5))) {
  s <- summary(m[[1]])
  cat(sprintf("  %s      k = %d      %.4f   %.4f     %.4f\n",
              m[[2]], m[[3]], s$r.squared, s$adj.r.squared, s$sigma))
}

cat("\n  CONCLUSIÓN:")
cat("\n  - R² SIEMPRE crece al añadir variables (nunca baja).")
cat("\n  - R² ajustado puede BAJAR si la variable no aporta.")
cat("\n  - Usar R² ajustado para comparar modelos con distinto k.\n")

pausa()

# ── 2. CÁLCULO MANUAL DEL R² AJUSTADO ─────────────────────────────────────
cat("\n--- 2. CÁLCULO MANUAL DEL R² AJUSTADO ---\n")

n <- nrow(caso_a)
k <- 2
R2 <- summary(mod2)$r.squared

R2_adj_manual <- 1 - ((n - 1) / (n - k - 1)) * (1 - R2)

cat(sprintf("\n  Modelo M2 (precio + publicidad):\n"))
cat(sprintf("  n = %d, k = %d\n", n, k))
cat(sprintf("  R² = %.4f\n", R2))
cat(sprintf("  R² ajustado (fórmula): 1 - ((%d-1)/(%d-%d-1)) * (1 - %.4f) = %.4f\n",
            n, n, k, R2, R2_adj_manual))
cat(sprintf("  R² ajustado (summary): %.4f\n", summary(mod2)$adj.r.squared))
cat("\n  ¡Coinciden!\n")

pausa()

# ── 3. SIMULACIÓN: INSESGADEZ DE MCO ─────────────────────────────────────
cat("\n--- 3. SIMULACIÓN: INSESGADEZ DE MCO ---\n")

cat("\n  Modelo verdadero: Y = 2 + 3*X + u,  con u ~ N(0, 1)")
cat("\n  Simulamos 1000 muestras de n=30 y estimamos b1 en cada una.\n\n")

set.seed(2024)
n_sim <- 1000
n_obs <- 30
beta_true <- 3

estimaciones <- replicate(n_sim, {
  x <- rnorm(n_obs, 5, 2)
  y <- 2 + beta_true * x + rnorm(n_obs, 0, 1)
  coef(lm(y ~ x))[2]
})

cat(sprintf("  Valor verdadero de b1:      %d\n", beta_true))
cat(sprintf("  Media de 1000 estimaciones: %.4f\n", mean(estimaciones)))
cat(sprintf("  Desv. típica:               %.4f\n", sd(estimaciones)))
cat(sprintf("\n  La media está MUY CERCA de 3 -> INSESGADEZ ✓\n"))

hist(estimaciones, breaks = 40, col = "gray85", border = "gray40",
     main = sprintf("1000 estimaciones de b1 (verdadero = %d)", beta_true),
     xlab = expression(hat(beta)[1]))
abline(v = beta_true, lwd = 2, lty = 2)
abline(v = mean(estimaciones), lwd = 2, lty = 3, col = "gray40")
legend("topright", legend = c("Valor real", "Media estimaciones"),
       lty = c(2, 3), lwd = 2, col = c("black", "gray40"), bty = "n")

pausa()

# ── 4. VARIANZA DEL ERROR Y ERRORES ESTÁNDAR ──────────────────────────────
cat("\n--- 4. VARIANZA DEL ERROR Y ERRORES ESTÁNDAR ---\n")

cat("\n  Modelo M2: Ventas = b0 + b1*Precio + b2*Publicidad + u\n\n")

s <- summary(mod2)
SCR <- sum(residuals(mod2)^2)
gl <- n - 2 - 1
sigma2 <- SCR / gl

cat(sprintf("  SCR = %.4f\n", SCR))
cat(sprintf("  Grados de libertad: n - k - 1 = %d - 2 - 1 = %d\n", n, gl))
cat(sprintf("  Varianza del error: s² = SCR/gl = %.4f\n", sigma2))
cat(sprintf("  Error estándar regresión: s = %.4f\n", sqrt(sigma2)))

cat("\n  Errores estándar de los coeficientes:\n")
cat(sprintf("    SE(b0) = %.4f\n", s$coefficients[1, 2]))
cat(sprintf("    SE(b1) = %.4f\n", s$coefficients[2, 2]))
cat(sprintf("    SE(b2) = %.4f\n", s$coefficients[3, 2]))

cat("\n  Un error estándar GRANDE indica INCERTIDUMBRE sobre el coeficiente.")
cat("\n  Un error estándar PEQUEÑO indica una estimación PRECISA.\n")

cat("\n=== FIN Script Tema 03 ===\n")
