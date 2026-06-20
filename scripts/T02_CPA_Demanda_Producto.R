# =============================================================================
# ECONOMETRÍA — Capítulo 2, Caso Práctico A
# CPA: Demanda de un producto de consumo — Estimación MCO
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
col_terciario  <- "#27AE60"; col_gris <- "#7F8C8D"; col_fondo <- "#F4F6F9"

caso_a <- read_excel("data/caso_a_demanda.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO A — Demanda de un producto de consumo
  Tema 2: Estimación MCO
=============================================================================

  Objetivo: estimar un modelo de demanda con MCO, interpretar los
  coeficientes y evaluar la bondad de ajuste.

  Modelo: Ventas = b0 + b1*Precio_propio + b2*Gasto_publicidad + u

=============================================================================
")

pausa()

# ── 1. RECORDATORIO DE LOS DATOS ───────────────────────────────────────────
cat("\n--- 1. RECORDATORIO DE LOS DATOS ---\n")

cat(sprintf("\n  Dataset: %d observaciones, %d variables\n",
            nrow(caso_a), ncol(caso_a)))
cat("\n  Resumen de las variables del modelo:\n")
print(summary(caso_a[, c("ventas", "precio_propio", "gasto_publicidad")]))

pausa()

# ── 2. ESTIMACIÓN MCO ─────────────────────────────────────────────────────
cat("\n--- 2. ESTIMACIÓN MCO ---\n")

cat("\n  Modelo: Ventas = b0 + b1*Precio + b2*Publicidad + u\n\n")

modelo_a <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
print(summary(modelo_a))

pausa()

# ── 3. INTERPRETACIÓN DE COEFICIENTES ────────────────────────────────────
cat("\n--- 3. INTERPRETACIÓN DE COEFICIENTES ---\n")

coefs <- coef(modelo_a)

cat(sprintf("\n  b0 = %.4f (intercepto)\n", coefs[1]))
cat(sprintf("     Si el precio fuese 0 EUR y no hubiese publicidad,\n"))
cat(sprintf("     las ventas esperadas serían %.2f MM EUR.\n\n", coefs[1]))

cat(sprintf("  b1 = %.4f (precio propio)\n", coefs[2]))
cat(sprintf("     Por cada EUR de aumento en el precio propio,\n"))
cat(sprintf("     las ventas DISMINUYEN en %.2f MM EUR (ceteris paribus).\n", abs(coefs[2])))
cat(sprintf("     Signo negativo: COHERENTE con la ley de la demanda.\n\n"))

cat(sprintf("  b2 = %.4f (gasto publicidad)\n", coefs[3]))
cat(sprintf("     Por cada MM EUR adicional de gasto en publicidad,\n"))
cat(sprintf("     las ventas AUMENTAN en %.2f MM EUR (ceteris paribus).\n", coefs[3]))
cat(sprintf("     Signo positivo: COHERENTE con la teoría económica.\n"))

pausa()

# ── 4. BONDAD DE AJUSTE ─────────────────────────────────────────────────
cat("\n--- 4. BONDAD DE AJUSTE: R² ---\n")

Y     <- caso_a$ventas
Y_hat <- fitted(modelo_a)
Y_bar <- mean(Y)

SCT <- sum((Y - Y_bar)^2)
SCE <- sum((Y_hat - Y_bar)^2)
SCR <- sum(residuals(modelo_a)^2)
R2  <- summary(modelo_a)$r.squared

cat(sprintf("\n  SCT = %.4f\n", SCT))
cat(sprintf("  SCE = %.4f\n", SCE))
cat(sprintf("  SCR = %.4f\n", SCR))
cat(sprintf("  R²  = %.4f\n\n", R2))
cat(sprintf("  El precio y la publicidad explican el %.1f%% de la\n", R2 * 100))
cat(sprintf("  variabilidad de las ventas.\n"))

pausa()

# ── 5. TABLA ANOVA ──────────────────────────────────────────────────────
cat("\n--- 5. TABLA ANOVA ---\n\n")

print(anova(modelo_a))

cat("\n  La tabla ANOVA descompone la variabilidad por variable.\n")
cat("  Sum Sq = contribución de cada variable a SCE.\n")

pausa()

# ── 6. VALORES AJUSTADOS Y RESIDUOS ────────────────────────────────────
cat("\n--- 6. VALORES AJUSTADOS Y RESIDUOS ---\n")

cat("\n  Primeras 10 observaciones:\n\n")
cat("  Obs  Ventas   Ajustado  Residuo\n")
cat("  ---  ------   --------  -------\n")
for (i in 1:10) {
  cat(sprintf("  %3d  %6.2f   %6.2f    %7.4f\n",
              i, Y[i], Y_hat[i], residuals(modelo_a)[i]))
}

cat(sprintf("\n  Suma de residuos:  %.8f  (= 0)\n", sum(residuals(modelo_a))))

n <- nrow(caso_a); k <- 2
sigma_hat <- sqrt(SCR / (n - k - 1))
cat(sprintf("  Error estándar residual (s): %.4f MM EUR\n", sigma_hat))

pausa()

# ── 7. VISUALIZACIÓN ───────────────────────────────────────────────────
cat("\n--- 7. VISUALIZACIÓN ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Ventas vs precio con recta parcial
plot(caso_a$precio_propio, caso_a$ventas,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Precio propio (EUR)", ylab = "Ventas (MM EUR)",
     main = "Ventas vs Precio propio")
abline(lm(ventas ~ precio_propio, data = caso_a),
       col = col_secundario, lwd = 2)

# Observado vs ajustado
plot(Y_hat, Y,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Ventas ajustadas (MM EUR)", ylab = "Ventas observadas (MM EUR)",
     main = sprintf("Observado vs Ajustado (R² = %.3f)", R2))
abline(0, 1, col = col_terciario, lwd = 2, lty = 2)

par(mfrow = c(1, 1))

cat("\n  Gráfico izquierdo: relación ventas-precio (pendiente negativa).")
cat("\n  Gráfico derecho: cuanto más cerca de la diagonal, mejor ajuste.\n")

cat("\n=== FIN Caso Práctico A — Tema 02 ===\n")
