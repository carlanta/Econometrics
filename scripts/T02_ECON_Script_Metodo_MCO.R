# =============================================================================
# ECONOMETRÍA — Capítulo 2
# Script Teórico: Cuantificación de la relación entre variables — Método MCO
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

DS_mini <- read_excel("data/DS_mini.xlsx")

cat("
=============================================================================
  ECONOMETRÍA — Tema 2: El Método de Mínimos Cuadrados Ordinarios (MCO)
=============================================================================

  En este script aprenderemos a:

    1. Calcular el estimador MCO paso a paso con matrices
    2. Verificar el cálculo con la función lm()
    3. Interpretar los coeficientes estimados
    4. Calcular la bondad de ajuste (R²)

  Usaremos el dataset DS_mini (10 observaciones) para que puedas
  replicar los cálculos a mano o en Excel.

=============================================================================
")

pausa()

# ── 1. LOS DATOS ────────────────────────────────────────────────────────────
cat("\n--- 1. EL DATASET DS_MINI ---\n\n")

print(DS_mini)

cat(sprintf("\n  %d observaciones de un mercado de productos de consumo.", nrow(DS_mini)))
cat("\n  Variables: ventas (Y), precio_propio, precio_sustit, gasto_publicidad\n")
cat("\n  Queremos estimar: Ventas = b0 + b1*Precio_propio + u\n")

pausa()

# ── 2. DEFINIR LAS MATRICES ────────────────────────────────────────────────
cat("\n--- 2. DEFINIR LAS MATRICES ---\n")

cat("\n  En notación matricial: Y = X*beta + u\n")
cat("\n  Necesitamos definir Y (nx1) y X (nx2, con columna de 1s):\n\n")

Y <- as.matrix(DS_mini$ventas)
X <- cbind(1, DS_mini$precio_propio)
colnames(X) <- c("const", "precio")

cat("  Vector Y (variable dependiente):\n")
print(round(Y, 2))

cat("\n  Matriz X (constante + precio propio):\n")
print(round(X, 2))

cat(sprintf("\n  Dimensiones: Y es %dx%d, X es %dx%d\n",
            nrow(Y), ncol(Y), nrow(X), ncol(X)))

pausa()

# ── 3. CÁLCULO MATRICIAL PASO A PASO ───────────────────────────────────────
cat("\n--- 3. CÁLCULO MCO PASO A PASO ---\n")

cat("\n  Fórmula: beta_hat = (X'X)^(-1) * X'Y\n")

# Paso 3a: X'X
cat("\n  PASO 1: Calcular X'X (traspuesta de X por X)\n")
XtX <- t(X) %*% X
cat("  X'X =\n"); print(round(XtX, 4))

pausa()

# Paso 3b: X'Y
cat("\n  PASO 2: Calcular X'Y (traspuesta de X por Y)\n")
XtY <- t(X) %*% Y
cat("  X'Y =\n"); print(round(XtY, 4))

pausa()

# Paso 3c: Inversa de X'X
cat("\n  PASO 3: Invertir X'X\n")
cat("  En R: solve(X'X)\n")
XtX_inv <- solve(XtX)
cat("  (X'X)^(-1) =\n"); print(round(XtX_inv, 6))

pausa()

# Paso 3d: beta_hat
cat("\n  PASO 4: Multiplicar (X'X)^(-1) * X'Y para obtener beta_hat\n\n")
beta_hat <- XtX_inv %*% XtY

cat(sprintf("  ╔══════════════════════════════════════════╗\n"))
cat(sprintf("  ║  ESTIMACIONES MCO                        ║\n"))
cat(sprintf("  ║  b0 (intercepto)     = %8.4f           ║\n", beta_hat[1]))
cat(sprintf("  ║  b1 (precio propio)  = %8.4f           ║\n", beta_hat[2]))
cat(sprintf("  ╚══════════════════════════════════════════╝\n"))

cat(sprintf("\n  Recta estimada: Ventas = %.4f + (%.4f) * Precio\n",
            beta_hat[1], beta_hat[2]))

pausa()

# ── 4. VERIFICACIÓN CON lm() ──────────────────────────────────────────────
cat("\n--- 4. VERIFICACIÓN CON lm() ---\n")

cat("\n  Ahora usamos la función lm() de R, que hace lo mismo internamente:\n\n")

modelo <- lm(ventas ~ precio_propio, data = DS_mini)
print(summary(modelo))

cat("\n  Comparación de coeficientes:\n")
cat(sprintf("  Cálculo matricial:  b0 = %.4f, b1 = %.4f\n", beta_hat[1], beta_hat[2]))
cat(sprintf("  Función lm():       b0 = %.4f, b1 = %.4f\n",
            coef(modelo)[1], coef(modelo)[2]))
cat("\n  ¡Son idénticos! R hace internamente el mismo cálculo matricial.\n")

pausa()

# ── 5. VALORES AJUSTADOS Y RESIDUOS ───────────────────────────────────────
cat("\n--- 5. VALORES AJUSTADOS Y RESIDUOS ---\n")

Y_hat <- X %*% beta_hat     # Valores ajustados (Y_hat = X * beta)
u_hat <- Y - Y_hat          # Residuos (u_hat = Y - Y_hat)

cat("\n  Obs  Y_obs    Y_ajust  Residuo\n")
cat("  ---  ------   ------   -------\n")
for (i in 1:nrow(Y)) {
  cat(sprintf("  %3d  %6.2f   %6.2f   %7.4f\n", i, Y[i], Y_hat[i], u_hat[i]))
}

cat(sprintf("\n  Suma de residuos:  %.8f  (= 0, propiedad MCO)\n", sum(u_hat)))
cat(sprintf("  Media de residuos: %.8f  (= 0)\n", mean(u_hat)))

pausa()

# ── 6. BONDAD DE AJUSTE: R² ──────────────────────────────────────────────
cat("\n--- 6. BONDAD DE AJUSTE: R² ---\n")

Y_bar <- mean(Y)

SCT <- sum((Y - Y_bar)^2)
SCE <- sum((Y_hat - Y_bar)^2)
SCR <- sum(u_hat^2)

R2 <- SCE / SCT

cat("\n  Descomposición: SCT = SCE + SCR\n\n")
cat(sprintf("  SCT (Total)     = %8.4f  (variabilidad total de Y)\n", SCT))
cat(sprintf("  SCE (Explicada) = %8.4f  (lo que explica el modelo)\n", SCE))
cat(sprintf("  SCR (Residual)  = %8.4f  (lo que NO explica el modelo)\n", SCR))
cat(sprintf("  SCE + SCR       = %8.4f  (= SCT ✓)\n\n", SCE + SCR))

cat(sprintf("  R² = SCE / SCT  = %.4f / %.4f = %.4f\n", SCE, SCT, R2))
cat(sprintf("\n  El precio propio explica el %.1f%% de la variabilidad de las ventas.\n",
            R2 * 100))

pausa()

# ── 7. VARIANZA DEL ERROR ────────────────────────────────────────────────
cat("\n--- 7. ESTIMACIÓN DE LA VARIANZA DEL ERROR ---\n")

n <- nrow(DS_mini)
k <- 1   # una variable explicativa
gl <- n - k - 1

sigma2 <- SCR / gl
sigma  <- sqrt(sigma2)

cat(sprintf("\n  Grados de libertad: n - k - 1 = %d - %d - 1 = %d\n", n, k, gl))
cat(sprintf("  Varianza estimada:  s² = SCR / gl = %.4f / %d = %.4f\n", SCR, gl, sigma2))
cat(sprintf("  Error estándar:     s  = sqrt(s²) = %.4f\n", sigma))
cat(sprintf("\n  Coincide con 'Residual standard error' de summary(lm(...)).\n"))

pausa()

# ── 8. GRÁFICO FINAL ────────────────────────────────────────────────────
cat("\n--- 8. GRÁFICO: DATOS, RECTA ESTIMADA Y RESIDUOS ---\n")

plot(DS_mini$precio_propio, DS_mini$ventas,
     pch = 16, col = col_principal, cex = 1.2,
     xlab = "Precio propio (EUR)",
     ylab = "Ventas (MM EUR)",
     main = sprintf("MCO: Ventas = %.2f + (%.2f) x Precio   R² = %.3f",
                    beta_hat[1], beta_hat[2], R2))

abline(modelo, col = col_secundario, lwd = 2)

# Dibujar residuos
segments(x0 = DS_mini$precio_propio, y0 = fitted(modelo),
         x1 = DS_mini$precio_propio, y1 = DS_mini$ventas,
         col = col_gris, lty = 2)

legend("topright",
       legend = c("Observado", "Recta MCO", "Residuos"),
       col = c(col_principal, col_secundario, col_gris),
       pch = c(16, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2, 1),
       bty = "n", cex = 0.85)

cat("\n  Los segmentos verticales discontinuos son los RESIDUOS.")
cat("\n  MCO minimiza la suma de los cuadrados de esos segmentos.\n")

cat("\n=== FIN Script Tema 02 ===\n")
