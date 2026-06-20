# =============================================================================
# ECONOMETRÍA — Capítulo 2, Caso Práctico B
# CPB: Cifra de negocio y empleo sectorial — Estimación MCO
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

caso_b <- read_excel("data/caso_b_cifra_negocio.xlsx")

cat("
=============================================================================
  CASO PRÁCTICO B — Cifra de negocio y empleo sectorial
  Tema 2: Estimación MCO
=============================================================================

  Objetivo: estimar un modelo que explique la cifra de negocio
  de empresas agroalimentarias en función de su tamaño.

  Modelo: Cifra_negocio = b0 + b1*Trabajadores + b2*Inversión + u

=============================================================================
")

pausa()

# ── 1. RECORDATORIO DE LOS DATOS ───────────────────────────────────────────
cat("\n--- 1. RECORDATORIO DE LOS DATOS ---\n")

cat(sprintf("\n  Dataset: %d empresas del sector agroalimentario\n", nrow(caso_b)))
cat("\n  Resumen de las variables del modelo:\n")
print(summary(caso_b[, c("cifra_negocio", "trabajadores", "inversion")]))

# Correlaciones
cat("\n  Correlaciones con cifra de negocio:\n")
vars_num <- c("trabajadores", "inversion", "superficie")
cors <- cor(caso_b$cifra_negocio, caso_b[, vars_num])
for (j in 1:length(vars_num)) {
  cat(sprintf("    %s: %.4f\n", vars_num[j], cors[j]))
}

pausa()

# ── 2. ESTIMACIÓN MCO ─────────────────────────────────────────────────────
cat("\n--- 2. ESTIMACIÓN MCO ---\n")

cat("\n  Modelo: Cifra = b0 + b1*Trabajadores + b2*Inversión + u\n\n")

modelo_b <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
print(summary(modelo_b))

pausa()

# ── 3. INTERPRETACIÓN DE COEFICIENTES ────────────────────────────────────
cat("\n--- 3. INTERPRETACIÓN DE COEFICIENTES ---\n")

coefs <- coef(modelo_b)

cat(sprintf("\n  b0 = %.4f (intercepto)\n", coefs[1]))
cat(sprintf("     Cifra de negocio base cuando trabajadores = 0 e inversión = 0.\n\n"))

cat(sprintf("  b1 = %.4f (trabajadores)\n", coefs[2]))
cat(sprintf("     Por cada trabajador adicional, la cifra de negocio\n"))
cat(sprintf("     aumenta en %.4f M EUR (ceteris paribus).\n\n", coefs[2]))

cat(sprintf("  b2 = %.4f (inversión)\n", coefs[3]))
cat(sprintf("     Por cada M EUR adicional de inversión, la cifra\n"))
cat(sprintf("     de negocio varía en %.4f M EUR (ceteris paribus).\n", coefs[3]))

# Evaluar significatividad informal
pvals <- summary(modelo_b)$coefficients[, 4]
cat("\n  Significatividad (p-valores):\n")
cat(sprintf("    Trabajadores: p = %.4f %s\n", pvals[2],
            ifelse(pvals[2] < 0.05, "(significativo al 5%)", "(no significativo al 5%)")))
cat(sprintf("    Inversión:    p = %.4f %s\n", pvals[3],
            ifelse(pvals[3] < 0.05, "(significativo al 5%)", "(no significativo al 5%)")))
cat("\n  NOTA: La significatividad formal la estudiaremos en el Tema 4.\n")

pausa()

# ── 4. BONDAD DE AJUSTE ─────────────────────────────────────────────────
cat("\n--- 4. BONDAD DE AJUSTE: R² ---\n")

R2 <- summary(modelo_b)$r.squared

cat(sprintf("\n  R² = %.4f\n", R2))
cat(sprintf("  Los trabajadores y la inversión explican el %.1f%% de la\n", R2 * 100))
cat(sprintf("  variabilidad de la cifra de negocio.\n"))

pausa()

# ── 5. TABLA ANOVA ──────────────────────────────────────────────────────
cat("\n--- 5. TABLA ANOVA ---\n\n")

print(anova(modelo_b))

cat("\n  Observa: casi toda la SCE proviene de 'trabajadores'.\n")
cat("  La variable 'inversión' aporta muy poco poder explicativo adicional.\n")

pausa()

# ── 6. VISUALIZACIÓN ───────────────────────────────────────────────────
cat("\n--- 6. VISUALIZACIÓN ---\n")

Y_hat <- fitted(modelo_b)
Y     <- caso_b$cifra_negocio

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Cifra vs trabajadores
plot(caso_b$trabajadores, caso_b$cifra_negocio,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Trabajadores", ylab = "Cifra de negocio (M EUR)",
     main = "Cifra negocio vs Trabajadores")
abline(lm(cifra_negocio ~ trabajadores, data = caso_b),
       col = col_secundario, lwd = 2)

# Observado vs ajustado
plot(Y_hat, Y,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Cifra ajustada (M EUR)", ylab = "Cifra observada (M EUR)",
     main = sprintf("Observado vs Ajustado (R² = %.3f)", R2))
abline(0, 1, col = col_terciario, lwd = 2, lty = 2)

par(mfrow = c(1, 1))

cat("\n  OBSERVACIÓN IMPORTANTE:")
cat("\n  En el gráfico izquierdo, fíjate en la dispersión de los puntos:")
cat("\n  las empresas con MÁS trabajadores tienen MAYOR variabilidad.")
cat("\n  Esto sugiere HETEROCEDASTICIDAD (Tema 9).\n")

cat("\n=== FIN Caso Práctico B — Tema 02 ===\n")
