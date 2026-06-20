# =============================================================================
# ECONOMETRÍA — Capítulo 1, Caso Práctico B
# CPB: Cifra de negocio y empleo sectorial — Primer contacto
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
  Tema 1: Primer contacto con los datos
=============================================================================
")

# ── 1. CARGAR Y EXPLORAR LOS DATOS ──────────────────────────────────────────
cat("\n--- 1. CARGAR Y EXPLORAR LOS DATOS ---\n")

cat(sprintf("\n  Dataset cargado: %d empresas del sector agroalimentario\n", nrow(caso_b)))
cat("\n  Primeras observaciones:\n")
print(head(caso_b, 8))

cat("\n  Estructura del dataset:\n")
str(caso_b)

pausa()

# ── 2. ESTADÍSTICAS DESCRIPTIVAS ─────────────────────────────────────────────
cat("\n--- 2. ESTADÍSTICAS DESCRIPTIVAS ---\n")

cat("\n  Resumen de las variables cuantitativas:\n")
print(summary(caso_b[, c("cifra_negocio", "trabajadores", "inversion", "superficie")]))

cat(sprintf("\n  Cifra de negocio media: %.2f M EUR\n", mean(caso_b$cifra_negocio)))
cat(sprintf("  Trabajadores medio:    %.0f\n", mean(caso_b$trabajadores)))
cat(sprintf("  Empresas con PAC:      %d de %d (%.0f%%)\n",
            sum(caso_b$pac), nrow(caso_b), 100 * mean(caso_b$pac)))
cat(sprintf("  Empresas del norte:    %d de %d (%.0f%%)\n",
            sum(caso_b$region), nrow(caso_b), 100 * mean(caso_b$region)))

pausa()

# ── 3. VISUALIZACIÓN ─────────────────────────────────────────────────────────
cat("\n--- 3. VISUALIZACIÓN ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(caso_b$trabajadores, caso_b$cifra_negocio,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Trabajadores", ylab = "Cifra de negocio (M EUR)",
     main = "Cifra negocio vs Trabajadores")
abline(lm(cifra_negocio ~ trabajadores, data = caso_b),
       col = col_secundario, lwd = 2)

plot(caso_b$inversion, caso_b$cifra_negocio,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Inversión (M EUR)", ylab = "Cifra de negocio (M EUR)",
     main = "Cifra negocio vs Inversión")
abline(lm(cifra_negocio ~ inversion, data = caso_b),
       col = col_secundario, lwd = 2)

par(mfrow = c(1, 1))

cat("\n  OBSERVACIÓN IMPORTANTE:")
cat("\n  Fíjate en el gráfico izquierdo: la DISPERSIÓN de los puntos")
cat("\n  AUMENTA con el número de trabajadores. Las empresas grandes")
cat("\n  tienen más variabilidad que las pequeñas.")
cat("\n  Esto es HETEROCEDASTICIDAD — lo estudiaremos en el Tema 9.\n")

pausa()

# ── 4. UN PRIMER MODELO (ANTICIPO) ──────────────────────────────────────────
cat("\n--- 4. UN PRIMER MODELO (ANTICIPO) ---\n")

cat("\n  Estimamos: Cifra = b0 + b1*Trabajadores + b2*Inversión + u\n\n")

modelo_b <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
print(summary(modelo_b))

coefs <- coef(modelo_b)
cat(sprintf("\n  b0 (constante)      = %8.4f\n", coefs[1]))
cat(sprintf("  b1 (trabajadores)   = %8.4f  -> Por cada trabajador adicional,\n", coefs[2]))
cat(sprintf("                                    la cifra de negocio sube en %.4f M EUR\n", coefs[2]))
cat(sprintf("  b2 (inversión)      = %8.4f  -> Por cada M EUR de inversión,\n", coefs[3]))
cat(sprintf("                                    la cifra de negocio sube en %.2f M EUR\n", coefs[3]))

cat("\n=== FIN Caso Práctico B — Tema 01 ===\n")
