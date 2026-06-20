# =============================================================================
# ECONOMETRÍA — Capítulo 1, Caso Práctico A
# CPA: Demanda de un producto de consumo — Primer contacto
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
  Tema 1: Primer contacto con los datos
=============================================================================
")

# ── 1. CARGAR Y EXPLORAR LOS DATOS ──────────────────────────────────────────
cat("\n--- 1. CARGAR Y EXPLORAR LOS DATOS ---\n")

cat(sprintf("\n  Dataset cargado: %d observaciones, %d variables\n",
            nrow(caso_a), ncol(caso_a)))

cat("\n  Primeras observaciones:\n")
print(head(caso_a, 8))

cat("\n  Estructura del dataset:\n")
str(caso_a)

pausa()

# ── 2. ESTADÍSTICAS DESCRIPTIVAS ─────────────────────────────────────────────
cat("\n--- 2. ESTADÍSTICAS DESCRIPTIVAS ---\n")

cat("\n  Resumen de todas las variables:\n")
print(summary(caso_a[, c("ventas", "precio_propio", "precio_sustit",
                          "gasto_publicidad", "promocion", "temporada")]))

cat(sprintf("\n  Media de ventas:          %.2f MM EUR\n", mean(caso_a$ventas)))
cat(sprintf("  Desv. típica de ventas:   %.2f MM EUR\n", sd(caso_a$ventas)))
cat(sprintf("  Precio medio propio:      %.2f EUR\n", mean(caso_a$precio_propio)))
cat(sprintf("  Precio medio sustitutivo: %.2f EUR\n", mean(caso_a$precio_sustit)))
cat(sprintf("  Obs. con promoción:       %d de %d (%.0f%%)\n",
            sum(caso_a$promocion), nrow(caso_a),
            100 * mean(caso_a$promocion)))

pausa()

# ── 3. VISUALIZACIÓN ─────────────────────────────────────────────────────────
cat("\n--- 3. VISUALIZACIÓN ---\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(caso_a$precio_propio, caso_a$ventas,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Precio propio (EUR)", ylab = "Ventas (MM EUR)",
     main = "Ventas vs Precio propio")
abline(lm(ventas ~ precio_propio, data = caso_a),
       col = col_secundario, lwd = 2)

plot(caso_a$gasto_publicidad, caso_a$ventas,
     pch = 16, col = col_principal, cex = 0.9,
     xlab = "Gasto publicidad (MM EUR)", ylab = "Ventas (MM EUR)",
     main = "Ventas vs Publicidad")
abline(lm(ventas ~ gasto_publicidad, data = caso_a),
       col = col_secundario, lwd = 2)

par(mfrow = c(1, 1))

cat("\n  El gráfico muestra:")
cat("\n  - Relación NEGATIVA entre ventas y precio propio (lógico)")
cat("\n  - Relación POSITIVA entre ventas y gasto en publicidad\n")

pausa()

# ── 4. UN PRIMER MODELO (ANTICIPO DEL TEMA 2) ───────────────────────────────
cat("\n--- 4. UN PRIMER MODELO (ANTICIPO) ---\n")

cat("\n  Estimamos: Ventas = b0 + b1*Precio + b2*Publicidad + u\n\n")

modelo_a <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
print(summary(modelo_a))

coefs <- coef(modelo_a)
cat(sprintf("\n  b0 (constante)      = %8.4f\n", coefs[1]))
cat(sprintf("  b1 (precio propio)  = %8.4f  -> Por cada EUR de subida de precio,\n", coefs[2]))
cat(sprintf("                                    las ventas caen en %.2f MM EUR\n", abs(coefs[2])))
cat(sprintf("  b2 (publicidad)     = %8.4f  -> Por cada MM EUR de publicidad,\n", coefs[3]))
cat(sprintf("                                    las ventas suben en %.2f MM EUR\n", coefs[3]))

cat("\n  Este es solo un primer vistazo. En los siguientes temas")
cat("\n  aprenderemos a interpretar estos resultados con rigor.\n")

cat("\n=== FIN Caso Práctico A — Tema 01 ===\n")
