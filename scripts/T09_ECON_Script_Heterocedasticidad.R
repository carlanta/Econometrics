# ECONOMETRÍA — Capítulo 9 Script Teórico: Heterocedasticidad
# Autor: Carlos de Anta Puig
# Abre primero mEconometria.Rproj en RStudio.
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl); library(lmtest); library(sandwich)
caso_b <- read_excel("data/caso_b_cifra_negocio.xlsx")
cat("\n=== ECONOMETRÍA — Tema 9: Heterocedasticidad ===\n")
pausa()
mod <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
cat("\n--- 1. DETECCIÓN GRÁFICA ---\n")
par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(fitted(mod), residuals(mod)^2, pch=16, col="gray30", xlab="Ajustados", ylab="Residuos²", main="Residuos² vs Ajustados")
plot(caso_b$trabajadores, residuals(mod)^2, pch=16, col="gray30", xlab="Trabajadores", ylab="Residuos²", main="Residuos² vs Trabajadores")
par(mfrow = c(1,1))
pausa()
cat("\n--- 2. CONTRASTE BREUSCH-PAGAN ---\n\n")
bp <- bptest(mod, studentize = FALSE)   # Breusch-Pagan clásico, el mismo valor que Gretl
cat(sprintf("  Estadístico BP = %.3f, p-valor = %.4f\n", bp$statistic, bp$p.value))
cat(sprintf("  Decisión: %s\n", ifelse(bp$p.value < 0.05, "HETEROCEDASTICIDAD", "Homocedasticidad")))
pausa()
cat("\n--- 3. MCO ROBUSTA ---\n\n")
cat("  Errores estándar convencionales:\n"); print(coeftest(mod))
cat("\n  Errores estándar robustos (HC1):\n"); print(coeftest(mod, vcov = vcovHC(mod, type = "HC1")))
cat("\n=== FIN Script Tema 09 ===\n")
