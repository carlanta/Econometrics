# ECONOMETRÍA — Capítulo 10 Script Teórico: Autocorrelación
# Autor: Carlos de Anta Puig
# Abre primero mEconometria.Rproj en RStudio.
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl); library(lmtest)
caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")
cat("\n=== ECONOMETRÍA — Tema 10: Autocorrelación serial ===\n")
pausa()
mod <- lm(consumo ~ pib + tipo_interes, data = caso_c)
cat("\n--- 1. RESIDUOS SECUENCIALES ---\n")
par(mfrow = c(1,2), mar = c(4,4,3,1))
res <- residuals(mod)
plot(1:length(res), res, type="b", pch=16, col="gray30", xlab="Trimestre", ylab="Residuos", main="Residuos en el tiempo")
abline(h=0, lty=2)
plot(res[-length(res)], res[-1], pch=16, col="gray30", xlab="Residuo(t)", ylab="Residuo(t+1)", main="Residuo t vs t+1")
abline(h=0, lty=2); abline(v=0, lty=2)
par(mfrow = c(1,1))
pausa()
cat("\n--- 2. CONTRASTE DURBIN-WATSON ---\n\n")
dw <- dwtest(mod)
cat(sprintf("  DW = %.4f, p-valor = %.4f\n", dw$statistic, dw$p.value))
cat(sprintf("  rho aprox = %.4f\n", 1 - dw$statistic/2))
cat(sprintf("  Decisión: %s\n", ifelse(dw$p.value < 0.05, "AUTOCORRELACIÓN POSITIVA", "No hay evidencia")))
cat("\n=== FIN Script Tema 10 ===\n")
