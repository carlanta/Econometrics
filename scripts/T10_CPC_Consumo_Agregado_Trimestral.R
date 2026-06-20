# ECONOMETRÍA — Cap 10, CPC: Consumo — Autocorrelación DW
# Abre primero mEconometria.Rproj en RStudio.
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl); library(lmtest)
caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")
cat("\n=== CASO PRÁCTICO C — Tema 10: Autocorrelación ===\n")
pausa()
mod1 <- lm(consumo ~ pib, data = caso_c)
mod2 <- lm(consumo ~ pib + tipo_interes, data = caso_c)
dw1 <- dwtest(mod1); dw2 <- dwtest(mod2)
cat("\n  Modelo simple:         DW =", sprintf("%.4f", dw1$statistic), " p =", sprintf("%.4f", dw1$p.value))
cat("\n  Modelo con tipo int.:  DW =", sprintf("%.4f", dw2$statistic), " p =", sprintf("%.4f", dw2$p.value))
cat("\n\n  Añadir tipo interés puede reducir la autocorrelación espuria.\n")
cat("\n=== FIN Caso Práctico C — Tema 10 ===\n")
