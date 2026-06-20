# ECONOMETRÍA — Cap 9, CPC: Consumo — Comprobar heterocedasticidad
# Abre primero mEconometria.Rproj en RStudio.
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl); library(lmtest); library(sandwich)
caso_c <- read_excel("data/caso_c_consumo_trimestral.xlsx")
cat("\n=== CASO PRÁCTICO C — Tema 9: Comprobar heterocedasticidad ===\n")
pausa()
mod <- lm(consumo ~ pib + tipo_interes, data = caso_c)
bp <- bptest(mod)
cat(sprintf("\n  BP: p = %.4f → %s\n", bp$p.value, ifelse(bp$p.value < 0.05, "HETEROCEDASTICIDAD", "Homocedasticidad")))
cat("\n=== FIN Caso Práctico C — Tema 09 ===\n")
