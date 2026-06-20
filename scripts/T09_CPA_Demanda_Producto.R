# ECONOMETRÍA — Cap 9, CPA: Demanda — Contraejemplo homocedasticidad
# Abre primero mEconometria.Rproj en RStudio.
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl); library(lmtest)
caso_a <- read_excel("data/caso_a_demanda.xlsx")
cat("\n=== CASO PRÁCTICO A — Tema 9: Heterocedasticidad (contraejemplo) ===\n")
pausa()
mod <- lm(ventas ~ precio_propio + gasto_publicidad, data = caso_a)
bp <- bptest(mod)
cat(sprintf("\n  BP: estadístico = %.3f, p = %.4f\n", bp$statistic, bp$p.value))
cat(sprintf("  Decisión: %s\n", ifelse(bp$p.value < 0.05, "Heterocedasticidad", "NO hay heterocedasticidad")))
cat("\n  En este caso, los errores estándar convencionales son VÁLIDOS.\n")
cat("\n=== FIN Caso Práctico A — Tema 09 ===\n")
