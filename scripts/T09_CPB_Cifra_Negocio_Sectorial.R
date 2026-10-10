# ECONOMETRÍA — Cap 9, CPB: Cifra negocio — Heterocedasticidad natural
# Abre primero mEconometria.Rproj en RStudio.
pausa <- function(msg = "\n  [Pulsa ENTER para continuar...]") { cat(msg); invisible(readline()) }
library(tidyverse); library(readxl); library(lmtest); library(sandwich)
caso_b <- read_excel("data/caso_b_cifra_negocio.xlsx")
cat("\n=== CASO PRÁCTICO B — Tema 9: Heterocedasticidad natural ===\n")
pausa()
mod <- lm(cifra_negocio ~ trabajadores + inversion, data = caso_b)
bp <- bptest(mod, studentize = FALSE)   # Breusch-Pagan clásico, el mismo valor que Gretl
cat(sprintf("\n  BP: p = %.4f → %s\n", bp$p.value, ifelse(bp$p.value < 0.05, "HETEROCEDASTICIDAD", "Homocedasticidad")))
cat("\n  Comparación estándar vs robusta:\n\n")
s <- summary(mod)$coefficients
r <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
cat("  Variable       SE estándar  SE robusto\n")
cat("  -------------  -----------  ----------\n")
for (i in 1:3) cat(sprintf("  %-13s  %11.4f  %10.4f\n", rownames(s)[i], s[i,2], r[i,2]))
cat("\n=== FIN Caso Práctico B — Tema 09 ===\n")
