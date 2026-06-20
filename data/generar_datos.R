# =============================================================================
# ECONOMETRÍA — Generador de Datasets
# Genera los 4 datasets del manual: DS_mini, Caso A, Caso B, Caso C
# Autor: Carlos de Anta Puig
# =============================================================================
# EJECUTAR UNA SOLA VEZ para crear los ficheros en data/
# =============================================================================

library(writexl)
set.seed(2024)

cat("Generando datasets del manual de Econometría...\n\n")

# ── DS_MINI — Ventas/precios (~10 obs, tipo Cola Cao) ────────────────────────
cat("  DS_mini: Ventas y precios (10 obs, cálculo manual)...\n")

ds_mini <- data.frame(
  obs              = 1:10,
  ventas           = c(10.0, 11.0, 12.0, 12.2, 12.5, 13.0, 13.4, 14.0, 14.2, 14.5),
  precio_propio    = c(0.92, 1.10, 1.20, 1.22, 1.23, 1.24, 1.28, 1.30, 1.35, 1.38),
  precio_sustit    = c(1.00, 0.93, 0.95, 0.97, 0.98, 0.99, 1.00, 1.10, 1.20, 1.25),
  gasto_publicidad = c(0.90, 0.80, 0.79, 0.80, 0.75, 0.85, 0.90, 0.95, 1.00, 1.05)
)

write_xlsx(ds_mini, "data/DS_mini.xlsx")
write.csv(ds_mini, "data/DS_mini.csv", row.names = FALSE)

cat("    OK — 10 obs, 5 variables\n")

# ── CASO A — Demanda de un producto de consumo (~35 obs, corte transversal) ──
cat("  Caso A: Demanda de producto de consumo (35 obs)...\n")

n_a <- 35
precio_a      <- round(runif(n_a, 1.80, 3.50), 2)
precio_sust_a <- round(runif(n_a, 1.50, 3.00), 2)
publicidad_a  <- round(runif(n_a, 0.50, 2.00), 2)
promocion_a   <- rbinom(n_a, 1, 0.3)
temporada_a   <- rbinom(n_a, 1, 0.25)

# Ventas: relación negativa con precio propio, positiva con sustitutivo y publicidad
ventas_a <- round(
  8.5
  - 2.1 * precio_a
  + 1.3 * precio_sust_a
  + 3.0 * publicidad_a
  + 1.5 * promocion_a
  + 0.8 * temporada_a
  + rnorm(n_a, 0, 0.6),
  2
)

caso_a <- data.frame(
  obs              = 1:n_a,
  ventas           = ventas_a,
  precio_propio    = precio_a,
  precio_sustit    = precio_sust_a,
  gasto_publicidad = publicidad_a,
  promocion        = promocion_a,
  temporada        = temporada_a
)

write_xlsx(caso_a, "data/caso_a_demanda.xlsx")
write.csv(caso_a, "data/caso_a_demanda.csv", row.names = FALSE)

cat(sprintf("    OK — %d obs, %d variables\n", nrow(caso_a), ncol(caso_a)))

# ── CASO B — Cifra de negocio y empleo sectorial (~40 obs, corte transversal) ─
cat("  Caso B: Cifra de negocio sectorial (40 obs)...\n")

n_b <- 40
trabajadores_b <- round(runif(n_b, 5, 200))
inversion_b    <- round(0.02 * trabajadores_b + runif(n_b, 0.1, 1.5), 2)
superficie_b   <- round(100 * trabajadores_b^0.6 + rnorm(n_b, 0, 80), 0)
superficie_b   <- pmax(superficie_b, 50)
pac_b          <- rbinom(n_b, 1, 0.4)
region_b       <- rbinom(n_b, 1, 0.5)  # 1 = norte, 0 = sur

# Cifra de negocio: relación con trabajadores, inversión; heterocedasticidad natural
cifra_b <- round(
  0.15
  + 0.025 * trabajadores_b
  + 1.8 * inversion_b
  + 0.001 * superficie_b
  + 0.3 * pac_b
  - 0.15 * region_b
  + rnorm(n_b, 0, 0.08 * trabajadores_b),  # heterocedasticidad: varianza crece con tamaño
  2
)
cifra_b <- pmax(cifra_b, 0.1)

caso_b <- data.frame(
  obs           = 1:n_b,
  cifra_negocio = cifra_b,
  trabajadores  = trabajadores_b,
  inversion     = inversion_b,
  superficie    = superficie_b,
  pac           = pac_b,
  region        = region_b
)

write_xlsx(caso_b, "data/caso_b_cifra_negocio.xlsx")
write.csv(caso_b, "data/caso_b_cifra_negocio.csv", row.names = FALSE)

cat(sprintf("    OK — %d obs, %d variables\n", nrow(caso_b), ncol(caso_b)))

# ── CASO C — Consumo agregado trimestral de España (~40 obs, serie temporal) ──
cat("  Caso C: Consumo agregado trimestral (40 obs, serie temporal)...\n")

n_c <- 40
trimestres <- seq(as.Date("2014-01-01"), by = "quarter", length.out = n_c)
trimestre_label <- paste0(format(trimestres, "%Y"), "-Q", quarters(trimestres) |>
                            gsub("Q", "", x = _))

# PIB con tendencia y ciclo
pib_c <- round(250000 + 2500 * (1:n_c) + 5000 * sin(2 * pi * (1:n_c) / 8) +
                 rnorm(n_c, 0, 3000), 0)

tipo_interes_c <- round(pmax(0.1, 2.5 - 0.04 * (1:n_c) + 0.5 * sin(2 * pi * (1:n_c) / 16) +
                                rnorm(n_c, 0, 0.3)), 2)

inflacion_c <- round(1.5 + 0.3 * sin(2 * pi * (1:n_c) / 12) + rnorm(n_c, 0, 0.4), 2)

# Crisis: trimestres que caen entre 2015-Q1 y 2016-Q4 (simulando un shock)
crisis_c <- as.integer(trimestres >= as.Date("2015-01-01") & trimestres <= as.Date("2016-12-31"))

# Consumo: función keynesiana con autocorrelación en el error
error_c <- numeric(n_c)
error_c[1] <- rnorm(1, 0, 2000)
for (i in 2:n_c) {
  error_c[i] <- 0.6 * error_c[i - 1] + rnorm(1, 0, 2000)  # AR(1) con rho = 0.6
}

consumo_c <- round(
  3000
  + 0.72 * pib_c
  - 800 * tipo_interes_c
  - 500 * inflacion_c
  - 4000 * crisis_c
  + error_c,
  0
)

caso_c <- data.frame(
  trimestre    = trimestre_label,
  consumo      = consumo_c,
  pib          = pib_c,
  tipo_interes = tipo_interes_c,
  inflacion    = inflacion_c,
  crisis       = crisis_c
)

write_xlsx(caso_c, "data/caso_c_consumo_trimestral.xlsx")
write.csv(caso_c, "data/caso_c_consumo_trimestral.csv", row.names = FALSE)

cat(sprintf("    OK — %d obs, %d variables\n", nrow(caso_c), ncol(caso_c)))

# ── RESUMEN ──────────────────────────────────────────────────────────────────
cat("\n=== 4 datasets generados en data/ ===\n")
cat("  DS_mini.xlsx             — 10 obs (cálculo manual Excel)\n")
cat("  caso_a_demanda.xlsx      — 35 obs (corte transversal, demanda)\n")
cat("  caso_b_cifra_negocio.xlsx — 40 obs (corte transversal, empresas)\n")
cat("  caso_c_consumo_trimestral.xlsx — 40 obs (serie temporal, macro)\n")
