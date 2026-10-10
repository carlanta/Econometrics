# ══════════════════════════════════════════════════════════════════
# mEconometria · Utilidades comunes de las presentaciones
#   tr(es, en), num(), pct(), tema gráfico CdeA y los datos de los tres casos del manual (data/).
#   Los modelos se estiman aquí con lm() sobre los mismos ficheros que usa el manual, así que las cifras de las
#   diapositivas coinciden con las del libro.
# ══════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(ggplot2)
})

if (!exists("idioma")) idioma <- "es"
tr <- function(es, en) if (idioma == "en") en else es

# Números con la convención de cada idioma (espacio duro antes de % y €)
num <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = tr(".", ","), decimal.mark = tr(",", "."))
pct <- function(x, d = 1) paste0(num(100 * x, d), " %")
eur <- function(x, d = 0) paste0(num(x, d), " €")
# Número para meter dentro de una ecuación LaTeX (M() o formula_caja): la coma va protegida como {,}
numtex <- function(x, d = 2) gsub(",", "{,}", num(x, d), fixed = TRUE)
pval <- function(p) if (p < 0.001) tr("< 0,001", "< 0.001") else num(p, 3)

# Tema ggplot con la identidad CdeA (para diapositivas)
tema_cdea <- function(base = 14) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#E2E6EB"),
          axis.title = element_text(colour = "#5B6470"),
          axis.text = element_text(colour = "#1B2430"),
          strip.text = element_text(face = "bold", colour = "#052A64", size = base),
          legend.position = "bottom",
          plot.margin = margin(8, 12, 8, 8))
}
COL <- list(navy = "#052A64", navy2 = "#113F86", oro = "#DBB246", oro_cl = "#ECD38E", gris = "#8A939E", linea = "#E2E6EB")

# ── Datos de los casos del manual ──────────────────────────────────
caso_a <- read.csv("data/caso_a_demanda.csv")
caso_b <- read.csv("data/caso_b_cifra_negocio.csv")
caso_c <- read.csv("data/caso_c_consumo_trimestral.csv")
ds_mini <- read.csv("data/DS_mini.csv")

# Modelos de referencia de cada caso (los de los capítulos 2–5 del manual)
mod_a <- lm(ventas ~ precio_propio + precio_sustit + gasto_publicidad, data = caso_a)
mod_b <- lm(cifra_negocio ~ trabajadores + inversion + superficie, data = caso_b)
mod_c <- lm(consumo ~ pib + tipo_interes + inflacion, data = caso_c)
mod_mini <- lm(ventas ~ precio_propio + precio_sustit + gasto_publicidad, data = ds_mini)

coef_txt <- function(m, v, d = 4) num(coef(m)[[v]], d)
