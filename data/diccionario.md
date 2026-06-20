# Diccionario de Datos

## DS_mini — Mini-dataset para cálculo manual
- **ventas**: Ventas del producto (MM€)
- **precio_propio**: Precio del producto (€)
- **precio_sustitutivo**: Precio del bien sustitutivo (€)
- **gasto_publicidad**: Gasto en publicidad (MM€)

## Caso A — Demanda de un producto de consumo (~35 obs, corte transversal)
- **ventas**: Ventas mensuales (MM€)
- **precio_propio**: Precio del producto (€)
- **precio_sustitutivo**: Precio del bien sustitutivo (€)
- **gasto_publicidad**: Gasto en publicidad (MM€)
- **promocion**: Campaña promocional activa (1 = sí, 0 = no)
- **temporada**: Temporada alta (1 = sí, 0 = no)

## Caso B — Cifra de negocio y empleo sectorial (~40 obs, corte transversal)
- **cifra_negocio**: Importe neto de cifra de negocio (M€)
- **trabajadores**: Número de trabajadores
- **inversion**: Inversión anual (M€)
- **superficie**: Superficie de explotación (m²)
- **pac**: Acogida a la PAC (1 = sí, 0 = no)
- **region**: Región (1 = norte, 0 = sur)

## Caso C — Consumo agregado trimestral de España (~40 obs, serie temporal)
- **trimestre**: Fecha del trimestre (YYYY-QX)
- **consumo**: Consumo privado (MM€, precios constantes)
- **pib**: PIB (MM€, precios constantes)
- **tipo_interes**: Tipo de interés nominal (%)
- **inflacion**: Tasa de inflación interanual (%)
- **crisis**: Período de crisis económica (1 = 2008Q1-2012Q4, 0 = resto)
