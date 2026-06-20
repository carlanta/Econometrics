# Manual de Econometría

## Econometría: Manual Práctico con R

**Autor:** Carlos de Anta Puig
Economista · Perito Financiero
Miembro del Colegio de Economistas de Madrid
Miembro del Instituto Español de Analistas Financieros (IEAF)
Profesor de Econometría y Series Temporales
carlos@cwconsultores.com

---

### Publicación

Este manual está publicado por **Digital Reasons** (www.digitalreasons.es).

---

### Prólogo

Este manual nació de la convicción de que la econometría puede aprenderse de otra manera. Los manuales de referencia — Wooldridge, Greene, Gujarati — son obras imprescindibles, pero están escritos para un lector que ya domina la materia. Los apuntes universitarios, por su parte, suelen ser telegráficos: pasan de la fórmula al resultado sin detenerse a explicar qué significa cada coeficiente ni por qué importa cada supuesto.

La propuesta de este libro es diferente. La teoría se presenta con rigor formal — las derivaciones están aquí, completas —, pero acompañada de una exposición pedagógica que conecta cada concepto con problemas económicos reales. Tres datasets recurrentes acompañan al alumno durante todo el curso: una demanda de producto de consumo, una cifra de negocio empresarial y una función de consumo keynesiana. En cada tema, el alumno aplica la técnica nueva sobre datos que ya conoce, consolidando la intuición antes de avanzar.

Si este libro consigue que algún alumno más disfrute aprendiendo econometría, habrá cumplido su propósito.

*Carlos de Anta Puig — Madrid, 2026*

---

### Índice

**Capítulo 1 — Introducción a la Econometría**

- ¿Qué es la econometría? El econometrista. Origen del término «regresión»
- El método econométrico: los 6 pasos (teoría → modelo → datos → estimación → contrastes → predicción)
- Tipos de datos: corte transversal, series temporales, panel
- Modelo matemático vs modelo econométrico
- Un primer vistazo a la estimación MCO

**Capítulo 2 — Cuantificación de la Relación entre Variables: el Método MCO**

- Especificación del Modelo de Regresión Lineal General (MRLG)
- Notación matricial: Y = Xβ + u
- Supuestos básicos del MRLG (los 6 supuestos de Gauss-Markov)
- Estimación MCO: derivación de β̂ = (X'X)⁻¹X'Y
- Cálculo matricial paso a paso con datos reales
- Estimación con `lm()` — comparativa con GRETL
- Interpretación de coeficientes (efecto marginal, ceteris paribus)
- Efectos del cambio de unidades sobre las estimaciones
- Bondad de ajuste: R², SCE, SCR, SCT

**Capítulo 3 — Propiedades de los Estimadores MCO**

- El problema del R²: ¿más variables es siempre mejor?
- R² ajustado: fórmula, interpretación y comparación con R²
- Propiedades de los estimadores MCO: Teorema de Gauss-Markov (BLUE)
- Insesgadez, eficiencia y consistencia
- Simulación de las propiedades BLUE
- Varianza del término de error y errores estándar

**Capítulo 4 — Contrastes de Hipótesis en el MRLG**

- Conceptos básicos: H₀, H₁, nivel de significación, errores tipo I y II
- Los 3 pasos del contraste
- Contraste t para un coeficiente individual: bilateral, unilateral derecho e izquierdo
- Regla del p-valor
- Significatividad individual: ¿es relevante la variable?

**Capítulo 5 — Intervalos de Confianza en el MRLG**

- Concepto de intervalo de confianza e interpretación correcta
- Relación IC ↔ contrastes de hipótesis (dualidad)
- Intervalo para un coeficiente individual: fórmula y cálculo con `confint()`
- Intervalo para combinación lineal de coeficientes
- Visualización gráfica de intervalos y significatividad

**Capítulo 6 — Problemas de Especificación**

- Inclusión de variables irrelevantes: consecuencias (insesgadez pero mayor varianza), detección
- Omisión de variables relevantes: sesgo, fórmula de la dirección del sesgo, detección
- Pasos sistemáticos para evaluar la especificación
- Variables proxy para variables no observables

**Capítulo 7 — Predicción y Análisis de Residuos**

- Predicción puntual con `predict()`
- Intervalo de confianza para la media vs intervalo de predicción para un individuo
- Análisis de residuos: gráfico residuos vs ajustados, Q-Q plot
- Diagnóstico de supuestos

**Capítulo 8 — Variables Cualitativas en el MRLG**

- Variables dummy: codificación 0/1, categoría de referencia
- Efecto sobre el intercepto: desplazamiento de la recta
- Interpretación y significatividad de coeficientes de dummies
- Trampa de las variables dummy

**Capítulo 9 — Heterocedasticidad y Estimación MCO Robusta**

- Repaso del supuesto de homocedasticidad y consecuencias de su violación
- Detección gráfica: residuos² vs ajustados
- Contraste de Breusch-Pagan: `lmtest::bptest()`
- Estimación MCO robusta: errores estándar de White con `sandwich::vcovHC()`
- Comparación de inferencia estándar vs robusta

**Capítulo 10 — Autocorrelación Serial**

- Concepto de autocorrelación: positiva y negativa
- Consecuencias: MCO insesgado pero no eficiente, errores estándar incorrectos
- Contraste de Durbin-Watson: `lmtest::dwtest()`
- Diagnóstico gráfico: residuos secuenciales, residuo(t) vs residuo(t+1)

---

### Estructura del repositorio

- `data/` — Datasets utilizados en los ejemplos prácticos (formato `.xlsx` y `.csv`, reproducibles con `set.seed` fijo)
- `scripts/` — Scripts R autocontenidos e interactivos por capítulo y caso práctico

---

### Scripts disponibles

Cada script es autocontenido: carga los datos con rutas relativas (abrir primero `mEconometria.Rproj`), presenta outputs con interpretaciones narrativas formateadas, y utiliza paradas interactivas (`readline`) para seguir el análisis paso a paso con Ctrl+Enter.

| Script | Descripción |
|--------|-------------|
| `T01_ECON_Script_Introduccion.R` | **T1** — Introducción a la Econometría |
| `T01_CPA_Demanda_Producto.R` | **T1 CPA** — Primer contacto con los datos de demanda |
| `T01_CPB_Cifra_Negocio_Sectorial.R` | **T1 CPB** — Primer contacto con datos empresariales |
| `T01_CPC_Consumo_Agregado_Trimestral.R` | **T1 CPC** — Primer contacto con serie temporal de consumo |
| `T02_ECON_Script_Metodo_MCO.R` | **T2** — Cálculo matricial MCO paso a paso |
| `T02_CPA_Demanda_Producto.R` | **T2 CPA** — MCO: modelo de demanda |
| `T02_CPB_Cifra_Negocio_Sectorial.R` | **T2 CPB** — MCO: cifra de negocio |
| `T02_CPC_Consumo_Agregado_Trimestral.R` | **T2 CPC** — MCO: función de consumo keynesiana |
| `T03_ECON_Script_Propiedades_MCO.R` | **T3** — R² vs R² ajustado, simulación BLUE |
| `T03_CPA_Demanda_Producto.R` | **T3 CPA** — Modelos progresivos: demanda |
| `T03_CPB_Cifra_Negocio_Sectorial.R` | **T3 CPB** — Modelos progresivos: cifra de negocio |
| `T03_CPC_Consumo_Agregado_Trimestral.R` | **T3 CPC** — Modelos progresivos: consumo |
| `T04_ECON_Script_Contrastes_Hipotesis.R` | **T4** — Contraste t paso a paso |
| `T04_CPA_Demanda_Producto.R` | **T4 CPA** — Significatividad: demanda |
| `T04_CPB_Cifra_Negocio_Sectorial.R` | **T4 CPB** — Significatividad: cifra de negocio |
| `T04_CPC_Consumo_Agregado_Trimestral.R` | **T4 CPC** — Significatividad: PMC keynesiana |
| `T05_ECON_Script_Intervalos_Confianza.R` | **T5** — IC manual y con confint() |
| `T05_CPA_Demanda_Producto.R` | **T5 CPA** — Intervalos: demanda |
| `T05_CPB_Cifra_Negocio_Sectorial.R` | **T5 CPB** — Intervalos: cifra de negocio |
| `T05_CPC_Consumo_Agregado_Trimestral.R` | **T5 CPC** — Intervalos: PMC |
| `T06_ECON_Script_Problemas_Especificacion.R` | **T6** — Sesgo por omisión, variable irrelevante |
| `T06_CPA_Demanda_Producto.R` | **T6 CPA** — Especificación: demanda |
| `T06_CPB_Cifra_Negocio_Sectorial.R` | **T6 CPB** — Especificación: cifra de negocio |
| `T06_CPC_Consumo_Agregado_Trimestral.R` | **T6 CPC** — Especificación: consumo |
| `T07_ECON_Script_Prediccion_Residuos.R` | **T7** — Predicción, IC vs IP, diagnóstico |
| `T07_CPA_Demanda_Producto.R` | **T7 CPA** — Predicción: demanda |
| `T07_CPB_Cifra_Negocio_Sectorial.R` | **T7 CPB** — Predicción: perfiles de empresa |
| `T07_CPC_Consumo_Agregado_Trimestral.R` | **T7 CPC** — Predicción: consumo futuro |
| `T08_ECON_Script_Variables_Cualitativas.R` | **T8** — Dummies: efecto sobre intercepto |
| `T08_CPA_Demanda_Producto.R` | **T8 CPA** — Dummy promoción y temporada |
| `T08_CPB_Cifra_Negocio_Sectorial.R` | **T8 CPB** — Dummies PAC y región |
| `T08_CPC_Consumo_Agregado_Trimestral.R` | **T8 CPC** — Dummy crisis 2008-2012 |
| `T09_ECON_Script_Heterocedasticidad.R` | **T9** — BP test, MCO robusta |
| `T09_CPA_Demanda_Producto.R` | **T9 CPA** — Contraejemplo: homocedasticidad |
| `T09_CPB_Cifra_Negocio_Sectorial.R` | **T9 CPB** — Heterocedasticidad natural |
| `T09_CPC_Consumo_Agregado_Trimestral.R` | **T9 CPC** — Comprobar heterocedasticidad |
| `T10_ECON_Script_Autocorrelacion.R` | **T10** — Durbin-Watson, residuos secuenciales |
| `T10_CPC_Consumo_Agregado_Trimestral.R` | **T10 CPC** — Autocorrelación en la función de consumo |

---

### Datasets

| Dataset | Descripción |
|---------|-------------|
| `DS_mini.xlsx/.csv` | Mini-dataset (10 obs) para cálculo matricial MCO en Excel |
| `caso_a_demanda.xlsx/.csv` | **Caso A** — Demanda de producto de consumo (35 obs, corte transversal): ventas, precio propio, precio sustitutivo, publicidad, promoción, temporada |
| `caso_b_cifra_negocio.xlsx/.csv` | **Caso B** — Cifra de negocio sectorial (40 obs, corte transversal): cifra negocio, trabajadores, inversión, superficie, PAC, región |
| `caso_c_consumo_trimestral.xlsx/.csv` | **Caso C** — Consumo agregado trimestral España (40 obs, serie temporal): consumo, PIB, tipo interés, inflación, crisis |
| `diccionario.md` | Descripción de todas las variables |
| `generar_datos.R` | Script generador de los datasets (semilla fija para reproducibilidad) |

Todos los datasets son sintéticos pero realistas, calibrados con datos reales españoles y generados con `set.seed(2024)`.

---

### Requisitos

- R >= 4.0
- Paquetes principales: `tidyverse`, `readxl`, `ggplot2`, `lmtest`, `sandwich`, `broom`
- Los paquetes deben instalarse manualmente: `install.packages(c("tidyverse", "readxl", "lmtest", "sandwich", "broom"))`

---

### Cómo ejecutar los scripts

1. Clonar el repositorio: `git clone https://github.com/carlanta/Econometrics.git`
2. Abrir `mEconometria.Rproj` en RStudio
3. Abrir el script deseado y ejecutar bloque a bloque con **Ctrl+Enter**

---

*© Carlos de Anta Puig, 2026. Publicado por Digital Reasons. Todos los derechos reservados.*
