# webMotor · motor común de las webs docentes

Una sola copia de todo lo genérico de las webs de las asignaturas (identidad CdeA, VM 130 «WEBS»). Cada asignatura
guarda solo lo suyo y recibe el motor como copia en `web/motor/`.

Repositorio: `/home/carlos/900·RESEARCH/MANUALES/webMotor` (servidor R).

## Qué hay aquí

| Fichero | Qué hace |
|---|---|
| `R/motor.R` | Funciones comunes de `build_web.R`: idiomas y traducciones (diccionario o plan español aparte con `diccionario_plan()`), registro de ficheros, PDF por capítulos, piezas HTML, `pagina()` (menú, banderas, enlaces en ventana nueva), portada abierta, recuadro «esta semana», calendario, páginas de semana (`paginas_semanas()`), y las páginas del profesor Liberar contenidos, Acceso de alumnos, Grupos y avance y Plan docente |
| `assets/` | `style.css` (identidad CdeA), `release.js` (contenidos liberados y enlaces de clase), `profesor.js` (grupos, horas reales, ticks y avance), banderas |
| `servidor/liberar.py` | Servicio de la zona del profesor. **Común a todas las webs** de la VM (`/srv/webs/_app/liberar.py`) |
| `servidor/nginx.conf.plantilla` | Configuración de nginx de una web (`@WEB@`, `@NOMBRE@`) |
| `servidor/webs-liberar@.service`, `webs-limites.conf` | Unidad systemd (una instancia por web) y límite de peticiones |
| `publicar.sh` | Publicador común: construye en R, copia a la VM e instala |
| `actualizar.sh` | Copia el motor a una asignatura |
| `VERSION` | Versión del motor (`AAAA.MM.DD`, con `.2`, `.3`… si hay más de una el mismo día) |

## Qué pone cada asignatura

- `web/curso.conf`: `WEB=…`, `PROYECTO=…`, `NOMBRE="…"` (sin tildes: va a nginx).
- `web/build_web.R`: define `CURSO` (ver la cabecera de `R/motor.R`), carga `web/motor/R/motor.R`, llama a
  `iniciar_sitio()` y construye sus páginas propias (portada, calendario, materiales, actividades, portada del
  profesor) con las funciones del motor. Define `material_html()` y llama a `pagina_liberar()`, `pagina_acceso()`,
  `pagina_grupos()` y `pagina_plan_docente()` con sus datos.
- `web/plan_semanas.R` (cada sesión con `tipo`, `dia` y `bloques`; cada semana con `inicio`) y `web/plan_semanas_es.R`.
- Opcional: `web/assets/curso.css`, que se añade al final de los estilos comunes.
- `_tools/publicar_web.sh`: una línea, `bash web/motor/publicar.sh`.

## Cómo se cambia el motor

1. Cambiar aquí (nunca en `web/motor/` de una asignatura) y subir `VERSION`.
2. `liberar.py` debe seguir sirviendo a las webs con páginas de versiones anteriores: añadir, no romper.
3. `bash actualizar.sh <asignatura>` en cada asignatura, construir, comparar y publicar.
4. Commit aquí y en cada asignatura.

## Asignaturas que lo usan

| Asignatura | Carpeta | Web | Desde |
|---|---|---|---|
| Financial Economics | `mFinEconomics` | financialeconomics | 2026.09.22 |
| Análisis y Gestión de Riesgos | `mRiesgos` | riskmanagement | 2026.09.22.2 (días provisionales: lunes y jueves) |
| Series temporales y predicción | `mTimeSeriesUNIR` | timeseries | 2026.09.22.3 (zona del profesor en español; días provisionales: lunes y jueves) |
| Gestión de Renta Fija | `mRentaFija` | fixedincome | 2026.09.22.3 (web nueva; plan bilingüe en un fichero con `TT()`; calendario provisional) |
