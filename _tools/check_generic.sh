#!/usr/bin/env bash
# ══════════════════════════════════════════════════════════════════
# Comprueba que lo que va a git es GENÉRICO: ni el libro ni sus scripts, apps, datos o la web
# (fuentes) mencionan universidades, grados, campus ni guías docentes. Decisión del autor.
# El sitio construido (web/_site) y materiales/ (fuera de git) no se revisan: el plan del profesor sí puede nombrarlos.
# Uso: bash _tools/check_generic.sh   (antes de cada commit)
# ══════════════════════════════════════════════════════════════════
set -uo pipefail
cd "$(dirname "$0")/.."
PATRON='\bUNIR\b|Universidad Internacional|La Rioja|Grado en|gu[ií]a docente|[Cc]ourse guide|[Aa]ula virtual'
fuentes=$(grep -rn -I -E "$PATRON" --exclude-dir=_site --exclude-dir=motor --exclude=check_generic.sh \
          *.qmd scripts apps data web _tools README.md 2>/dev/null)
if [ -n "$fuentes" ]; then echo "✘ Menciones a revisar:"; echo "$fuentes" | cut -c1-180; exit 1; fi
echo "OK: fuentes genéricas, sin referencias a universidades, grados ni guías docentes"
