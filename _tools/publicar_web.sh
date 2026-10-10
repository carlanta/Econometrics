#!/usr/bin/env bash
# Publica la web de la asignatura (desde el equipo local, Git Bash, en la raíz del proyecto).
# El publicador es común a todas las webs docentes: web/motor/publicar.sh (motor webMotor), con los datos de web/curso.conf.
set -euo pipefail
cd "$(dirname "$0")/.."
bash web/motor/publicar.sh
