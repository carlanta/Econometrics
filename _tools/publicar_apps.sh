#!/usr/bin/env bash
# Sube las apps de la asignatura a su VM de apps (desde el equipo local, Git Bash, en la raíz del proyecto).
# El publicador es común: web/motor/publicar_apps.sh (motor webMotor), con web/curso.conf (APPS) y web/apps.conf.
set -euo pipefail
cd "$(dirname "$0")/.."
bash web/motor/publicar_apps.sh
