#!/usr/bin/env bash
# ══════════════════════════════════════════════════════════════════
# Copia las apps Shiny de una asignatura a SU VM de apps (una VM por asignatura, en HP04) · motor común webMotor
# Lo llama _tools/publicar_apps.sh de la asignatura, desde su raíz en el equipo local (Git Bash), a través de «puerta».
# Lee web/curso.conf (PROYECTO y APPS=<ip>:<puerto>) y web/apps.conf, una línea por app:
#     <clave> <carpeta> [ficheros de datos…]     p. ej.   T01 apps/01-leyes-financieras
#                                                         T03 apps/03-procesos-ma-ar data/aurora_ventas_mensuales.csv
# Los ficheros de datos (rutas del proyecto) se copian en <clave>/data/, junto a app.R, que es donde las apps los buscan.
# La clave es la de liberación del contenido en la web: la app sale en /apps/<clave>/ cuando se libera esa clave.
# La lista manda: en la VM queda exactamente lo que diga apps.conf (lo que no esté, se borra).
# La VM no acepta conexiones del servidor R, así que el código viaja R → equipo local → VM. Nunca pasa por la web.
# ══════════════════════════════════════════════════════════════════
set -euo pipefail
[ -f web/curso.conf ] || { echo "Ejecuta desde la raíz de la asignatura: falta web/curso.conf"; exit 1; }
# shellcheck disable=SC1091
source web/curso.conf
: "${PROYECTO:?Falta PROYECTO en web/curso.conf}" "${APPS:?Falta APPS (ip:puerto de la VM de apps) en web/curso.conf}"
IP=${APPS%%:*}
R=root@192.168.100.144
A=carlos@$IP
P="/home/carlos/900·RESEARCH/MANUALES/$PROYECTO"
T=$(mktemp)
trap 'rm -f "$T"' EXIT

echo "== 1. Apps de $PROYECTO en el servidor R"
ssh puerta "ssh $R \"cd '$P' && test -f web/apps.conf && tar --exclude='.Rhistory' --exclude='*.zip' --exclude='rsconnect' --exclude='.git*' -cf - web/apps.conf \\\$(awk '!/^#/ && NF>=2 {for (i=2; i<=NF; i++) print \\\$i}' web/apps.conf)\"" > "$T"
tar -tf "$T" >/dev/null
echo "   $(tar -tf "$T" | grep -c '/app.R$') apps, $(du -k "$T" | cut -f1) KB"

echo "== 2. Copia a la VM de apps ($IP)"
ssh puerta "ssh $A 'cat > /tmp/apps-nuevas.tar'" < "$T"

echo "== 3. Instalación"
ssh puerta "ssh $A 'sudo bash -s'" <<'EOF'
set -euo pipefail
S=/srv/shiny-server; N=$(mktemp -d)
tar -xf /tmp/apps-nuevas.tar --no-same-owner -C "$N"
rm -f /tmp/apps-nuevas.tar
CLAVES=()
while read -r clave carpeta extras; do
  [[ -z "$clave" || "$clave" == \#* ]] && continue
  [[ "$clave" =~ ^[A-Za-z0-9_-]+$ ]] || { echo "Clave no válida: $clave"; exit 1; }
  [ -f "$N/$carpeta/app.R" ] || [ -f "$N/$carpeta/server.R" ] || { echo "Falta la app de $clave en $carpeta"; exit 1; }
  rm -rf "$S/.$clave.nueva"; cp -a "$N/$carpeta" "$S/.$clave.nueva"
  for f in $extras; do
    [ -f "$N/$f" ] || { echo "Falta el fichero de datos $f de $clave"; exit 1; }
    install -d "$S/.$clave.nueva/data"; cp -a "$N/$f" "$S/.$clave.nueva/data/"
  done
  rm -rf "$S/$clave"; mv "$S/.$clave.nueva" "$S/$clave"
  touch "$S/$clave/restart.txt"          # las sesiones nuevas cargan la versión nueva
  CLAVES+=("$clave"); echo "   $clave ← $carpeta"
done < "$N/web/apps.conf"
for d in "$S"/*/; do d=$(basename "$d"); [[ " ${CLAVES[*]} " == *" $d "* ]] || { rm -rf "${S:?}/$d"; echo "   retirada: $d"; }; done
rm -rf "$N"
chown -R root:shiny "$S"; chmod -R u=rwX,g=rX,o= "$S"   # el usuario shiny solo lee el código
systemctl is-active shiny-server >/dev/null || systemctl restart shiny-server
for c in "${CLAVES[@]}"; do printf '   %s: %s\n' "$c" "$(curl -s -o /dev/null -m 30 -w '%{http_code}' "http://127.0.0.1:3838/$c/")"; done
EOF
echo "== Apps instaladas. Los alumnos las ven en la web cuando se libera su clave; el profesor, en /profesor/apps/<clave>/"
