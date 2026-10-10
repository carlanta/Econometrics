#!/usr/bin/env bash
# ══════════════════════════════════════════════════════════════════
# Publica la web de una asignatura en el servidor de webs (VM 130 «WEBS», 192.168.50.4) · motor común webMotor
# Lo llama _tools/publicar_web.sh de la asignatura, desde su raíz en el equipo local (Git Bash), a través de «puerta».
# Lee web/curso.conf:  WEB (subdominio), PROYECTO (carpeta en MANUALES) y NOMBRE (título, sin tildes, para nginx).
#   1. construye el sitio en el servidor R (Rscript web/build_web.R)
#   2. lo copia a la VM y sustituye public/ y store/ (el estado de publicación, estado/, se conserva)
#   3. instala la configuración de nginx de la web (plantilla del motor) y el servicio del profesor, y los recarga
# El servicio (/srv/webs/_app/liberar.py) es COMÚN a todas las webs de la VM: solo se instala si la versión del motor
# de esta asignatura es igual o más nueva que la instalada (/srv/webs/_app/VERSION). Si cambia, se reinician también
# las instancias de las otras webs que ya usan el motor (/srv/webs/<web>/.motor). nginx se recarga, no se reinicia.
# ══════════════════════════════════════════════════════════════════
set -euo pipefail
[ -f web/curso.conf ] || { echo "Ejecuta desde la raíz de la asignatura: falta web/curso.conf"; exit 1; }
# shellcheck disable=SC1091
source web/curso.conf
: "${WEB:?Falta WEB en web/curso.conf}" "${PROYECTO:?Falta PROYECTO en web/curso.conf}" "${NOMBRE:?Falta NOMBRE en web/curso.conf}"
R=root@192.168.100.144
V=root@192.168.50.4
P="/home/carlos/900·RESEARCH/MANUALES/$PROYECTO"

echo "== 1. Construcción en el servidor R"
ssh puerta "ssh $R \"cd '$P' && runuser -u carlos -- env LC_ALL=es_ES.utf8 LANG=es_ES.utf8 HOME=/home/carlos Rscript web/build_web.R\""

echo "== 2. Copia al servidor de webs"
ssh puerta "ssh $R \"tar -C '$P/web' -czf - _site motor/servidor motor/VERSION\" | ssh $V 'rm -rf /srv/webs/.nuevo && mkdir -p /srv/webs/.nuevo && tar -C /srv/webs/.nuevo -xzf -'"

echo "== 3. Instalación"
ssh puerta "ssh $V bash -s" <<EOF
set -euo pipefail
W=$WEB; NOMBRE='$NOMBRE'; B=/srv/webs/\$W; N=/srv/webs/.nuevo; A=/srv/webs/_app
install -d -o root -g root -m 755 \$A
NUEVA=\$(cat \$N/motor/VERSION); ACTUAL=\$(cat \$A/VERSION 2>/dev/null || echo 0)
REINICIAR_TODAS=0
if [ "\$(printf '%s\n%s\n' "\$ACTUAL" "\$NUEVA" | sort -V | tail -1)" = "\$NUEVA" ]; then
  cmp -s \$N/motor/servidor/liberar.py \$A/liberar.py || REINICIAR_TODAS=1
  install -m 644 \$N/motor/servidor/liberar.py \$A/liberar.py
  install -m 644 \$N/motor/servidor/webs-liberar@.service /etc/systemd/system/webs-liberar@.service
  install -m 644 \$N/motor/servidor/webs-limites.conf /etc/nginx/conf.d/webs-limites.conf
  echo "\$NUEVA" > \$A/VERSION
  echo "motor del servidor: \$ACTUAL → \$NUEVA"
else
  echo "AVISO: el servidor tiene el motor \$ACTUAL, más nuevo que el de esta asignatura (\$NUEVA)."
  echo "       No se toca el servicio común. Actualiza el motor de la asignatura (webMotor/actualizar.sh) y vuelve a publicar."
fi
# Apps de la asignatura (opcional): APPS=<ip>:<puerto> de su VM en web/curso.conf. Sin APPS, el bloque se quita.
APPS='${APPS:-}'
install -d -m 755 /etc/nginx/snippets
[ -f \$N/motor/servidor/webs-apps-proxy.conf ] && install -m 644 \$N/motor/servidor/webs-apps-proxy.conf /etc/nginx/snippets/webs-apps-proxy.conf
[ -f \$N/motor/servidor/webs-apps-http.conf ] && install -m 644 \$N/motor/servidor/webs-apps-http.conf /etc/nginx/conf.d/webs-apps-http.conf
if [ -n "\$APPS" ]; then
  [[ "\$APPS" =~ ^[0-9.]+:[0-9]+\$ ]] || { echo "APPS mal escrito en web/curso.conf (ip:puerto): \$APPS"; exit 1; }
  sed -e "s/@WEB@/\$W/g" -e "s/@NOMBRE@/\$NOMBRE/g" -e "s/@APPS@/\$APPS/g" \$N/motor/servidor/nginx.conf.plantilla > /etc/nginx/sites-available/\$W.conf
  echo "apps de la asignatura: \$APPS"
else
  sed -e "s/@WEB@/\$W/g" -e "s/@NOMBRE@/\$NOMBRE/g" -e '/# APPS-INICIO/,/# APPS-FIN/d' \$N/motor/servidor/nginx.conf.plantilla > /etc/nginx/sites-available/\$W.conf
fi
ln -sf /etc/nginx/sites-available/\$W.conf /etc/nginx/sites-enabled/\$W.conf
id -nG www-data | grep -qw webs || usermod -aG webs www-data
install -d -o webs -g webs -m 2775 \$B/estado
[ -f \$B/estado/alumnos.htpasswd ] || install -m 640 -o webs -g webs /dev/null \$B/estado/alumnos.htpasswd
for d in public store; do
  rm -rf \$B/\$d.old
  [ -d \$B/\$d ] && mv \$B/\$d \$B/\$d.old
  mv \$N/_site/\$d \$B/\$d
done
chown -R webs:webs \$B/public \$B/store
find \$B/public \$B/store -type d -exec chmod 2775 {} +
find \$B/public \$B/store -type f -exec chmod 0664 {} +
systemctl daemon-reload
systemctl enable webs-liberar@\$W >/dev/null 2>&1
echo "\$NUEVA" > \$B/.motor          # esta web ya usa el motor común (y en qué versión)
if [ \$REINICIAR_TODAS = 1 ]; then
  # Solo las webs que ya usan el motor: las demás siguen con el código que tenían cargado hasta su próxima publicación
  for u in \$(systemctl list-units 'webs-liberar@*' --no-legend --plain | awk '{print \$1}'); do
    w=\${u#webs-liberar@}; w=\${w%.service}
    [ "\$w" != "\$W" ] && [ -f /srv/webs/\$w/.motor ] && systemctl restart \$u && echo "reiniciada también: \$w"
  done
fi
systemctl restart webs-liberar@\$W
nginx -t
systemctl reload nginx
rm -rf \$B/public.old \$B/store.old \$N
sleep 1
systemctl is-active webs-liberar@\$W nginx | paste -sd' '
echo "publicados: \$(cat \$B/public/released.json | tr -d '\n ')"
EOF
echo "== Publicada: https://$WEB.carlosdeanta.net"
