#!/usr/bin/env python3
"""Servicio de la zona del profesor de una web docente (una instancia por web).

Lo usan /profesor/liberar.html y /profesor/acceso.html, ya protegidas con usuario y clave en nginx.
Solo biblioteca estándar (y el comando openssl para cifrar la clave). Escucha en un socket Unix, nunca en la red.

  GET  /profesor/api/estado          → {"keys", "released", "updated", "alumnos": {"usuarios": [...]}, "modo", "enlaces"}
  POST /profesor/api/liberar         ← {"key": "T03", "released": true}
  POST /profesor/api/clave-alumnos   ← {"usuario": "grupoA", "clave": "..."}   (crea el usuario o le cambia la clave)
  POST /profesor/api/borrar-alumno   ← {"usuario": "grupoA"}
  POST /profesor/api/modo            ← {"abierta": true}   (crea o borra estado/web-abierta; nginx lo mira en cada petición)
  POST /profesor/api/enlace          ← {"tipo": "asistencia" | "carpeta", "url": "https://…" | ""}
  POST /profesor/api/grupo           ← {"id"?: "g…", "nombre", "aula", "horas": {"0": "16:00", "4": "15:30"}}
  POST /profesor/api/borrar-grupo    ← {"id": "g…"}
  POST /profesor/api/hecho           ← {"grupo": "g…", "bloque": "s05A-2", "hecho": true}

Es un fichero COMÚN a todas las webs de la VM (/srv/webs/_app/liberar.py, una instancia systemd por web): viene del
motor webMotor y el publicador solo lo instala si su versión es igual o más nueva que la instalada. Debe seguir
sirviendo a las webs que aún tengan páginas de versiones anteriores del motor.

Publicar una clave crea el enlace  public/downloads/<clave> → ../../store/<clave>
y reescribe public/released.json, que la parte pública usa para mostrar u ocultar secciones.
Usuarios de alumnos (uno por grupo o universidad, sin datos personales): estado/alumnos.htpasswd, con las claves
cifradas (SHA-512 crypt); nunca en claro. Enlaces de clase: estado/enlaces.json, copiado a public/enlaces.json.
"""
import datetime
import json
import os
import re
import socketserver
import subprocess
import sys
import tempfile
from http.server import BaseHTTPRequestHandler
from urllib.parse import urlsplit

WEB = os.environ["WEB"]
BASE = os.path.join("/srv/webs", WEB)
PUBLIC = os.path.join(BASE, "public")
STORE = os.path.join(BASE, "store")
VIDEOS = os.path.join(BASE, "videos")           # vídeos docentes propios, fuera del ciclo de publicación
VIDEOS_PUB = os.path.join(VIDEOS, "publicos")   # solo enlaces a lo liberado; es lo que sirve nginx
ESTADO_DIR = os.path.join(BASE, "estado")
ESTADO = os.path.join(ESTADO_DIR, "liberados.json")
HISTORIAL = os.path.join(ESTADO_DIR, "historial.log")
SOCKET = os.environ.get("SOCKET", f"/run/webs-liberar/{WEB}.sock")
CLAVE = re.compile(r"^[A-Za-z0-9-]{1,40}$")
ALUMNOS = os.path.join(ESTADO_DIR, "alumnos.htpasswd")
ALUMNOS_INFO = os.path.join(ESTADO_DIR, "alumnos.json")
ABIERTA = os.path.join(ESTADO_DIR, "web-abierta")      # si existe, nginx no pide la clave de los alumnos
ENLACES = os.path.join(ESTADO_DIR, "enlaces.json")         # enlaces de clase que pone y quita el profesor
ASISTENCIA_ANTIGUO = os.path.join(ESTADO_DIR, "asistencia.json")   # formato anterior (solo el enlace de asistencia)
TIPOS_ENLACE = ("asistencia", "carpeta")
GRUPOS = os.path.join(ESTADO_DIR, "grupos.json")          # grupos de clase (horario, aula) y bloques ya dados
HORA = re.compile(r"^([01]\d|2[0-3]):[0-5]\d$")
BLOQUE = re.compile(r"^s\d{2}[A-Z]-\d{1,2}$")              # semana, sesión y número de bloque del plan
ID_GRUPO = re.compile(r"^g[0-9a-z]{1,20}$")
DIAS_ANTIGUOS = {"lunes": "0", "viernes": "4"}             # primer formato de los grupos (solo lunes y viernes)


def normalizar_grupo(g):
    """Grupo con horas = {día desde el lunes: "HH:MM"}; convierte el formato antiguo (lunes, viernes)."""
    horas = {k: v for k, v in (g.get("horas") or {}).items() if v}
    for nombre, dia in DIAS_ANTIGUOS.items():
        if g.get(nombre):
            horas.setdefault(dia, g[nombre])
    return {"id": g["id"], "nombre": g["nombre"], "aula": g.get("aula", ""), "horas": horas}


def grupos():
    try:
        with open(GRUPOS, encoding="utf-8") as f:
            datos = json.load(f)
    except (FileNotFoundError, ValueError):
        datos = {}
    return {"grupos": [normalizar_grupo(g) for g in datos.get("grupos", [])], "hechos": datos.get("hechos", {})}


def guardar_grupos(datos):
    escribir_json(GRUPOS, datos)
    return datos
USUARIO = re.compile(r"^[A-Za-z0-9._-]{3,32}$")
CLAVE_MIN = 4                                              # claves sencillas: se dictan en clase
ENLACE = re.compile(r"^https://[A-Za-z0-9.-]+(\.[A-Za-z]{2,})(/[^\s\"'<>]*)?$")


def enlaces():
    try:
        with open(ENLACES, encoding="utf-8") as f:
            datos = json.load(f)
    except (FileNotFoundError, ValueError):
        datos = {}
        try:                                               # migra el enlace de asistencia del formato anterior
            with open(ASISTENCIA_ANTIGUO, encoding="utf-8") as f:
                datos["asistencia"] = json.load(f)
        except (FileNotFoundError, ValueError):
            pass
    return {t: datos.get(t) or {"url": "", "actualizada": None} for t in TIPOS_ENLACE}


def cambiar_enlace(tipo, url):
    datos = enlaces()
    datos[tipo] = {"url": url, "actualizada": ahora() if url else None}
    escribir_json(ENLACES, datos)
    escribir_json(os.path.join(PUBLIC, "enlaces.json"), datos)   # lo lee assets/release.js en cada página
    return datos


def modo_web():
    if os.path.exists(ABIERTA):
        desde = datetime.datetime.fromtimestamp(os.path.getmtime(ABIERTA), datetime.timezone.utc).isoformat(timespec="seconds")
        return {"abierta": True, "desde": desde}
    return {"abierta": False, "desde": None}


def cambiar_modo(abierta):
    if abierta:
        with open(ABIERTA, "w", encoding="utf-8") as f:
            f.write("La web de los alumnos está abierta sin clave. Bórralo o ciérrala desde /profesor/acceso.html\n")
        os.chmod(ABIERTA, 0o664)
    elif os.path.exists(ABIERTA):
        os.unlink(ABIERTA)
    return modo_web()


def ahora():
    return datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds")


def info_alumnos():
    """Usuarios de alumnos y fecha del último cambio de clave de cada uno (nunca la clave)."""
    try:
        with open(ALUMNOS_INFO, encoding="utf-8") as f:
            datos = json.load(f)
    except (FileNotFoundError, ValueError):
        datos = {}
    if "usuarios" not in datos:                            # formato anterior: un solo usuario
        datos = {"usuarios": [{"usuario": datos["usuario"], "actualizada": datos.get("actualizada")}] if datos.get("usuario") else []}
    en_fichero = {u for u, _ in leer_htpasswd()}
    datos["usuarios"] = sorted([x for x in datos["usuarios"] if x["usuario"] in en_fichero], key=lambda x: x["usuario"])
    return datos


def leer_htpasswd():
    try:
        with open(ALUMNOS, encoding="utf-8") as f:
            return [tuple(l.rstrip("\n").split(":", 1)) for l in f if ":" in l]
    except FileNotFoundError:
        return []


def escribir_htpasswd(lineas):
    fd, tmp = tempfile.mkstemp(dir=ESTADO_DIR, prefix=".tmp-")
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        f.writelines(f"{u}:{h}\n" for u, h in lineas)
    os.chmod(tmp, 0o640)
    os.replace(tmp, ALUMNOS)


def cambiar_clave_alumnos(usuario, clave):
    # openssl lee la clave por la entrada estándar: no aparece en la lista de procesos
    cifrada = subprocess.run(["openssl", "passwd", "-6", "-stdin"], input=clave + "\n",
                             capture_output=True, text=True, check=True).stdout.strip()
    if not cifrada.startswith("$6$"):
        raise RuntimeError("no se pudo cifrar la clave")
    escribir_htpasswd([(u, h) for u, h in leer_htpasswd() if u != usuario] + [(usuario, cifrada)])
    info = info_alumnos()
    info["usuarios"] = [x for x in info["usuarios"] if x["usuario"] != usuario] + [{"usuario": usuario, "actualizada": ahora()}]
    escribir_json(ALUMNOS_INFO, info)
    return info_alumnos()


def borrar_alumno(usuario):
    escribir_htpasswd([(u, h) for u, h in leer_htpasswd() if u != usuario])
    info = info_alumnos()
    escribir_json(ALUMNOS_INFO, info)
    return info


def claves():
    return sorted(d for d in os.listdir(STORE) if CLAVE.match(d) and os.path.isdir(os.path.join(STORE, d)))


def leer_estado():
    try:
        with open(ESTADO, encoding="utf-8") as f:
            datos = json.load(f)
        return set(k for k in datos.get("released", []) if isinstance(k, str) and CLAVE.match(k)), datos.get("updated")
    except FileNotFoundError:
        return set(), None


def escribir_json(ruta, datos):
    carpeta = os.path.dirname(ruta)
    fd, tmp = tempfile.mkstemp(dir=carpeta, prefix=".tmp-")
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        json.dump(datos, f, ensure_ascii=False, indent=1)
    os.chmod(tmp, 0o664)
    os.replace(tmp, ruta)


def enlazar_videos(liberados, validas):
    """Los vídeos docentes propios viven en videos/<clave>/, fuera de public/, porque el publicador
    reemplaza public/ y store/ enteros en cada publicación y mover gigas de vídeo en cada cambio del
    plan no tiene sentido. Se liberan con la misma regla que todo lo demás: nginx sirve /videos/ desde
    videos/publicos/, que solo contiene enlaces a las claves liberadas. Lo no liberado no existe en la
    web. Si la asignatura no tiene vídeos, no se hace nada."""
    if not os.path.isdir(VIDEOS):
        return
    try:
        os.makedirs(VIDEOS_PUB, exist_ok=True)
    except OSError as e:
        # Callarse aquí costó una tarde: con ProtectSystem=strict, si videos/ no está en ReadWritePaths
        # del servicio, esto falla y los vídeos no se sirven nunca, sin un solo mensaje. Que se vea.
        sys.stderr.write(f"webs-liberar {WEB}: no puedo preparar {VIDEOS_PUB} ({e}). "
                         f"¿Falta /srv/webs/{WEB}/videos en ReadWritePaths del servicio?\n")
        return
    for k in validas:
        origen = os.path.join(VIDEOS, k)
        enlace = os.path.join(VIDEOS_PUB, k)
        if k in liberados and os.path.isdir(origen):
            if not os.path.islink(enlace):
                if os.path.exists(enlace):
                    raise RuntimeError(f"{enlace} existe y no es un enlace")
                os.symlink(os.path.join("..", k), enlace)
        elif os.path.islink(enlace):
            os.unlink(enlace)


def aplicar(liberados, actualizado=None):
    validas = set(claves())
    liberados = set(liberados) & validas
    descargas = os.path.join(PUBLIC, "downloads")
    os.makedirs(descargas, exist_ok=True)
    for k in validas:
        enlace = os.path.join(descargas, k)
        if k in liberados:
            if not os.path.islink(enlace):
                if os.path.exists(enlace):
                    raise RuntimeError(f"{enlace} existe y no es un enlace")
                os.symlink(os.path.join("..", "..", "store", k), enlace)
        elif os.path.islink(enlace):
            os.unlink(enlace)
    enlazar_videos(liberados, validas)
    datos = {"released": sorted(liberados), "updated": actualizado}
    escribir_json(ESTADO, datos)
    escribir_json(os.path.join(PUBLIC, "released.json"), datos)
    return datos


class Manejador(BaseHTTPRequestHandler):
    server_version = "webs-liberar"
    sys_version = ""

    def address_string(self):
        return self.headers.get("X-Real-IP", "unix") if hasattr(self, "headers") and self.headers else "unix"

    def log_message(self, formato, *args):
        sys.stderr.write("%s %s\n" % (self.address_string(), formato % args))

    def responder(self, codigo, datos):
        cuerpo = json.dumps(datos, ensure_ascii=False).encode("utf-8")
        self.send_response(codigo)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(cuerpo)))
        self.end_headers()
        self.wfile.write(cuerpo)

    def peticion_valida(self):
        # Solo peticiones hechas por la propia página: cabecera propia y, en POST, mismo origen
        if self.headers.get("X-Requested-With") != "fetch":
            return False
        if self.command == "POST":
            origen = self.headers.get("Origin")
            if not origen or urlsplit(origen).netloc != self.headers.get("Host", ""):
                return False
            if not self.headers.get("Content-Type", "").startswith("application/json"):
                return False
        return True

    def do_GET(self):
        if self.path.split("?")[0] != "/profesor/api/estado":
            return self.responder(404, {"error": "no encontrado"})
        if not self.peticion_valida():
            return self.responder(403, {"error": "petición no permitida"})
        liberados, actualizado = leer_estado()
        self.responder(200, {"keys": claves(), "released": sorted(liberados & set(claves())), "updated": actualizado,
                             "alumnos": info_alumnos(), "modo": modo_web(), "enlaces": enlaces(), "grupos": grupos()})

    def leer_json(self):
        largo = int(self.headers.get("Content-Length", "0"))
        if largo <= 0 or largo > 1024:
            raise ValueError
        return json.loads(self.rfile.read(largo))

    def do_POST(self):
        rutas = ("/profesor/api/liberar", "/profesor/api/clave-alumnos", "/profesor/api/borrar-alumno", "/profesor/api/modo", "/profesor/api/enlace",
                 "/profesor/api/grupo", "/profesor/api/borrar-grupo", "/profesor/api/hecho")
        if self.path not in rutas:
            return self.responder(404, {"error": "no encontrado"})
        if not self.peticion_valida():
            return self.responder(403, {"error": "petición no permitida"})
        if self.path == "/profesor/api/clave-alumnos":
            return self.clave_alumnos()
        if self.path == "/profesor/api/borrar-alumno":
            return self.borrar_alumno_post()
        if self.path == "/profesor/api/modo":
            return self.modo()
        if self.path == "/profesor/api/enlace":
            return self.enlace_post()
        if self.path == "/profesor/api/grupo":
            return self.grupo_post()
        if self.path == "/profesor/api/borrar-grupo":
            return self.borrar_grupo_post()
        if self.path == "/profesor/api/hecho":
            return self.hecho_post()
        try:
            datos = self.leer_json()
            clave = datos["key"]
            publicar = datos["released"]
            if not isinstance(clave, str) or not isinstance(publicar, bool) or clave not in claves():
                raise ValueError
        except (ValueError, KeyError, TypeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        liberados, _ = leer_estado()
        (liberados.add if publicar else liberados.discard)(clave)
        momento = ahora()
        resultado = aplicar(liberados, momento)
        self.historial(momento, "publicar" if publicar else "ocultar", clave)
        self.responder(200, resultado)

    def historial(self, momento, accion, detalle):
        usuario = self.headers.get("X-Remote-User", "?")
        with open(HISTORIAL, "a", encoding="utf-8") as f:
            f.write(f"{momento}\t{usuario}\t{accion}\t{detalle}\n")

    def modo(self):
        try:
            abierta = self.leer_json()["abierta"]
            if not isinstance(abierta, bool):
                raise ValueError
        except (ValueError, KeyError, TypeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        resultado = cambiar_modo(abierta)
        self.historial(ahora(), "abrir-web" if abierta else "cerrar-web", "alumnos")
        self.responder(200, resultado)

    def enlace_post(self):
        try:
            datos = self.leer_json()
            tipo, url = datos["tipo"], datos["url"].strip()
            if tipo not in TIPOS_ENLACE:
                raise ValueError
            if url and not ENLACE.match(url):
                return self.responder(400, {"error": "El enlace debe empezar por https:// y no llevar espacios."})
            if len(url) > 500:
                return self.responder(400, {"error": "El enlace es demasiado largo."})
        except (ValueError, KeyError, TypeError, AttributeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        resultado = cambiar_enlace(tipo, url)
        self.historial(ahora(), "enlace-" + tipo, url or "(sin enlace)")
        self.responder(200, resultado)

    def grupo_post(self):
        """Crea o modifica un grupo: nombre, aula y hora de inicio de cada día de clase (horas = {"0": "16:00", …})."""
        try:
            d = self.leer_json()
            gid = d.get("id") or ""
            nombre, aula = d["nombre"].strip(), d.get("aula", "").strip()
            horas = d.get("horas") or {}
            if not isinstance(horas, dict):
                raise ValueError
            horas = {str(k): v.strip() for k, v in horas.items() if isinstance(v, str) and v.strip()}
            for antiguo, dia in DIAS_ANTIGUOS.items():      # páginas con el formato anterior
                if isinstance(d.get(antiguo), str) and d[antiguo].strip():
                    horas.setdefault(dia, d[antiguo].strip())
            if gid and not ID_GRUPO.match(gid):
                raise ValueError
            if not 1 <= len(nombre) <= 40 or len(aula) > 40 or any(c in nombre + aula for c in "<>\r\n"):
                return self.responder(400, {"error": "Nombre de 1 a 40 caracteres y aula de 40 como máximo, sin < ni >."})
            if any(k not in "0123456" or len(k) != 1 or not HORA.match(v) for k, v in horas.items()):
                return self.responder(400, {"error": "Las horas se escriben como 15:30."})
        except (ValueError, KeyError, TypeError, AttributeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        datos = grupos()
        existentes = {g["id"] for g in datos["grupos"]}
        if not gid:
            gid = "g" + datetime.datetime.now().strftime("%y%m%d%H%M%S").lstrip("0")[-12:]
            while gid in existentes:
                gid += "b"
        grupo = {"id": gid, "nombre": nombre, "aula": aula, "horas": dict(sorted(horas.items()))}
        datos["grupos"] = [g for g in datos["grupos"] if g["id"] != gid] + [grupo]
        datos["grupos"].sort(key=lambda g: g["nombre"].lower())
        guardar_grupos(datos)
        self.historial(ahora(), "grupo", f"{nombre} ({gid})")
        self.responder(200, datos)

    def borrar_grupo_post(self):
        try:
            gid = self.leer_json()["id"]
            if not isinstance(gid, str) or not ID_GRUPO.match(gid):
                raise ValueError
        except (ValueError, KeyError, TypeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        datos = grupos()
        if gid not in {g["id"] for g in datos["grupos"]}:
            return self.responder(404, {"error": "Ese grupo no existe."})
        datos["grupos"] = [g for g in datos["grupos"] if g["id"] != gid]
        datos["hechos"].pop(gid, None)
        guardar_grupos(datos)
        self.historial(ahora(), "borrar-grupo", gid)
        self.responder(200, datos)

    def hecho_post(self):
        """Marca o desmarca un bloque del plan de clase como dado para un grupo."""
        try:
            d = self.leer_json()
            gid, bloque, hecho = d["grupo"], d["bloque"], d["hecho"]
            if not isinstance(gid, str) or not ID_GRUPO.match(gid) or not isinstance(bloque, str) or not BLOQUE.match(bloque) or not isinstance(hecho, bool):
                raise ValueError
        except (ValueError, KeyError, TypeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        datos = grupos()
        if gid not in {g["id"] for g in datos["grupos"]}:
            return self.responder(404, {"error": "Ese grupo no existe."})
        hechos = set(datos["hechos"].get(gid, []))
        (hechos.add if hecho else hechos.discard)(bloque)
        datos["hechos"][gid] = sorted(hechos)
        guardar_grupos(datos)
        self.responder(200, datos)

    def borrar_alumno_post(self):
        try:
            usuario = self.leer_json()["usuario"]
            if not isinstance(usuario, str) or not USUARIO.match(usuario):
                raise ValueError
        except (ValueError, KeyError, TypeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        if usuario not in {u for u, _ in leer_htpasswd()}:
            return self.responder(404, {"error": "Ese usuario no existe."})
        info = borrar_alumno(usuario)
        self.historial(ahora(), "borrar-usuario-alumnos", usuario)
        self.responder(200, info)

    def clave_alumnos(self):
        try:
            datos = self.leer_json()
            usuario, clave = datos["usuario"], datos["clave"]
            if not isinstance(usuario, str) or not USUARIO.match(usuario):
                return self.responder(400, {"error": "El usuario debe tener entre 3 y 32 letras, números, puntos, guiones o guiones bajos."})
            if not isinstance(clave, str) or not CLAVE_MIN <= len(clave) <= 128 or any(c in clave for c in "\r\n"):
                return self.responder(400, {"error": f"La clave debe tener entre {CLAVE_MIN} y 128 caracteres."})
        except (ValueError, KeyError, TypeError, json.JSONDecodeError):
            return self.responder(400, {"error": "datos no válidos"})
        info = cambiar_clave_alumnos(usuario, clave)
        self.historial(ahora(), "clave-usuario-alumnos", usuario)   # nunca se registra la clave
        self.responder(200, info)


class Servidor(socketserver.UnixStreamServer):
    def get_request(self):
        peticion, _ = super().get_request()
        return peticion, ("unix", 0)


def main():
    os.makedirs(ESTADO_DIR, exist_ok=True)
    if not os.path.exists(ALUMNOS):
        # Sin clave de alumnos la web queda cerrada (401), no rota (500)
        with open(ALUMNOS, "w", encoding="utf-8"):
            pass
        os.chmod(ALUMNOS, 0o640)
    liberados, actualizado = leer_estado()
    aplicar(liberados, actualizado)
    escribir_json(os.path.join(PUBLIC, "enlaces.json"), enlaces())   # rehace public/enlaces.json al publicar de nuevo
    if os.path.exists(SOCKET):
        os.unlink(SOCKET)
    servidor = Servidor(SOCKET, Manejador)
    os.chmod(SOCKET, 0o660)
    sys.stderr.write(f"webs-liberar {WEB}: {len(claves())} contenidos, {len(liberados)} publicados, socket {SOCKET}\n")
    servidor.serve_forever()


if __name__ == "__main__":
    main()
