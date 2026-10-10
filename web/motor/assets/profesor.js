/* Zona del profesor · grupos, horas reales y avance.
   - En cada plan de clase (tr[data-bloque]) calcula la hora de inicio y fin de cada bloque para el grupo elegido
     y permite marcar el bloque como dado.
   - En /profesor/grupos.html gestiona los grupos (nombre, aula y hora de inicio de cada día de clase) y muestra
     el avance de cada uno y lo que toca en su siguiente sesión (el plan completo llega en window.PLAN y los días
     de clase de la asignatura en window.DIAS_CLASE).
   Los datos los guarda el servicio de la zona del profesor en estado/grupos.json: cada grupo lleva
   horas = {"0": "16:00", "4": "15:30"} (días desde el lunes → hora de inicio). Motor común webMotor. */
(function () {
  var API = "/profesor/api/";
  var CLAVE = "grupo";                  // localStorage: cada web tiene su propio origen
  var DIAS = ["lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"];
  var MESES = ["enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"];

  function get() {
    return fetch(API + "estado", { cache: "no-store", credentials: "same-origin", headers: { "X-Requested-With": "fetch" } })
      .then(function (r) { if (!r.ok) throw new Error("HTTP " + r.status); return r.json(); });
  }
  function post(ruta, datos) {
    return fetch(API + ruta, { method: "POST", credentials: "same-origin",
      headers: { "Content-Type": "application/json", "X-Requested-With": "fetch" }, body: JSON.stringify(datos) })
      .then(function (r) { return r.json().then(function (d) { if (!r.ok) throw new Error(d.error || ("HTTP " + r.status)); return d; }); });
  }
  function esc(t) { var d = document.createElement("div"); d.textContent = t == null ? "" : t; return d.innerHTML; }
  function dos(n) { return (n < 10 ? "0" : "") + n; }
  function hhmm(base, min) {
    if (!base) return "";
    var p = base.split(":"), t = (+p[0]) * 60 + (+p[1]) + min;
    return dos(Math.floor(t / 60) % 24) + ":" + dos(t % 60);
  }
  function inicio(g, dia) { return (g.horas || {})[dia] || ""; }      // dia: 0 = lunes, 4 = viernes
  function fechaTxt(iso, dia) {
    var d = new Date(iso + "T12:00:00");
    return DIAS[dia] + " " + d.getDate() + " de " + MESES[d.getMonth()];
  }
  function elegido(gs) {
    var id = null;
    try { id = localStorage.getItem(CLAVE); } catch (e) {}
    if (!gs.some(function (g) { return g.id === id; })) id = gs.length ? gs[0].id : null;
    return id;
  }
  function recordar(id) { try { localStorage.setItem(CLAVE, id); } catch (e) {} }

  // ── Plan de clase de una semana: horas, aula y ticks ────────────
  function semana(d) {
    var cont = document.getElementById("grupos-sel");
    var gs = (d.grupos && d.grupos.grupos) || [];
    var hechos = (d.grupos && d.grupos.hechos) || {};
    function pintar(id) {
      cont.innerHTML = "";
      if (!gs.length) {
        cont.innerHTML = '<p class="note">Define tus grupos en <a href="/profesor/grupos.html">Grupos y avance</a> para ver aquí la hora de cada bloque y marcar lo que vas dando.</p>';
      } else {
        var et = document.createElement("span"); et.className = "note"; et.textContent = "Grupo: "; cont.appendChild(et);
        gs.forEach(function (g) {
          var b = document.createElement("button");
          b.type = "button"; b.className = "toggle" + (g.id === id ? "" : " secundario"); b.textContent = g.nombre;
          b.onclick = function () { recordar(g.id); pintar(g.id); };
          cont.appendChild(b);
        });
      }
      var g = gs.filter(function (x) { return x.id === id; })[0];
      document.querySelectorAll("[data-sesion-dia]").forEach(function (el) {
        var dia = +el.getAttribute("data-sesion-dia"), base = g ? inicio(g, dia) : "";
        var fin = +el.getAttribute("data-sesion-fin");
        el.textContent = g ? [base ? hhmm(base, 0) + "–" + hhmm(base, fin) : "", g.aula ? "aula " + g.aula : ""].filter(Boolean).join(" · ") : "";
      });
      document.querySelectorAll("tr[data-bloque]").forEach(function (tr) {
        var h = tr.querySelector("td.hora"), c = tr.querySelector("input.hecho");
        if (!g) { h.textContent = ""; c.disabled = true; c.checked = false; tr.classList.remove("hecho"); return; }
        var base = inicio(g, +tr.getAttribute("data-dia"));
        h.textContent = base ? hhmm(base, +tr.getAttribute("data-ini")) + "–" + hhmm(base, +tr.getAttribute("data-fin")) : "—";
        c.disabled = false;
        c.checked = (hechos[id] || []).indexOf(tr.getAttribute("data-bloque")) >= 0;
        tr.classList.toggle("hecho", c.checked);
        c.onchange = function () {
          var v = c.checked; c.disabled = true;
          post("hecho", { grupo: id, bloque: tr.getAttribute("data-bloque"), hecho: v })
            .then(function (r) { hechos = r.hechos; tr.classList.toggle("hecho", v); })
            .catch(function (e) { alert("No se pudo guardar: " + e.message); c.checked = !v; })
            .then(function () { c.disabled = false; });
        };
      });
    }
    pintar(elegido(gs));
  }

  // ── Panel de grupos: alta, avance y siguiente sesión ────────────
  function panel(d) {
    var plan = window.PLAN || [];
    var dias = window.DIAS_CLASE || [];
    var total = plan.reduce(function (n, s) { return n + s.bloques.length; }, 0);
    var msg = document.getElementById("msg");
    function aviso(t, mal) { msg.textContent = t; msg.hidden = false; msg.className = "notice" + (mal ? " bad" : ""); }
    function siguiente(g, hechos, desde) {
      var hecho = {}; (hechos[g.id] || []).forEach(function (b) { hecho[b] = true; });
      var r = [];
      for (var i = 0; i < plan.length && r.length < 2; i++) {
        var s = plan[i];
        if (s.bloques.some(function (b) { return !hecho[b.id]; })) r.push({ s: s, hecho: hecho });
      }
      return r;
    }
    function tarjetaSesion(g, x, etiqueta) {
      var s = x.s, base = inicio(g, s.dia);
      var filas = s.bloques.map(function (b) {
        return '<tr class="' + (x.hecho[b.id] ? "hecho" : "") + '"><td class="t">' + (base ? hhmm(base, b.ini) + "–" + hhmm(base, b.fin) : b.ini + "–" + b.fin + " min") +
          '</td><td class="blk">' + esc(b.act) + "</td><td>" + (x.hecho[b.id] ? "✔" : "") + "</td></tr>";
      }).join("");
      return '<article class="session"><header><span class="tag">' + etiqueta + " · " + esc(s.etiqueta || ("Sesión " + s.tipo)) + " · " + esc(fechaTxt(s.fecha, s.dia)) +
        (base ? " · " + hhmm(base, 0) + "–" + hhmm(base, s.fin) : "") + (g.aula ? " · aula " + esc(g.aula) : "") + "</span>" +
        '<h3><a href="/profesor/week/' + dos(s.semana) + '.html">Semana ' + s.semana + " · " + esc(s.titulo) + "</a></h3></header>" +
        '<div class="tbl"><table class="agenda"><thead><tr><th>Hora</th><th>Bloque</th><th>Dado</th></tr></thead><tbody>' + filas + "</tbody></table></div></article>";
    }
    function pintar(dd) {
      var gs = dd.grupos || [], hechos = dd.hechos || {};
      var lista = document.getElementById("lista-grupos");
      lista.innerHTML = gs.length ? gs.map(function (g) {
        return "<tr><td><b>" + esc(g.nombre) + "</b></td><td>" + esc(g.aula || "—") + "</td>" +
          dias.map(function (x) { return "<td>" + esc(inicio(g, x.dia) || "—") + "</td>"; }).join("") +
          '<td><button type="button" class="toggle secundario" data-editar="' + g.id + '">Editar</button> <button type="button" class="toggle" data-borrar="' + g.id + '">Borrar</button></td></tr>';
      }).join("") : '<tr><td colspan="' + (dias.length + 3) + '">Todavía no hay grupos.</td></tr>';
      var av = document.getElementById("avance");
      av.innerHTML = gs.map(function (g) {
        var n = (hechos[g.id] || []).length, pct = total ? Math.round(100 * n / total) : 0;
        var sig = siguiente(g, hechos);
        return '<section class="grupo"><h2>' + esc(g.nombre) + '</h2><p class="note">' + n + " de " + total + " bloques dados (" + pct + " %)</p>" +
          '<div class="barra"><span style="width:' + pct + '%"></span></div>' +
          (sig.length ? sig.map(function (x, k) { return tarjetaSesion(g, x, k ? "Después" : "Siguiente"); }).join("") : '<p class="notice">Todo el plan está dado.</p>') +
          "</section>";
      }).join("");
      lista.onclick = function (ev) {
        var be = ev.target.closest("button[data-editar]"), bb = ev.target.closest("button[data-borrar]");
        if (be) {
          var g = gs.filter(function (x) { return x.id === be.getAttribute("data-editar"); })[0], f = document.getElementById("form-grupo");
          f.id_grupo.value = g.id; f.nombre.value = g.nombre; f.aula.value = g.aula || "";
          dias.forEach(function (x) { f["h" + x.dia].value = inicio(g, x.dia); });
          f.querySelector("button[type=submit]").textContent = "Guardar cambios"; f.scrollIntoView({ behavior: "smooth" });
        }
        if (bb) {
          var gid = bb.getAttribute("data-borrar");
          if (!confirm("¿Borrar este grupo y todo su avance?")) return;
          post("borrar-grupo", { id: gid }).then(function (r) { aviso("Grupo borrado.", false); pintar(r); }).catch(function (e) { aviso("No se pudo borrar: " + e.message, true); });
        }
      };
    }
    var f = document.getElementById("form-grupo");
    f.addEventListener("submit", function (ev) {
      ev.preventDefault();
      var horas = {};
      dias.forEach(function (x) { if (f["h" + x.dia].value) horas[x.dia] = f["h" + x.dia].value; });
      var datos = { id: f.id_grupo.value, nombre: f.nombre.value.trim(), aula: f.aula.value.trim(), horas: horas };
      post("grupo", datos).then(function (r) {
        aviso("Grupo «" + datos.nombre + "» guardado.", false); f.reset(); f.id_grupo.value = "";
        f.querySelector("button[type=submit]").textContent = "Añadir grupo"; pintar(r);
      }).catch(function (e) { aviso("No se pudo guardar: " + e.message, true); });
    });
    pintar(d.grupos || {});
  }

  /* Estado de liberación en los botones del plan del profesor.
     Cualquier <a class="dl" data-estado="CLAVE"> se pinta de verde si esa clave está liberada y de rojo
     si no lo está, y se le añade delante un punto del mismo color. Se lee del servicio, no del HTML,
     porque el estado cambia sin volver a publicar: la página es de ayer y la liberación es de hace un rato.
     Si la lectura falla no se toca nada: un botón sin color dice «no lo sé», que es la verdad. */
  function estados(d) {
    var libres = {};
    (d.released || []).forEach(function (k) { libres[k] = 1; });
    var n = 0;
    Array.prototype.forEach.call(document.querySelectorAll("a.dl[data-estado]"), function (a) {
      var on = !!libres[a.dataset.estado];
      a.classList.toggle("est-si", on);
      a.classList.toggle("est-no", !on);
      a.title = on ? "Liberado: los alumnos ya lo ven" : "Sin liberar: solo lo ves tú";
      var m = a.querySelector(".dl-m");
      if (m && !m.querySelector(".est-pt")) {
        var p = document.createElement("span");
        p.className = "est-pt";
        p.textContent = "● ";
        m.insertBefore(p, m.firstChild);
      }
      n++;
    });
    /* Botones para liberar u ocultar sin salir del plan (scripts de R que se sueltan en clase):
       <button data-liberar="CLAVE" data-nombre="…">. Mismo servicio y misma petición que /profesor/liberar.html. */
    Array.prototype.forEach.call(document.querySelectorAll("button[data-liberar]"), function (b) {
      var on = !!libres[b.dataset.liberar];
      b.setAttribute("aria-pressed", on ? "true" : "false");
      b.textContent = on ? "Ocultar" : "Liberar";
      b.title = on ? "Los alumnos lo ven: pulsa para retirarlo" : "Solo lo ves tú: pulsa para que lo vean los alumnos";
      b.disabled = false;
      b.hidden = false;
    });
    return n;
  }

  document.addEventListener("click", function (ev) {
    var b = ev.target.closest("button[data-liberar]");
    if (!b) return;
    var on = b.getAttribute("aria-pressed") !== "true";
    if (!confirm((on ? "¿Liberar para los alumnos?\n\n" : "¿Ocultar a los alumnos?\n\n") + (b.dataset.nombre || b.dataset.liberar))) return;
    b.disabled = true;
    post("liberar", { key: b.dataset.liberar, released: on })
      .then(get).then(estados)
      .catch(function (e) { alert("No se pudo guardar el cambio: " + e.message); b.disabled = false; });
  });

  var hayEstado = document.querySelector("a.dl[data-estado], button[data-liberar]");
  if (!document.getElementById("grupos-sel") && !document.getElementById("panel-grupos") && !hayEstado) return;
  get().then(function (d) {
    if (document.getElementById("grupos-sel")) semana(d);
    if (document.getElementById("panel-grupos")) panel(d);
    estados(d);
  }).catch(function (e) {
    var el = document.getElementById("grupos-sel") || document.getElementById("msg");
    if (el) { el.hidden = false; el.textContent = "No se pueden leer los grupos (" + e.message + ")."; }
  });
})();
