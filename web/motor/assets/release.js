/* Muestra solo los contenidos liberados por el profesor (lista en /released.json).
   Elementos: data-release="T03 T04" se muestran si alguna clave está liberada;
              data-pending="T03 T04" se ocultan en ese caso. Los ficheros no liberados
              ni siquiera están publicados en el servidor: esto solo ordena la página. */
(function () {
  function aplicar(lista) {
    var liberado = {};
    lista.forEach(function (k) { liberado[k] = true; });
    function alguna(el, attr) {
      return el.getAttribute(attr).split(/\s+/).some(function (k) { return liberado[k]; });
    }
    document.querySelectorAll("[data-release]").forEach(function (el) { el.hidden = !alguna(el, "data-release"); });
    document.querySelectorAll("[data-pending]").forEach(function (el) { el.hidden = alguna(el, "data-pending"); });
  }
  fetch("/released.json", { cache: "no-store" })
    .then(function (r) { return r.ok ? r.json() : { released: [] }; })
    .then(function (d) { aplicar(d.released || []); })
    .catch(function () { aplicar([]); });

  /* Enlaces de clase (hoja de asistencia, carpeta compartida): el profesor los pone y los quita en
     /profesor/acceso.html y el servicio los guarda en /enlaces.json. Sin enlace, el botón
     a[data-enlace="tipo"] sigue oculto. */
  fetch("/enlaces.json", { cache: "no-store" })
    .then(function (r) { return r.ok ? r.json() : {}; })
    .then(function (d) {
      document.querySelectorAll("a[data-enlace]").forEach(function (a) {
        var e = d && d[a.getAttribute("data-enlace")];
        if (!e || !e.url) return;
        a.href = e.url;
        a.target = "_blank";
        a.rel = "noopener noreferrer";
        a.hidden = false;
      });
    })
    .catch(function () {});
})();
