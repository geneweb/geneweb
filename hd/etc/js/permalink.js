(function () {
  "use strict";
  var G = window.GWPERMA;
  if (!G || !G.q) return;
  if (document.getElementById("gw-perma")) return;

  var t = G.t || {};
  var L = { c: t.c || "Copy link", p: t.p || "", f: t.f || "", w: t.w || "" };
  var role = G.r || 0;

  var origin = window.location.origin;
  var path = window.location.pathname;
  var params = new URLSearchParams(window.location.search);
  var cgi = params.has("b");
  var rawBase, prefix;
  if (cgi) { rawBase = params.get("b") || ""; prefix = origin + path; }
  else {
    var segs = path.split("/").filter(Boolean);
    rawBase = segs.length ? segs[0] : "";
    prefix = origin;
  }
  var bare = rawBase;
  if (role === 2 && /_w$/.test(bare)) bare = bare.slice(0, -2);
  else if (role === 1 && /_f$/.test(bare)) bare = bare.slice(0, -2);
  function build(s) {
    var b = bare + s;
    return cgi ? prefix + "?b=" + b + "&" + G.q
               : prefix + "/" + b + "?" + G.q;
  }
  var avail = [{ label: L.p, url: build("") }];
  if (role >= 1) avail.push({ label: L.f, url: build("_f") });
  if (role >= 2) avail.push({ label: L.w, url: build("_w") });
  var primary = avail[avail.length - 1];
  var others = avail.slice(0, -1);

  function writeClipboard(text) {
    if (navigator.clipboard && navigator.clipboard.writeText)
      return navigator.clipboard.writeText(text);
    return new Promise(function (resolve, reject) {
      var ta = document.createElement("textarea");
      ta.value = text;
      ta.setAttribute("readonly", "");
      ta.style.position = "fixed";
      ta.style.opacity = "0";
      document.body.appendChild(ta);
      ta.select();
      var ok = false;
      try { ok = document.execCommand("copy"); } catch (e) {}
      document.body.removeChild(ta);
      if (ok) resolve(); else reject();
    });
  }

  var live = document.createElement("span");
  live.className = "visually-hidden";
  live.setAttribute("aria-live", "polite");

  function flash(btn) {
    var icon = btn.querySelector("i");
    if (!icon) return;
    icon.classList.remove("fa-link");
    icon.classList.add("fa-check");
    live.textContent = L.c + " \u2713";
    setTimeout(function () {
      icon.classList.remove("fa-check");
      icon.classList.add("fa-link");
    }, 1400);
  }
  function doCopy(url, btn) {
    writeClipboard(url).then(
      function () { flash(btn); },
      function () { live.textContent = "\u2717"; }
    );
  }
  function tip(label) { return L.c + " (" + label + ")"; }

  var tipped = [];

  function makeMain() {
    var b = document.createElement("button");
    b.type = "button";
    b.className = "btn btn-link px-0";
    b.title = tip(primary.label);
    b.setAttribute("aria-label", tip(primary.label));
    b.innerHTML = '<i class="fa fa-link fa-fw"></i>';
    b.addEventListener("click", function () { doCopy(primary.url, b); });
    tipped.push(b);
    return b;
  }

  var root = document.createElement("div");
  root.id = "gw-perma";
  var tog = null;

  if (others.length === 0) {
    root.appendChild(makeMain());
  } else {
    root.className = "btn-group dropup";

    tog = document.createElement("button");
    tog.type = "button";
    tog.className = "btn btn-link dropdown-toggle dropdown-toggle-split";
    tog.setAttribute("data-bs-toggle", "dropdown");
    tog.setAttribute("data-bs-display", "static");
    tog.setAttribute("data-bs-auto-close", "outside");
    tog.setAttribute("aria-expanded", "false");
    var hid = document.createElement("span");
    hid.className = "visually-hidden";
    hid.textContent = L.c;
    tog.appendChild(hid);

    var menu = document.createElement("ul");
    menu.className = "dropdown-menu";
    menu.style.minWidth = "max-content";
    others.forEach(function (o) {
      var li = document.createElement("li");
      var b = document.createElement("button");
      b.type = "button";
      b.className = "dropdown-item";
      b.title = tip(o.label);
      b.innerHTML = '<i class="fa fa-link fa-fw me-2"></i>';
      b.appendChild(document.createTextNode(o.label));
      b.addEventListener("click", function () {
        doCopy(o.url, b);
        setTimeout(function () {
          if (window.bootstrap && bootstrap.Dropdown) {
            var inst = bootstrap.Dropdown.getInstance(tog);
            if (inst) inst.hide();
          }
        }, 1000);
      });
      tipped.push(b);
      li.appendChild(b);
      menu.appendChild(li);
    });

    root.appendChild(makeMain());
    root.appendChild(tog);
    root.appendChild(menu);
  }

  root.appendChild(live);

  var placement = "left";
  var anchor = document.getElementById("gw-perma-anchor");
  var langTog = document.getElementById("dropdownMenu1");
  var langGrp = langTog ? langTog.closest(".btn-group") : null;
  var copyr = document.getElementById("copyr");
  if (anchor && anchor.parentNode) {
    root.classList.add("me-2");
    anchor.parentNode.insertBefore(root, anchor);
  } else if (langGrp && langGrp.parentNode) {
    root.classList.add("me-2");
    langGrp.parentNode.insertBefore(root, langGrp);
  } else if (copyr) {
    root.classList.add("me-2");
    copyr.insertBefore(root, copyr.firstChild);
  } else {
    placement = "right";
    root.style.cssText =
      "position:absolute;inset-block-end:1rem;inset-inline-start:1rem;" +
      "z-index:1030";
    if (getComputedStyle(document.body).position === "static")
      document.body.style.position = "relative";
    document.body.appendChild(root);
  }

  tipped.forEach(function (el) {
    el.setAttribute("data-bs-toggle", "tooltip");
    el.setAttribute("data-bs-placement", placement);
    if (window.bootstrap && bootstrap.Tooltip)
      bootstrap.Tooltip.getOrCreateInstance(el);
  });
})();