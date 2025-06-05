const root = document.documentElement;
const fanchart = document.getElementById("fanchart");
const places_list = document.getElementById("places_list");
const pixel = document.getElementById("pixel").getContext("2d");
var sheet;
for (var i in document.styleSheets) {
  if (document.styleSheets[i].title == "fc-auto") {
    sheet = document.styleSheets[i];
    break;
  }
}
var standard, standard_width;
var center_x, center_y, svg_w, svg_h;
var max_gen_loaded; // Génération max disponible en mémoire
var max_gen, max_r;
var lieux = {};
var lieux_a = [];
var sortMode = "frequency";
var has_bi = false, has_ba = false, has_ma = false, has_de = false, has_bu = false;
var svg_viewbox_x = 0, svg_viewbox_y = 0, svg_viewbox_w = 0, svg_viewbox_h = 0;

// ====== Configuration et constantes =======
const CONFIG = {
  security: 0.95,
  zoom_factor: 1.25,
  d_all: 220,
  a_r: [50, 50, 50, 50, 80, 70, 100, 150, 130, 90],
  a_m: ["S1", "C3", "C3", "C3", "R3", "R3", "R2", "R1", "R1", "R1"],
  marriage_length_thresholds: [4, 14, 24, 34, 44, 54],
  text_reduction_factor: 0.9,
  svg_margin: 5
};

// ========== Utilitaires généraux ==========
const Utils = {
  deathAgeClass: function(age) {
    if (age < 30) return "DA0";
    var adjustedAge = age - 30;
    var n = Math.trunc(adjustedAge / 15) + 1;
    if (n > 6) { n = 6; }
    return "DA" + n;
  },

  marriageLengthClass: function(length) {
    const years = parseInt(length);
    if (isNaN(years) || years < 0) return "";

    const index = CONFIG.marriage_length_thresholds.findIndex(threshold => years <= threshold);
    return index === -1 ? "DAM6" : `DAM${index}`;
  },

  relativeLuminance: function(color) {
    pixel.fillStyle = color;
    pixel.fillRect(0, 0, 1, 1);
    const data = pixel.getImageData(0, 0, 1, 1).data;
    const rsrgb = data[0] / 255;
    const gsrgb = data[1] / 255;
    const bsrgb = data[2] / 255;
    const r = rsrgb <= 0.03928 ? rsrgb / 12.92 : Math.pow((rsrgb + 0.055) / 1.055, 2.4);
    const g = gsrgb <= 0.03928 ? gsrgb / 12.92 : Math.pow((gsrgb + 0.055) / 1.055, 2.4);
    const b = bsrgb <= 0.03928 ? bsrgb / 12.92 : Math.pow((bsrgb + 0.055) / 1.055, 2.4);
    return r * 0.2126 + g * 0.7152 + b * 0.0722;
  },

  contrastRatio: function(color1, color2) {
    return (this.relativeLuminance(color1) + 0.05) / (this.relativeLuminance(color2) + 0.05);
  },

  buildUrlParams: function(p) {
    return `m=A&t=FC${mono === "1" ? "&mono=1" : ""}${tool ? "&tool=" + tool : ""}${implex === "0" ? "&implex=0" : ""}&p=${p.fnk}&n=${p.snk}${p.oc ? "&oc=" + p.oc : ""}`;
  },

  navigateWithParams: function(newGen) {
    var p = ancestor["S1"];
    var url = link_to_person + this.buildUrlParams(p) + "&v=" + newGen;
    if (has_ba) url += "&ba=on";
    if (has_bu) url += "&bu=on";
    window.location = url;
  },

  updateUrlWithCurrentState: function() {
    const p = ancestor["S1"];
    const newUrl = link_to_person + Utils.buildUrlParams(p) + "&v=" + max_gen;
    history.replaceState(null, '', newUrl);
  }
};

// ========== Fonctions utilitaires pour la géométrie ==========
function polarToCartesian(r, angle) {
  const rad = Math.PI / 180 * angle;
  return {
    x: center_x + r * Math.cos(rad),
    y: center_y + r * Math.sin(rad)
  };
}

// ========== Rendu SVG ==========
const SVGRenderer = {
  drawContour: function(g, r1, r2, a1, a2) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const p1 = polarToCartesian(r2, a1);
    const p2 = polarToCartesian(r2, a2);
    const p3 = polarToCartesian(r1, a2);
    const p4 = polarToCartesian(r1, a1);

    path.setAttribute("d",
      `M ${p1.x},${p1.y} ` +
      `A ${r2} ${r2} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${p2.x},${p2.y} ` +
      `L ${p3.x},${p3.y} ` +
      `A ${r1} ${r1} 0 ${(a2 - a1 > 180 ? 1 : 0)} 0 ${p4.x},${p4.y} Z`
    );
    path.setAttribute("class", "contour");
    g.append(path);
  },

  drawRadialLine: function(g, r1, r2, a) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const p1 = polarToCartesian(r2, a);
    const p2 = polarToCartesian(r1, a);

    path.setAttribute("d", `M ${p1.x},${p1.y} L ${p2.x},${p2.y}`);
    path.setAttribute("class", "middle");
    g.append(path);
  },

  drawCircle: function(g, r, cx, cy, p, options = {}) {
    const circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    circle.setAttribute("cx", cx);
    circle.setAttribute("cy", cy);
    circle.setAttribute("r", r);

    if (options.isBackground) {
      // Version background - applique les classes pour lieux et âge
      let classes = ['bg'];

      if (p.birth_place && lieux[p.birth_place]) {
        classes.push(`bi-${lieux[p.birth_place].c}`);
      }
      if (p.baptism_place && lieux[p.baptism_place]) {
        classes.push(`ba-${lieux[p.baptism_place].c}`);
      }
      if (p.death_place && lieux[p.death_place]) {
        classes.push(`de-${lieux[p.death_place].c}`);
      }
      if (p.burial_place && lieux[p.burial_place]) {
        classes.push(`bu-${lieux[p.burial_place].c}`);
      }
      if (p.death_age) {
        classes.push(Utils.deathAgeClass(p.death_age));
      }

      circle.setAttribute("class", classes.join(' '));
    } else {
      // Version interactive
      circle.setAttribute("class", "link");

      // Titre
      const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
      const age = (p.death_age && p.death_age !== "" && !isNaN(parseInt(p.death_age)))
        ? ` (${p.death_age} ans)`
        : "";
      title.textContent = `(Sosa 1) ${p.fn} ${p.sn}${age}`;
      circle.appendChild(title);

      // Événements - réutilisation des méthodes universelles
      circle.onclick = (e) => this.handleClick(e, p);
      circle.onmouseenter = (e) => this.handleMouseEnter(p, 'person', e);
      circle.onmouseleave = (e) => this.handleMouseLeave(p, 'person', e);
    }

    g.append(circle);
    return circle;
  },

  drawPie: function(g, r1, r2, a1, a2, p, options = {}) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const p1 = polarToCartesian(r2, a1);
    const p2 = polarToCartesian(r2, a2);
    const p3 = polarToCartesian(r1, a2);
    const p4 = polarToCartesian(r1, a1);

    path.setAttribute("d",
      `M ${p1.x},${p1.y} ` +
      `A ${r2} ${r2} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${p2.x},${p2.y} ` +
      `L ${p3.x},${p3.y} ` +
      `A ${r1} ${r1} 0 ${(a2 - a1 > 180 ? 1 : 0)} 0 ${p4.x},${p4.y} Z`
    );

    if (options.isBackground) {
      // Version background - applique les classes CSS pour les lieux et âges
      this.applyBackgroundClasses(path, p, options.type);
    } else {
      // Version interactive - ajoute événements et titre
      this.applyInteractiveFeatures(path, p, options.type);
    }

    g.append(path);
    return path;
  },

  applyBackgroundClasses: function(element, p, type) {
    let classes = [];

    if (type === 'person') {
      classes.push('bg');
      // Classes pour les lieux de vie
      if (p.birth_place) classes.push(`bi-${lieux[p.birth_place].c}`);
      if (p.baptism_place) classes.push(`ba-${lieux[p.baptism_place].c}`);
      if (p.death_place) classes.push(`de-${lieux[p.death_place].c}`);
      if (p.burial_place) classes.push(`bu-${lieux[p.burial_place].c}`);
      // Classe pour l'âge au décès
      if (p.death_age) classes.push(Utils.deathAgeClass(p.death_age));
    } else if (type === 'marriage') {
      // Classes pour les mariages
      if (p.marriage_place) classes.push(`ma-${lieux[p.marriage_place].c}`);
      if (p.marriage_length) {
        const marriageClass = Utils.marriageLengthClass(p.marriage_length);
        if (marriageClass) classes.push(marriageClass);
      }
    }

    element.setAttribute("class", classes.join(' '));
  },

  applyInteractiveFeatures: function(element, p, type) {
    if (!p || p.fn === "?" || p.fn === "" || !p.fn) {
      return;
    }
    element.setAttribute("class", "link");

    // Titre contextuel
    const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
    if (type === 'person') {
      const age = (p.death_age && p.death_age !== "" && !isNaN(parseInt(p.death_age)))
        ? ` (${p.death_age} ans)`
        : "";
      title.textContent = `(Sosa ${p.sosa}) ${p.fn} ${p.sn}${age}`;
    } else if (type === 'marriage') {
      const years = parseInt(p.marriage_length) || -1;
      if (years >= 0) {
        title.textContent = years === 1 ? "1 année de mariage" : `${years} années de mariage`;
      }
    }
    element.appendChild(title);

    // Événements de clic
    element.onclick = (e) => this.handleClick(e, p);

    // Événements de survol
    element.onmouseenter = (e) => this.handleMouseEnter(p, type, e);
    element.onmouseleave = (e) => this.handleMouseLeave(p, type, e);
  },

  handleClick: function(e, p) {
    if (!link_to_person) {
      alert("Erreur: Impossible d'accéder à la fiche individuelle");
      return;
    }

    const oc = p.oc ? `&oc=${p.oc}` : '';
    const url = `${link_to_person}p=${p.fnk}&n=${p.snk}${oc}`;

    if (e.ctrlKey || e.shiftKey) {
      window.open(url, '_blank');
    }
    e.stopPropagation();
  },

  togglePlaceHighlights: function(p, show) {
    const places = [
      { prop: 'birth_place', prefix: 'bi' },
      { prop: 'baptism_place', prefix: 'ba' },
      { prop: 'marriage_place', prefix: 'ma' },
      { prop: 'death_place', prefix: 'de' },
      { prop: 'burial_place', prefix: 'bu' }
    ];

    places.forEach(place => {
      if (p[place.prop]) {
        const el = document.getElementById(`${place.prefix}-${lieux[p[place.prop]].c}`);
        if (el) el.classList.toggle("hidden", !show);
        LocationManager.hlPlace(p[place.prop], show);
      }
    });
  },

  handleMouseEnter: function(p, type, event) {
    // Gestion des lieux
    this.togglePlaceHighlights(p, true);

    // Gestion du background
    if (event && event.currentTarget) {
      const group = event.currentTarget.parentNode;
      const backgroundSector = group.querySelector('.bg');
      if (backgroundSector) {
        backgroundSector.style.fill = "lightgrey";
      }
    }

    // Gestion spécifique par type
    if (type === 'person' && p.death_age) {
      const ageEl = document.getElementById(Utils.deathAgeClass(p.death_age));
      if (ageEl) ageEl.classList.add("hl");
    } else if (type === 'marriage' && p.marriage_length) {
      const marriageClass = Utils.marriageLengthClass(p.marriage_length);
      if (marriageClass) {
        const marriageEl = document.getElementById(marriageClass);
        if (marriageEl) marriageEl.classList.add("hl");
      }
    }

    // Gestion des implexes
    if (p.fn === "=") {
      const ref = document.getElementById("S" + p.sn);
      if (ref) ref.classList.add("same_hl");
    }
  },

  handleMouseLeave: function(p, type, event) {
    // Gestion des lieux
    this.togglePlaceHighlights(p, false);

    // Gestion du background
    if (event && event.currentTarget) {
      const group = event.currentTarget.parentNode;
      const backgroundSector = group.querySelector('.bg');
      if (backgroundSector) {
        backgroundSector.style.fill = "";
      }
    }

    // Gestion spécifique par type
    if (type === 'person' && p.death_age) {
      const ageEl = document.getElementById(Utils.deathAgeClass(p.death_age));
      if (ageEl) ageEl.classList.remove("hl");
    } else if (type === 'marriage' && p.marriage_length) {
      const marriageClass = Utils.marriageLengthClass(p.marriage_length);
      if (marriageClass) {
        const marriageEl = document.getElementById(marriageClass);
        if (marriageEl) marriageEl.classList.remove("hl");
      }
    }

    // Gestion des implexes
    if (p.fn === "=") {
      const ref = document.getElementById("S" + p.sn);
      if (ref) ref.classList.remove("same_hl");
    }
  },

  drawSectorText: function(pg, r1, r2, a1, a2, sosa, p, classes, generation, isSame = false) {
    let mode;

    if (CONFIG.a_m[generation - 1] === "C3") {
      mode = 'C3';
    } else if (CONFIG.a_m[generation - 1] === "R3" && !isSame) {
      mode = 'R3';
    } else if (CONFIG.a_m[generation - 1] === "R2" && !isSame) {
      mode = 'R2';
    } else if (CONFIG.a_m[generation - 1] === "R1" || isSame) {
      mode = 'R1';
    }

    return T.drawText(pg, mode, {
      r1: r1 + 10,
      r2: r2,
      a1: a1,
      a2: a2,
      sosa: sosa,
      p: p,
      classes: classes
    });
  },

  drawNavigationSymbol: function(g, pathId, p, pathLength, hasParents) {
    let fontSize = 80;
    if (2 * standard_width > pathLength) {
      fontSize = Math.round(100 * pathLength / 2 / standard_width);
    }
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    if (hasParents) {
      text.setAttribute("class", "link icon");
      text.innerHTML = `<textPath xlink:href="#${pathId}" startOffset="50%" style="font-size:${fontSize}%;">&#x25B2;</textPath>`;
      text.onclick = (e) => {
        Utils.navigateWithParams(max_gen);
      };
    } else {
      text.setAttribute("class", "no-link");
      text.innerHTML = `<textPath xlink:href="#${pathId}" startOffset="50%" style="font-size:${fontSize}%;">&#x2716;</textPath>`;
    }
    const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
    title.textContent = hasParents ? `Recentrer l'arbre sur ${p.fn} ${p.sn}` : `${p.fn} ${p.sn} : aucun parent connu`;
    text.appendChild(title);
    g.append(text);
    return text;
  },

  drawParentIndicator: function(g, r, a1, a2, sosa, p) {
    if (!p || p.fn === "?" || p.fn === "" || !p.fn) {
      return; // Pas d'icône du tout
    }
    const pathLength = T.createCircularPath(g, `tpiS${sosa}`, r, a1, a2);
    return this.drawNavigationSymbol(g, `tpiS${sosa}`, p, pathLength, p.has_parents);
  }
};

// ========== Système de rendu de texte unifié ==========
const TextRenderer = {
  drawText: function(g, mode, params) {
    // Construire les classes CSS pour les lieux
    const textClasses = this.buildLocationClasses(params.p, params.classes || "");

    switch(mode) {
      case 'S1':
        return this.drawCentralText(g, params.x, params.y, params.p, textClasses);

      case 'C3':
        return this.drawCircularText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses);

      case 'R3':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 3);

      case 'R2':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 2);

      case 'R1':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 1);

      default:
        console.warn(`Mode de texte non reconnu: ${mode}`);
        return null;
    }
  },

  buildLocationClasses: function(p, baseClasses) {
    let classes = baseClasses;

    if (p.birth_place && lieux[p.birth_place]) {
      classes += " bi-t" + lieux[p.birth_place].c;
    }
    if (p.baptism_place && lieux[p.baptism_place]) {
      classes += " ba-t" + lieux[p.baptism_place].c;
    }
    if (p.death_place && lieux[p.death_place]) {
      classes += " de-t" + lieux[p.death_place].c;
    }
    if (p.burial_place && lieux[p.burial_place]) {
      classes += " bu-t" + lieux[p.burial_place].c;
    }

    return classes.trim();
  },

  drawCentralText: function(g, x, y, p, classes) {
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    text.setAttribute("x", x);
    text.setAttribute("y", y);
    text.setAttribute("class", classes);

    // Calcul des tailles de police adaptatives pour éviter le débordement
    const fontSizes = this.calculateAdaptiveFontSizes([p.fn, p.sn]);

    // Construction du texte avec tailles adaptatives
    text.innerHTML =
      `<tspan style="font-size:${fontSizes[0]}%">${p.fn}</tspan>` +
      `<tspan x="${x}" dy="15" style="font-size:${fontSizes[1]}%">${p.sn}</tspan>` +
      `<tspan class="dates" x="${x}" dy="15">${p.dates}</tspan>`;

    g.append(text);
    return text;
  },

  drawCircularText: function(g, r1, r2, a1, a2, sosa, p, classes) {
    const height = Math.abs(r2 - r1) / 3;

    // Trois arcs concentriques pour prénom, nom, dates
    const pathLength1 = this.createCircularPath(g, `tp1S${sosa}`, (r2-r1)*3/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp1S${sosa}`, p.fn, classes, pathLength1, height);

    const pathLength2 = this.createCircularPath(g, `tp2S${sosa}`, (r2-r1)*2/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp2S${sosa}`, p.sn, classes, pathLength2, height);

    const pathLength3 = this.createCircularPath(g, `tp3S${sosa}`, (r2-r1)/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp3S${sosa}`, p.dates, classes + " dates", pathLength3, height);

    return g;
  },

  drawRadialText: function(g, r1, r2, a1, a2, sosa, p, classes, lineCount) {
    // Calcul des paramètres de direction selon l'orientation
    const params = this.calculateRadialParameters(r1, r2, a1, a2, lineCount);
    const height = Math.abs(a2 - a1) / 360 * 2 * Math.PI * r1 / lineCount;

    if (lineCount === 3) {
      // Trois lignes : prénom, nom, dates
      const pathLength1 = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, p.fn, classes, pathLength1, height);

      const pathLength2 = this.createRadialPath(g, `tp2S${sosa}`, params.r1, params.r2, params.angles[1]);
      this.placeTextOnPath(g, `tp2S${sosa}`, p.sn, classes, pathLength2, height);

      const pathLength3 = this.createRadialPath(g, `tp3S${sosa}`, params.r1, params.r2, params.angles[2]);
      this.placeTextOnPath(g, `tp3S${sosa}`, p.dates, classes + " dates", pathLength3, height);

    } else if (lineCount === 2) {
      // Deux lignes : nom complet, dates
      const pathLength1 = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, `${p.fn} ${p.sn}`, classes, pathLength1, height);

      const pathLength2 = this.createRadialPath(g, `tp2S${sosa}`, params.r1, params.r2, params.angles[1]);
      this.placeTextOnPath(g, `tp2S${sosa}`, p.dates, classes + " dates", pathLength2, height);

    } else { // lineCount === 1
      // Une ligne : nom complet seulement
      const pathLength = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, `${p.fn} ${p.sn}`, classes, pathLength, height);
    }

    return g;
  },

  calculateRadialParameters: function(r1, r2, a1, a2, lineCount) {
    let myR1, myR2, angles = [];

    if (a1 >= -90) {
      // Orientation normale
      myR1 = r1;
      myR2 = r2;

      if (lineCount === 3) {
        angles = [
          a2 - (a2-a1)*3/4,  // Position 3/4
          a2 - (a2-a1)*2/4,  // Position 1/2
          a2 - (a2-a1)/4     // Position 1/4
        ];
      } else if (lineCount === 2) {
        angles = [
          a2 - (a2-a1)*2/3,  // Position 2/3
          a2 - (a2-a1)/3     // Position 1/3
        ];
      } else {
        angles = [a2 - (a2-a1)/2]; // Position centrale
      }
    } else {
      // Orientation inversée
      myR1 = r2;
      myR2 = r1;

      if (lineCount === 3) {
        angles = [
          a1 + (a2-a1)*3/4,
          a1 + (a2-a1)*2/4,
          a1 + (a2-a1)/4
        ];
      } else if (lineCount === 2) {
        angles = [
          a1 + (a2-a1)*2/3,
          a1 + (a2-a1)/3
        ];
      } else {
        angles = [a1 + (a2-a1)/2];
      }
    }

    return { r1: myR1, r2: myR2, angles: angles };
  },

  createCircularPath: function(g, id, r, a1, a2) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("class", "none");
    const p1 = polarToCartesian(r, a1);
    const p2 = polarToCartesian(r, a2);
    path.setAttribute("d",
      `M ${p1.x},${p1.y} A ${r} ${r} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${p2.x},${p2.y}`
    );
    path.setAttribute("id", id);
    g.append(path);

    // Retourne la longueur approximative du chemin
    return Math.abs(a2 - a1) / 360 * 2 * Math.PI * r;
  },

  createRadialPath: function(g, id, r1, r2, a) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("class", "none");
    const p1 = polarToCartesian(r1, a);
    const p2 = polarToCartesian(r2, a);
    path.setAttribute("d", `M ${p1.x},${p1.y} L ${p2.x},${p2.y}`);
    path.setAttribute("id", id);
    g.append(path);

    // Retourne la longueur du chemin
    return Math.abs(r2 - r1);
  },

  placeTextOnPath: function(g, pathId, textContent, classes, pathLength, pathHeight) {
    // Calcul de la taille de police optimale
    standard.textContent = textContent;
    const textWidth = standard.getBBox().width;
    const textHeight = standard.getBBox().height;

    let fontSizeByWidth = 100;
    if (textWidth > pathLength * CONFIG.security) {
      fontSizeByWidth = Math.round(100 * pathLength * CONFIG.security / textWidth);
    }

    let fontSizeByHeight = 100;
    if (textHeight > pathHeight * CONFIG.security) {
      fontSizeByHeight = Math.round(100 * pathHeight * CONFIG.security / textHeight);
    }

    const finalFontSize = Math.min(fontSizeByWidth, fontSizeByHeight);

    // Création de l'élément text avec textPath
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    text.setAttribute("class", "text " + classes);
    text.innerHTML =
      `<textPath xlink:href="#${pathId}" startOffset="50%" style="font-size:${finalFontSize}%;">` +
      textContent +
      `</textPath>`;

    g.append(text);
    return text;
  },

  calculateAdaptiveFontSizes: function(texts) {
    const textReductionFactor = CONFIG.text_reduction_factor || 0.9;
    const maxWidth = 2 * CONFIG.a_r[0] * CONFIG.security;

    return texts.map(text => {
      standard.textContent = text;
      const width = standard.getBBox().width;

      if (width > maxWidth) {
        return Math.round(100 * maxWidth / width * textReductionFactor);
      } else {
        return Math.round(100 * textReductionFactor);
      }
    });
  },

  drawMarriageDate: function(g, sosa, r, a1, a2, marriageDate, classes) {
    const pathId = "pmS" + sosa;

    // Réutilise createCircularPath qui remplace path1
    const pathLength = T.createCircularPath(g, pathId, r, a1, a2);

    // Réutilise placeTextOnPath qui remplace text2
    return this.placeTextOnPath(g, pathId, marriageDate, classes, pathLength, 10);
  }
};

// ========== Interface utilisateur ==========
const UIManager = {
  toggleSort: function() {
    const button = document.getElementById("b-sort-places");
    if (!button) return;
    const icon = button.querySelector("i");

    if (sortMode === "frequency") {
      sortMode = "alphabetical";
      icon.className = "fa fa-arrow-down-wide-short fa-fw";
      button.title = "Trier par fréquence";

      lieux_a.sort(function(e1, e2) {
        var place1 = LocationManager.parseLocationName(e1[0]);
        var place2 = LocationManager.parseLocationName(e2[0]);

        var mainComparison = place1.mainPlace.localeCompare(place2.mainPlace, 'fr', {
          sensitivity: 'base', ignorePunctuation: true, numeric: true
        });

        if (mainComparison !== 0) return mainComparison;
        if (!place1.isSubPlace && place2.isSubPlace) return -1;
        if (place1.isSubPlace && !place2.isSubPlace) return 1;

        if (place1.isSubPlace && place2.isSubPlace) {
          return place1.subPlace.localeCompare(place2.subPlace, 'fr', {
            sensitivity: 'base', ignorePunctuation: true, numeric: true
          });
        }
        return 0;
      });

    } else {
      sortMode = "frequency";
      icon.className = "fa fa-arrow-down-a-z fa-fw";
      button.title = "Trier par ordre alphabétique";
      lieux_a.sort(function(e1, e2) {
        return e2[1].cnt - e1[1].cnt;
      });
    }

    LocationManager.rebuildListVisualOnly();
  },

  addNavigationHelp: function() {
    var helpPanel = document.createElement('div');
    helpPanel.id = 'navigation-help';
    helpPanel.setAttribute('role', 'complementary');
    helpPanel.setAttribute('aria-label', 'Aide à la navigation');
    helpPanel.setAttribute('tabindex', '0');
    helpPanel.addEventListener('keydown', function(e) {
      if (e.key === 'Escape' || e.key === 'Enter') {
        helpPanel.style.display = 'none';
      }
    });
    helpPanel.innerHTML = `
    <div class="help-title">💡 Navigation</div>
    <div>– Ctrl+clic : fiche individuelle</div>
    <div>– <span class="text-success">▲</span> redéfinir la racine</div>
    `;

    setTimeout(() => {
      helpPanel.style.opacity = '0.5';
    }, 8000);

    helpPanel.onclick = function() {
      helpPanel.style.display = 'none';
    };

    document.body.appendChild(helpPanel);
  }
};

const ColorManager = {
  setColorMode: function(newMode) {
    // Nettoyer l'état précédent
    document.body.classList.remove('place_color', 'death-age', 'places-list');


    // Désactiver tous les toggles NMBDS
    ["bi", "ba", "ma", "de", "bu"].forEach(id => {
      const checkbox = document.getElementById(id);
      if (checkbox) checkbox.checked = false;
    });

    // Appliquer le nouveau mode
    if (newMode === 'place_color') {
      document.body.className = "places-list place_color";
      tool = "place_color";
      // Activer M par défaut
      const maCheckbox = document.getElementById("ma");
      if (maCheckbox) maCheckbox.checked = true;
      this.applyColorization();
    } else if (newMode === 'death-age') {
      document.body.className = "death-age";
      tool = "death-age";
    } else {
      document.body.className = "";
      tool = "";
    }

    // Mettre à jour la visibilité des contrôles
    this.updateControlsVisibility();

    // Mettre à jour l'état visuel des boutons
    this.updateButtonStates();

    // Synchroniser l'URL
    Utils.updateUrlWithCurrentState();
  },

  updateControlsVisibility: function() {
    const isPlaceColorActive = document.body.classList.contains('place_color');

    // Event toggles NMBDS
    const eventToggles = document.querySelector('.event-toggles');
    if (eventToggles) {
      eventToggles.style.display = isPlaceColorActive ? 'flex' : 'none';
    }

    // Bouton de tri
    const sortButton = document.getElementById("b-sort-places");
    if (sortButton) {
      sortButton.style.display = isPlaceColorActive ? 'inline-flex' : 'none';
    }
  },

  updateButtonStates: function() {
    // Tous les boutons utilisent la même classe .active
    const monoButton = document.getElementById("b-mono");
    const ageButton = document.getElementById("b-death-age");
    const placesButton = document.getElementById("b-places-colorise");
    const sortButton = document.getElementById("b-sort-places");

    if (monoButton) monoButton.classList.toggle("active", mono === "1");
    if (ageButton) ageButton.classList.toggle("active", tool === "death-age");
    if (placesButton) placesButton.classList.toggle("active", tool === "place_color");
    if (sortButton) sortButton.classList.toggle("active", sortMode === "alphabetical");
  },

  applyColorization: function() {
    var bi_checked = document.getElementById("bi").checked;
    var ba_checked = document.getElementById("ba").checked;
    var ma_checked = document.getElementById("ma").checked;
    var de_checked = document.getElementById("de").checked;
    var bu_checked = document.getElementById("bu").checked;

    const fanchart = document.getElementById("fanchart");
    fanchart.classList.toggle("bi", bi_checked);
    fanchart.classList.toggle("ba", ba_checked);
    fanchart.classList.toggle("ma", ma_checked);
    fanchart.classList.toggle("de", de_checked);
    fanchart.classList.toggle("bu", bu_checked);

    Utils.updateUrlWithCurrentState();
  },

  initializeColorEvents: function() {
    // Événements des checkboxes NMBDS
    ["bi", "ba", "ma", "de", "bu"].forEach(id => {
      const checkbox = document.getElementById(id);
      if (checkbox) {
        checkbox.onclick = this.applyColorization.bind(this);
      }
    });

    // Bouton colorisation lieux
    document.getElementById("b-places-colorise").onclick = function() {
      const isActive = document.body.classList.contains("place_color");

      if (isActive) {
        // Désactiver
        document.body.className = "";
        tool = "";
        this.classList.remove("active");

        // Tout désactiver
        document.getElementById("bi").checked = false;
        document.getElementById("ba").checked = false;
        document.getElementById("ma").checked = false;
        document.getElementById("de").checked = false;
        document.getElementById("bu").checked = false;
      } else {
        // Activer avec M par défaut uniquement
        document.body.className = "places-list place_color";
        tool = "place_color";
        this.classList.add("active");

        // Désactiver death-age si actif
        const ageButton = document.getElementById("b-death-age");
        if (ageButton) ageButton.classList.remove("active");

        // Activer seulement M
        document.getElementById("bi").checked = false;
        document.getElementById("ba").checked = false;
        document.getElementById("ma").checked = true;
        document.getElementById("de").checked = false;
        document.getElementById("bu").checked = false;
      }

      // Appliquer la colorisation
      ColorManager.applyColorization();
      ColorManager.updateControlsVisibility();
      Utils.updateUrlWithCurrentState();
    };

    // Bouton âges (exclusion mutuelle)
    document.getElementById("b-death-age").onclick = function() {
      const isActive = document.body.classList.contains("death-age");

      if (isActive) {
        // Désactiver complètement
        document.body.className = "";
        tool = "";
        this.classList.remove("active");
      } else {
        document.body.className = "death-age";
        tool = "death-age";
        this.classList.add("active");

        // Désactiver colorisation lieux si active
        const placesButton = document.getElementById("b-places-colorise");
        if (placesButton) placesButton.classList.remove("active");
      }

      Utils.updateUrlWithCurrentState();
    };

    // Masquer les contrôles au démarrage si nécessaire
    if (!has_ba) {
      const baLabel = document.getElementById("bal");
      if (baLabel) baLabel.style.display = "none";
    }
    if (!has_bu) {
      const buLabel = document.getElementById("bul");
      if (buLabel) buLabel.style.display = "none";
    }
  }
};

const LegendManager = {
  initializeAgeEvents: function() {
    const ageIds = ["DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6"];

    ageIds.forEach(function(id) {
      var element = document.getElementById(id);
      if (!element) return; // Protection si l'élément n'existe pas

      element.onmouseenter = function() {
        // Surligner tous les éléments avec cette classe
        var elements = document.getElementsByClassName(id);
        for (var i = 0; i < elements.length; i++) {
          elements[i].classList.add("highlight");
        }
        // Surligner l'élément de légende lui-même
        document.getElementById(id).classList.add("hl");
      };

      element.onmouseleave = function() {
        // Retirer le surlignage
        var elements = document.getElementsByClassName(id);
        for (var i = 0; i < elements.length; i++) {
          elements[i].classList.remove("highlight");
        }
        document.getElementById(id).classList.remove("hl");
      };
    });
  },

  initializeMarriageEvents: function() {
    const marriageIds = ["DAM0", "DAM1", "DAM2", "DAM3", "DAM4", "DAM5", "DAM6", "DAM7"];

    marriageIds.forEach(function(id) {
      var element = document.getElementById(id);
      if (!element) return; // Protection si l'élément n'existe pas

      element.onmouseenter = function() {
        // Surligner tous les éléments avec cette classe
        var elements = document.getElementsByClassName(id);
        for (var i = 0; i < elements.length; i++) {
          elements[i].classList.add("highlight");
        }
        // Surligner l'élément de légende lui-même
        document.getElementById(id).classList.add("hl");
      };

      element.onmouseleave = function() {
        // Retirer le surlignage
        var elements = document.getElementsByClassName(id);
        for (var i = 0; i < elements.length; i++) {
          elements[i].classList.remove("highlight");
        }
        document.getElementById(id).classList.remove("hl");
      };
    });
  },

  initializeAllEvents: function() {
    this.initializeAgeEvents();
    this.initializeMarriageEvents();
  }
};

const LocationManager = {
  parseLocationName: function(placeName) {
    // Regexp pour capturer « [sous-lieu] [–—-] lieu-principal »
    var match = placeName.match(/^(.+?)\s+[–—-]\s+(.+)$/);

    if (match) {
      return {
        mainPlace: match[2].trim(),      // "Montigny"
        subPlace: match[1].trim(),       // "[Bourg]" ou "Bourg"
        isSubPlace: true,
        original: placeName,             // "[Bourg] – Montigny"
        sortKey: match[2].trim().toLowerCase() + "|" + match[1].trim().toLowerCase()
      };
    }

    return {
      mainPlace: placeName,
      subPlace: null,
      isSubPlace: false,
      original: placeName,
      sortKey: placeName.toLowerCase() + "|"  // "|" place les lieux principaux avant leurs sous-lieux
    };
  },

  buildEventIndicators: function(index) {
    const events = [
      { id: 'bi', label: 'N', condition: has_bi },
      { id: 'ba', label: 'B', condition: has_ba },
      { id: 'ma', label: 'M', condition: has_ma },
      { id: 'de', label: 'D', condition: has_de },
      { id: 'bu', label: 'S', condition: has_bu }
    ];

    return events
      .filter(e => e.condition)
      .map(e => `<span id="${e.id}-L${index}" class="hidden">${e.label}</span>`)
      .join('');
  },

  buildLocationTooltip: function(lieuData) {
    if (!lieuData || typeof lieuData !== 'object') {
      return window.t('no_info_available', "Aucune information disponible");
    }

    var total = parseInt(lieuData.cnt) || 0;
    var details = [];

    var eventTypes = [
      { key: 'bi_count', singular: 'birth', plural: 'births' },
      { key: 'ba_count', singular: 'baptism', plural: 'baptisms' },
      { key: 'ma_count', singular: 'marriage', plural: 'marriages' },
      { key: 'de_count', singular: 'death', plural: 'deaths' },
      { key: 'bu_count', singular: 'burial', plural: 'burials' }
    ];

    eventTypes.forEach(function(event) {
      var count = parseInt(lieuData[event.key]) || 0;
      if (count > 0) {
        var labelKey = count > 1 ? event.plural : event.singular;
        var label = window.t(labelKey, labelKey);
        details.push(count + " " + label);
      }
    });

    var occurrenceKey = total > 1 ? 'occurrences' : 'occurrence';
    var occurrenceLabel = window.t(occurrenceKey, occurrenceKey);

    return total + " " + occurrenceLabel +
           (details.length > 0 ? " : " + details.join(", ") : "");
  },

  addPlaceCount: function(placeName, eventType) {
    if (placeName === undefined || placeName === "") return;

    var countKey = eventType + "_count";

    if (lieux[placeName] === undefined) {
      lieux[placeName] = {
        "cnt": 1
      };
      lieux[placeName][eventType] = true;
      lieux[placeName][countKey] = 1;
    } else {
      lieux[placeName].cnt++;
      lieux[placeName][eventType] = true;
      lieux[placeName][countKey] = (lieux[placeName][countKey] || 0) + 1;
    }
  },

  togglePlaceHl: function(locationKey, index, show) {
    const eventTypes = ["bi", "ba", "ma", "de", "bu"];

    eventTypes.forEach(function(ev) {
      // Toggle sur les éléments graphiques et textes
      [`${ev}-L${index}`, `${ev}-tL${index}`].forEach(className => {
        const elements = document.getElementsByClassName(className);
        const isText = className.includes('-t');
        
        for (const element of elements) {
          if (isText) {
            element.classList.toggle("text_highlight", show);
          } else {
            element.classList.toggle("highlight", show);
            element.classList.toggle("highlight-from-list", show);
          }
        }
      });

      // Toggle sur l'indicateur si le lieu a cet événement
      if (lieux[locationKey] && lieux[locationKey][ev]) {
        const indicator = document.getElementById(`${ev}-L${index}`);
        if (indicator) {
          indicator.classList.toggle("hidden", !show);
        }
      }
    });

    this.hlPlace(locationKey, show);
  },

  hlPlace: function(placeKey, highlight = true) {
    if (!placeKey || !lieux[placeKey]) return;

    var locationElement = document.getElementById(lieux[placeKey].c);

    if (locationElement) {
      if (highlight) {
        locationElement.classList.add("location-highlighted");
      } else {
        locationElement.classList.remove("location-highlighted");
      }
    }
  },

  createLocationStyles: function(index, c_h, c_l) {
    root.style.setProperty('--fc-color-' + index, 'hsl(' + c_h + ',100%,' + c_l + '%)');

    // Déterminer la couleur du texte selon le contraste
    var rb = Utils.contrastRatio('hsl(' + c_h + ',100%,' + c_l + '%)', 'black');
    var rw = Utils.contrastRatio('white', 'hsl(' + c_h + ',100%,' + c_l + '%)');
    if (rw > rb) {
      root.style.setProperty('--fc-text-color-' + index, 'white');
    }

    // Définir les types d'événements à styliser
    const eventTypes = [
      { prefix: 'bi', condition: true },
      { prefix: 'ba', condition: has_ba },
      { prefix: 'ma', condition: true },
      { prefix: 'de', condition: true },
      { prefix: 'bu', condition: has_bu }
    ];

    // Créer les règles CSS pour chaque type d'événement
    eventTypes.forEach(event => {
      if (event.condition) {
        sheet.insertRule(`body.place_color svg.${event.prefix} .${event.prefix}-L${index} {fill: var(--fc-color-${index}, transparent);}`);
        sheet.insertRule(`body.place_color svg.${event.prefix} .${event.prefix}-tL${index} {fill: var(--fc-text-color-${index}, black);}`);
      }
    });

    // Règle pour le carré coloré dans la liste
    sheet.insertRule(`body.place_color #L${index} .square { color: var(--fc-color-${index}, transparent); }`);
  },

  updateLocationListItem: function(li, placeName, displayIndex) {
    var locationInfo = LocationManager.parseLocationName(placeName);
    var originalIndex = parseInt(li.dataset.originalIndex);
    var showSubIndicator = false;

    if (sortMode === "alphabetical" && locationInfo.isSubPlace) {
      var previousLi = li.previousElementSibling;
      if (previousLi) {
        var previousPlace = previousLi.dataset.location;
        var previousInfo = LocationManager.parseLocationName(previousPlace);

        if (!previousInfo.isSubPlace && previousInfo.mainPlace === locationInfo.mainPlace) {
          showSubIndicator = true;
          li.classList.add("sublocation");
        }
      }
    } else {
      li.classList.remove("sublocation");
    }

    var subIndicator = showSubIndicator ? '<span class="sublocation-indicator">└</span>' : '';

    li.innerHTML =
      this.buildEventIndicators(originalIndex) +
      '<span class="square">■</span>' + subIndicator + ' ' + placeName;

    li.setAttribute("id", lieux[placeName].c);
    li.setAttribute("title", LocationManager.buildLocationTooltip(lieux[placeName]));
  },

  calculateAndBuild: function(maxGeneration) {
    lieux = {};
    lieux_a = [];

    let maxSosa;
    if (maxGeneration !== undefined) {
      maxSosa = Math.pow(2, maxGeneration + 1) - 1;
    } else {
      const ak = Object.keys(ancestor);
      maxSosa = Number(ak[ak.length-1].replace(/^S/, ""));
    }

    for (let sosa = 1; sosa <= maxSosa; sosa++) {
      const key = "S" + sosa;
      const p = ancestor[key];

      if (p) {
        this.addPlaceCount(p.birth_place, "bi");
        this.addPlaceCount(p.baptism_place, "ba");
        this.addPlaceCount(p.marriage_place, "ma");
        this.addPlaceCount(p.death_place, "de");
        this.addPlaceCount(p.burial_place, "bu");
      }
    }

    for (var key in lieux) {
      lieux_a.push([key, lieux[key]]);
    }
    lieux_a.sort(function(e1, e2) {
      return e2[1].cnt - e1[1].cnt;
    });

    this.buildInterface();
  },

  buildInterface: function() {
    this.buildList();
    this.buildHeader();
  },

  buildList: function() {
    var places_list = document.getElementById("places_list");
    places_list.innerHTML = "";

    var c_h = 0;
    var c_dh = 60;
    var c_l = 90;

    lieux_a.forEach(function(l, i) {
      var placeName = l[0];
      lieux[placeName].c = "L" + i;

      var li = document.createElement("li");
      li.dataset.location = placeName;
      li.dataset.index = i;
      li.dataset.originalIndex = i;

      LocationManager.updateLocationListItem(li, placeName, i);
      places_list.append(li);
      LocationManager.createLocationStyles(i, c_h, c_l);

      c_h += c_dh;
      if (c_h >= 360) {
        c_dh = Math.round(c_dh / 2);
        c_h = c_dh;
        c_l -= 15;
      }
    });

    this.initializeEvents();
  },

  rebuildListVisualOnly: function() {
    var places_list = document.getElementById("places_list");
    places_list.innerHTML = "";

    lieux_a.forEach(function(l, index) {
      var placeName = l[0];
      var originalIndex = parseInt(lieux[placeName].c.substring(1));

      var li = document.createElement("li");
      li.dataset.location = placeName;
      li.dataset.index = originalIndex;
      li.dataset.originalIndex = originalIndex;

      var locationInfo = LocationManager.parseLocationName(placeName);
      var showSubIndicator = false;

      if (sortMode === "alphabetical" && locationInfo.isSubPlace) {
        if (index > 0) {
          var previousPlace = lieux_a[index - 1][0];
          var previousInfo = LocationManager.parseLocationName(previousPlace);

          if (!previousInfo.isSubPlace && previousInfo.mainPlace === locationInfo.mainPlace) {
            showSubIndicator = true;
            li.classList.add("sublocation");
          }
        }
      }

      var subIndicator = showSubIndicator ? '<span class="sublocation-indicator">└</span>' : '';

      li.innerHTML =
        (has_bi ? '<span id="bi-L'+originalIndex+'" class="hidden">N</span>' : '') +
        (has_ba ? '<span id="ba-L'+originalIndex+'" class="hidden">B</span>' : '') +
        (has_ma ? '<span id="ma-L'+originalIndex+'" class="hidden">M</span>' : '') +
        (has_de ? '<span id="de-L'+originalIndex+'" class="hidden">D</span>' : '') +
        (has_bu ? '<span id="bu-L'+originalIndex+'" class="hidden">S</span>' : '') +
        '<span class="square">■</span>' + subIndicator + ' ' + placeName;

      li.setAttribute("id", lieux[placeName].c);
      li.setAttribute("title", LocationManager.buildLocationTooltip(lieux[placeName]));

      places_list.append(li);
    });

    this.initializeEvents();
  },

  buildHeader: function() {
    var placesContainer = document.getElementById("places-list");
    var existingHeader = document.getElementById('places-header');

    // Supprimer l'ancien header s'il existe
    if (existingHeader) {
      existingHeader.remove();
    }

    // Créer le nouveau header avec les bons comptages
    var header = this.createHeader();
    placesContainer.insertBefore(header, placesContainer.firstChild);
  },

  createHeader: function() {
    const totals = this.calculateTotals();
    const placesKey = totals.places > 1 ? 'places' : 'place';
    const placesLabel = window.t(placesKey, placesKey);
    const eventsKey = totals.events > 1 ? 'events' : 'event';
    const eventsLabel = window.t(eventsKey, eventsKey);

    var header = document.createElement('div');
    header.id = 'places-header';
    header.innerHTML = `
      <span style="color: #666;">
        ${max_gen} gén. : ${totals.places} ${placesLabel}, ${totals.events} ${eventsLabel}
      </span>
    `;
    return header;
  },

  calculateTotals: function() {
    var totalPlaces = Object.keys(lieux).length;
    var totalEvents = 0;

    Object.keys(lieux).forEach(function(placeName) {
      totalEvents += lieux[placeName].cnt;
    });

    return {
      places: totalPlaces,
      events: totalEvents
    };
  },

  initializeEvents: function() {
    const placesList = document.getElementById("places_list");
    if (!placesList) return;

    function getLocationData(target) {
      const li = target.closest("li[data-location]");
      if (!li) return null;

      const index = li.dataset.index;
      return {
        locationKey: li.dataset.location,
        index: index !== undefined ? parseInt(index) : null,
        element: li
      };
    }

    const placesContainer = document.getElementById("places-list");
    if (placesContainer) {
      placesContainer.addEventListener('click', function(e) {
        if (e.target.closest('#sort-toggle')) {
          e.preventDefault();
          UIManager.toggleSort();
        }
      });
    }

    placesList.addEventListener("mouseenter", function(e) {
      const data = getLocationData(e.target);
      if (data && data.index >= 0) {
        data.element.classList.add("hovered");
        LocationManager.togglePlaceHl(data.locationKey, data.index, true);
      }
    }, true);

    placesList.addEventListener("mouseleave", function(e) {
      const data = getLocationData(e.target);
      if (data && data.index >= 0) {
        data.element.classList.remove("hovered");
        LocationManager.togglePlaceHl(data.locationKey, data.index, false);
      }
    }, true);
  }
};

// ========== Application principale ==========
const FanchartApp = {
  window_w: 0,
  window_h: 0,
  zoom_factor: CONFIG.zoom_factor,

  // Méthodes de ViewManager intégrées
  zoom: function(zx, zy, factor, direction) {
    var w = svg_viewbox_w;
    var h = svg_viewbox_h;
    if (direction > 0) {
      h = Math.round(h/factor);
      w = Math.round(w/factor);
    } else {
      h = Math.round(h*factor);
      w = Math.round(w*factor);
    }
    this.set_svg_viewbox(
      svg_viewbox_x + Math.round(zx * (svg_viewbox_w - w) / this.window_w),
      svg_viewbox_y + Math.round(zy * (svg_viewbox_h - h) / this.window_h),
      w, h
    );
  },

  set_svg_viewbox: function(x, y, w, h) {
    svg_viewbox_x = x;
    svg_viewbox_y = y;
    svg_viewbox_w = w;
    svg_viewbox_h = h;
    fanchart.setAttribute("viewBox", x + " " + y + " " + w + " " + h);
  },

  fitScreen: function() {
    this.set_svg_viewbox(0, 0, svg_w, svg_h);
  },

  init: function() {
    this.calculateDimensions();
    this.processAncestorData();
    LocationManager.calculateAndBuild()
    this.renderFanchart();
    this.updateGenerationTitle();
    this.initializeEvents();
    ColorManager.initializeColorEvents();
    LegendManager.initializeAllEvents();
    this.applyInitialState();
    UIManager.addNavigationHelp();
    this.fitScreen();
  },

  processAncestorData: function() {
    // Nettoyer les données des ancêtres (une seule fois au chargement)
    var ak = Object.keys(ancestor);
    ak.forEach(function(s) {
      var p = ancestor[s];

      // Nettoyer les données et définir les flags
      if (p.birth_place !== undefined) {
        has_bi = true;
        ancestor[s].birth_place = p.birth_place.replace(/^\?, /, "");
      }
      if (p.baptism_place !== undefined) {
        has_ba = true;
        ancestor[s].baptism_place = p.baptism_place.replace(/^\?, /, "");
      }
      if (p.marriage_place !== undefined) {
        has_ma = true;
        ancestor[s].marriage_place = p.marriage_place.replace(/^\?, /, "");
      }
      if (p.death_place !== undefined) {
        has_de = true;
        ancestor[s].death_place = p.death_place.replace(/^\?, /, "");
      }
      if (p.burial_place !== undefined) {
        has_bu = true;
        ancestor[s].burial_place = p.burial_place.replace(/^\?, /, "");
      }
      if (p.death_age !== undefined) {
        ancestor[s].death_age = p.death_age.replace(/[^0123456789]/g, "");
      }
      // Nettoyer les dates (balises HTML)
      ancestor[s].dates = p.dates.replace(/\s?<\/?bdo[^>]*>/g, "");
      ancestor[s].dates = ancestor[s].dates.replace(/\bca\s+/g, "~");
    });
  },

  checkForImplexes: function() {
    for (let key in ancestor) {
      if (ancestor[key].fn === "=") {
        return true;
      }
    }
    return false;
  },

  hasParentsInNextGeneration: function() {
    const lastGenStart = Math.pow(2, max_gen);
    const lastGenEnd = Math.pow(2, max_gen + 1) - 1;

    for (let sosa = lastGenStart; sosa <= lastGenEnd; sosa++) {
      const person = ancestor["S" + sosa];
      if (person && person.has_parents) {
        return true;
      }
    }
    return false;
  },

  updateButtonStates: function() {
    // Gestion du bouton implexes
    const implexButton = document.getElementById("b-implex");
    if (implexButton) {
      if (this.checkForImplexes()) {
        implexButton.style.display = "inline-flex";
      } else {
        implexButton.style.display = "none";
      }
    }

    // Gestion intelligente du bouton ajouter génération
    const addButton = document.getElementById("b-gen-add");
    if (addButton) {
      let canAdd = false;
      let hasParentsAvailable = false;

      if (max_gen < max_gen_loaded) {
        canAdd = true;
      } else if (max_gen < 10) {
        hasParentsAvailable = FanchartApp.hasParentsInNextGeneration();
        canAdd = hasParentsAvailable;
      }

      addButton.classList.toggle("disabled", !canAdd);
      addButton.disabled = !canAdd;

      if (max_gen < max_gen_loaded) {
        addButton.title = "Afficher la génération suivante (données en mémoire)";
      } else if (hasParentsAvailable) {
        addButton.title = "Charger la génération suivante";
      } else {
        addButton.title = "Aucun parent dans la génération suivante";
      }
    }
  },

  calculateDimensions: function() {
    // Calculer max_gen depuis ancestor
    var ak = Object.keys(ancestor);
    max_gen_loaded = Math.trunc(Math.log(Number(ak[ak.length-1].replace(/^S/, "")))/Math.log(2));

    if (typeof max_gen === 'undefined') {
      max_gen = max_gen_loaded;
    }
    if (max_gen > max_gen_loaded) {
      max_gen = max_gen_loaded;
      Utils.updateUrlWithCurrentState();
    }
    // Calculer max_r avec validation
    max_r = 0;
    for (var i = 0; i < max_gen+1 && i < CONFIG.a_r.length; i++) {
      max_r += CONFIG.a_r[i];
    }

    if (isNaN(max_r) || max_r <= 0) {
      console.error("max_r invalide:", max_r);
      max_r = 300;
    }

    // Définir les dimensions du SVG avec validation
    center_x = max_r + CONFIG.svg_margin;
    center_y = max_r + CONFIG.svg_margin;
    svg_w = 2 * center_x;
    svg_h = 2 * CONFIG.svg_margin + max_r +
            Math.max(CONFIG.a_r[0], Math.round(max_r * Math.sin(Math.PI/180*(CONFIG.d_all-180)/2)));
    if (isNaN(svg_w) || isNaN(svg_h) || svg_w <= 0 || svg_h <= 0) {
      console.error("Dimensions SVG calculées invalides:", { svg_w, svg_h, max_r, center_x, center_y });
      svg_w = 800;
      svg_h = 600;
      center_x = svg_w / 2;
      center_y = svg_h / 2;
    }

    // Dimensions de la fenêtre
    this.window_h = window.innerHeight;
    this.window_w = Math.round(this.window_h * svg_w / svg_h);

    // Configurer le SVG
    fanchart.setAttribute("height", this.window_h);
    fanchart.setAttribute("width", this.window_w);
    root.style.setProperty('--fc-tool-size', (window.innerWidth - this.window_w) + "px");

    // Initialisation de la viewbox
    svg_viewbox_x = 0;
    svg_viewbox_y = 0;
    svg_viewbox_w = svg_w;
    svg_viewbox_h = svg_h;
  },

  renderFanchart: function() {
    standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
    standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
    standard.setAttribute("id", "standard");
    standard.setAttribute("x", center_x);
    standard.setAttribute("y", center_y);
    fanchart.append(standard);
    standard_width = standard.getBBox().width / standard.textContent.length;

    var gen = 1;
    var sosa = 1;
    var r1 = 0;
    var r2 = CONFIG.a_r[0];
    var a1, a2;
    var delta = CONFIG.d_all;

    // Création du groupe pour sosa 1
    var g1 = document.createElementNS("http://www.w3.org/2000/svg", "g");
    g1.setAttribute("id", "S"+sosa);
    fanchart.append(g1);

    SVGRenderer.drawCircle(g1, r2, center_x, center_y, ancestor["S"+sosa], { isBackground: true });
    T.drawText(g1, 'S1', { x: center_x, y: center_y - 10, p: ancestor["S"+sosa], classes: "" });
    SVGRenderer.drawCircle(g1, r2, center_x, center_y, ancestor["S"+sosa]);

    while(true) {
      sosa++;
      if(sosa >= (2 ** gen)) {
        gen++;
        if(gen >= CONFIG.a_r.length+1 || gen > max_gen + 1) {
          break;
        }
        delta = delta / 2;
        r1 = r2;
        r2 = r1 + CONFIG.a_r[gen-1];
        a1 = -90 - CONFIG.d_all/2;
        a2 = a1 + delta;
      } else {
        a1 += delta;
        a2 += delta;
      }
      var p = ancestor["S"+sosa];
      if(p !== undefined) {
        var pg = document.createElementNS("http://www.w3.org/2000/svg", "g");
        pg.setAttribute("id", "S"+sosa);
        fanchart.append(pg);

        var same = (p.fn == "=" ? true : false);
        if(same && implex != "") {
          var p2 = ancestor["S"+(2 * p.sn)];
          if(p2 !== undefined) {
            ancestor["S"+(2*sosa)] = { "fn" : "=", "sn": 2*p.sn, "fnk": p2.fnk, "snk": p2.snk, "oc": p2.oc, "dates": "", "has_parents": p2.has_parents };
          }
          p2 = ancestor["S"+(2*p.sn+1)];
          if(p2 !== undefined) {
            ancestor["S"+(2*sosa+1)] = { "fn" : "=", "sn": 2*p.sn+1, "fnk": p2.fnk, "snk": p2.snk, "oc": p2.oc, "dates": "", "has_parents": p2.has_parents };
          }
          p = ancestor["S"+p.sn];
          same = false;
        }
        SVGRenderer.drawPie(pg, r1+10, r2, a1, a2, p, { type: 'person', isBackground: true });
        if(p.fn != "?") {
          var c = "";
          if(p.birth_place !== undefined && p.birth_place != "") {
            c += " bi-t"+lieux[p.birth_place].c;
          }
          if(p.baptism_place !== undefined && p.baptism_place != "") {
            c += " ba-t"+lieux[p.baptism_place].c;
          }
          if(p.death_place !== undefined && p.death_place != "") {
            c += " de-t"+lieux[p.death_place].c;
          }
          if(p.burial_place !== undefined && p.burial_place != "") {
            c += " bu-t"+lieux[p.burial_place].c;
          }
          SVGRenderer.drawSectorText(pg, r1, r2, a1, a2, sosa, p, c, gen, same);
        }
        if(sosa % 2 == 0) {
          SVGRenderer.drawPie(pg, r1, r1+10, a1, a2+delta, p, { type: 'marriage', isBackground: true });
          if(p.marriage_date !== undefined) {
            var c = "";
            if(p.marriage_place !== undefined && p.marriage_place != "") {
              c += " ma-t"+lieux[p.marriage_place].c;
            }
            T.drawMarriageDate(pg, sosa, r1+5, a1, a2+delta, p.marriage_date, c);
          }
          SVGRenderer.drawContour(pg, r1, r2, a1, a2+delta);
          SVGRenderer.drawRadialLine(pg, r1+10, r2, a2);
          SVGRenderer.drawPie(pg, r1, r1+10, a1, a2+delta, p, { type: 'marriage' });
        } else {
          ancestor["S"+sosa].marriage_place = ancestor["S"+(sosa-1)].marriage_place;
        }
        SVGRenderer.drawPie(pg, r1+10, r2, a1, a2, p, { type: 'person' });
        SVGRenderer.drawParentIndicator(pg, r1+10, a1, a2, sosa, p);
      }
    }
    this.updateButtonStates();
  },

  updateGenerationTitle: function() {
    const genTitle = document.getElementById('generation-section-title');
    if (genTitle) {
      const genLabel = window.t(max_gen > 1 ? 'generations' : 'generation',
                               max_gen > 1 ? 'générations' : 'génération');
      genTitle.textContent = `${max_gen} ${genLabel}`;
    }
  },

  reRenderWithCurrentGenerations: function() {
    this.calculateDimensions();
    LocationManager.calculateAndBuild(max_gen);
    const fanchart = document.getElementById("fanchart");
    fanchart.innerHTML = "";
    this.renderFanchart();
    this.fitScreen();
    this.updateButtonStates();
    this.updateGenerationTitle();
  },

  initializeEvents: function() {
    // Zoom
    fanchart.addEventListener("wheel", (event) => {
      this.zoom(event.clientX, event.clientY, CONFIG.zoom_factor,
      (event.deltaY < 0 ? +1 : -1));
    }, { passive: false });

    // Drag
    var drag_state = false;
    fanchart.onmousedown = function(e) {
      e.preventDefault();
      drag_state = true;
    };
    fanchart.onmouseup = () => drag_state = false;
    fanchart.onmousemove = (e) => {
      if (drag_state) {
        e.preventDefault();
        this.set_svg_viewbox(
          svg_viewbox_x - Math.round(e.movementX * svg_viewbox_w / this.window_w),
          svg_viewbox_y - Math.round(e.movementY * svg_viewbox_h / this.window_h),
          svg_viewbox_w, svg_viewbox_h
        );
      }
    };

    // Boutons de navigation
    document.getElementById("b-no-buttons").onclick = function() {
      document.getElementById("fanchart-controls").style.display = "none";
    };
    document.getElementById("b-home").onclick = () => {
      window.location = link_to_person;
    };
    document.getElementById("b-rng").onclick = function() {
      const url = this.getAttribute("data-url");
      if (url) { window.location = url; }
    };
    document.getElementById("b-refresh").onclick = () => {
      this.fitScreen();
    };
    document.getElementById("b-zoom-in").onclick = () => {
      this.zoom(this.window_w / 2, this.window_h / 2, this.zoom_factor, +1);
    };
    document.getElementById("b-zoom-out").onclick = () => {
      this.zoom(this.window_w / 2, this.window_h / 2, this.zoom_factor, -1);
    };
    document.getElementById("b-gen-add").onclick = () => {
      if (this.disabled) return;
      if (max_gen < max_gen_loaded) {
        max_gen++;
        FanchartApp.reRenderWithCurrentGenerations();
        Utils.updateUrlWithCurrentState();
      } else {
        Utils.navigateWithParams(max_gen + 1);
      }
    };
    document.getElementById("b-gen-del").onclick = () => {
      if(max_gen > 1) {
        max_gen--;
        FanchartApp.reRenderWithCurrentGenerations();
        Utils.updateUrlWithCurrentState();
      }
    };
    document.getElementById("b-implex").onclick = () => {
      implex = (implex === "0") ? "" : "0";
      Utils.navigateWithParams(max_gen);
    };
    document.getElementById("b-mono").onclick = () => {
      const fanchart = document.getElementById("fanchart");
      const isCurrentlyMono = fanchart.classList.contains("mono");
      fanchart.classList.toggle("mono", !isCurrentlyMono);
      mono = isCurrentlyMono ? "" : "1";
      ColorManager.updateButtonStates();
      Utils.updateUrlWithCurrentState();
    };
    document.getElementById("b-sort-places").onclick = () => {
      if (!document.body.classList.contains('place_color')) return;
      UIManager.toggleSort();
    };
  },

  applyInitialState: function() {
    // Configurer l'état initial des outils
    if (tool == "death-age") {
      document.body.className = "death-age";
      const ageButton = document.getElementById("b-death-age");
      if (ageButton) ageButton.classList.add("active");
    } else {
      document.body.className = "places-list place_color";
      tool = "place_color";
      const placesButton = document.getElementById("b-places-colorise");
      if (placesButton) placesButton.classList.add("active");

      const maCheckbox = document.getElementById("ma");
      if (maCheckbox) maCheckbox.checked = true;
      ColorManager.applyColorization();
    }

    const monoButton = document.getElementById("b-mono");
    const fanchart = document.getElementById("fanchart");
    if (monoButton && fanchart) {
      if (mono === "1") {
        fanchart.classList.add("mono");
        monoButton.classList.add("active");
      }
    }
  }
};

// Alias pour la rétrocompatibilité
const R = SVGRenderer;
const T = TextRenderer;

// Lancement de l'application
FanchartApp.init();