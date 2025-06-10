const root = document.documentElement;
const fanchart = document.getElementById("fanchart");
const places_list = document.getElementById("places_list");
const pixel = document.getElementById("pixel").getContext("2d", { willReadFrequently: true });
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
  default_angle: 220,
  available_angles: [180, 220, 260],
  a_r: [50, 50, 50, 50, 80, 70, 100, 150, 130, 90],
  a_m: ["S1", "C3", "C3", "C3", "R3", "R3", "R2", "R1", "R1", "R1"],
  marriage_length_thresholds: [4, 14, 24, 34, 44, 54],
  text_reduction_factor: 0.9,
  svg_margin: 5
};

let current_angle = CONFIG.default_angle;

// 2. Fonction pour récupérer l'angle depuis l'URL
function getAngleFromURL() {
  const urlParams = new URLSearchParams(window.location.search);
  const angleParam = urlParams.get('angle');
  
  if (angleParam) {
    const angle = parseInt(angleParam);
    // Vérifier que l'angle est valide
    if (CONFIG.available_angles.includes(angle)) {
      return angle;
    }
  }
  
  return CONFIG.default_angle;
}

// 3. Fonction pour mettre à jour l'URL avec le nouvel angle
function updateURLWithAngle(angle) {
  const urlParams = new URLSearchParams(window.location.search);
  
  if (angle === CONFIG.default_angle) {
    // Si c'est l'angle par défaut, on peut omettre le paramètre
    urlParams.delete('angle');
  } else {
    urlParams.set('angle', angle);
  }
  
  const newURL = window.location.pathname + '?' + urlParams.toString();
  history.replaceState(null, '', newURL);
}

// Créer un module DOMCache au début du fichier, après CONFIG
const DOMCache = {
  // Cache pour les éléments individuels
  elements: {},

  // Cache pour les collections d'éléments par classe
  collections: {},

  // Récupérer un élément par ID avec mise en cache
  getElementById: function(id) {
    if (!this.elements[id]) {
      this.elements[id] = document.getElementById(id);
    }
    return this.elements[id];
  },

  // Récupérer des éléments par classe avec mise en cache
  getElementsByClassName: function(className) {
    if (!this.collections[className]) {
      // Convertir en Array pour avoir une référence stable
      this.collections[className] = Array.from(document.getElementsByClassName(className));
    }
    return this.collections[className];
  },

  // Invalider le cache quand le DOM change
  invalidate: function(type = 'all') {
    if (type === 'all') {
      this.elements = {};
      this.collections = {};
    } else if (type === 'collections') {
      this.collections = {};
    }
  },

  // Pré-charger les éléments fréquemment utilisés
  preload: function() {
    // Boutons fréquemment utilisés
    ["b-death-age", "b-places-colorise", "b-sort-places"].forEach(id => {
      this.getElementById(id);
    });
    // s indicateurs d'âge et de mariage
    ["DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6"].forEach(id => {
      this.getElementById(id);
    });
    ["DAM0", "DAM1", "DAM2", "DAM3", "DAM4", "DAM5", "DAM6"].forEach(id => {
      this.getElementById(id);
    });
  }
};

// ========== Utilitaires généraux ==========
const Utils = {
  calculateAgeCategory: function(age) {
    const boundaries = [30, 45, 60, 75, 90, 105, Infinity];
    const category = boundaries.findIndex(boundary => age < boundary);
    return Math.min(category, 6);
  },

  deathAgeClass: function(age) {
    return "DA" + this.calculateAgeCategory(age);
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
    let url = `m=A&t=FC&p=${p.fnk}&n=${p.snk}`;
    if (p.oc) url += `&oc=${p.oc}`;
    if (tool && tool !== "") url += `&tool=${tool}`;
    if (implexMode === "numbered") url += "&implex=num";
    else if (implexMode === "full") url += "&implex=full";
    if (current_angle !== CONFIG.default_angle) {
      url += `&angle=${current_angle}`;
    }
    return url;
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
    let classes = ['bg'];

    if (type === 'person') {
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
    if (!p || (p.fn === "?" || (!p.fn && !p.sosasame && type !== 'marriage'))) return;

    element.setAttribute("class", "link");
    const panel = document.getElementById("person-panel");

    // Gestion du clic avec propagation contrôlée
    element.addEventListener("click", (e) => {
      e.stopPropagation();
      this.handleClick(e, p);
    });

    element.addEventListener("mouseenter", (e) => {
      e.stopPropagation();
      if (panel) {
        this.buildTooltipContent(panel, p, type);
        panel.style.display = "block";
      }
      this.handleMouseEnter(p, type, e);
    });

    element.addEventListener("mouseleave", (e) => {
      e.stopPropagation();
      if (panel) {
        panel.style.display = "none";
        panel.innerHTML = "";
      }
      this.handleMouseLeave(p, type, e);
    });
  },

  buildTooltipContent: function(panel, p, type) {
    if (type === "person") {
      panel.innerHTML = `
        <h2>${p.fn} ${p.sn}</h2>
        <div class="subtitle">${p.dates}${p.death_age && !isNaN(parseInt(p.death_age)) ? ` ${p.death_age} ans` : ""}</div>
        ${p.birth_place ? `<div><strong>Naissance :</strong> ${p.birth_place}</div>` : ""}
        ${p.death_place ? `<div><strong>Décès :</strong> ${p.death_place}</div>` : ""}
      `;
    } else if (type === "marriage") {
      const years = parseInt(p.marriage_length) || -1;
      panel.innerHTML = `
        <h2>Mariage</h2>
        ${p.marriage_date ? `<div><strong>Date :</strong> ${p.marriage_date}</div>` : ""}
        ${p.marriage_place ? `<div><strong>Lieu :</strong> ${p.marriage_place}</div>` : ""}
        ${years >= 0 ? `<div><strong>Durée :</strong> ${years} ${years === 1 ? "an" : "ans"}</div>` : ""}
      `;
    }
  },

  handleClick: function(e, person) {
    if (!link_to_person) {
      alert("Erreur: Impossible d'accéder à la fiche individuelle");
      return;
    }
    const li = e.target.closest('li[data-location]');

    // Clic sur une personne (secteur du fanchart)
    if (person && person.fnk && person.snk) {
      const useNewTab = e.ctrlKey || e.metaKey;
      NavigationHelper.openPersonLink(person, useNewTab, false);
      return;
    }

    // Clic sur un lieu en mode wizard
    if (li && document.body.dataset.wizard === "1") {
      e.preventDefault();
      const placeName = li.dataset.location;
      const useNewTab = e.ctrlKey || e.metaKey;
      NavigationHelper.openPlaceLink(placeName, useNewTab);
      return;
    }

    // Autres clics (bouton de tri, etc.)
    if (e.target.closest('#sort-toggle')) {
      e.preventDefault();
      UIManager.toggleSort();
    }
  },

  togglePlaceHighlights: function(p, show, type) {
    if (type === 'marriage') {
      // Pour les mariages, seulement le lieu de mariage
      if (p.marriage_place) {
        const el = DOMCache.getElementById(`ma-${lieux[p.marriage_place].c}`);
        if (el) el.classList.toggle("hidden", !show);
        LocationManager.hlPlace(p.marriage_place, show);
      }
    } else {
      const places = [
        { prop: 'birth_place', prefix: 'bi' },
        { prop: 'baptism_place', prefix: 'ba' },
        { prop: 'marriage_place', prefix: 'ma' },
        { prop: 'death_place', prefix: 'de' },
        { prop: 'burial_place', prefix: 'bu' }
      ];

      places.forEach(place => {
        if (p[place.prop]) {
          const el = DOMCache.getElementById(`${place.prefix}-${lieux[p[place.prop]].c}`);
          if (el) el.classList.toggle("hidden", !show);
          LocationManager.hlPlace(p[place.prop], show);
        }
      });
    }
  },

  handleMouseEnter: function(p, type, event) {
    // Gestion des lieux
    this.togglePlaceHighlights(p, true, type);

    // Gestion du background
    if (event && event.currentTarget) {
      const group = event.currentTarget.parentNode;
      const backgroundSector = group.querySelector('.bg');

      if (backgroundSector && backgroundSector.style.fill !== "lightgrey") {
        backgroundSector.style.fill = "lightgrey";
        backgroundSector.dataset.highlighted = "true";
      }
    }

    // Gestion spécifique par type
    if (type === 'person' && p.death_age) {
      const ageEl = DOMCache.getElementById(Utils.deathAgeClass(p.death_age));
      if (ageEl) ageEl.classList.add("hl");
    } else if (type === 'marriage' && p.marriage_length) {
      const marriageClass = Utils.marriageLengthClass(p.marriage_length);
      if (marriageClass) {
        const marriageEl = document.getElementById(marriageClass);
        if (marriageEl) marriageEl.classList.add("hl");
      }
    }

    // Gestion des implexes
    if (p.sosasame) {
      const ref = document.getElementById("S" + p.sosasame);
      if (ref) ref.classList.add("same_hl");
    }
  },

  handleMouseLeave: function(p, type, event) {
    // Gestion des lieux
    this.togglePlaceHighlights(p, false, type);

    // Gestion du background
    if (event && event.currentTarget) {
      const group = event.currentTarget.parentNode;
      const backgroundSector = group.querySelector('.bg');
      
      if (backgroundSector) {
        backgroundSector.style.fill = "";
        delete backgroundSector.dataset.highlighted;
      }
    }

    // Gestion spécifique par type
    if (type === 'person' && p.death_age) {
      const ageEl = DOMCache.getElementById(Utils.deathAgeClass(p.death_age));
      if (ageEl) ageEl.classList.remove("hl");
    } else if (type === 'marriage' && p.marriage_length) {
      const marriageClass = Utils.marriageLengthClass(p.marriage_length);
      if (marriageClass) {
        const marriageEl = document.getElementById(marriageClass);
        if (marriageEl) marriageEl.classList.remove("hl");
      }
    }

    // Gestion des implexes
    if (p.sosasame) {
      const ref = document.getElementById("S" + p.sosasame);
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
        const useNewTab = e.ctrlKey || e.metaKey;
        NavigationHelper.openPersonLink(p, false, true);
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
  _bboxCache: {},

  getBBoxCached: function(textContent) {
    if (!this._bboxCache[textContent]) {
      standard.textContent = textContent;
      this._bboxCache[textContent] = standard.getBBox();
    }
    return this._bboxCache[textContent];
  },

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
    const bbox = this.getBBoxCached(textContent);
    const textWidth = bbox.width;
    const textHeight = bbox.height;

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
      const bbox = this.getBBoxCached(text);
      const width = bbox.width;

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
    } else {
      sortMode = "frequency";
      icon.className = "fa fa-arrow-down-a-z fa-fw";
      button.title = "Trier par ordre alphabétique";
    }

    const headerIcon = document.getElementById('header-sort-icon');
    if (headerIcon) {
      headerIcon.className = sortMode === "alphabetical"
        ? 'fa fa-arrow-down-a-z'
        : 'fa fa-arrow-down-wide-short';
    }

    LocationManager.rebuildListVisualOnly();
  },

  addNavigationHelp: function() {
    var helpPanel = document.createElement('div');
    helpPanel.id = 'navigation-help';
    helpPanel.style.display = 'none'; // Caché par défaut
    helpPanel.innerHTML = `
      <div class="help-title">💡 Aide Navigation</div>
      <div><strong>Souris :</strong></div>
      <div>– Glisser : déplacer l'arbre</div>
      <div>– Molette : zoomer</div>
      <div>– Survol : voir les détails</div>
      <div><strong>Raccourcis :</strong></div>
      <div>– Ctrl+clic : fiche individuelle</div>
      <div>– ▲ clic : navigation ancêtre</div>
      <div style="margin-top: 8px; text-align: center;">
      </div>
    `;
    document.body.appendChild(helpPanel);
  },
};

const ColorManager = {
  EVENT_TYPES: ["bi", "ba", "ma", "de", "bu"],

  setColorMode: function(newMode) {
    // Nettoyer l'état précédent
    document.body.classList.remove('place_color', 'death-age', 'places-list');


    // Désactiver tous les toggles NMBDS
    this.EVENT_TYPES.forEach(id => {
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
    const ageButton = document.getElementById("b-death-age");
    const placesButton = document.getElementById("b-places-colorise");
    const sortButton = document.getElementById("b-sort-places");

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
    this.EVENT_TYPES.forEach(id => {
      const checkbox = document.getElementById(id);
      if (checkbox) checkbox.onclick = this.applyColorization.bind(this);
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
        ColorManager.EVENT_TYPES.forEach(id => {
          document.getElementById(id).checked = false;
        });
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
  initializeLegendEvents: function(ids) {
    ids.forEach(function(id) {
      var element = document.getElementById(id);
      if (!element) return;

      element.onmouseenter = function() {
        var elements = document.getElementsByClassName(id);
        for (var i = 0; i < elements.length; i++) {
          elements[i].classList.add("highlight");
        }
        document.getElementById(id).classList.add("hl");
      };

      element.onmouseleave = function() {
        var elements = document.getElementsByClassName(id);
        for (var i = 0; i < elements.length; i++) {
          elements[i].classList.remove("highlight");
        }
        document.getElementById(id).classList.remove("hl");
      };
    });
  },

  initializeAllEvents: function() {
    this.initializeLegendEvents(["DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6"]);
    this.initializeLegendEvents(["DAM0", "DAM1", "DAM2", "DAM3", "DAM4", "DAM5", "DAM6", "DAM7"]);
  }
};

const NavigationHelper = {
  openPersonLink: function(person, newTab = false, stayInFanchart = false) {
    if (!person || !person.fnk || !person.snk) return false;

    const oc = person.oc ? `&oc=${person.oc}` : '';
    let url;

    if (stayInFanchart) {
      url = `${link_to_person}p=${person.fnk}&n=${person.snk}${oc}&m=A&t=FC&v=${max_gen}`;
      if (tool && tool !== "") url += `&tool=${tool}`;
    } else {
      // Navigation externe: va vers fiche individuelle
      url = `${link_to_person}p=${person.fnk}&n=${person.snk}${oc}`;
    }

    if (newTab) {
      window.open(url, '_blank');
    } else {
      window.location.href = url;
    }
    return true;
  },

  openPlaceLink: function(placeName, newTab = false) {
    const searchTerm = placeName.length > 2 ? placeName.slice(0, -2) : placeName;
    const url = `${link_to_person}m=MOD_DATA&data=place&s=${encodeURIComponent(searchTerm)}&s1=${encodeURIComponent(placeName)}`;

    if (newTab) {
      window.open(url, '_blank');
    } else {
      window.location.href = url;
    }
    return true;
  }
};


const LocationManager = {
  _sortedIndexes: {
    frequency: null,
    alphabetical: null
  },

  getSortedData: function() {
    if (!this._sortedIndexes[sortMode]) {
      if (sortMode === "frequency") {
        this._sortedIndexes.frequency = [...lieux_a].sort(function(e1, e2) {
          return e2[1].cnt - e1[1].cnt;
        });
      } else { // alphabetical
        this._sortedIndexes.alphabetical = [...lieux_a].sort(function(e1, e2) {
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
      }
    }
    return this._sortedIndexes[sortMode];
  },

  invalidateSortCache: function() {
    this._sortedIndexes = { frequency: null, alphabetical: null };
  },

  parseLocationName: function(placeName) {
    // Extraction du pattern de matching en constante
    const SUBLOCATION_PATTERN = /^(.+?)\s+[–—-]\s+(.+)$/;
    const match = placeName.match(SUBLOCATION_PATTERN);

    // Structure de retour cohérente
    const baseStructure = {
      original: placeName,
      isSubPlace: false,
      mainPlace: placeName,
      subPlace: null
    };

    if (!match) {
      return {
        ...baseStructure,
        sortKey: placeName.toLowerCase() + "|"
      };
    }

    return {
      ...baseStructure,
      mainPlace: match[2].trim(),
      subPlace: match[1].trim(),
      isSubPlace: true,
      sortKey: match[2].trim().toLowerCase() + "|" + match[1].trim().toLowerCase()
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

  calculateHighlightState: function(locationKey, index, eventTypes) {
    const state = {
      elementsToHighlight: [],
      elementsToShow: [],
      indicatorsToToggle: []
    };

    eventTypes.forEach(eventType => {
      if (lieux[locationKey] && lieux[locationKey][eventType]) {
        state.elementsToHighlight.push({
          className: `${eventType}-L${index}`,
          textClassName: `${eventType}-tL${index}`
        });
        state.indicatorsToToggle.push(`${eventType}-L${index}`);
      }
    });

    return state;
  },

  applyHighlightState: function(state, show) {
    // Cette fonction ne contient que de la manipulation DOM
    state.elementsToHighlight.forEach(({ className, textClassName }) => {
      // Manipulation des éléments graphiques
      const graphElements = DOMCache.getElementsByClassName(className);
      graphElements.forEach(el => {
        el.classList.toggle("highlight", show);
        el.classList.toggle("highlight-from-list", show);
      });

      // Manipulation des éléments texte
      const textElements = DOMCache.getElementsByClassName(textClassName);
      textElements.forEach(el => {
        el.classList.toggle("text_highlight", show);
      });
    });

    // Toggle des indicateurs
    state.indicatorsToToggle.forEach(id => {
      const indicator = DOMCache.getElementById(id);
      if (indicator) {
        indicator.classList.toggle("hidden", !show);
      }
    });
  },


  togglePlaceHl: function(locationKey, index, show) {
    const state = this.calculateHighlightState(locationKey, index , ["bi", "ba", "ma", "de", "bu"]);
    this.applyHighlightState(state, show);
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

    // Structure HTML simple - pas de liens imbriqués
    li.innerHTML = this.buildEventIndicators(originalIndex) +
                   '<span class="square">■</span>' +
                   subIndicator + ' ' + placeName;

    li.setAttribute("id", lieux[placeName].c);
    li.setAttribute("title", this.buildLocationTooltip(lieux[placeName]));

    // Indiquer visuellement que c'est cliquable en mode wizard
    if (document.body.dataset.wizard === "1") {
      li.classList.add("clickable-place");
    }
  },

  shouldShowSubIndicator: function(li, locationInfo) {
    if (!locationInfo.isSubPlace) return false;
    const previousLi = li.previousElementSibling;
    if (!previousLi) return false;

    const previousPlace = previousLi.dataset.location;
    const previousInfo = this.parseLocationName(previousPlace);
    return !previousInfo.isSubPlace && previousInfo.mainPlace === locationInfo.mainPlace;
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

    this.invalidateSortCache();
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

    LocationManager.getSortedData().forEach(function(l, i) {
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

    this.initializeLocationEvents();
  },

  rebuildListVisualOnly: function() {
    var places_list = document.getElementById("places_list");
    places_list.innerHTML = "";

    DOMCache.invalidate();

    LocationManager.getSortedData().forEach(function(l, visualIndex) {
      var placeName = l[0];
      var originalIndex = parseInt(lieux[placeName].c.substring(1));

      var li = document.createElement("li");
      li.dataset.location = placeName;
      li.dataset.index = originalIndex;
      li.dataset.originalIndex = originalIndex;

      LocationManager.updateLocationListItem(li, placeName, visualIndex);
      places_list.append(li);
    });

    this.initializeLocationEvents();
  },

  buildHeader: function() {
    var placesContainer = document.getElementById("places-list");
    var existingHeader = document.getElementById('places-header');

    if (existingHeader) { existingHeader.remove(); }

    var header = this.createHeader();
    placesContainer.insertBefore(header, placesContainer.firstChild);
  },

  createHeader: function() {
    const totals = this.calculateTotals();
    const placesKey = totals.places > 1 ? 'places' : 'place';
    const placesLabel = window.t(placesKey, placesKey);
    const eventsKey = totals.events > 1 ? 'events' : 'event';
    const eventsLabel = window.t(eventsKey, eventsKey);

    const sortIconClass = sortMode === "alphabetical"
      ? 'fa-arrow-down-a-z'
      : 'fa-arrow-down-wide-short';

    var header = document.createElement('div');
    header.id = 'places-header';
    header.innerHTML = `
      <span style="color: #666;">
        ${max_gen} gén. : ${totals.places} ${placesLabel}, ${totals.events} ${eventsLabel}
        <i id="header-sort-icon" class="fa ${sortIconClass}" style="font-size: 12px; margin-left: 8px;"></i>
      </span>
    `;
    return header;
  },

  calculateTotals: function() {
    const totals = Object.values(lieux).reduce((acc, lieu) => {
      return {
        places: acc.places + 1,
        events: acc.events + (lieu.cnt || 0)
      };
    }, { places: 0, events: 0 });

    return totals;
  },

  initializeLocationEvents: function() {
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

    // Survol identique à la version qui marchait
    placesList.addEventListener("mouseenter", (e) => {
      const data = getLocationData(e.target);
      if (data) {
        data.element.classList.add("hovered");
        LocationManager.togglePlaceHl(data.locationKey, data.index, true);
      }
    }, true);

    placesList.addEventListener("mouseleave", (e) => {
      const data = getLocationData(e.target);
      if (data) {
        data.element.classList.remove("hovered");
        LocationManager.togglePlaceHl(data.locationKey, data.index, false);
      }
    }, true);

    placesList.addEventListener("click", (e) => {
      if (document.body.dataset.wizard !== "1") return;
      const data = getLocationData(e.target);
      if (data) {
        e.preventDefault();
        NavigationHelper.openPlaceLink(data.locationKey, true);
      }
    });

    // Gestionnaire pour le tri (délégation aussi)
    const placesContainer = document.getElementById("places-list");
    if (placesContainer) {
      placesContainer.addEventListener('click', function(e) {
        if (e.target.closest('#sort-toggle')) {
          e.preventDefault();
          UIManager.toggleSort();
        }
      });
    }
  },
};

const AngleManager = {
  // Obtenir l'angle actuel
  getCurrentAngle: function() {
    return current_angle;
  },
  
  // Changer l'angle et redessiner
  setAngle: function(newAngle) {
    if (!CONFIG.available_angles.includes(newAngle)) {
      console.warn(`Angle ${newAngle} non supporté`);
      return;
    }
    
    if (newAngle === current_angle) {
      return; // Pas de changement nécessaire
    }
    
    current_angle = newAngle;
    updateURLWithAngle(newAngle);
    
    // Mettre à jour l'interface
    this.updateAngleButtons();
    
    // Redessiner le graphique
    FanchartApp.reRenderWithCurrentGenerations();
  },
  
  // Mettre à jour l'état visuel des boutons
  updateAngleButtons: function() {
    CONFIG.available_angles.forEach(angle => {
      const btn = document.getElementById(`b-angle-${angle}`);
      if (btn) {
        btn.classList.toggle('active', angle === current_angle);
      }
    });
  },
  
  // Obtenir le label pour un angle
  getAngleLabel: function(angle) {
    switch(angle) {
      case 180: return "Compact";
      case 220: return "Standard";
      case 260: return "Étendu";
      default: return `${angle}°`;
    }
  },
  
  // Initialiser depuis l'URL
  initialize: function() {
    current_angle = getAngleFromURL();
    this.updateAngleButtons();
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
    DOMCache.preload();
    this.renderFanchart();
    this.updateGenerationTitle();
    this.initializeEvents();
    this.initializeAngleEvents();
    LocationManager.initializeLocationEvents();
    ColorManager.initializeColorEvents();
    LegendManager.initializeAllEvents();
    this.applyInitialState();
    UIManager.addNavigationHelp();
    this.fitScreen();
  },

  processAncestorData: function() {
    // Vue d'ensemble claire : on voit immédiatement les étapes du traitement
    const ancestorKeys = Object.keys(ancestor);

    ancestorKeys.forEach(key => {
      const person = ancestor[key];

      // Chaque transformation a sa propre fonction dédiée
      this.cleanPersonPlaces(person, key);
      this.cleanPersonDates(person, key);
      this.cleanPersonAge(person, key);
    });

    // Après le nettoyage, mettre à jour les flags globaux
    this.updateGlobalFlags();
  },

  // Fonction dédiée au nettoyage des lieux
  cleanPersonPlaces: function(person, key) {
    // Cette fonction a une responsabilité unique : nettoyer les lieux
    const placeFields = [
      { field: 'birth_place', flag: 'has_bi' },
      { field: 'baptism_place', flag: 'has_ba' },
      { field: 'marriage_place', flag: 'has_ma' },
      { field: 'death_place', flag: 'has_de' },
      { field: 'burial_place', flag: 'has_bu' }
    ];

    placeFields.forEach(({ field, flag }) => {
      if (person[field] !== undefined) {
        // Le nettoyage lui-même est extrait dans une fonction pure
        ancestor[key][field] = this.cleanPlaceName(person[field]);
        // On ne modifie pas les flags globaux ici, c'est une autre responsabilité
        window[flag] = true;
      }
    });
  },

  // Fonction pure pour nettoyer un nom de lieu
  cleanPlaceName: function(placeName) {
    // Cette fonction est pure : même entrée = même sortie, pas d'effets de bord
    return placeName.replace(/^\?, /, "");
  },

  // Fonction dédiée au nettoyage des dates
  cleanPersonDates: function(person, key) {
    if (person.dates !== undefined) {
      // Chaînage des transformations de manière claire
      let cleanedDates = person.dates;
      cleanedDates = this.removeHtmlTags(cleanedDates);
      cleanedDates = this.abbreviateCirca(cleanedDates);
      ancestor[key].dates = cleanedDates;
    }
  },

  // Fonctions pures pour les transformations de dates
  removeHtmlTags: function(text) {
    return text.replace(/\s?<\/?bdo[^>]*>/g, "");
  },

  abbreviateCirca: function(text) {
    return text.replace(/\bca\s+/g, "~");
  },

  // Fonction dédiée au nettoyage de l'âge
  cleanPersonAge: function(person, key) {
    if (person.death_age !== undefined) {
      ancestor[key].death_age = this.extractNumericAge(person.death_age);
    }
  },

  // Fonction pure pour extraire l'âge numérique
  extractNumericAge: function(ageString) {
    return ageString.replace(/[^0-9]/g, "");
  },

  // Mise à jour des flags globaux basée sur l'état actuel des données
  updateGlobalFlags: function() {
    // Réinitialiser les flags
    has_bi = has_ba = has_ma = has_de = has_bu = false;

    // Parcourir les ancêtres pour déterminer quels types de données sont présents
    Object.values(ancestor).forEach(person => {
      if (person.birth_place) has_bi = true;
      if (person.baptism_place) has_ba = true;
      if (person.marriage_place) has_ma = true;
      if (person.death_place) has_de = true;
      if (person.burial_place) has_bu = true;
    });
  },

  checkForImplexes: function() {
    for (let key in ancestor) {
      if (ancestor[key].sosasame) {
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
    const margin = CONFIG.svg_margin;
    if (current_angle === 180) {
      // À 180°, on n'a pas besoin d'autant de largeur car l'arbre
      // ne s'étend pas au-delà du demi-cercle
      svg_w = 2 * max_r + 2 * margin;
      center_x = max_r + margin;
      
      // La hauteur reste max_r + margin car pas d'extension vers le bas
      svg_h = max_r + 2 * margin;
      center_y = max_r + margin;
    } else {
      // Calcul standard pour les autres angles
      center_x = max_r + margin;
      center_y = max_r + margin;
      svg_w = 2 * center_x;

      // Utiliser l'angle dynamique pour calculer la hauteur
      const halfAngleRad = Math.PI/180 * (current_angle - 180) / 2;
      const extraHeight = Math.max(CONFIG.a_r[0], Math.round(max_r * Math.sin(halfAngleRad)));
      svg_h = 2 * margin + max_r + extraHeight;
    }

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
    const standardInfo = this.initializeStandardText();
    standard = standardInfo.element;
    standard_width = standardInfo.width;

    this.renderCenterPerson();
    this.renderAncestorsByGeneration();
    this.updateButtonStates();

    const svg = document.getElementById("fanchart");
    const container = document.getElementById("fanchart-container");
    /* todo calculate dynamicaly good position for the perso/marr info panel
    panel.style.left = (bbox.left + 20) + "px";
    panel.style.top = (bbox.top + 20) + "px";
    */
  },

  renderAncestorsByGeneration: function() {
    // rayon total accumulé
    let cumulativeR = CONFIG.a_r[0];
    const rings = max_gen + 1;

    for (let gen = 2; gen <= rings; gen++) {
      const innerR = cumulativeR;                // rayon intérieur
      const outerR = innerR + CONFIG.a_r[gen-1]; // rayon extérieur
      cumulativeR = outerR;                      // pour la génération suivante

      // angle total à découper - utilise l'angle dynamique
      const delta = current_angle / Math.pow(2, gen-1);
      // angle de départ au-dessus du centre
      let angle = -90 - current_angle / 2 + delta/2;

      // on itère sur les 2^(gen-1) cases de cette génération
      const firstSosa = Math.pow(2, gen-1);
      const lastSosa  = Math.pow(2, gen) - 1;

      for (let sosa = firstSosa; sosa <= lastSosa; sosa++, angle += delta) {
        const person = this.getEffectivePerson(sosa);
        if (!person) continue;

        // prépare la position de ce secteur
        const pos = {
          r1: innerR, r2: outerR,
          a1: angle - delta/2, a2: angle + delta/2,
          generation: gen, delta: delta
        };

        this.renderAncestorSector(sosa, pos, person);
      }
    }

    this.updateButtonStates();
  },

  initializeAngleEvents: function() {
    CONFIG.available_angles.forEach(angle => {
      const btn = document.getElementById(`b-angle-${angle}`);
      if (btn) {
        btn.onclick = function() {
          AngleManager.setAngle(angle);
        };
      }
    });
  },

  // Résoudre les implexes virtuellement
  getEffectivePerson: function(sosa) {
    let person = ancestor["S" + sosa];
    // Si pas de personne à ce sosa, chercher si c’est un enfant d’implexe
    if (!person && implexMode !== "reduced") {
      const parentSosa = Math.floor(sosa / 2);
      const parentPerson = ancestor["S" + parentSosa];

      if (parentPerson && parentPerson.sosasame) {
        const refSosa = parentPerson.sosasame;
        const childSosa = sosa % 2 === 0 ? 2 * refSosa : 2 * refSosa + 1;
        const childPerson = ancestor["S" + childSosa];

        if (childPerson) {
          // Créer un enfant virtuel basé sur l'implexe
          return {
            ...childPerson,
            fn: implexMode === "numbered" ? "" : childPerson.fn,
            sn: implexMode === "numbered" ? `${sosa} › ${childSosa}` : childPerson.sn,
            sosasame: implexMode === "numbered" ? childSosa : undefined,
            dates: implexMode === "numbered" ? "" : childPerson.dates
          };
        }
      }
    }
    return person;
  },

  initializeStandardText: function() {
    const standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
    standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
    standard.setAttribute("id", "standard");
    standard.setAttribute("x", center_x);
    standard.setAttribute("y", center_y);
    fanchart.append(standard);

    const bbox = standard.getBBox();

    return {
      element: standard,
      width: bbox.width / standard.textContent.length,
      height: bbox.height  // Si besoin plus tard
    };
  },

  // Rendu du centre (Sosa 1)
  renderCenterPerson: function() {
    const sosa = 1;
    const person = ancestor["S" + sosa];
    const r = CONFIG.a_r[0];

    // Créer le groupe SVG
    const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
    group.setAttribute("id", "S" + sosa);
    fanchart.append(group);

    // Dessiner les éléments
    SVGRenderer.drawCircle(group, r, center_x, center_y, person, { isBackground: true });
    T.drawText(group, 'S1', {
      x: center_x,
      y: center_y - 10,
      p: person,
      classes: ""
    });
    SVGRenderer.drawCircle(group, r, center_x, center_y, person);

    return group;
  },

  // Gestion des implexes
  handleImplex: function(sosa, person) {
    if (!person.sosasame) {
      return { person: person, isImplex: false };
    }

    const referenceSosa = person.sosasame;
    const referencedPerson = ancestor["S" + referenceSosa];


    if (implexMode === "reduced") {
      // Mode initial (réduit) : garder les " < " et pas de propagation
      return { person: person, isImplex: false };
    }

    if (implexMode === "numbered") {
     // Mode numéroté : utiliser les vraies données mais avec numérotation
      return { 
        person: {
          ...referencedPerson,  // Utiliser les vraies données pour les couleurs
          fn: "",               // Mais remplacer le prénom
          sn: `${sosa} › ${referenceSosa}`,  // Et le nom par la numérotation
          dates: "",            // Et les dates
          sosasame: referenceSosa
        }, 
        isImplex: true,
        originalSosa: referenceSosa 
      };
    }

    // Mode "full" : remplacer par les vraies données
    return { person: referencedPerson, isImplex: false };
  },

  // Rendre un secteur complet d’ancêtre
  renderAncestorSector: function(sosa, position, person) {
      // Créer le groupe pour cet ancêtre
      const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
      group.setAttribute("id", "S" + sosa);
      fanchart.append(group);

      // Gérer les implexes
      const implexInfo = this.handleImplex(sosa, person);
      const actualPerson = implexInfo.person;
      actualPerson.sosa = sosa;

      // Dessiner le secteur de fond
      SVGRenderer.drawPie(group, position.r1 + 10, position.r2,
        position.a1, position.a2, actualPerson,
        { type: 'person', isBackground: true });

      // Dessiner le texte si la personne est connue
      if (actualPerson.fn !== "?") {
        const textClasses = this.buildTextClasses(actualPerson);
        SVGRenderer.drawSectorText(group, position.r1, position.r2,
          position.a1, position.a2, sosa, actualPerson,
          textClasses, position.generation, implexInfo.isImplex);
      }

      // Gérer le mariage pour les ancêtres pairs (pères) - AVANT le secteur interactif
      if (sosa % 2 === 0) {
        const marriageGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
        marriageGroup.setAttribute("id", "M" + sosa);
        group.appendChild(marriageGroup);  // Ajouter au groupe de la personne, pas au fanchart
        
        let marriagePerson = actualPerson;
        if (implexInfo.isImplex && implexMode === "numbered" && implexInfo.originalSosa) {
          marriagePerson = ancestor["S" + implexInfo.originalSosa];
        }
        
        this.renderMarriageInfo(marriageGroup, sosa, position, marriagePerson);
      } else if (sosa % 2 !== 0) {
        // Pour les mères, propager les infos de mariage
        const fatherSosa = sosa - 1;
        const father = this.getEffectivePerson(fatherSosa);
        
        if (father && father.marriage_place) {
          actualPerson.marriage_place = father.marriage_place;
        }
      }

      // Dessiner le secteur interactif EN DERNIER pour qu'il soit au-dessus
      SVGRenderer.drawPie(group, position.r1 + 10, position.r2,
        position.a1, position.a2, actualPerson,
        { type: 'person' });

      // Ajouter l'indicateur de navigation
      SVGRenderer.drawParentIndicator(group, position.r1 + 10,
        position.a1, position.a2, sosa, actualPerson);

      return group;
  },

  buildTextClasses: function(person) {
    let classes = "";

    if (person.birth_place && person.birth_place !== "" && lieux[person.birth_place]) {
      classes += " bi-t" + lieux[person.birth_place].c;
    }
    if (person.baptism_place && person.baptism_place !== "" && lieux[person.baptism_place]) {
      classes += " ba-t" + lieux[person.baptism_place].c;
    }
    if (person.death_place && person.death_place !== "" && lieux[person.death_place]) {
      classes += " de-t" + lieux[person.death_place].c;
    }
    if (person.burial_place && person.burial_place !== "" && lieux[person.burial_place]) {
      classes += " bu-t" + lieux[person.burial_place].c;
    }

    return classes.trim();
  },

  // Informations de mariage
  renderMarriageInfo: function(marriageGroup, sosa, position, person) {
    const extendedA2 = position.a2 + position.delta;

    // Dessiner le secteur de mariage (fond)
    SVGRenderer.drawPie(marriageGroup, position.r1, position.r1 + 10,
    position.a1, extendedA2, person,
    { type: 'marriage', isBackground: true });

    // Dessiner la date de mariage si elle existe
    if (person.marriage_date !== undefined) {
      let classes = "";
      if (person.marriage_place && person.marriage_place !== "" && lieux[person.marriage_place]) {
        classes += " ma-t" + lieux[person.marriage_place].c;
      }
      T.drawMarriageDate(marriageGroup, sosa, position.r1 + 5,
        position.a1, extendedA2, person.marriage_date, classes);
    }

    // Dessiner le contour et la ligne radiale
    SVGRenderer.drawContour(marriageGroup, position.r1, position.r2, position.a1, extendedA2);
    SVGRenderer.drawRadialLine(marriageGroup, position.r1 + 10, position.r2, position.a2);

    // Dessiner le secteur de mariage interactif
    SVGRenderer.drawPie(marriageGroup, position.r1, position.r1 + 10,
    position.a1, extendedA2, person,
    { type: 'marriage' });
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
    DOMCache.invalidate();
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
    document.getElementById("b-help").onclick = function() {
      const helpPanel = document.getElementById('navigation-help');
      if (helpPanel) {
        const isVisible = helpPanel.style.display !== 'none';
        helpPanel.style.display = isVisible ? 'none' : 'block';
        this.classList.toggle("active", !isVisible);
      }
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
    document.getElementById("b-implex").onclick = function() {
      // Cycle : reduced → numbered → full → reduced
      switch(implexMode) {
        case "reduced":
          implexMode = "numbered";
          this.title = "Afficher tous les ancêtres";
          this.querySelector("i").className = "fa fa-comment fa-fw";
          break;
        case "numbered":
          implexMode = "full";
          this.title = "Réduire les implexes";
          this.querySelector("i").className = "fa fa-comment-slash fa-fw";
          break;
        case "full":
          implexMode = "reduced";
          this.title = "Numéroter les implexes";
          this.querySelector("i").className = "fa fa-comment-dots fa-fw";
          break;
      }

      FanchartApp.reRenderWithCurrentGenerations();
      Utils.updateUrlWithCurrentState();
    };
    document.getElementById("font-selector").onchange = function() {
      const fanchart = document.getElementById("fanchart");

      ['mono', 'serif', 'large', 'readable'].forEach(cls => {
        fanchart.classList.remove(cls);
      });

      if (this.value) {
        fanchart.classList.add(this.value);
      }

      // Mettre à jour l'URL si nécessaire
      // Utils.updateUrlWithCurrentState();
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
  }
};

// Alias pour la rétrocompatibilité
const R = SVGRenderer;
const T = TextRenderer;

// Lancement de l'application
FanchartApp.init();