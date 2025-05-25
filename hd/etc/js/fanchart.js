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
var standard, standard_width, standard_height;
var center_x, center_y, svg_w, svg_h, svg_ratio;
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
  safeParseInt: function(value, defaultValue = 0) {
    var parsed = parseInt(value);
    return isNaN(parsed) ? defaultValue : parsed;
  },

  safeGetProperty: function(obj, ...keys) {
    return keys.reduce((current, key) => current?.[key], obj);
  },

  safeToggleClass: function(elementId, className, force) {
    var element = document.getElementById(elementId);
    if (element && element.classList) {
      if (force !== undefined) {
        element.classList.toggle(className, force);
      } else {
        element.classList.toggle(className);
      }
      return true;
    }
    return false;
  },

  relativeLuminance: function( color ) {
    pixel.fillStyle = color;
    pixel.fillRect( 0, 0, 1, 1 );
    const data = pixel.getImageData(0, 0, 1, 1).data;
    const rsrgb = data[0] / 255;
    const gsrgb = data[1] / 255;
    const bsrgb = data[2] / 255;
    const r = rsrgb <= 0.03928 ? rsrgb / 12.92 : Math.pow((rsrgb + 0.055) / 1.055, 2.4);
    const g = gsrgb <= 0.03928 ? gsrgb / 12.92 : Math.pow((gsrgb + 0.055) / 1.055, 2.4);
    const b = bsrgb <= 0.03928 ? bsrgb / 12.92 : Math.pow((bsrgb + 0.055) / 1.055, 2.4);
    return r * 0.2126 + g * 0.7152 + b * 0.0722;
  },

  contrastRatio: function( color1, color2 ) {
    return (this.relativeLuminance(color1) + 0.05) / (this.relativeLuminance(color2) + 0.05);
  }
};

// ========== Gestion des lieux ==========
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

  buildLocationTooltip: function(lieuData) {
    if (!lieuData || typeof lieuData !== 'object') {
      return "Aucune information disponible";
    }

    var total = Utils.safeParseInt(lieuData.cnt, 0);
    var details = [];

    var eventTypes = [
    { key: 'bi_count', label: 'naissance' },
    { key: 'ba_count', label: 'baptême' },
    { key: 'ma_count', label: 'mariage' },
    { key: 'de_count', label: 'décès', plural: 'décès' },
    { key: 'bu_count', label: 'sépulture' }
    ];

    eventTypes.forEach(function(event) {
      var count = Utils.safeParseInt(lieuData[event.key], 0);
      if (count > 0) {
        var label = event.plural && count > 1 ? event.plural : event.label;
        details.push(count + " " + label + (count > 1 && !event.plural ? "s" : ""));
      }
    });

    return total + " occurrence" + (total > 1 ? "s" : "") +
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
      const graphElements = document.getElementsByClassName(ev + "-L" + index);
      const textElements = document.getElementsByClassName(ev + "-tL" + index);

      for (const element of graphElements) {
        element.classList.toggle("highlight", show);
        element.classList.toggle("highlight-from-list", show);
      }
      for (const element of textElements) {
        element.classList.toggle("text_highlight", show);
      }

      if (lieux[locationKey] && lieux[locationKey][ev]) {
        const indicator = document.getElementById(ev + "-L" + index);
        if (indicator) {
          indicator.classList.toggle("hidden", !show);
        }
      }
    });

    const marriageElements = document.getElementsByClassName("ma-L" + index);
    for (const element of marriageElements) {
      element.classList.toggle("highlight", show);
      element.classList.toggle("highlight-from-list", show);
    }

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

    var rb = Utils.contrastRatio('hsl(' + c_h + ',100%,' + c_l + '%)', 'black');
    var rw = Utils.contrastRatio('white', 'hsl(' + c_h + ',100%,' + c_l + '%)');
    if (rw > rb) {
      root.style.setProperty('--fc-text-color-' + index, 'white');
    }

    sheet.insertRule('body.place_color svg.bi .bi-L' + index + ' {fill: var(--fc-color-' + index + ', transparent);}');
    sheet.insertRule('body.place_color svg.bi .bi-tL' + index + ' {fill: var(--fc-text-color-' + index + ', black);}');

    if (has_ba) {
      sheet.insertRule('body.place_color svg.ba .ba-L' + index + ' {fill: var(--fc-color-' + index + ', transparent);}');
      sheet.insertRule('body.place_color svg.ba .ba-tL' + index + ' {fill: var(--fc-text-color-' + index + ', black);}');
    }

    sheet.insertRule('body.place_color svg.ma .ma-L' + index + ' {fill: var(--fc-color-' + index + ', transparent);}');
    sheet.insertRule('body.place_color svg.ma .ma-tL' + index + ' {fill: var(--fc-text-color-' + index + ', black);}');
    sheet.insertRule('body.place_color svg.de .de-L' + index + ' {fill: var(--fc-color-' + index + ', transparent);}');
    sheet.insertRule('body.place_color svg.de .de-tL' + index + ' {fill: var(--fc-text-color-' + index + ', black);}');

    if (has_bu) {
      sheet.insertRule('body.place_color svg.bu .bu-L' + index + ' {fill: var(--fc-color-' + index + ', transparent);}');
      sheet.insertRule('body.place_color svg.bu .bu-tL' + index + ' {fill: var(--fc-text-color-' + index + ', black);}');
    }

    sheet.insertRule('body.place_color #L' + index + ' .square { color: var(--fc-color-' + index + ', transparent); }');
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
    (has_bi ? '<span id="bi-L'+originalIndex+'" class="hidden">N</span>' : '') +
    (has_ba ? '<span id="ba-L'+originalIndex+'" class="hidden">B</span>' : '') +
    (has_ma ? '<span id="ma-L'+originalIndex+'" class="hidden">M</span>' : '') +
    (has_de ? '<span id="de-L'+originalIndex+'" class="hidden">D</span>' : '') +
    (has_bu ? '<span id="bu-L'+originalIndex+'" class="hidden">S</span>' : '') +
    '<span class="square">■</span>' + subIndicator + ' ' + placeName;

    li.setAttribute("id", lieux[placeName].c);
    li.setAttribute("title", LocationManager.buildLocationTooltip(lieux[placeName]));
  }
};

// ========== Rendu SVG ==========
const SVGRenderer = {
  /**
   * @param {SVGElement} g     — le <g> parent
   * @param {number}     r     — rayon
   * @param {number}     cx, cy— centre du cercle
   * @param {Object}     p     — données de la personne (p.fn, p.sn…)
   * @param {Object}     [options]
   *        options.isBackground  // si true, trace en mode “background”
   */
  pos_x: function( r, a ) {
    return center_x + r * Math.cos( Math.PI / 180 * a );
  },
  pos_y: function( r, a ) {
    return center_y + r * Math.sin( Math.PI / 180 * a );
  },
  createGroup: function( id ) {
    var g = document.createElementNS("http://www.w3.org/2000/svg", "g");
    g.setAttribute( "id", id );
    fanchart.append(g);
    return g;
  },
  drawContour( g, r1, r2, a1, a2 ) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute( "d",
    'M ' + R.pos_x(r2, a1) + ',' + R.pos_y(r2, a1) +
    ' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + R.pos_x(r2, a2) + ',' + R.pos_y(r2, a2) +
    ' L ' + R.pos_x(r1, a2) + ',' + R.pos_y(r1, a2) +
    ' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + R.pos_x(r1, a1) + ',' + R.pos_y(r1, a1) +
    ' Z'
    );
    path.setAttribute( "class", "contour" );
    g.append(path);
  },
  drawRadialLine: function( g, r1, r2, a ) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute( "d",
    'M ' + R.pos_x(r2, a) + ',' + R.pos_y(r2, a) +
    ' L ' + R.pos_x(r1, a) + ',' + R.pos_y(r1, a)
    );
    path.setAttribute( "class", "middle" );
    g.append(path);
  },
  /**
   * Dessine un cercle combinant circle_bg et circle
   * @param {SVGElement} g
   * @param {number} r - rayon
   * @param {number} cx, cy - centre
   * @param {Object} p - données de la personne
   * @param {Object} [options]
   *        options.isBackground — si true, version fond
   */
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
        classes.push(Calculations.deathAgeClass(p.death_age));
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
  /**
   * Fonction universelle pour dessiner tous types de secteurs
   * @param {SVGElement} g - groupe parent
   * @param {number} r1, r2 - rayons intérieur et extérieur
   * @param {number} a1, a2 - angles début et fin
   * @param {Object} p - données de la personne
   * @param {Object} options - configuration
   *   options.type: 'person'|'marriage' - type de secteur
   *   options.isBackground: boolean - version fond ou interactive
   */
  drawPie: function(g, r1, r2, a1, a2, p, options = {}) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("d",
      'M ' + this.pos_x(r2, a1) + ',' + this.pos_y(r2, a1) +
      ' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + this.pos_x(r2, a2) + ',' + this.pos_y(r2, a2) +
      ' L ' + this.pos_x(r1, a2) + ',' + this.pos_y(r1, a2) +
      ' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + this.pos_x(r1, a1) + ',' + this.pos_y(r1, a1) +
      ' Z'
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
  // Classes CSS pour les backgrounds
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
      if (p.death_age) classes.push(Calculations.deathAgeClass(p.death_age));
    } else if (type === 'marriage') {
      // Classes pour les mariages
      if (p.marriage_place) classes.push(`ma-${lieux[p.marriage_place].c}`);
      if (p.marriage_length) {
        const marriageClass = Calculations.marriageLengthClass(p.marriage_length);
        if (marriageClass) classes.push(marriageClass);
      }
    }
    
    element.setAttribute("class", classes.join(' '));
  },
  // Fonctionnalités interactives (événements, titres)
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
      const years = Utils.safeParseInt(p.marriage_length, -1);
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
  // Gestion universelle des clics
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
  // Gestion universelle du survol (entrée)
  handleMouseEnter: function(p, type, event) {
    const places = [
      { prop: 'birth_place', prefix: 'bi' },
      { prop: 'baptism_place', prefix: 'ba' },
      { prop: 'marriage_place', prefix: 'ma' },
      { prop: 'death_place', prefix: 'de' },
      { prop: 'burial_place', prefix: 'bu' }
    ];
    
    places.forEach(place => {
      if (p[place.prop]) {
        Utils.safeToggleClass(`${place.prefix}-${lieux[p[place.prop]].c}`, "hidden", false);
        LocationManager.hlPlace(p[place.prop], true);
      }
    });
    
    if (event && event.currentTarget) {
      const group = event.currentTarget.parentNode;
      const backgroundSector = group.querySelector('.bg');
      if (backgroundSector) {
        backgroundSector.style.fill = "lightgrey";
      }
    }
    
    // Gestion des âges pour les personnes
    if (type === 'person' && p.death_age) {
      Utils.safeToggleClass(Calculations.deathAgeClass(p.death_age), "hl", true);
    }
    
    // Gestion des durées de mariage
    if (type === 'marriage' && p.marriage_length) {
      const marriageClass = Calculations.marriageLengthClass(p.marriage_length);
      if (marriageClass) {
        Utils.safeToggleClass(marriageClass, "hl", true);
      }
    }
    
    // Gestion des implexes (personnes identiques)
    if (p.fn === "=") {
      const ref = document.getElementById("S" + p.sn);
      if (ref) ref.classList.add("same_hl");
    }
  },
  //  Gestion universelle du survol (sortie)
  handleMouseLeave: function(p, type, event) {
    const places = [
      { prop: 'birth_place', prefix: 'bi' },
      { prop: 'baptism_place', prefix: 'ba' },
      { prop: 'marriage_place', prefix: 'ma' },
      { prop: 'death_place', prefix: 'de' },
      { prop: 'burial_place', prefix: 'bu' }
    ];
    
    places.forEach(place => {
      if (p[place.prop]) {
        Utils.safeToggleClass(`${place.prefix}-${lieux[p[place.prop]].c}`, "hidden", true);
        LocationManager.hlPlace(p[place.prop], false);
      }
    });
    
    if (event && event.currentTarget) {
      const group = event.currentTarget.parentNode;
      const backgroundSector = group.querySelector('.bg');
      if (backgroundSector) {
        backgroundSector.style.fill = "";
      }
    }
   
    // Nettoyage des âges
    if (type === 'person' && p.death_age) {
      Utils.safeToggleClass(Calculations.deathAgeClass(p.death_age), "hl", false);
    }
    
    // Nettoyage des durées de mariage
    if (type === 'marriage' && p.marriage_length) {
      const marriageClass = Calculations.marriageLengthClass(p.marriage_length);
      if (marriageClass) {
        Utils.safeToggleClass(marriageClass, "hl", false);
      }
    }
    
    // Nettoyage des implexes
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
      text.onclick = (e) => this.handleNavigationClick(e, p);
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
  handleNavigationClick: function(e, p) {
    const oc = (p.oc && p.oc !== "" && p.oc !== 0) ? `&oc=${p.oc}` : "";
    const url = `${link_to_fanchart}p=${p.fnk}&n=${p.snk}${oc}&v=${max_gen}&tool=${tool}` +
                `${has_ba ? "&ba=on" : ""}${has_bu ? "&bu=on" : ""}`;
    window.location = url;
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
  /**
   * Fonction principale de rendu de texte - Point d'entrée unique
   * @param {SVGElement} g - groupe SVG parent
   * @param {string} mode - 'S1'|'C3'|'R3'|'R2'|'R1'
   * @param {Object} params - paramètres selon le mode
   */
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
  /**
   * Construit les classes CSS pour les lieux d'une personne
   * @param {Object} p - données de la personne
   * @param {string} baseClasses - classes de base
   */
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
  /**
   * Rendu du texte central (mode S1) - Position absolue
   * @param {SVGElement} g - groupe parent
   * @param {number} x, y - position centrale
   * @param {Object} p - données de la personne
   * @param {string} classes - classes CSS
   */
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
  /**
   * Rendu du texte circulaire (mode C3) - 3 arcs concentriques
   * @param {SVGElement} g - groupe parent
   * @param {number} r1, r2 - rayons intérieur et extérieur
   * @param {number} a1, a2 - angles début et fin
   * @param {string} sosa - identifiant SOSA
   * @param {Object} p - données de la personne
   * @param {string} classes - classes CSS
   */
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
  /**
   * Rendu du texte radial (modes R1, R2, R3) - Lignes radiales
   * @param {SVGElement} g - groupe parent
   * @param {number} r1, r2 - rayons intérieur et extérieur
   * @param {number} a1, a2 - angles début et fin
   * @param {string} sosa - identifiant SOSA
   * @param {Object} p - données de la personne
   * @param {string} classes - classes CSS
   * @param {number} lineCount - nombre de lignes (1, 2, ou 3)
   */
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
  /**
   * Calcule les paramètres pour le texte radial selon l'orientation
   * @param {number} r1, r2 - rayons
   * @param {number} a1, a2 - angles
   * @param {number} lineCount - nombre de lignes
   */
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
  /**
   * Crée un chemin circulaire (arc) pour le placement de texte
   * @param {SVGElement} g - groupe parent
   * @param {string} id - identifiant du chemin
   * @param {number} r - rayon
   * @param {number} a1, a2 - angles début et fin
   * @returns {number} longueur du chemin
   */
  createCircularPath: function(g, id, r, a1, a2) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("class", "none");
    path.setAttribute("d",
      `M ${SVGRenderer.pos_x(r, a1)},${SVGRenderer.pos_y(r, a1)} ` +
      `A ${r} ${r} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${SVGRenderer.pos_x(r, a2)},${SVGRenderer.pos_y(r, a2)}`
    );
    path.setAttribute("id", id);
    g.append(path);
    
    // Retourne la longueur approximative du chemin
    return Math.abs(a2 - a1) / 360 * 2 * Math.PI * r;
  },

  /**
   * Crée un chemin radial (ligne droite) pour le placement de texte
   * @param {SVGElement} g - groupe parent
   * @param {string} id - identifiant du chemin
   * @param {number} r1, r2 - rayons début et fin
   * @param {number} a - angle
   * @returns {number} longueur du chemin
   */
  createRadialPath: function(g, id, r1, r2, a) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("class", "none");
    path.setAttribute("d",
      `M ${SVGRenderer.pos_x(r1, a)},${SVGRenderer.pos_y(r1, a)} ` +
      `L ${SVGRenderer.pos_x(r2, a)},${SVGRenderer.pos_y(r2, a)}`
    );
    path.setAttribute("id", id);
    g.append(path);
    
    // Retourne la longueur du chemin
    return Math.abs(r2 - r1);
  },
  /**
   * Place un texte le long d'un chemin avec dimensionnement adaptatif
   * @param {SVGElement} g - groupe parent
   * @param {string} pathId - identifiant du chemin
   * @param {string} textContent - contenu du texte
   * @param {string} classes - classes CSS
   * @param {number} pathLength - longueur disponible
   * @param {number} pathHeight - hauteur disponible
   */
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
  /**
   * Calcule les tailles de police adaptatives pour le texte central
   * @param {Array} texts - tableau des textes à dimensionner
   * @returns {Array} tailles de police en pourcentage
   */
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
  // Date de mariage
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
  initializePlacesListEvents: function() {
    const placesList = document.getElementById("places_list");
    if (!placesList) return;

    function getLocationData(target) {
      const li = target.closest("li[data-location]");
      if (!li) return null;

      return {
        locationKey: li.dataset.location,
        index: Utils.safeParseInt(li.dataset.index, -1),
        element: li
      };
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
  },

  buildPlacesList: function() {
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

    UIManager.initializePlacesListEvents();
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
  },

  toggleSort: function() {
    var button = document.getElementById("sort-toggle");
    var icon = button.querySelector("i");

    if (sortMode === "frequency") {
      sortMode = "alphabetical";
      icon.className = "fa fa-arrow-down-wide-short";
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
      icon.className = "fa fa-arrow-down-a-z";
      button.title = "Trier par ordre alphabétique";

      lieux_a.sort(function(e1, e2) {
        return e2[1].cnt - e1[1].cnt;
      });
    }

    UIManager.rebuildListVisualOnly();
  },

  createPlacesHeader: function() {
    var totals = Calculations.calculateTotals();
    var header = document.createElement('div');
    header.id = 'places-header';
    header.innerHTML = `
    <span style="color: #666;">
    ${max_gen} gén. : ${totals.places} lieu${totals.places > 1 ? 'x' : ''}, ${totals.events} événement${totals.events > 1 ? 's' : ''}
    </span>
    <button id="sort-toggle" title="Trier par ordre alphabétique">
    <i class="fa fa-arrow-down-a-z"></i>
    </button>
    `;
    return header;
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
    <div>– Ctrl+clic : fiche individuelle</div>
    <div>– <span class="text-success">▲</span> redéfinir la racine</div>
    `;

    setTimeout(() => {
      helpPanel.style.opacity = '0.4';
    }, 10000);

    helpPanel.onclick = function() {
      helpPanel.style.display = 'none';
    };

    document.body.appendChild(helpPanel);
  }
};

const Calculations = {
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
  }
};

// ========== Gestion de la vue ==========
const ViewManager = {
  initializeViewBox: function() {
    svg_viewbox_x = 0;
    svg_viewbox_y = 0;
    svg_viewbox_w = svg_w;
    svg_viewbox_h = svg_h;
  },

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
      svg_viewbox_x + Math.round(zx * (svg_viewbox_w - w) / FanchartApp.window_w),
      svg_viewbox_y + Math.round(zy * (svg_viewbox_h - h) / FanchartApp.window_h),
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
  }
};

const ColorManager = {
  applyColorization: function() {
    var bi_checked = document.getElementById("bi").checked;
    var ba_checked = document.getElementById("ba").checked;
    var ma_checked = document.getElementById("ma").checked;
    var de_checked = document.getElementById("de").checked;
    var bu_checked = document.getElementById("bu").checked;
    fanchart.classList.toggle("bi", bi_checked);
    fanchart.classList.toggle("ba", ba_checked);
    fanchart.classList.toggle("ma", ma_checked);
    fanchart.classList.toggle("de", de_checked);
    fanchart.classList.toggle("bu", bu_checked);
  },

  setColorPreset: function(bi, ba, ma, de, bu) {
    document.getElementById("bi").checked = bi;
    document.getElementById("ba").checked = ba;
    document.getElementById("ma").checked = ma;
    document.getElementById("de").checked = de;
    document.getElementById("bu").checked = bu;
    this.applyColorization();
  },

  toggleColorMode: function() {
    if (!document.body.classList.contains('place_color')) {
      document.body.className = "places-list place_color";
      tool = "place_color";
      this.setColorPreset(false, false, true, false, false);
      return;
    }

    var bi = document.getElementById("bi").checked;
    var ba = document.getElementById("ba").checked;
    var ma = document.getElementById("ma").checked;
    var de = document.getElementById("de").checked;
    var bu = document.getElementById("bu").checked;

    // Cycle entre les modes
    if (!bi && !ba && ma && !de && !bu) {
      // Mode 1 → Mode 2 : naissances + mariages
      this.setColorPreset(true, false, true, false, false);
    } else if (bi && !ba && ma && !de && !bu) {
      // Mode 2 → Mode 3 : mariages + décès
      this.setColorPreset(false, false, true, true, false);
    } else if (!bi && !ba && ma && de && !bu) {
      // Mode 3 → Mode 4 : tout activé (si données disponibles)
      this.setColorPreset(true, has_ba, true, true, has_bu);
    } else {
      // Mode 4 → désactivation
      document.body.className = "";
      tool = "";
      this.setColorPreset(false, false, false, false, false);
    }
  },

  initializeColorEvents: function() {
    // Événements des checkboxes individuelles
    document.getElementById("bi").onclick = this.applyColorization.bind(this);
    document.getElementById("ba").onclick = this.applyColorization.bind(this);
    document.getElementById("ma").onclick = this.applyColorization.bind(this);
    document.getElementById("de").onclick = this.applyColorization.bind(this);
    document.getElementById("bu").onclick = this.applyColorization.bind(this);

    document.getElementById("b-death-age").onclick = () => {
      document.body.className = "death-age";
      tool = "death-age";
    };

    document.getElementById("b-no-tool").onclick = () => {
      document.body.className = "";
      tool = "";
    };

    document.getElementById("b-places-colorise").onclick = () => {
      this.toggleColorMode();
    };

    document.getElementById("b-places-hl").onclick = () => {
      document.body.className = "places-list place_hl";
      tool = "place_hl";
    };

    // Initialisation des états
    this.setColorPreset(false, false, true, false, false);
    
    if (!has_ba) {
      document.getElementById("bal").classList.add("none");
    }
    if (!has_bu) {
      document.getElementById("bul").classList.add("none");
    }
  }
};

const LegendManager = {
  // Initialise les événements pour les légendes d'âge au décès
  initializeAgeEvents: function() {
    const ageIds = ["DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6", "DA7"];
    
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

  // Initialise les événements pour les légendes de durée de mariage
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

  // Initialise tous les événements de légende
  initializeAllEvents: function() {
    this.initializeAgeEvents();
    this.initializeMarriageEvents();
  }
};

// ========== Application principale ==========
const FanchartApp = {
  window_w: 0,
  window_h: 0,
  zoom_factor: CONFIG.zoom_factor,
  init: function() {
    this.calculateDimensions();
    this.processAncestorData();
    this.buildInterface();
    this.renderFanchart();
    this.initializeEvents();
    UIManager.initializePlacesListEvents();
    ColorManager.initializeColorEvents();
    LegendManager.initializeAllEvents();
    this.applyInitialState();
    UIManager.addNavigationHelp();
    ViewManager.fitScreen();
  },

  calculateDimensions: function() {
    // Calculer max_gen depuis ancestor
    if (typeof max_gen === 'undefined') {
      var ak = Object.keys(ancestor);
      max_gen = Math.trunc(Math.log(Number(ak[ak.length-1].replace(/^S/, "")))/Math.log(2));
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

    svg_ratio = svg_w / svg_h;

    // Dimensions de la fenêtre
    this.window_h = window.innerHeight;
    this.window_w = Math.round(this.window_h * svg_ratio);

    // Configurer le SVG
    fanchart.setAttribute("height", this.window_h);
    fanchart.setAttribute("width", this.window_w);
    root.style.setProperty('--fc-tool-size', (window.innerWidth - this.window_w) + "px");

    svg_viewbox_x = 0;
    svg_viewbox_y = 0;
    svg_viewbox_w = svg_w;
    svg_viewbox_h = svg_h;

    ViewManager.initializeViewBox();
  },

  processAncestorData: function() {
    // Traiter chaque ancêtre
    var ak = Object.keys(ancestor);
    ak.forEach(function(s) {
      var p = ancestor[s];

      // Nettoyer les données
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
      ancestor[s].dates = p.dates.replace(/\s?<\/?bdo[^>]*>/g, "");

      // Compter les lieux
      p = ancestor[s];
      LocationManager.addPlaceCount(p.birth_place, "bi");
      LocationManager.addPlaceCount(p.baptism_place, "ba");
      LocationManager.addPlaceCount(p.marriage_place, "ma");
      LocationManager.addPlaceCount(p.death_place, "de");
      LocationManager.addPlaceCount(p.burial_place, "bu");
    });

    // Préparer le tableau des lieux
    lieux_a = [];
    for (var key in lieux) {
      lieux_a.push([key, lieux[key]]);
    }
    lieux_a.sort(function(e1, e2) {
      return e2[1].cnt - e1[1].cnt;
    });
  },

  buildInterface: function() {
    UIManager.buildPlacesList();
    var placesContainer = document.getElementById("places-list");
    var header = UIManager.createPlacesHeader();
    placesContainer.insertBefore(header, placesContainer.firstChild);
  },

  renderFanchart: function() {
    standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
    standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
    standard.setAttribute("id", "standard");
    standard.setAttribute("x", center_x);
    standard.setAttribute("y", center_y);
    fanchart.append(standard);
    standard_width = standard.getBBox().width / standard.textContent.length;
    standard_height = standard.getBBox().height;

    var gen = 1;
    var sosa = 1;
    var r1 = 0;
    var r2 = CONFIG.a_r[0];
    var a1, a2;
    var delta = CONFIG.d_all;

    var g1 = R.createGroup( "S"+sosa );
    R.drawCircle( g1, r2, center_x, center_y, ancestor["S"+sosa], { isBackground: true } );
    T.drawText(g1, 'S1', { x: center_x, y: center_y - 10, p: ancestor["S"+sosa], classes: "" });
    R.drawCircle( g1, r2, center_x, center_y, ancestor["S"+sosa] );

    while( true ) {
      sosa++;
      if( sosa >= (2 ** gen) ) {
        gen++;
        if( gen >= CONFIG.a_r.length+1 ) {
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
      if( p !== undefined ) {
        var pg = R.createGroup( "S"+sosa );
        var same = (p.fn == "=" ? true : false);
        if( same &&  implex != "" ) {
          var p2 = ancestor["S"+(2 * p.sn)];
          if( p2 !== undefined ) {
            ancestor["S"+(2*sosa)] = { "fn" : "=", "sn": 2*p.sn, "fnk": p2.fnk, "snk": p2.snk, "oc": p2.oc, "dates": "", "has_parents": p2.has_parents };
          }
          p2 = ancestor["S"+(2*p.sn+1)];
          if( p2 !== undefined ) {
            ancestor["S"+(2*sosa+1)] = { "fn" : "=", "sn": 2*p.sn+1, "fnk": p2.fnk, "snk": p2.snk, "oc": p2.oc, "dates": "", "has_parents": p2.has_parents };
          }
          p = ancestor["S"+p.sn];
          same = false;
        }
        R.drawPie(pg, r1+10, r2, a1, a2, p, { type: 'person', isBackground: true });
        if( p.fn != "?" ) {
          var c = "";
          if( p.birth_place !== undefined && p.birth_place != "" ) {
            c += " bi-t"+lieux[p.birth_place].c;
          }
          if( p.baptism_place !== undefined && p.baptism_place != "" ) {
            c += " ba-t"+lieux[p.baptism_place].c;
          }
          if( p.death_place !== undefined && p.death_place != "" ) {
            c += " de-t"+lieux[p.death_place].c;
          }
          if( p.burial_place !== undefined && p.burial_place != "" ) {
            c += " bu-t"+lieux[p.burial_place].c;
          }
          R.drawSectorText(pg, r1, r2, a1, a2, sosa, p, c, gen, same);
        }
        if( sosa % 2 == 0 ) {
          R.drawPie( pg, r1, r1+10, a1, a2+delta, p, { type: 'marriage', isBackground: true });
          if( p.marriage_date !== undefined ) {
            var c = "";
            if( p.marriage_place !== undefined && p.marriage_place != "" ) {
              c += " ma-t"+lieux[p.marriage_place].c;
            }
            T.drawMarriageDate( pg, sosa, r1+5, a1, a2+delta, p.marriage_date, c );
          }
          R.drawContour( pg, r1, r2, a1, a2+delta );
          R.drawRadialLine( pg, r1+10, r2, a2 );
          R.drawPie( pg, r1, r1+10, a1, a2+delta, p, { type: 'marriage' });
        } else {
          ancestor["S"+sosa].marriage_place = ancestor["S"+(sosa-1)].marriage_place;
        }
        R.drawPie(pg, r1+10, r2, a1, a2, p, { type: 'person' });
        R.drawParentIndicator(pg, r1+10, a1, a2, sosa, p);
      }
    }
  },

  initializeEvents: function() {
    // Zoom
    fanchart.addEventListener("wheel", (event) => {
      ViewManager.zoom(event.clientX, event.clientY, CONFIG.zoom_factor,
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
        ViewManager.set_svg_viewbox(
          svg_viewbox_x - Math.round(e.movementX * svg_viewbox_w / this.window_w),
          svg_viewbox_y - Math.round(e.movementY * svg_viewbox_h / this.window_h),
          svg_viewbox_w, svg_viewbox_h
        );
      }
    };
    document.getElementById("b-home").onclick = () => {
      window.location = link_to_person;
    };
    document.getElementById("b-refresh").onclick = () => {
      ViewManager.fitScreen();
    };
    document.getElementById("b-zoom-in").onclick = () => {
      ViewManager.zoom(this.window_w / 2, this.window_h / 2, this.zoom_factor, +1);
    };
    document.getElementById("b-zoom-out").onclick = () => {
      ViewManager.zoom(this.window_w / 2, this.window_h / 2, this.zoom_factor, -1);
    };
    document.getElementById("b-gen-add").onclick = function() {
      if( max_gen < 10 ) {
        var p = ancestor["S1"];
        var oc = p.oc;
        if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
        window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&implex=" + implex + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + (max_gen+1);
      }
    };
    document.getElementById("b-gen-del").onclick = function() {
      if( max_gen > 1 ) {
        var p = ancestor["S1"];
        var oc = p.oc;
        if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
        window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&implex=" + implex + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + (max_gen-1);
      }
    };
    document.getElementById("b-implex").onclick = function() {
      if( implex == "" ) {
        implex = "off";
      } else {
        implex = "";
      }
      var p = ancestor["S1"];
      var oc = p.oc;
      if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
      window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&implex=" + implex + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + max_gen;
    }
    document.getElementById("b-places-hl").onclick = function() {
      document.body.className = "places-list place_hl";
      tool = "place_hl";
    };
    document.getElementById("b-no-buttons").onclick = function() {
      document.getElementById("buttons").style.display = "none";
    };
    // Gestionnaire de tri
    document.getElementById("sort-toggle").onclick = UIManager.toggleSort;
  },

  applyInitialState: function() {
    // Configurer l'état initial des outils
    if (tool == "place_hl") {
      document.body.className = "places-list place_hl";
    } else if (tool == "place_color") {
      document.body.className = "places-list place_color";
      ColorManager.applyColorization();
    } else if (tool == "death-age") {
      document.body.className = "death-age";
    } else {
      document.body.className = "places-list place_color";
      tool = "place_color";
      ColorManager.applyColorization();
    }
  }
};

const R = SVGRenderer;  // R.pos_x(), R.createGroup()
const T = TextRenderer;  // T.drawText(g1…)

FanchartApp.init();