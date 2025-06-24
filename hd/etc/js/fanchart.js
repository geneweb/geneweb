const root = document.documentElement;
const fanchart = document.getElementById("fanchart");
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
var max_gen_loaded; // Génération max disponible en "mémoire"
var max_gen, max_r;
var lieux = {};      // Objet principal des lieux : clé = nom du lieu, valeur = données
var lieux_a = [];    // Array des lieux pour le tri et l'itération
var sortMode = "frequency";
var showEvents = false;
var svg_viewbox_x = 0, svg_viewbox_y = 0, svg_viewbox_w = 0, svg_viewbox_h = 0;

// ========== CONSTANTES D'ÉVÉNEMENTS ==========
const EVENT_CONFIG = {
  eventOrder: ['birth', 'baptism', 'marriage', 'death', 'burial'],
  eventToClass: { 'birth': 'n', 'baptism': 'b', 'marriage': 'm', 'death': 'd', 'burial': 's' },
  eventToLabel: { 'birth': 'N', 'baptism': 'B', 'marriage': 'M', 'death': 'D', 'burial': 'S' }
};

const EVENT_COLORS = {
  'birth': '#9FD1A4',
  'baptism': '#C7E0C9',
  'marriage': '#E67E22',
  'death': '#A5CCC9',
  'burial': '#6FA8A2'
};

const Events = {
  get types() { return EVENT_CONFIG.eventOrder; },
  cssClass: (type) => EVENT_CONFIG.eventToClass[type],
  label: (type) => EVENT_CONFIG.eventToLabel[type],
  count: (type) => type + '_count', // 'birth' -> 'birth_count'
  place: (type) => type + '_place', // 'birth' -> 'birth_place'
  svgPrefix: (type) => type.substring(0, 2), // 'birth' → 'bi'
  flagProp: (type) => 'has_' + Events.svgPrefix(type), // 'birth' → 'has_bi' > TODO DEPRECIATE THIS!
  findByClass: (cssClass) => EVENT_CONFIG.eventOrder.find(type => Events.cssClass(type) === cssClass),
  findBySvgPrefix: (prefix) => EVENT_CONFIG.eventOrder.find(type => Events.svgPrefix(type) === prefix),
  isValid: (type) => EVENT_CONFIG.eventOrder.includes(type),
  translate: (type, count = 1) => {
    const translationKey = count > 1 ? type + 's' : type;
    return window.FC_TRANSLATIONS?.[translationKey] || type;
  }
};

// ====== Configuration =======
const CONFIG = {
  security: 0.95,
  zoom_factor: 1.25,
  default_angle: 220,
  available_angles: [180, 220, 359],
  a_r: [56, 56, 54, 54, 70, 72, 100, 150, 130, 90],
  a_m: ["S1", "C3", "C2", "C1", "R4", "R3", "R2", "R1", "R1", "R1"],
  text_mode_factors: { "C3": 1.10, "C2": 1.05 }, // Mode circulaire (parents/grand-parents)
  marriage_length_thresholds: [4, 14, 24, 34, 44, 54],
  text_reduction_factor: 1,
  svg_margin: 4
};

let isCircularMode = false;
let renderTarget = null; // (null = fanchart direct)
let current_angle = CONFIG.default_angle;

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
    ["b-age", "b-places-colorise", "b-sort-places"].forEach(id => {
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

const LayoutCalculator = {
  /**
   * Mesure la largeur nécessaire pour afficher confortablement la liste des lieux
   * Utilise un élément temporaire pour mesurer le texte réel
   */
  calculatePlacesListWidth: function() {
    // Si pas de liste de lieux, retourner une largeur minimale
    if (!document.body.classList.contains('place')) {
      return 0;
    }

    // Créer un élément de mesure temporaire
    const measurer = document.createElement('div');
    measurer.style.cssText = `
      position: absolute;
      visibility: hidden;
      white-space: nowrap;
      font-family: inherit;
      font-size: inherit;
    `;
    document.body.appendChild(measurer);

    let maxWidth = 200; // Largeur minimale par défaut

    // Mesurer chaque lieu
    lieux_a.forEach(([placeName, data]) => {
      // Construire le texte complet comme il apparaîtra
      let text = '';

      // Indicateurs d'événements (N B M D S)
      if (has_bi && data.bi) text += 'N ';
      if (has_ba && data.ba) text += 'B ';
      if (has_ma && data.ma) text += 'M ';
      if (has_de && data.de) text += 'D ';
      if (has_bu && data.bu) text += 'S ';

      // Carré coloré et nom du lieu
      text += '■ ' + placeName;

      measurer.textContent = text;
      const width = measurer.offsetWidth;

      if (width > maxWidth) {
        maxWidth = width;
      }
    });

    // Nettoyer
    document.body.removeChild(measurer);

    // Ajouter des marges (padding, scrollbar, etc.)
    return maxWidth + 40; // 20px de chaque côté pour le confort
  },

  /**
   * Calcule la largeur maximale disponible pour le fanchart
   * en tenant compte du contenu réel
   */
  calculateMaxFanchartWidth: function() {
    const windowWidth = window.innerWidth;
    const placesListWidth = this.calculatePlacesListWidth();

    // Pour le mode 359° (très carré), ajouter une marge à gauche
    const isSquareChart = (current_angle >= 310 || isCircularMode);
    const leftMargin = isSquareChart ? 150 : 0; // Espace pour les contrôles

    // Calculer l'espace disponible
    const availableWidth = windowWidth - placesListWidth - leftMargin;

    // S'assurer qu'on utilise au moins 50% de l'écran pour le graphique
    return Math.max(availableWidth, windowWidth * 0.5);
  }
};


// ========== URLManager centralisé ==========
const URLManager = {
  config: {
    basePerson: link_to_person, // URL de base
    defaultParams: { module: 'A', template: 'FC' }, // Paramètres par défaut
    specialParams: { place: { module: 'MOD_DATA', data: 'place' } } // Paramètres spéciaux
  },

  readCurrentState: function() {
    const urlParams = new URLSearchParams(window.location.search);

    return {
      tool: urlParams.get('tool') || '',
      sortMode: urlParams.has('sort') ? 'alphabetical' : 'frequency',
      showEvents: urlParams.has('events'),
      isCircular: urlParams.get('mode') === 'couple',
      angle: parseInt(urlParams.get('angle')) || 220,
      implexMode: urlParams.get('implex') === 'num' ? 'numbered' :
                  urlParams.get('implex') === 'full' ? 'full' : 'reduced'
    };
  },

  /**
   * Méthode principale : construit une URL pour une personne
   *
   * @param {Object} person - Objet personne avec fnk, snk, oc
   * @param {Object} options - Options de construction de l'URL
   * @returns {string} URL complète
   */
  buildPersonURL: function(person, options = {}) {
    // Options par défaut avec fusion intelligente
    const opts = {
      // Comportement de navigation
      useCurrentState: true,     // Inclure l'état actuel (générations, mode, etc.)
      externalNavigation: false, // true = fiche individuelle, false = fanchart
      targetGeneration: null,    // Forcer une génération spécifique

      // Préservation de l'état existant
      preserveTools: true,       // Garder les outils actifs (colorisation, etc.)
      preserveMode: true,        // Garder le mode (circulaire, angle)
      preserveView: true,        // Garder la vue (zoom, position)

      // Fusion avec les options passées
      ...options
    };

    // Construction de l'URL selon le type de navigation
    if (opts.externalNavigation) {
      // Navigation externe : URL simple vers la fiche individuelle
      return this._buildExternalPersonURL(person);
    } else {
      // Navigation interne : URL complète avec état du fanchart
      return this._buildFanchartPersonURL(person, opts);
    }
  },

  /**
   * Construit une URL pour une recherche de lieu
   *
   * @param {string} placeName - Nom du lieu à rechercher
   * @param {Object} options - Options de recherche
   * @returns {string} URL de recherche
   */
  buildPlaceURL: function(placeName, options = {}) {
    const opts = {
      exactSearch: true,    // Recherche exacte ou partielle
      searchPrefix: true,   // Inclure une recherche par préfixe
      ...options
    };

    const baseURL = this.config.basePerson;
    const params = [];

    // Paramètres de base pour la recherche de lieux
    params.push(`m=${this.config.specialParams.place.module}`);
    params.push(`data=${this.config.specialParams.place.data}`);

    // Logique de recherche intelligente
    if (opts.exactSearch && placeName.length > 2) {
      // Recherche principale avec nom tronqué (logique existante)
      const searchTerm = placeName.slice(0, -2);
      params.push(`s=${encodeURIComponent(searchTerm)}`);
    }

    // Recherche exacte complémentaire
    params.push(`s1=${encodeURIComponent(placeName)}`);

    return baseURL + params.join('&');
  },

  /**
   * Met à jour l'URL de la page courante avec l'état actuel
   *
   * @param {Object} stateOverrides - Remplacements ponctuels de l'état
   */
  updateCurrentURL: function(stateOverrides = {}) {
    const person = ancestor["S1"];
    if (!person) return;

    // Construire l'URL avec l'état actuel + les remplacements
    const options = {
      useCurrentState: true,
      preserveTools: true,
      preserveMode: true,
      preserveView: false, // Ne pas préserver le zoom pour les URL
      ...stateOverrides
    };

    const newURL = this.buildPersonURL(person, options);

    // Mise à jour de l'historique sans rechargement
    history.replaceState(null, '', newURL);
  },

  /**
   * Navigation vers une nouvelle personne avec paramètres
   * Remplace Utils.navigateWithParams
   *
   * @param {number} targetGeneration - Génération cible
   * @param {Object} additionalOptions - Options supplémentaires
   */
  navigateToGeneration: function(targetGeneration, additionalOptions = {}) {
    const person = ancestor["S1"];
    if (!person) return;

    // Sauvegarder l'état actuel pour restauration
    const savedMaxGen = max_gen;

    // Construire l'URL avec la nouvelle génération
    max_gen = targetGeneration;
    const options = {
      targetGeneration: targetGeneration,
      useCurrentState: true,
      preserveTools: true,
      preserveMode: true,
      ...additionalOptions
    };

    const url = this.buildPersonURL(person, options);

    // Restaurer l'état et naviguer
    max_gen = savedMaxGen;
    window.location = url;
  },

  /**
   * Navigation directe vers une personne
   * Centralise les appels depuis les événements SVG et les clics
   *
   * @param {Object} person - Personne cible
   * @param {boolean} newTab - Ouvrir dans un nouvel onglet
   * @param {boolean} stayInFanchart - Rester dans le fanchart vs fiche individuelle
   */
  navigateToPerson: function(person, newTab = false, stayInFanchart = false) {
    // Nettoyer le cache car on change de contexte
    if (!stayInFanchart && !newTab) {
      LocationDataBuilder.clearCache();
    }

    if (!person || !person.fnk || !person.snk) {
      console.warn('URLManager: Personne invalide pour navigation', person);
      return false;
    }

    const url = this.buildPersonURL(person, {
      externalNavigation: !stayInFanchart,
      useCurrentState: stayInFanchart
    });

    // Exécution de la navigation
    if (newTab) {
      window.open(url, '_blank');
    } else {
      window.location.href = url;
    }

    return true;
  },

  /**
   * Navigation vers un lieu (recherche)
   * @param {string} placeName - Nom du lieu
   * @param {boolean} newTab - Nouvel onglet
   */
  navigateToPlace: function(placeName, newTab = false) {
    const url = this.buildPlaceURL(placeName);

    if (newTab) {
      window.open(url, '_blank');
    } else {
      window.location.href = url;
    }

    return true;
  },

  // ========== MÉTHODES PRIVÉES ==========
  /**
   * Construit une URL externe simple (fiche individuelle)
   * @private
   */
  _buildExternalPersonURL: function(person) {
    const params = [`p=${person.fnk}`, `n=${person.snk}`];

    if (person.oc) {
      params.push(`oc=${person.oc}`);
    }

    return this.config.basePerson + params.join('&');
  },

  /**
   * Construit une URL complète pour navigation fanchart
   * @private
   */
  _buildFanchartPersonURL: function(person, options) {
    // Paramètres de base obligatoires
    const params = [
      `m=${this.config.defaultParams.module}`,
      `t=${this.config.defaultParams.template}`,
      `p=${person.fnk}`,
      `n=${person.snk}`
    ];

    // Paramètres optionnels de la personne
    if (person.oc) params.push(`oc=${person.oc}`);

    // État du fanchart si demandé
    if (options.useCurrentState) this._addFanchartState(params, options);

    return this.config.basePerson + params.join('&');
  },

  /**
   * Ajoute l'état actuel du fanchart aux paramètres
   * @private
   */
  _addFanchartState: function(params, options) {
    const targetGen = options.targetGeneration || max_gen;
    params.push(`v=${targetGen}`);

    if (options.preserveMode) {
      if (isCircularMode) {
        params.push('mode=couple');
      } else if (current_angle !== 220) {
        params.push(`angle=${current_angle}`);
      }
    }

    if (options.preserveTools) {
      if (tool) params.push(`tool=${tool}`);

      if (sortMode === 'alphabetical') params.push('sort');

      const placesPanel = document.querySelector('.places-panel');
      if (placesPanel?.classList.contains('show-events')) {
        params.push('events');
      }

      if (implexMode === 'numbered') params.push('implex=num');
      else if (implexMode === 'full') params.push('implex=full');
    }
  }
};

// ========== Utilitaires généraux ==========
const Utils = {
  ageCategory: function(age) {
    const numericAge = parseInt(age);
    if (isNaN(numericAge) || numericAge <= 0) return null;
    
    const boundaries = [40, 55, 70, 85, Infinity];
    const category = boundaries.findIndex(boundary => numericAge < boundary);
    return Math.min(category, 4);
  },

  ageClass: function(age) {
    const category = this.ageCategory(age);
    return category !== null ? "DA" + category : "";
  },

  marriageLengthClass: function(length) {
    const years = parseInt(length);
    if (isNaN(years) || years < 0) return "";
    
    const thresholds = [10, 25, 40, 55];
    const index = thresholds.findIndex(threshold => years < threshold);
    return index === -1 ? "DAM4" : `DAM${index}`;
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
};

// ========== Module de construction des données de lieux ==========
const LocationDataBuilder = {
  _locationCache: new Map(),
  _generationCache: new Map(),

  /*
   * Fonction principale qui orchestre toute la construction des données de lieux
   * @param {number} maxGeneration - Génération maximum à considérer
   */
  buildCompleteLocationData: function(maxGeneration = null) {
    const targetGeneration = maxGeneration || max_gen;

    // Invalider le cache de tri car les lieux ont changé
    if (PlacesInterface.cache) {
        PlacesInterface.cache.invalidateSort();
    }

    // Vérifier le cache de génération
    if (this._generationCache.has(targetGeneration)) {
      const cached = this._generationCache.get(targetGeneration);
      lieux = cached.lieux;
      lieux_a = cached.lieux_a;
      this.restoreGlobalFlags();
      return;
    }

    // Construction normale si pas en cache
    this.resetLocationData();
    const filteredAncestors = this.filterAncestorsByGeneration(targetGeneration);

    // Traitement principal
    Object.values(filteredAncestors).forEach(person => {
      this.processPersonAllLocations(person);
    });

    // Construction de l'array final
    this.buildFinalLocationArray();

    // Mettre en cache pour cette génération
    this._generationCache.set(targetGeneration, {
      lieux: { ...lieux },
      lieux_a: [...lieux_a],
      flags: this.captureGlobalFlags()
    });
  },

  /**
   * Filtre les ancêtres selon la génération maximum
   * Optimisé avec calcul direct des plages de Sosa
   */
  filterAncestorsByGeneration: function(maxGeneration) {
    const filteredAncestors = {};

    for (let gen = 1; gen <= maxGeneration + 1; gen++) {
      const startSosa = 1 << (gen - 1);  // Décalage à gauche de bits = 2^(gen-1)
      const endSosa = (1 << gen) - 1;    // Décalage à gauche de bits = 2^gen-1

      for (let sosa = startSosa; sosa <= endSosa; sosa++) {
        const key = "S" + sosa;
        if (ancestor[key]) {
          filteredAncestors[key] = ancestor[key];
        }
      }
    }

    return filteredAncestors;
  },

  /**
   * Réinitialise toutes les structures de données globales
   */
  resetLocationData: function() {
    lieux = {};
    lieux_a = [];

    // Reset des flags globaux via Events
    Events.types.forEach(eventType => {
      window[Events.flagProp(eventType)] = false;
    });
  },

  /**
   * Traite tous les lieux associés à une personne
   */
  processPersonAllLocations: function(person) {
    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType);

      if (person[placeField]?.trim()) {
        this.processSingleLocation(person[placeField], eventType, person);
      }
    });
  },

  /**
   * Traite un lieu spécifique pour un type d'événement donné
   * @param {string} placeName - Nom du lieu brut
   * @param {string} eventType - Type d'événement (bi, ba, ma, de, bu)
   * @param {Object} person - Référence à la personne (pour de futures extensions)
   * @returns {Object} Information sur la méthode utilisée
   */
  processSingleLocation: function(placeName, eventType, person) {
    // Nettoyage du nom de lieu
    const cleanPlaceName = placeName.replace(/^\?, /, "");

     // Initialisation de l'entrée si première occurrence de ce lieu
    if (!lieux[cleanPlaceName]) {
      const locationStructure = this.extractLocationStructure(cleanPlaceName, eventType, person);
      lieux[cleanPlaceName] = this.createLocationEntry(cleanPlaceName, locationStructure);
    }

    // Mise à jour des compteurs
    this.updateLocationCounters(lieux[cleanPlaceName], eventType), person.sosa;
  },

  /**
   * Extrait la structure du lieu
   */
  extractLocationStructure: function(placeName, eventType, person) {
    const subField = eventType + '_sub';
    const mainField = eventType + '_main';

    return {
      fullName: placeName,
      isSubLocation: !!person[subField],
      subName: person[subField] || null,
      parentLocation: person[mainField] || null
    };
  },

  /**
   * Crée une entrée de lieu optimisée avec les métadonnées nécessaires
   * @param {string} cleanPlaceName - Nom nettoyé du lieu
   * @param {Object} locationStructure - Structure du lieu
   * @returns {Object} Entrée complète pour l'objet lieux
   */
  createLocationEntry: function(cleanPlaceName, locationStructure) {
    const entry = {
      cnt: 0,
      c: null,
      maxGeneration: 0,

      // Métadonnées géographiques
      isSubLocation: locationStructure.isSubLocation,
      subName: locationStructure.subName,
      parentLocation: locationStructure.parentLocation,

      // Données DOM préparées
      domAttributes: {
        'data-place': cleanPlaceName,
        'data-is-sublocation': locationStructure.isSubLocation,
        'data-events': []
      }
    };

    // Initialisation des compteurs et flags via Events
    Events.types.forEach(eventType => {
      entry[Events.count(eventType)] = 0;
      entry[Events.svgPrefix(eventType)] = false;
    });

    return entry;
  },

  /**
   * Met à jour les compteurs pour un lieu donné avec tracking de la génération
   * @param {Object} locationEntry - Entrée de lieu dans l'objet lieux
   * @param {string} eventType - Type d'événement à incrémenter
   */
  updateLocationCounters: function(locationEntry, eventType, sosa) {
    const countField = Events.count(eventType);
    const flagField = Events.svgPrefix(eventType);

    // Calculer la génération depuis le Sosa
    const generation = sosa ? Math.floor(Math.log2(sosa)) + 1 : 0;
    locationEntry.maxGeneration = Math.max(locationEntry.maxGeneration, generation);

    // Incréments
    locationEntry[countField]++;
    locationEntry.cnt++;

    // Flags et DOM attributes
    if (!locationEntry[flagField]) {
      locationEntry[flagField] = true;
      window[Events.flagProp(eventType)] = true;
      locationEntry.domAttributes['data-events'].push(eventType);
    }
  },

  /**
   * Construit l’array final lieux_a et assigne les IDs CSS pour la colorisation
   * Transforme l’objet lieux en array utilisable par les fonctions de tri
   */
  buildFinalLocationArray: function() {
    if (!lieux || typeof lieux !== 'object') {
      console.error('❌ Objet lieux corrompu, réinitialisation');
      lieux = {};
      lieux_a = [];
      return;
    }

    // Conversion en array avec validation
    try {
      lieux_a = Object.entries(lieux);
    } catch (error) {
      console.error('❌ Erreur lors de la conversion lieux->array:', error);
      lieux_a = [];
      return;
    }

    // Assignation des IDs avec protection
    lieux_a.forEach(([placeName, locationData], index) => {
      if (locationData && typeof locationData === 'object') {
        locationData.c = "L" + index;
      } else {
        console.warn(`⚠️ Données corrompues pour le lieu: ${placeName}`);
      }
    });
  },

  /**
   * Capture l'état des flags globaux pour le cache
   */
  captureGlobalFlags: function() {
    const flags = {};
    Events.types.forEach(eventType => {
      flags[Events.flagProp(eventType)] = window[Events.flagProp(eventType)];
    });
    return flags;
  },

  /**
   * Restaure les flags depuis le cache
   */
  restoreGlobalFlags: function() {
    const cached = this._generationCache.get(max_gen);
    if (cached?.flags) {
      Object.entries(cached.flags).forEach(([flag, value]) => {
        window[flag] = value;
      });
    }
  },

  /**
   * Valide et corrige les relations parent-enfant
   * @returns {Object} Rapport de validation
   */
  validateLocationHierarchy: function() {
    let corrections = 0;
    let subLocations = 0;
    let orphans = [];

    Object.entries(lieux).forEach(([placeName, locationData]) => {
      if (locationData.isSubLocation && locationData.parentLocation) {
        subLocations++;

        // Vérifier l'existence du parent
        if (!lieux[locationData.parentLocation]) {
          orphans.push(placeName);

          // Correction automatique
          locationData.isSubLocation = false;
          locationData.subName = null;
          locationData.parentLocation = null;
          locationData.domAttributes['data-is-sublocation'] = false;
          corrections++;
        }
      }
    });

    if (orphans.length > 0) {
      console.warn(`⚠️ Sous-lieux orphelins corrigés:`, orphans);
    }

    return {
      corrections,
      subLocations,
      orphans,
      totalLocations: Object.keys(lieux).length
    };
  },

  /**
   * Pré-calcule les classes CSS pour optimisation
   */
  precomputeCSSClasses: function(locationData, index) {
    const classes = {
      colorClass: `color-${(index % 12) + 1}`,
      eventClasses: []
    };

    Events.types.forEach(eventType => {
      if (locationData[Events.svgPrefix(eventType)]) {
        classes.eventClasses.push(Events.cssClass(eventType));
      }
    });

    return classes;
  },

  /**
   * Statistiques par type d’événement
   * @returns {Object} Statistiques détaillées par type
   */
  getEventStatistics: function() {
    const stats = {};
    Events.types.forEach(eventType => { stats[eventType] = 0; });

    Object.values(lieux).forEach(locationData => {
      Events.types.forEach(eventType => {
        const count = Events.count(eventType);
        stats[eventType] += locationData[count] || 0;
      });
    });

    return stats;
  },

 /**
   * Nettoie le cache (appelé lors des changements majeurs)
   */
  clearCache: function() {
    this._locationCache.clear();
    this._generationCache.clear();
  }
};

// ========== Interface du panneau des lieux ==========
const PlacesInterface = {
  cache: { // Cache avec invalidation sélective
    elements: {},
    sortedPlaces: null,
    lastSortMode: null,
    fragment: null, // Réutilisation du fragment DOM

    invalidateSort: function() {
      this.sortedPlaces = null;
    }
  },

 initialize: function() {
    this.cache.elements = { // Cache des éléments critiques
      panel: document.querySelector('.places-panel'),
      placesList: document.querySelector('.places-list'),
      summaryPlaces: document.querySelector('.summary-places-info'),
      summaryEventCounts: document.querySelectorAll('.summary-event-count'),
      summaryTotal: document.querySelector('.summary-total-events'),
      summaryPersons: document.querySelector('.summary-persons-count')
    };

    if (!this.cache.elements.placesList) {
      console.error('❌ Éléments HTML requis manquants');
      return false;
    }

    this.generatePlacesList();
    this.updateSummarySection();
    this.setupEventListeners();

    return true;
  },

  placeRowTemplate: function(placeName, placeData, index) {
    // Calculer l'index de couleur (rotation sur 12 couleurs)
    const colorIndex = (index % 12) + 1;

    // Préparer les attributs data-* en une seule chaîne
    const dataAttributes = `
      data-place="${placeName}"
      data-place-class="${placeData.c || ''}"
      data-total="${placeData.cnt || 0}"
    `.trim();

    // Template HTML complet
    return `
      <div class="place-row" data-index="${index}">
        <div class="place-indicators" id="indic-${index}"></div>
        <div class="place-content" id="place-${index}" ${dataAttributes}>
          <div class="place-left">
            <div class="place-color color-${colorIndex}"></div>
            <div class="place-name">${this.formatPlaceName(placeName, placeData)}</div>
          </div>
          <div class="place-right">
            <div class="place-events">
              ${this.generateEventItemsHTML(placeData)}
            </div>
            <div class="place-count">${placeData.cnt || 0}</div>
          </div>
        </div>
      </div>
    `;
  },

  formatPlaceName: function(placeName, placeData) {
    if (sortMode === 'alphabetical' && placeData.isSubLocation) {
      // Mode alphabétique : afficher le sous-nom avec indentation
      const label = placeData.subName || placeName;
      return `<span class="sublocation-indicator">└ </span>${label}`;
    }
    // Mode fréquence ou lieu principal : afficher le nom complet
    return placeName;
  },

  generateEventItemsHTML: function(placeData) {
    const htmlParts = Events.types.map(eventType => {
      const label = Events.label(eventType);
      const eventCount = Events.count(eventType);
      const count = placeData[eventCount] || 0;
      const isActive = count > 0;

      return `
        <div class="event-item ${isActive ? 'active' : ''}" data-event="${eventType}">
          <span class="event-count">${count > 1 ? count : ''}</span>
          <span class="event-label">${label}</span>
        </div>
      `;
    });

    return htmlParts.join('');
  },

  /**
   * Génèration de la liste des lieux enrichie
   */
  generatePlacesList: function() {
    const container = this.cache.elements.placesList;
    if (!container) return;

    // Utiliser le cache de tri
    const sortedPlaces = this.getCachedSortedPlaces();

    // Fragment DOM pour performance
    const fragment = document.createDocumentFragment();

    sortedPlaces.forEach(([placeName, placeData], index) => {
      const row = this.createPlaceRow(placeName, placeData, index);

      // Enrichissement immédiat des références DOM
      placeData.domElement = row.querySelector('.place-content');
      placeData.visualIndex = index;
      placeData.indicatorElement = row.querySelector('.place-indicators');

      fragment.appendChild(row);
    });

    // Une seule manipulation DOM
    container.innerHTML = '';
    container.appendChild(fragment);
  },

  /**
   * Tri avec cache intelligent
   */
  getCachedSortedPlaces: function() {
    if (this.cache.sortedPlaces && this.cache.lastSortMode === sortMode) {
      return this.cache.sortedPlaces;
    }

    this.cache.sortedPlaces = this.getSortedPlaces();
    this.cache.lastSortMode = sortMode;
    return this.cache.sortedPlaces;
  },

  /**
   * Tri optimisé avec support des données natives
     */
  getSortedPlaces: function() {
    if (sortMode === 'alphabetical') {
      // Utilisation directe des données natives _main pour le regroupement
      const groups = new Map();

      Object.entries(lieux).forEach(([name, data]) => {
        if (data.isSubLocation && data.parentLocation) {
          // Sous-lieu : l'ajouter au groupe du parent
          if (!groups.has(data.parentLocation)) {
            groups.set(data.parentLocation, { main: null, subs: [] });
          }
          groups.get(data.parentLocation).subs.push([name, data]);
        } else {
          // Lieu principal
          if (!groups.has(name)) {
            groups.set(name, { main: null, subs: [] });
          }
          groups.get(name).main = [name, data];
        }
      });

      // Construire l'array trié
      const sorted = [];
      Array.from(groups.entries())
        .sort(([a], [b]) => a.localeCompare(b, 'fr', { sensitivity: 'base' }))
        .forEach(([groupName, group]) => {
          if (group.main) sorted.push(group.main);
          group.subs
            .sort(([a], [b]) => a.localeCompare(b, 'fr', { sensitivity: 'base' }))
            .forEach(sub => sorted.push(sub));
        });

      return sorted;
    }

    // Mode fréquence : simple tri par compteur
    return Object.entries(lieux).sort((a, b) => b[1].cnt - a[1].cnt);
  },

  /**
   * Création optimisée des éléments DOM
   */
  createPlaceRow: function(placeName, placeData, index) {
    const row = document.createElement('div');
    row.className = 'place-row';
    row.dataset.index = index;

    // Indicateurs d'événements
    const indicators = document.createElement('div');
    indicators.className = 'place-indicators';
    indicators.id = `indic-${index}`;

    // Contenu principal
    const content = document.createElement('div');
    content.className = 'place-content';
    content.id = `place-${index}`;
    content.dataset.place = placeName;
    content.dataset.placeClass = placeData.c || '';
    content.dataset.total = placeData.cnt || 0;

    // Construction du contenu interne
    content.innerHTML = this.buildPlaceContentHTML(placeName, placeData, index);

    row.appendChild(indicators);
    row.appendChild(content);

    return row;
  },

  /**
   * Construction optimisée du HTML interne
   */
  buildPlaceContentHTML: function(placeName, placeData, index) {
    const colorIndex = (index % 12) + 1;
    const displayName = this.getDisplayName(placeName, placeData);
    const eventItemsHTML = this.buildEventItemsHTML(placeData);

    return `
      <div class="place-left">
        <div class="place-color color-${colorIndex}"></div>
        <div class="place-name">${displayName}</div>
      </div>
      <div class="place-right">
        <div class="place-events">${eventItemsHTML}</div>
        <div class="place-count">${placeData.cnt || 0}</div>
      </div>
    `;
  },

  /**
   * Nom d'affichage selon le mode et les données natives
   */
  getDisplayName: function(placeName, placeData) {
    if (sortMode === 'alphabetical' && placeData.isSubLocation && placeData.subName) {
      return `<span class="sublocation-indicator">└ </span>${placeData.subName}`;
    }
    return placeName;
  },

  /**
   * Construction des indicateurs d'événements
   */
  buildEventItemsHTML: function(placeData) {
    return Events.types.map(eventType => {
      const count = placeData[Events.count(eventType)] || 0;
      const isActive = count > 0;

      return `
        <div class="event-item ${isActive ? 'active' : ''}" data-event="${eventType}">
          <span class="event-count">${count > 1 ? count : ''}</span>
          <span class="event-label">${Events.label(eventType)}</span>
        </div>
      `;
    }).join('');
  },

  /**
   * Mise à jour du résumé et support du survol des totaux
   */
  updateSummarySection: function() {
    // Générations
    const genElement = this.cache.elements.panel.querySelector('.generation-count');
    if (genElement) {
      const genLabel = window.FC_TRANSLATIONS?.[max_gen > 1 ? 'generations' : 'generation'] || 'génération';
      genElement.textContent = `${max_gen} ${genLabel}`;
    }

    // Lieux
    const placeCount = Object.keys(lieux).length;
    const placeLabel = window.FC_TRANSLATIONS?.[placeCount > 1 ? 'places' : 'place'] || 'lieu';
    this.cache.elements.summaryPlaces.textContent = `${placeCount} ${placeLabel}`;

    // Statistiques par événement
    const stats = LocationDataBuilder.getEventStatistics();

    Events.types.forEach((eventType, index) => {
      const countElement = this.cache.elements.summaryEventCounts[index];
      if (countElement) {
        const count = stats[eventType] || 0;
        countElement.textContent = count;

        // Préparation pour le futur surlignage NBMDS
        const labelElement = countElement.previousElementSibling;
        if (labelElement) {
          labelElement.dataset.eventType = eventType;
          labelElement.dataset.eventCount = count;
        }
      }
    });

    // Total
    const totalEvents = Object.values(stats).reduce((sum, count) => sum + count, 0);
    this.cache.elements.summaryTotal.textContent = totalEvents;

    // Personnes
    this.updatePersonsCounter();
  },

  /**
   * Compteur de personnes
   */
  updatePersonsCounter: function() {
    const personsElement = this.cache.elements.summaryPersons;
    if (!personsElement) return;

    // Utiliser le cache des ancêtres filtrés de LocationDataBuilder
    const filteredAncestors = LocationDataBuilder.filterAncestorsByGeneration(max_gen);

    let personsWithPlaces = 0;
    Object.values(filteredAncestors).forEach(person => {
      // Vérifier si au moins un lieu existe
      const hasPlace = Events.types.some(eventType => {
        const placeField = Events.place(eventType);
        return person[placeField]?.trim();
      });

      if (hasPlace) personsWithPlaces++;
    });

    personsElement.textContent = personsWithPlaces;
    const label = window.FC_TRANSLATIONS?.[personsWithPlaces > 1 ? 'persons' : 'person'] || 'personne';
    personsElement.title = `${personsWithPlaces} ${label} avec lieux`;
  },

 /**
   * Configuration des événements avec support du surlignage bidirectionnel
   */
  setupEventListeners: function() {
    if (!this.cache.elements.placesList) return;
    PlacesHighlighter.initialize();

    this.cache.elements.placesList.addEventListener('click', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        e.preventDefault();
        e.stopPropagation();

        if (e.ctrlKey || e.metaKey) {
          const placeName = placeContent.dataset.place;
          if (placeName && URLManager.navigateToPlace) {
            URLManager.navigateToPlace(placeName, true);
          }
        }
      }
    });
  },

  /**
   * Configuration du surlignage des totaux d'événements
   */
  setupEventTotalHighlights: function() {
    document.querySelectorAll('.summary-event-label').forEach(label => {
      const eventType = label.dataset.eventType;
      if (!eventType) return;

      label.style.cursor = 'pointer';

      label.addEventListener('mouseenter', () => {
        PlacesHighlighter.highlightByEventType(eventType);
      });

      label.addEventListener('mouseleave', () => {
        PlacesHighlighter.clearAllHighlights();
      });
    });
  },

  handlePlaceClick: function(placeName, event) {
    if (!event.ctrlKey && !event.metaKey) {
      event.preventDefault();
      return;
    }

    if (placeName && URLManager.navigateToPlace) {
      const newTab = true;
      URLManager.navigateToPlace(placeName, newTab);
    }
  }
};

// ========== Interface utilisateur pour le panneau des lieux ==========
const PlacesPanelControls = {
  searchDebounceTimer: null, // Timers pour optimisation

  initialize: function() {
    if (!PlacesInterface.cache.elements.panel) {
      console.error('❌ PlacesInterface doit être initialisé avant PlacesPanelControls');
      return false;
    }

    this.initializeDefaultStates();
    this.setupEventListeners();
    return true;
  },

  /**
   * Configuration des listeners avec délégation
   */
  setupEventListeners: function() {
    const panel = PlacesInterface.cache.elements.panel;

    // Délégation pour tous les clics
    panel.addEventListener('click', (e) => {
      // Fermeture du panneau
      if (e.target.matches('.panel-close')) {
        e.preventDefault();
        this.togglePanel();
      }
      // Tri
      else if (e.target.closest('.sort-toggle')) {
        e.preventDefault();
        this.toggleSort();
      }
      // Événements
      else if (e.target.closest('.events-toggle')) {
        e.preventDefault();
        this.toggleEventsDisplay();
      }
      // Effacer recherche
      else if (e.target.matches('.search-clear')) {
        e.preventDefault();
        this.clearSearch();
      }
    });

    // Recherche avec debounce
    this.setupSearchListener();
  },

  /**
   * Recherche optimisée avec debounce
   */
  setupSearchListener: function() {
    const panel = PlacesInterface.cache.elements.panel;
    const searchInput = panel.querySelector('.search-input');
    if (!searchInput) return;

    searchInput.addEventListener('input', (e) => {
      clearTimeout(this.searchDebounceTimer);
      const query = e.target.value;

      // Recherche immédiate si effacement
      if (!query) {
        this.filterPlaces('');
        return;
      }

      // Debounce pour les autres cas
      this.searchDebounceTimer = setTimeout(() => {
        this.filterPlaces(query);
      }, 150);
    });
  },

  /**
   * États par défaut cohérents
   */
  initializeDefaultStates: function() {
    this.updateSortButtonIcon();

    const panel = PlacesInterface.cache.elements.panel;

    // État des événements
    if (showEvents) {
      panel.classList.add('show-events');
      const icon = document.querySelector('.events-toggle i');
      if (icon) icon.className = 'far fa-eye-slash';
    }

    // Recherche
    const searchInput = panel.querySelector('.search-input');
    if (searchInput) searchInput.value = '';
  },

  /**
   * Basculement du panneau
   */
  togglePanel: function() {
    const panel = PlacesInterface.cache.elements.panel;
    const isVisible = panel.style.display !== 'none';
    panel.style.display = isVisible ? 'none' : 'block';
  },

  /**
   * Basculement du tri
   */
  toggleSort: function() {
    sortMode = sortMode === 'frequency' ? 'alphabetical' : 'frequency';

    PlacesInterface.cache.invalidateSort();
    this.updateSortButtonIcon();
    PlacesInterface.generatePlacesList();
    URLManager.updateCurrentURL();
  },

  /**
   * Mise à jour de l'icône de tri
   */
  updateSortButtonIcon: function() {
    const icon = document.querySelector('.sort-toggle i');
    if (icon) {
      icon.className = sortMode === 'alphabetical'
        ? 'fas fa-arrow-down-wide-short'
        : 'fas fa-arrow-down-a-z';
    }

    const button = document.querySelector('.sort-toggle');
    if (button) {
      const key = sortMode === 'alphabetical' ? 'sort_alphabetically' : 'sort_by_frequency';
      button.title = window.FC_TRANSLATIONS?.[key] || 'Changer le tri';
    }
  },

  /**
   * Basculement de l'affichage détaillé
   */
  toggleEventsDisplay: function() {
    showEvents = !showEvents;

    const panel = PlacesInterface.cache.elements.panel;
    panel.classList.toggle('show-events');

    const icon = document.querySelector('.events-toggle i');
    if (icon) {
      icon.className = panel.classList.contains('show-events')
        ? 'far fa-eye-slash'
        : 'far fa-eye';
    }

    URLManager.updateCurrentURL();
  },

  /**
   * Filtrage optimisé
   */
  filterPlaces: function(query) {
    const normalizedQuery = query.toLowerCase().trim();

    requestAnimationFrame(() => {
      const rows = document.querySelectorAll('.place-row');
      let visibleCount = 0;
      let totalCount = rows.length;

      if (!normalizedQuery) {
        // Réafficher tout rapidement
        rows.forEach(row => {
          row.style.display = '';
        });
        visibleCount = totalCount;
      } else {
        // Filtrer
        rows.forEach(row => {
          const nameEl = row.querySelector('.place-name');
          const placeName = nameEl ? nameEl.textContent.toLowerCase() : '';
          const matches = placeName.includes(normalizedQuery);

          row.style.display = matches ? '' : 'none';
          if (matches) visibleCount++;
        });
      }

      this.updateClearButtonVisibility(normalizedQuery);
      this.updateSearchResultsCount(visibleCount, totalCount);
    });
  },

  /**
   * Affichage du compteur de résultats
   */
  updateSearchResultsCount: function(visible, total) {
    let counter = document.querySelector('.search-results-count');

    if (visible < total) {
      if (!counter) {
        counter = document.createElement('div');
        counter.className = 'search-results-count';
        counter.style.cssText = 'font-size: 10px; color: #666; margin-top: 2px;';
        const section = document.querySelector('.controls-search-section');
        if (section) section.appendChild(counter);
      }
      counter.textContent = `${visible}/${total}`;
      counter.style.display = 'block';
    } else if (counter) {
      counter.style.display = 'none';
    }
  },

  /**
   * Visibilité du bouton clear
   */
  updateClearButtonVisibility: function(query) {
    const clearBtn = document.querySelector('.search-clear');
    if (clearBtn) {
      clearBtn.style.display = query ? 'block' : 'none';
    }
  },

  /**
   * Effacement de la recherche
   */
  clearSearch: function() {
    const input = document.querySelector('.search-input');
    if (input) {
      input.value = '';
      input.focus();
    }

    this.filterPlaces('');
  }
};


// ========== Module de surlignage bidirectionnel pour les lieux ==========
const PlacesHighlighter = {
  // État interne
  state: {
    highlighted: new Set(),
    svgElements: new Map(),
    indicators: new Map(),
    expandedNames: new Map(),
    currentHoveredPlace: null,
    hoverTimeout: null

  },

  /**
   * Configure un système d'événements robuste pour le surlignage
   * Utilise une approche basée sur l'état plutôt que sur les événements directs
   */
  setupEventHandlers: function() {
    const placesList = document.querySelector('.places-list');
    if (!placesList) return;

    // Utiliser la capture d'événements pour intercepter tous les mouvements
    placesList.addEventListener('mousemove', (e) => {
      // Trouver le place-row le plus proche
      const placeRow = e.target.closest('.place-row');

      if (placeRow) {
        const placeContent = placeRow.querySelector('.place-content');
        if (placeContent) {
          const placeName = placeContent.dataset.place;

          // Si on est déjà sur ce lieu, ne rien faire
          if (this.state.currentHoveredPlace === placeName) {
            return;
          }

          // Si on était sur un autre lieu, le nettoyer d'abord
          if (this.state.currentHoveredPlace && this.state.currentHoveredPlace !== placeName) {
            this.clearHighlightForPlace(this.state.currentHoveredPlace);
          }

          // Appliquer le nouveau surlignage
          this.state.currentHoveredPlace = placeName;
          this.highlightPlace(placeName, 'list');
        }
      }
    }, { capture: true, passive: true });

    // Gérer la sortie complète de la liste
    placesList.addEventListener('mouseleave', (e) => {
      // Vérifier que la souris quitte vraiment la liste
      if (!placesList.contains(e.relatedTarget)) {
        // Utiliser un petit délai pour éviter les clignotements
        clearTimeout(this.state.hoverTimeout);
        this.state.hoverTimeout = setTimeout(() => {
          if (this.state.currentHoveredPlace) {
            this.clearAllHighlights();
            this.state.currentHoveredPlace = null;
          }
        }, 50);
      }
    });

    // Annuler le timeout si on revient dans la liste
    placesList.addEventListener('mouseenter', () => {
      clearTimeout(this.state.hoverTimeout);
    });
  },

  /**
   * Nettoyage spécifique pour un lieu
   */
  clearHighlightForPlace: function(placeName) {
    const placeData = lieux[placeName];
    if (!placeData) return;

    // Nettoyer le surlignage visuel
    if (placeData.domElement) {
      placeData.domElement.classList.remove('person-match');

      // Restaurer la hauteur minimale
      const placeRow = placeData.domElement.closest('.place-row');
      if (placeRow) {
        placeRow.style.minHeight = '';
        placeRow.classList.remove('tall-row');
      }
    }

    // Restaurer le nom si expansé
    if (this.state.expandedNames.has(placeData.domElement)) {
      const originalHtml = this.state.expandedNames.get(placeData.domElement);
      const nameElement = placeData.domElement.querySelector('.place-name');
      if (nameElement) {
        nameElement.innerHTML = originalHtml;
        nameElement.style.fontWeight = '';
      }
      this.state.expandedNames.delete(placeData.domElement);
    }

    // Nettoyer les indicateurs
    if (placeData.indicatorElement && this.state.indicators.has(placeData.indicatorElement)) {
      const indicators = this.state.indicators.get(placeData.indicatorElement);
      indicators.forEach(el => el.remove());
      this.state.indicators.delete(placeData.indicatorElement);
    }

    // Nettoyer le surlignage SVG
    this.highlightInSVG(placeName, placeData, false);

    this.state.highlighted.delete(placeName);
  },

  highlight: function(placeNames, eventTypes, source = 'svg') {
    this.clearAllHighlights();

    if (!placeNames?.length) return;

    // Créer une map pour organiser les données
    const highlightMap = new Map();

    placeNames.forEach((placeName, index) => {
      const placeData = lieux[placeName];
      if (!placeData) return;

      const events = eventTypes[index] || [];
      highlightMap.set(placeName, { placeData, events });
      this.state.highlighted.add(placeName);
    });

    // Appliquer les surlignages
    this.applyHighlights(highlightMap, source);
  },

  /**
   * Fonction principale de surlignage multi-lieux
   * Utilisée pour le survol des totaux NBMDS
   */
  highlightPlace: function(placeName, source = 'list') {
    const placeData = lieux[placeName];
    if (!placeData) return;

    // Marquer visuellement le lieu
    if (placeData.domElement) {
      // Capturer la hauteur actuelle avant les changements
      const placeRow = placeData.domElement.closest('.place-row');
      if (placeRow && !placeRow.style.minHeight) {
        // Fixer une hauteur minimale pour éviter les sauts
        placeRow.style.minHeight = placeRow.offsetHeight + 'px';
      }

      placeData.domElement.classList.add('person-match');
      this.state.highlighted.add(placeName);

      // Expansion du nom si nécessaire
      if (placeData.isSubLocation && sortMode === 'alphabetical') {
        this.expandSubLocationName(placeData);
      }

      // Collecter TOUS les types d'événements pour ce lieu
      const eventTypes = [];
      Events.types.forEach(eventType => {
        if (placeData[Events.svgPrefix(eventType)]) {
          eventTypes.push(eventType);
        }
      });

      // Ajouter les indicateurs avec gestion de la hauteur
      if (placeData.indicatorElement && eventTypes.length > 0) {
        this.addIndicatorsWithHeightManagement(placeData, eventTypes);
      }
    }

    // Surligner dans le SVG avec colorisation
    this.highlightInSVG(placeName, placeData, true);

    // Griser les autres éléments
    this.grayOutOthers();
  },

  /**
   * Version améliorée de addIndicators qui gère mieux les changements de hauteur
   */
  addIndicatorsWithHeightManagement: function(placeData, events) {
    const container = placeData.indicatorElement;
    const placeRow = container.closest('.place-row');
    if (!container || !events.length) return;

    // Préparer le conteneur pour recevoir les indicateurs
    container.style.visibility = 'hidden'; // Masquer temporairement

    // Ajouter les indicateurs
    const indicators = [];
    events.forEach((event, index) => {
      const indicator = document.createElement('div');
      indicator.className = `indicator ${Events.cssClass(event)}`;
      indicator.textContent = Events.label(event);
      container.appendChild(indicator);
      indicators.push(indicator);

      // Line break pour 4+ événements
      if (index === 1 && events.length >= 4) {
        const breaker = document.createElement('div');
        breaker.className = 'line-break';
        container.appendChild(breaker);
        indicators.push(breaker);
      }
    });

    // Ajuster la classe tall-row si nécessaire
    if (events.length >= 4 && placeRow) {
      placeRow.classList.add('tall-row');
    }

    // Rendre visible avec une micro-animation
    requestAnimationFrame(() => {
      container.style.visibility = 'visible';
      container.style.opacity = '0';
      container.style.transition = 'opacity 0.1s ease-in';

      requestAnimationFrame(() => {
        container.style.opacity = '1';
      });
    });

    // Stocker pour nettoyage
    this.state.indicators.set(container, indicators);
  },

  /**
   * Application des surlignages
   */
  applyHighlights: function(highlightMap, source) {
    const elementsToShow = [];

    highlightMap.forEach(({ placeData, events }, placeName) => {
      // Surlignage dans la liste
      if (placeData.domElement) {
        placeData.domElement.classList.add('person-match');

        // Expansion du nom si nécessaire
        if (placeData.isSubLocation && sortMode === 'alphabetical') {
          this.expandSubLocationName(placeData);
        }

        // Ajout des indicateurs
        if (placeData.indicatorElement && events.length > 0) {
          this.addIndicators(placeData.indicatorElement, events);
        }

        elementsToShow.push({
          element: placeData.domElement,
          index: placeData.visualIndex,
          placeName: placeName
        });
      }

      // Surlignage dans le SVG (bidirectionnel)
      if (source === 'list' || source === 'totals') {
        this.highlightInSVG(placeName, placeData, true);
      }
    });

    // Griser les autres
    this.grayOutOthers();

    // Gestion de l'overflow
    if (elementsToShow.length > 0 && typeof ModernOverflowManager !== 'undefined') {
      setTimeout(() => {
        ModernOverflowManager.handleOverflow(elementsToShow);
      }, 50);
    }
  },

  /**
   * Surlignage d'un lieu unique depuis la liste
   */
  highlightPlace: function(placeName, source = 'list') {
    this.clearAllHighlights();

    const placeData = lieux[placeName];
    if (!placeData) return;

    // Surligner dans la liste
    if (placeData.domElement) {
      placeData.domElement.classList.add('person-match');
      this.state.highlighted.add(placeName);

      // Expansion du nom si nécessaire
      if (placeData.isSubLocation && sortMode === 'alphabetical') {
        this.expandSubLocationName(placeData);
      }

      // Collecter TOUS les types d'événements pour ce lieu
      const eventTypes = [];
      Events.types.forEach(eventType => {
        if (placeData[Events.svgPrefix(eventType)]) {
          eventTypes.push(eventType);
        }
      });

      // Ajouter les indicateurs pour tous les événements
      if (placeData.indicatorElement && eventTypes.length > 0) {
        this.addIndicators(placeData.indicatorElement, eventTypes);
      }
    }

    // Surligner dans le SVG avec colorisation
    this.highlightInSVG(placeName, placeData, true);

    // Griser les autres éléments
    this.grayOutOthers();
  },

  /**
   * Surlignage bidirectionnel corrigé avec colorisation
   */
  highlightInSVG: function(placeName, placeData, highlight) {
    const placeClass = placeData.c; // Ex: "L0"

    // Pour chaque type d'événement présent dans ce lieu
    Events.types.forEach(eventType => {
      if (placeData[Events.svgPrefix(eventType)]) {
        const svgClass = `${Events.svgPrefix(eventType)}-${placeClass}`;
        const elements = document.getElementsByClassName(svgClass);

        Array.from(elements).forEach(element => {
          if (highlight) {
            // Déterminer le contexte de l'élément
            const isMarriageClass = svgClass.startsWith('ma-');
            const parentGroup = element.parentNode;
            const isInMarriageGroup = parentGroup && parentGroup.id && parentGroup.id.startsWith('M');
            const isInPersonGroup = parentGroup && parentGroup.id && parentGroup.id.startsWith('S');

            // Pour les mariages, coloriser uniquement les éléments dans les groupes de mariage
            if (isMarriageClass && isInPersonGroup && !isInMarriageGroup) {
              // Ne pas coloriser les secteurs de personnes avec la couleur mariage
              return;
            }

            // Sauvegarder l'état original
            if (!element.dataset.originalFill) {
              element.dataset.originalFill = element.style.fill || '';
              element.dataset.originalClass = element.getAttribute('class') || '';
            }

            // Appliquer la colorisation selon le type d'événement
            element.classList.add('svg-place-highlight');
            element.classList.add(`event-highlight-${eventType}`);

            // Tracker pour éviter les conflits
            this.state.svgElements.set(element, placeName);
          } else {
            // Restaurer l'état original
            element.classList.remove('svg-place-highlight');
            Events.types.forEach(et => {
              element.classList.remove(`event-highlight-${et}`);
            });

            element.style.fill = element.dataset.originalFill || '';

            delete element.dataset.originalFill;
            delete element.dataset.originalClass;

            this.state.svgElements.delete(element);
          }
        });
      }
    });
  },

  /**
   * Surlignage par type d'événement (pour les totaux NBMDS)
   */
  highlightByEventType: function(eventType) {
    this.clearAllHighlights();

    const placesToHighlight = [];
    const eventsForEachPlace = [];

    // Collecter tous les lieux ayant ce type d'événement
    Object.entries(lieux).forEach(([placeName, placeData]) => {
      if (placeData[Events.svgPrefix(eventType)]) {
        placesToHighlight.push(placeName);
        eventsForEachPlace.push([eventType]);
      }
    });

    // NE PAS ajouter d'indicateurs pour le survol des totaux
    // Surligner uniquement les lieux dans la liste
    placesToHighlight.forEach(placeName => {
      const placeData = lieux[placeName];
      if (placeData?.domElement) {
        placeData.domElement.classList.add('person-match');
        this.state.highlighted.add(placeName);
      }
    });

    // Coloriser les éléments SVG avec la couleur de l'événement
    if (eventType === 'marriage') {
      // Pour les mariages, coloriser UNIQUEMENT les secteurs de mariage
      this.highlightMarriageSectorsOnly(placesToHighlight);
    } else {
      // Pour NBDS, coloriser les individus avec la couleur de l'événement
      this.highlightIndividualsByEventType(placesToHighlight, eventType);
    }

    // Griser les autres éléments
    this.grayOutOthers();
  },

  // Surligner uniquement les secteurs de mariage
  highlightMarriageSectorsOnly: function(placesToHighlight) {
    placesToHighlight.forEach(placeName => {
      const placeData = lieux[placeName];
      if (!placeData) return;

      const placeClass = placeData.c;
      const svgClass = `ma-${placeClass}`;
      const elements = document.getElementsByClassName(svgClass);

      Array.from(elements).forEach(element => {
        // Vérifier que c'est bien dans un groupe de mariage
        const parentGroup = element.parentNode;
        if (parentGroup && parentGroup.id && parentGroup.id.startsWith('M')) {
          if (!element.dataset.originalFill) {
            element.dataset.originalFill = element.style.fill || '';
          }
          element.classList.add('event-highlight-marriage');
          this.state.svgElements.set(element, placeName);
        }
      });
    });
  },

  // Surligner les individus par type d'événement
  highlightIndividualsByEventType: function(placesToHighlight, eventType) {
    placesToHighlight.forEach(placeName => {
      const placeData = lieux[placeName];
      if (!placeData) return;

      const placeClass = placeData.c;
      const svgPrefix = Events.svgPrefix(eventType);
      const svgClass = `${svgPrefix}-${placeClass}`;
      const elements = document.getElementsByClassName(svgClass);

      Array.from(elements).forEach(element => {
        // Vérifier que c'est bien un secteur de personne
        const parentGroup = element.parentNode;
        if (parentGroup && parentGroup.id && parentGroup.id.startsWith('S')) {
          if (!element.dataset.originalFill) {
            element.dataset.originalFill = element.style.fill || '';
          }
          element.classList.add(`event-highlight-${eventType}`);
          this.state.svgElements.set(element, placeName);
        }
      });
    });
  },

  /**
   * Expansion du nom de sous-lieu
   */
  expandSubLocationName: function(placeData) {
    const nameElement = placeData.domElement.querySelector('.place-name');
    if (!nameElement || this.state.expandedNames.has(placeData.domElement)) return;

    // Sauvegarder l'original
    this.state.expandedNames.set(placeData.domElement, nameElement.innerHTML);

    // Créer le nouveau contenu sans vider l'élément
    const fullName = placeData.parentLocation ?
      `   ${placeData.subName} – ${placeData.parentLocation}` :
      placeName;

    // Mettre à jour le texte sans perdre le style
    nameElement.textContent = fullName;
    nameElement.style.fontWeight = '600';
  },

  /**
   * Ajout des indicateurs d'événements
   */
  addIndicators: function(container, events) {
    if (!container || !events.length) return;

    const eventArray = Array.isArray(events) ? events : [events];
    const indicators = [];

    events.forEach((event, index) => {
      const indicator = document.createElement('div');
      indicator.className = `indicator ${Events.cssClass(event)}`;
      indicator.textContent = Events.label(event);
      container.appendChild(indicator);
      indicators.push(indicator);

      // Line break pour 4+ événements
      if (index === 1 && events.length >= 4) {
        const breaker = document.createElement('div');
        breaker.className = 'line-break';
        container.appendChild(breaker);
        indicators.push(breaker);
      }
    });

    // Ajuster la hauteur si nécessaire
    if (events.length >= 4) {
      container.closest('.place-row')?.classList.add('tall-row');
    }

    // Stocker pour nettoyage
    this.state.indicators.set(container, indicators);
  },

  /**
   * Grise les éléments non surlignés
   */
  grayOutOthers: function() {
    document.querySelectorAll('.place-content').forEach(el => {
      if (!el.classList.contains('person-match')) {
        el.classList.add('grayed-out');
      }
    });
  },

  /**
   * Nettoie tous les surlignages
   */
  clearAllHighlights: function() {
    // Nettoyer chaque lieu individuellement
    this.state.highlighted.forEach(placeName => {
      this.clearHighlightForPlace(placeName);
    });

    // Nettoyer les éléments grisés
    document.querySelectorAll('.grayed-out').forEach(el => {
      el.classList.remove('grayed-out');
    });

    // Reset de l'état
    this.state.highlighted.clear();
    this.state.svgElements.clear();
    this.state.indicators.clear();
    this.state.expandedNames.clear();
    this.state.currentHoveredPlace = null;

    // Nettoyer l'overflow si présent
    if (typeof ModernOverflowManager !== 'undefined' && ModernOverflowManager.clearOverflowSections) {
      ModernOverflowManager.clearOverflowSections();
    }
  },

  initialize: function() {
    this.setupEventHandlers()
    // Configurer les événements pour les totaux NBMDS
    PlacesInterface.setupEventTotalHighlights();
  }
};

// ========== Module de rendu circulaire ==========
const CircularModeRenderer = {
  // Active/désactive le mode circulaire
  toggle: function() {
    isCircularMode = !isCircularMode;

    const btn = document.getElementById('b-circular-mode');
    if (btn) {
      btn.classList.toggle('active', isCircularMode);
      btn.title = isCircularMode ? 'Revenir au mode éventail' : 'Mode circulaire (360°)';
    }

    // Désactiver/activer les boutons d'angle au lieu de les masquer
    CONFIG.available_angles.forEach(angle => {
      const angleBtn = document.getElementById(`b-angle-${angle}`);
      if (angleBtn) {
        angleBtn.classList.toggle('disabled', isCircularMode);
        angleBtn.disabled = isCircularMode;
      }
    });

    // Mettre à jour l'URL
    URLManager.updateCurrentURL();

    // Redessiner
    FanchartApp.calculateDimensions();
    FanchartApp.reRenderWithCurrentGenerations();
  },

  /**
   * Rend le centre en mode couple (S2 au nord, S3 au sud)
   */
  renderCoupleCenter: function() {
    const centerGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    centerGroup.setAttribute("id", "couple-center");
    fanchart.appendChild(centerGroup);

    const s2 = ancestor["S2"]; // Père
    const s3 = ancestor["S3"]; // Mère
    const r = CONFIG.a_r[0];

    // Groupes individuels pour chaque parent,
    // facilite la gestion des événements et du surlignage —
    // Groupe pour le père (demi-cercle nord)
    if (s2) {
        const s2Group = document.createElementNS("http://www.w3.org/2000/svg", "g");
        s2Group.setAttribute("id", "couple-s2");
        centerGroup.appendChild(s2Group);

        // Secteur de fond
        SVGRenderer.drawPie(s2Group, 0, r, -90, 90, s2,
          { type: 'person', isBackground: true });

        // Texte centré dans le demi-cercle nord
        const text2 = document.createElementNS("http://www.w3.org/2000/svg", "text");
        text2.setAttribute("x", center_x);
        text2.setAttribute("y", center_y - r/3);
        text2.setAttribute("text-anchor", "middle");
        text2.setAttribute("class", "couple-text");
        text2.innerHTML = `<tspan>${s2.fn}</tspan><tspan x="${center_x}" dy="15">${s2.sn}</tspan>`;
        s2Group.appendChild(text2);

        // Secteur interactif (doit être en dernier pour capturer les événements)
        SVGRenderer.drawPie(s2Group, 0, r, -90, 90, s2, { type: 'person' });
      }

      // Groupe pour la mère (demi-cercle sud)
      if (s3) {
        const s3Group = document.createElementNS("http://www.w3.org/2000/svg", "g");
        s3Group.setAttribute("id", "couple-s3");
        centerGroup.appendChild(s3Group);

        // Secteur de fond
        SVGRenderer.drawPie(s3Group, 0, r, 90, 270, s3,
          { type: 'person', isBackground: true });

        // Texte centré dans le demi-cercle sud
        const text3 = document.createElementNS("http://www.w3.org/2000/svg", "text");
        text3.setAttribute("x", center_x);
        text3.setAttribute("y", center_y + r/3);
        text3.setAttribute("text-anchor", "middle");
        text3.setAttribute("class", "couple-text");
        text3.innerHTML = `<tspan>${s3.fn}</tspan><tspan x="${center_x}" dy="15">${s3.sn}</tspan>`;
        s3Group.appendChild(text3);

        // Secteur interactif
        SVGRenderer.drawPie(s3Group, 0, r, 90, 270, s3, { type: 'person' });
      }

      // Ligne de séparation élégante
      const separator = document.createElementNS("http://www.w3.org/2000/svg", "line");
      separator.setAttribute("x1", center_x - r);
      separator.setAttribute("y1", center_y);
      separator.setAttribute("x2", center_x + r);
      separator.setAttribute("y2", center_y);
      separator.setAttribute("stroke", "#ccc");
      separator.setAttribute("stroke-width", "1");
      separator.setAttribute("stroke-dasharray", "3,3"); // Ligne pointillée pour plus d'élégance
      centerGroup.appendChild(separator);
      /*
      // Optionnel : Ajouter un petit texte pour le S1 au centre
      if (ancestor["S1"]) {
        const s1Text = document.createElementNS("http://www.w3.org/2000/svg", "text");
        s1Text.setAttribute("x", center_x);
        s1Text.setAttribute("y", center_y);
        s1Text.setAttribute("text-anchor", "middle");
        s1Text.setAttribute("class", "s1-indicator");
        s1Text.setAttribute("font-size", "10");
        s1Text.setAttribute("fill", "#666");
        s1Text.textContent = "⬤"; // Point central discret
        const s1Title = document.createElementNS("http://www.w3.org/2000/svg", "title");
        s1Title.textContent = `${ancestor["S1"].fn} ${ancestor["S1"].sn} (enfant du couple)`;
        s1Text.appendChild(s1Title);
        centerGroup.appendChild(s1Text);
      }*/
    },

  /**
   * Décale une branche d'ancêtres pour qu'un parent devienne S1
   */
  shiftAncestorsForParent: function(originalAncestors, parentSosa) {
    const shifted = {};

    // Le parent devient S1 pour le rendu
    shifted["S1"] = originalAncestors["S" + parentSosa];

    // Fonction récursive pour décaler toute la branche
    const shiftBranch = (oldSosa, newSosa) => {
      const person = originalAncestors["S" + oldSosa];
      if (person) {
        shifted["S" + newSosa] = person;
        // Décaler récursivement les parents
        shiftBranch(oldSosa * 2, newSosa * 2);       // Père
        shiftBranch(oldSosa * 2 + 1, newSosa * 2 + 1); // Mère
      }
    };

    // Décaler les grands-parents
    shiftBranch(parentSosa * 2, 2);
    shiftBranch(parentSosa * 2 + 1, 3);

    return shifted;
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
      if (p.age) {
        classes.push(Utils.ageClass(p.age));
      }

      circle.setAttribute("class", classes.join(' '));
    } else {
      circle.setAttribute("class", "link");

      const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
      title.textContent = `(Sosa 1) ${p.fn} ${p.sn} (${p.age_text})\nCtrl+clic pour la fiche individuelle`;
      circle.appendChild(title);

      circle.onclick = (e) => this.handleClick(e, p);
      circle.onmouseenter = (e) => {
        circle.classList.add('highlight');

        const bgCircle = g.querySelector('circle.bg');
        if (bgCircle) {
          bgCircle.classList.add('highlight');
        }

        this.handleMouseEnter(p, 'person', e);
      };
      circle.onmouseleave = (e) => {
        circle.classList.remove('highlight');

        const bgCircle = g.querySelector('circle.bg');
        if (bgCircle) {
          bgCircle.classList.remove('highlight');
        }

        this.handleMouseLeave(p, 'person', e);
      };
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
      if (options.type === 'marriage' && !p.marriage_place) {
        path.setAttribute("class", ""); // Secteur de mariage sans lieu
      } else {
        this.applyInteractiveFeatures(path, p, options.type);
      }
    }

    g.append(path);
    return path;
  },

  applyBackgroundClasses: function(element, p, type) {
    let classes = ['bg'];

    if (type === 'person') {
      Events.types.forEach(eventType => {
        // Éviter l'erreur sur Events.place pour les mariages
        if (eventType === 'marriage') {
          // Les mariages d'individus ne sont pas directement dans p.marriage_place
          // mais gérés séparément dans les secteurs de mariage
          return;
        }

        const placeField = Events.place ? Events.place(eventType) : `${eventType}_place`;
        const svgPrefix = Events.svgPrefix(eventType);

        if (p[placeField] && lieux[p[placeField]]) {
          classes.push(`${svgPrefix}-${lieux[p[placeField]].c}`);
        }
      });

      if (p.age) classes.push(Utils.ageClass(p.age));

    } else if (type === 'marriage') {
      // Pour les secteurs de mariage, traiter directement
      if (p.marriage_place && lieux[p.marriage_place]) {
        const svgPrefix = Events.svgPrefix('marriage');
        classes.push(`${svgPrefix}-${lieux[p.marriage_place].c}`);
      }

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

    element.addEventListener("click", (e) => {
      e.stopPropagation();
      this.handleClick(e, p);
    });

    element.addEventListener("mouseenter", (e) => {
      e.stopPropagation();

      // 1. Panneau d'information
      const panel = document.getElementById("person-panel");
      if (panel) {
        this.buildTooltipContent(panel, p, type);
        panel.style.display = "block";
      }

      // 2. Surlignage du secteur
      element.classList.add('highlight');

      // 3. Surlignage du fond correspondant
      const parentGroup = element.parentNode;
      if (parentGroup) {
        if (type === 'person') {
          const bgElement = parentGroup.querySelector('.bg:not([class*="ma-"])');
          if (bgElement) {
            bgElement.classList.add('highlight');
          }
        } else if (type === 'marriage') {
          const bgElement = parentGroup.querySelector('.bg');
          if (bgElement) {
            // Surlignage gris standard pour tous les mariages
            bgElement.classList.add('highlight');
            // Surlignage orange SEULEMENT si lieu ET mode place
            if (p.marriage_place && document.body.classList.contains('place')) {
              element.classList.add('event-highlight-marriage');
              bgElement.classList.add('event-highlight-marriage');
            }
          }
        }
      }

      // 4. Surlignage dans la liste des lieux (uniquement si lieu présent)
      if (PlacesHighlighter && document.body.classList.contains('place')) {
        const placesMap = new Map();

        if (type === 'person') {
          Events.types.forEach(eventType => {
            if (eventType === 'marriage') return;
            const placeField = Events.place(eventType);
            if (p[placeField]) {
              if (!placesMap.has(p[placeField])) {
                placesMap.set(p[placeField], []);
              }
              placesMap.get(p[placeField]).push(eventType);
            }
          });
        } else if (type === 'marriage' && p.marriage_place) {
          // ✅ CORRECTION : Ajouter à la map seulement si lieu présent
          placesMap.set(p.marriage_place, ['marriage']);
        }

        if (placesMap.size > 0) {
          const places = Array.from(placesMap.keys());
          const events = Array.from(placesMap.values());
          PlacesHighlighter.highlight(places, events, 'svg');
        }
      }
    });

    element.addEventListener("mouseleave", (e) => {
      e.stopPropagation();

      // Nettoyage identique pour tous les types
      const panel = document.getElementById("person-panel");
      if (panel) {
        panel.style.display = "none";
        panel.innerHTML = "";
      }

      element.classList.remove('highlight');
      element.classList.remove('event-highlight-marriage');

      const parentGroup = element.parentNode;
      if (parentGroup) {
        const bgElements = parentGroup.querySelectorAll('.bg.highlight, .bg.event-highlight-marriage');
        bgElements.forEach(bg => {
          bg.classList.remove('highlight');
          bg.classList.remove('event-highlight-marriage');
        });
      }

      if (PlacesHighlighter && PlacesHighlighter.clearAllHighlights) {
        PlacesHighlighter.clearAllHighlights();
      }
    });
  },

  buildTooltipContent: function(panel, p, type) {
    panel.className = 'person-panel';

    if (type === "person") {
      let html = `<h2>${p.fn} ${p.sn}</h2>`;

      // Utiliser les dates détaillées si disponibles
      if (p.birth_date || p.birth_place) {
        html += `<div><strong>Naissance :</strong> `;
        if (p.birth_date) html += `${p.birth_date}`;
        if (p.birth_place) html += ` – ${p.birth_place}`;
        html += `</div>`;
      }

      if (p.baptism_date || p.baptism_place) {
        html += `<div><strong>Baptême :</strong> `;
        if (p.baptism_date) html += `${p.baptism_date}`;
        if (p.baptism_place) html += ` – ${p.baptism_place}`;
        html += `</div>`;
      }

      if (p.death_date || p.death_place) {
        html += `<div><strong>Décès :</strong> `;
        if (p.death_date) html += `${p.death_date}`;
        if (p.death_place) html += ` – ${p.death_place}`;
        html += `</div>`;
      }
      if (p.burial_date || p.burial_place) {
        html += `<div><strong>Sépulture :</strong> `;
        if (p.burial_date) html += `${p.burial_date}`;
        if (p.burial_place) html += ` – ${p.burial_place}`;
        html += `</div>`;
      }
      if (p.age_text) {
        html += `<strong>Âge :</strong> ${p.age_text}`;
        html += `</div>`;
      }

      panel.innerHTML = html;

    } else if (type === "marriage") {
      const years = parseInt(p.marriage_length) || -1;
      let html = `<h2>Mariage</h2>`;

      // Utiliser marriage_date_ (format détaillé) si disponible, sinon marriage_date
      const marriageDate = p.marriage_date_ || p.marriage_date;
      if (marriageDate) {
        html += `<div><strong>Date :</strong> ${marriageDate}</div>`;
      }

      if (p.marriage_place) {
        html += `<div><strong>Lieu :</strong> ${p.marriage_place}</div>`;
      }

      if (years >= 0) {
        html += `<div><strong>Durée :</strong> ${years} ${years === 1 ? "an" : "ans"}</div>`;
      }

      if (p.marriage_age) {
        html += `<div><strong>Âge au mariage (!TO RM) :</strong> ${p.marriage_age} ans</div>`;
      }

      panel.innerHTML = html;
    }
  },

  handleClick: function(e, person) {
    const sortToggle = e.target.closest('#b-sort-places');
      if (sortToggle) {
      e.preventDefault();
      UIManager.toggleSort();
      return;
    }
    // Pour la navigation vers les fiches, exiger Ctrl/Cmd
    if (!e.ctrlKey && !e.metaKey) {
      // Pas de navigation sans modificateur
      return;
    }
    if (!link_to_person) {
      alert("Erreur: Impossible d'accéder à la fiche individuelle");
      return;
    }
    const li = e.target.closest('li[data-location]');

    // Clic sur une personne (secteur du fanchart)
    if (person && person.fnk && person.snk) {
      URLManager.navigateToPerson(person, true, false);
      return;
    }

    // Clic sur un lieu en mode wizard
    if (li && document.body.dataset.wizard === "1") {
      e.preventDefault();
      const placeName = li.dataset.location;
      URLManager.navigateToPlace(placeName, true);
      return;
    }
  },

  handleMouseEnter: function(p, type, event) {
    const panel = document.getElementById("person-panel");
    if (panel) {
      this.buildTooltipContent(panel, p, type);
        panel.style.display = "block";
    }

    if (document.body.classList.contains('age')) {
      AgeHighlighter.handleSVGHover(p, type, 'enter');
    } else if (document.body.classList.contains('place') && PlacesInterface.cache.elements.panel) {
          const placesToHighlight = [];
          const eventsToHighlight = [];

          if (type === 'marriage' && p.marriage_place) {
              placesToHighlight.push(p.marriage_place);
              eventsToHighlight.push(['marriage']);
          } else if (type === 'person') {
              Events.types.forEach(eventType => {
                  if (eventType === 'marriage') return; // Esquiver marriage
                  const placeField = Events.place(eventType);
                  if (p[placeField]) {
                      placesToHighlight.push(p[placeField]);
                      eventsToHighlight.push([eventType]);
                  }
              });
          }

          if (placesToHighlight.length > 0) {
            PlacesHighlighter.highlight(placesToHighlight, eventsToHighlight, 'svg');
          }
    }

    // Gestion des implexes
    if (p.sosasame) {
      const ref = document.getElementById("S" + p.sosasame);
      if (ref) ref.classList.add("same_hl");
    }
  },

  handleMouseLeave: function(p, type, event) {
    const panel = document.getElementById("person-panel");
    if (panel) {
      panel.style.display = "none";
    panel.innerHTML = "";
    }

    if (document.body.classList.contains('place')) {
      PlacesHighlighter.clearAllHighlights();
    }
    else if (document.body.classList.contains('age')) {
      AgeHighlighter.clearAllHighlights();
    }

    // Gestion des implexes
    if (p.sosasame) {
      const ref = document.getElementById("S" + p.sosasame);
      if (ref) ref.classList.remove("same_hl");
    }
  },

  // Extraire les lieux
  extractPlacesFromPerson: function(person, type) {
    const places = [];

    if (type === 'marriage' && person.marriage_place) {
      places.push({ place: person.marriage_place, event: 'marriage' });
    } else if (type === 'person') {
      Events.types.forEach(eventType => {
        const placeField = Events.place(eventType);
        if (person[placeField]) {
          places.push({ place: person[placeField], event: eventType });
        }
      });
    }

    return places;
  },

  drawSectorText: function(pg, r1, r2, a1, a2, sosa, p, classes, generation, isSame = false) {
    let mode;

    if (CONFIG.a_m[generation - 1] === "C3") {
      mode = 'C3';
    } else if (CONFIG.a_m[generation - 1] === "C2") {
      mode = 'C2';
    } else if (CONFIG.a_m[generation - 1] === "C1") {
      mode = 'C1';
    } else if (CONFIG.a_m[generation - 1] === "R4" && !isSame) {
      mode = 'R4';
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
        URLManager.navigateToPerson(p, useNewTab, true);
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
      standard.textContent = "";
    }
    return this._bboxCache[textContent];
  },

  drawText: function(g, mode, params) {
    // Construire les classes CSS pour les lieux
    const textClasses = this.buildLocationClasses(params.p, params.classes || "");
    const modeFactor = CONFIG.text_mode_factors[mode] || 1.0;

    switch(mode) {
      case 'S1':
        return this.drawCentralText(g, params.x, params.y, params.p, textClasses);

      case 'C3':
        return this.drawCircularText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, modeFactor);

      case 'C2':
        return this.drawCircularText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, modeFactor);

      case 'C1':
        return this.drawCircularText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, modeFactor);

      case 'R4':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 4, modeFactor);

      case 'R3':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 3, modeFactor);

      case 'R2':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 2, modeFactor);

      case 'R1':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 1, modeFactor);

      default:
        console.warn(`Mode de texte non reconnu: ${mode}`);
        return null;
    }
  },

  buildLocationClasses: function(p, baseClasses) {
    let classes = baseClasses;

    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType);
      const svgPrefix = Events.svgPrefix(eventType);

      if (p[placeField] && lieux[p[placeField]]) {
        classes += ` ${svgPrefix}-t${lieux[p[placeField]].c}`;
      }
    });

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

  drawCircularText: function(g, r1, r2, a1, a2, sosa, p, classes, sizeFactor = 1.0) {
    const height = Math.abs(r2 - r1) / 3;

    // Trois arcs concentriques pour prénom, nom, dates
    const pathLength1 = this.createCircularPath(g, `tp1S${sosa}`, (r2-r1)*3/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp1S${sosa}`, p.fn, classes, pathLength1, height, sizeFactor);

    const pathLength2 = this.createCircularPath(g, `tp2S${sosa}`, (r2-r1)*2/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp2S${sosa}`, p.sn, classes, pathLength2, height, sizeFactor);

    const pathLength3 = this.createCircularPath(g, `tp3S${sosa}`, (r2-r1)/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp3S${sosa}`, p.dates, classes + " dates", pathLength3, height, sizeFactor);

    return g;
  },

  drawRadialText: function(g, r1, r2, a1, a2, sosa, p, classes, lineCount, sizeFactor = 1.0) {
    // Calcul des paramètres de direction selon l'orientation
    const params = this.calculateRadialParameters(r1, r2, a1, a2, lineCount);
    const height = Math.abs(a2 - a1) / 360 * 2 * Math.PI * r1 / lineCount;

    if (lineCount >= 3) {
      // Trois lignes : prénom, nom, dates
      const pathLength1 = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, p.fn, classes, pathLength1, height, sizeFactor);

      const pathLength2 = this.createRadialPath(g, `tp2S${sosa}`, params.r1, params.r2, params.angles[1]);
      this.placeTextOnPath(g, `tp2S${sosa}`, p.sn, classes, pathLength2, height, sizeFactor);

      const pathLength3 = this.createRadialPath(g, `tp3S${sosa}`, params.r1, params.r2, params.angles[2]);
      this.placeTextOnPath(g, `tp3S${sosa}`, p.dates, classes + " dates", pathLength3, height, sizeFactor);

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

      if (lineCount === 4) {
        angles = [
         a2 - (a2-a1)*4/6,  // Position 4/6
         a2 - (a2-a1)*3/6,  // Position 3/6
         a2 - (a2-a1)*2/6   // Position 2/6
        ];
      } else if (lineCount === 3) {
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

      if (lineCount === 4) {
        angles = [
         a1 + (a2-a1)*4/6,  // Position 4/6
         a1 + (a2-a1)*3/6,  // Position 3/6
         a1 + (a2-a1)*2/6   // Position 2/6
        ];
      } else if (lineCount === 3) {
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

  placeTextOnPath: function(g, pathId, textContent, classes, pathLength, pathHeight, sizeFactor = 1.0) {
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

    let finalFontSize = Math.min(fontSizeByWidth, fontSizeByHeight) * sizeFactor;

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
      <div>– <kbd>Ctrl</kbd>+clic : fiche individuelle</div>
      <div>– ▲ : navigation sur ancêtre</div>
      <div style="margin-top: 8px; text-align: center;">
      </div>
    `;
    document.body.appendChild(helpPanel);
  },
};

const ColorManager = {
  EVENT_TYPES: ["bi", "ba", "ma", "de", "bu"],

  createLocationStyles: function(index, c_h, c_l) {
    const root = document.documentElement;
    const sheet = [...document.styleSheets].find(s => s.title === "fc-auto");

    root.style.setProperty('--fc-color-' + index, 'hsl(' + c_h + ',100%,' + c_l + '%)');

    // Créer les règles CSS pour les secteurs SVG
    const eventTypes = ['bi', 'ba', 'ma', 'de', 'bu'];
    eventTypes.forEach(eventType => {
      sheet.insertRule(
        `body.place svg .${eventType}-L${index} { fill: var(--fc-color-${index}); }`,
        sheet.cssRules.length
      );
    });
    // Règle générique sans préfixe
    sheet.insertRule(
      `body.place svg .L${index} { fill: var(--fc-color-${index}); }`,
      sheet.cssRules.length
    );
  },

  setColorMode: function(newMode) {
    // Nettoyer l'état précédent
    document.body.classList.remove('place', 'age');


    // Désactiver tous les toggles NMBDS
    this.EVENT_TYPES.forEach(id => {
      const checkbox = document.getElementById(id);
      if (checkbox) checkbox.checked = false;
    });

    // Appliquer le nouveau mode
    if (newMode === 'place') {
      document.body.className = "place";
      tool = "place";
      // Activer M par défaut
      const maCheckbox = document.getElementById("ma");
      if (maCheckbox) maCheckbox.checked = true;
      this.applyColorization();
    } else if (newMode === 'age') {
      document.body.className = "age";
      tool = "age";
    } else {
      document.body.className = "";
      tool = "";
    }

    // Mettre à jour la visibilité des contrôles
    this.updateControlsVisibility();

    // Mettre à jour l'état visuel des boutons
    this.updateButtonStates();

    // Synchroniser l'URL
    URLManager.updateCurrentURL();
  },

  updateControlsVisibility: function() {
    const isPlaceColorActive = document.body.classList.contains('place');

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
    const ageButton = document.getElementById("b-age");
    const placesButton = document.getElementById("b-places-colorise");
    const sortButton = document.getElementById("b-sort-places");

    if (ageButton) ageButton.classList.toggle("active", tool === "age");
    if (placesButton) placesButton.classList.toggle("active", tool === "place");
    if (sortButton) sortButton.classList.toggle("active", sortMode === "alphabetical");
  },

  applyColorization: function() {
    const fanchart = document.getElementById("fanchart");

    Events.types.forEach(eventType => {
      const svgPrefix = Events.svgPrefix(eventType);
      const checkbox = document.getElementById(svgPrefix);
      const isChecked = checkbox ? checkbox.checked : false;

      fanchart.classList.toggle(svgPrefix, isChecked);
    });

    URLManager.updateCurrentURL();
  },

  initializeColorEvents: function() {
    // Événements des checkboxes NMBDS
    Events.types.forEach(eventType => {
      const svgPrefix = Events.svgPrefix(eventType); // 'bi', 'ba', 'ma', 'de', 'bu'
      const checkbox = document.getElementById(svgPrefix);
      if (checkbox) {
        checkbox.onclick = this.applyColorization.bind(this);
      }
    });

    document.getElementById("b-circular-mode").onclick = () => CircularModeRenderer.toggle();

    // Bouton colorisation lieux
    document.getElementById("b-places-colorise").onclick = function() {
      const isActive = document.body.classList.contains("place");

      if (isActive) {
        // Désactiver
        document.body.className = "";
        tool = "";
        this.classList.remove("active");
      } else {
        document.body.className = "place";
        tool = "place";
        this.classList.add("active");

        // Désactiver age si actif
        const ageButton = document.getElementById("b-age");
        if (ageButton) ageButton.classList.remove("active");
      }

      // Appliquer la colorisation
      ColorManager.applyColorization();
      ColorManager.updateControlsVisibility();
      URLManager.updateCurrentURL();
    };

    // Bouton âges (exclusion mutuelle)
    document.getElementById("b-age").onclick = function() {
      const isActive = document.body.classList.contains("age");

      if (isActive) {
        // Désactiver complètement
        document.body.className = "";
        tool = "";
        this.classList.remove("active");
      } else {
        document.body.className = "age";
        tool = "age";
        this.classList.add("active");

        // Désactiver colorisation lieux si active
        const placesButton = document.getElementById("b-places-colorise");
        if (placesButton) placesButton.classList.remove("active");
      }

      URLManager.updateCurrentURL();
    };
  }
};

const AgeHighlighter = {
  currentHighlight: null,

  initialize: function() {
    const closeBtn = document.querySelector('.legend-close');
    if (closeBtn) {
      closeBtn.onclick = () => {
        document.getElementById('age-legend').style.display = 'none';
        document.body.classList.remove('age');
        if (typeof tool !== 'undefined') tool = '';
        this.clearAllHighlights();
      };
    }

    const container = document.getElementById('age-legend');
    if (container) {
      container.addEventListener('mouseenter', this.handleLegendEnter.bind(this), true);
      container.addEventListener('mouseleave', this.handleLegendLeave.bind(this), true);
    }
  },

  handleLegendEnter: function(e) {
    const legendItem = e.target.closest('.legend-item');
    if (!legendItem || !legendItem.id) return;
    this.setHighlight(legendItem.id);
  },

  handleLegendLeave: function(e) {
    if (!e.relatedTarget || !document.getElementById('age-legend').contains(e.relatedTarget)) {
      this.clearAllHighlights();
    }
  },

  // Nouvelle méthode centralisée pour gérer les interactions SVG
  handleSVGHover: function(person, type, action) {
    if (action === 'enter') {
      this.handleSVGEnter(person, type);
    } else if (action === 'leave') {
      this.clearAllHighlights();
    }
  },

  handleSVGEnter: function(person, type) {
    this.clearAllHighlights();

    let ageClassToHighlight = null;

    if (type === 'person' && person.age) {
      ageClassToHighlight = Utils.ageClass(person.age);
    } else if (type === 'marriage' && person.marriage_length) {
      ageClassToHighlight = Utils.marriageLengthClass(person.marriage_length);
    }

    if (ageClassToHighlight) {
      this.setHighlight(ageClassToHighlight);
    }
  },

  setHighlight: function(legendId) {
    this.clearAllHighlights();
    this.currentHighlight = legendId;

    requestAnimationFrame(() => {
      const legendItem = document.getElementById(legendId);
      if (legendItem) {
        legendItem.classList.add('hl');
      }

      const svgElements = document.getElementsByClassName(legendId);
      for (let i = 0; i < svgElements.length; i++) {
        svgElements[i].classList.add('highlight');
      }
    });
  },

  clearAllHighlights: function() {
    document.querySelectorAll('.legend-item.hl').forEach(item => {
      item.classList.remove('hl');
    });

    document.querySelectorAll('svg .highlight').forEach(element => {
      element.classList.remove('highlight');
    });

    this.currentHighlight = null;
  }
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
      return;
    }

    current_angle = newAngle;

    // Mettre à jour l'URL avec le système existant
    URLManager.updateCurrentURL();

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

  // Initialiser les boutons (appelé depuis FanchartApp.init)
  initialize: function() {
    this.updateAngleButtons();

    // Mettre à jour l'état du bouton circulaire
    const circularBtn = document.getElementById('b-circular-mode');
    if (circularBtn && isCircularMode) {
      circularBtn.classList.add('active');
    }
  }
};

// ========== MODULE DE GESTION DE L'OVERFLOW ==========
const ModernOverflowManager = {
  // Configuration
  config: {
    itemHeight: 30,        // Hauteur d'un élément (sync avec CSS --place-height-compact)
    headerHeight: 28,      // Hauteur header overflow
    tolerance: { compact: 6, extended: 3 },
    maxIterations: 5,
    maxSpaceRatio: 0.5     // Maximum 50% de l'écran pour l'overflow
  },

  // État
  originalListHeight: null,
  currentOverflowSections: [],
  isProcessing: false,

  /**
   * Initialise le système d'overflow
   * @returns {boolean} true si l'initialisation réussit
   */
  initialize: function() {
    const list = document.querySelector('.places-list');
    if (!list) return false;

    // Sauvegarder la hauteur originale
    this.originalListHeight = list.clientHeight;
    return true;
  },

  /**
   * Gère l'overflow après un surlignage
   * @param {Array} matchingItems - Éléments surlignés à garder visibles
   */
  handleOverflow: function(matchingItems) {
    // Prévenir les appels multiples simultanés
    if (this.isProcessing) {
      return;
    }

    this.isProcessing = true;

    if (!this.initialize() || !matchingItems?.length) {
      this.isProcessing = false;
      return;
    }

    // Déduplication par lieu pour éviter les collisions
    const uniqueItems = this.deduplicateByPlace(matchingItems);

    // Nettoyer les sections d'overflow précédentes
    this.clearOverflowSections();

    // Lancer la stabilisation
    this.stabilizeOverflow(uniqueItems);

    this.isProcessing = false;
  },

  // Gestion de la déduplication
  deduplicateByPlace: function(matchingItems) {
    const seenPlaces = new Set();
    return matchingItems.filter(item => {
      const placeName = item.placeName || item.element?.dataset?.place;
      if (!placeName || seenPlaces.has(placeName)) {
        return false;
      }
      seenPlaces.add(placeName);
      return true;
    });
  },

  /**
   * Algorithme principal de stabilisation de l'overflow
   * @param {Array} matchingItems - Éléments à garder visibles
   */
  stabilizeOverflow: function(matchingItems) {
    const list = document.querySelector('.places-list');
    if (!list) return;

    const currentOverflow = this.calculateOverflowWithConstraints(matchingItems, list.clientHeight);

    if (!currentOverflow.above?.length && !currentOverflow.below?.length) {
      return;
    }

    this.displayOverflowInReservedSpace(currentOverflow);
  },

  /**
   * Calcule l'espace requis pour afficher l'overflow
   * @param {Object} overflowData - Données d'overflow {above: [], below: []}
   * @returns {number} Espace requis en pixels
   */
  calculateRequiredSpace: function(overflowData) {
    if (!overflowData || (!overflowData.above?.length && !overflowData.below?.length)) {
      return 0;
    }

    let requiredSpace = 0;

    // Espace pour overflow au-dessus
    if (overflowData.above?.length > 0) {
      const count = Math.min(overflowData.above.length, 5); // Max 5 éléments affichés
      requiredSpace += this.config.headerHeight + (count * this.config.itemHeight);
    }

    // Espace pour overflow en-dessous
    if (overflowData.below?.length > 0) {
      const count = Math.min(overflowData.below.length, 5);
      requiredSpace += this.config.headerHeight + (count * this.config.itemHeight);
    }

    // Limiter l'espace maximum
    const maxAllowedSpace = Math.floor(this.originalListHeight * this.config.maxSpaceRatio);
    const finalSpace = Math.min(requiredSpace, maxAllowedSpace);

    return finalSpace;
  },

  /**
   * Détecte quels éléments sont en overflow
   * @param {Array} highlightedItems - Éléments à vérifier
   * @param {number} maxHeight - Hauteur maximale de la liste
   * @returns {Object} {above: [], below: []} éléments en overflow
   */
  calculateOverflowWithConstraints: function(highlightedItems, maxHeight) {
    const list = document.querySelector('.places-list');
    if (!list || !highlightedItems.length || maxHeight <= 0) {
      return { above: [], below: [] };
    }

    const scrollTop = list.scrollTop;
    const scrollBottom = scrollTop + maxHeight;

    const overflowAbove = [];
    const overflowBelow = [];

    highlightedItems.forEach(item => {
      const element = item.element || item.place;
      if (!element) return;

      const row = element.closest('.place-row');
      if (!row) return;

      const itemTop = row.offsetTop;
      const itemBottom = itemTop + row.offsetHeight;

      // Simple test de visibilité : l'élément est-il entièrement visible ?
      const isCompletelyVisible = (itemTop >= scrollTop && itemBottom <= scrollBottom);

      if (!isCompletelyVisible) {
        // Déterminer si l'élément est plutôt au-dessus ou en-dessous du viewport
        const itemCenter = (itemTop + itemBottom) / 2;
        const viewCenter = (scrollTop + scrollBottom) / 2;

        if (itemCenter < viewCenter) {
          overflowAbove.push({
            element: element,
            index: item.index,
            row: row,
            placeName: item.placeName
          });
        } else {
          overflowBelow.push({
            element: element,
            index: item.index,
            row: row,
            placeName: item.placeName
          });
        }
      }
    });

    return { above: overflowAbove, below: overflowBelow };
  },

  /**
   * Vérifie si l'overflow est stable entre deux itérations
   * @param {Object} current - Overflow actuel
   * @param {Object} previous - Overflow précédent
   * @returns {boolean} true si stable
   */
  isOverflowStable: function(current, previous) {
    if (!previous) return false;

    const currentCounts = {
      above: current.above?.length || 0,
      below: current.below?.length || 0
    };

    const previousCounts = {
      above: previous.above?.length || 0,
      below: previous.below?.length || 0
    };

    return (currentCounts.above === previousCounts.above) &&
           (currentCounts.below === previousCounts.below);
  },

  /**
   * Détecte une oscillation dans le calcul
   * @param {Object} current - Overflow actuel
   * @param {Object} previous - Overflow précédent
   * @returns {boolean} true si oscillation détectée
   */
  detectOscillation: function(current, previous) {
    if (!previous) return false;

    // Oscillation = les éléments changent de côté
    const currentAboveIds = current.above.map(item => item.index).join(',');
    const currentBelowIds = current.below.map(item => item.index).join(',');
    const previousAboveIds = previous.above.map(item => item.index).join(',');
    const previousBelowIds = previous.below.map(item => item.index).join(',');

    return (currentAboveIds === previousBelowIds) || (currentBelowIds === previousAboveIds);
  },

  /**
   * Affiche l'overflow dans l'espace réservé
   * @param {Object} overflowData - Données d'overflow
   * @param {number} reservedSpace - Espace réservé en pixels
   */
  displayOverflowInReservedSpace: function(overflowData, reservedSpace) {
    this.clearOverflowSections();

    if (!overflowData.above?.length && !overflowData.below?.length) {
      return;
    }

    const container = document.querySelector('.places-container');
    if (!container) return;

    // Overflow au-dessus - positionner en HAUT de la liste
    if (overflowData.above?.length > 0) {
      const aboveSection = this.createOverflowSection('above', overflowData.above);
      aboveSection.style.top = '0px';
      container.appendChild(aboveSection);
      this.currentOverflowSections.push(aboveSection);
    }

    // Overflow en-dessous - utiliser les classes CSS (bottom: 0)
    if (overflowData.below?.length > 0) {
      const belowSection = this.createOverflowSection('below', overflowData.below);
      // La CSS gére avec .overflow-section.below
      container.appendChild(belowSection);
      this.currentOverflowSections.push(belowSection);
    }
  },

  /**
   * Crée une section d'overflow
   * @param {string} position - 'above' ou 'below'
   * @param {Array} items - Éléments en overflow
   * @returns {HTMLElement} Section créée
   */
  createOverflowSection: function(position, items) {
    const section = document.createElement('div');
    section.className = `overflow-section ${position} stabilized`;

    // Header - utilise uniquement la classe CSS existante
    const header = document.createElement('div');
    header.className = 'overflow-header';
    header.innerHTML = `
      <i class="fas fa-arrow-${position === 'above' ? 'up' : 'down'} fa-sm"></i>
      ${items.length} lieu${items.length > 1 ? 'x' : ''} hors écran
    `;

    // Contenu - utilise uniquement la classe CSS existante
    const content = document.createElement('div');
    content.className = 'overflow-content';

    // Trier les items selon l'ordre alphabétique au lieu de l'ordre DOM
    const sortedItems = this.sortItemsByLogicalOrder(items);

    items.slice(0, 5).forEach(item => {
      const row = item.row;
      if (row) {
        const clone = row.cloneNode(true);

        // Nettoyer IDs et classes conflictuelles
        const indicators = clone.querySelector('.place-indicators');
        const placeContent = clone.querySelector('.place-content');

        if (indicators) indicators.removeAttribute('id');
        if (placeContent) {
          placeContent.removeAttribute('id');
          placeContent.classList.remove('grayed-out');
        }

        content.appendChild(clone);
      }
    });

    section.appendChild(header);
    section.appendChild(content);
    return section;
  },

  // Trier selon l’ordre logique (parent avant enfant)
  sortItemsByLogicalOrder: function(items) {
    return items.sort((a, b) => {
      const aPlace = a.placeName;
      const bPlace = b.placeName;
      const aData = lieux[aPlace];
      const bData = lieux[bPlace];

      // Si A est parent de B, A avant B
      if (bData?.isSubLocation && bData.parentLocation === aPlace) return -1;
      // Si B est parent de A, B avant A
      if (aData?.isSubLocation && aData.parentLocation === bPlace) return 1;

      // Sinon tri alphabétique normal
      return aPlace.localeCompare(bPlace, 'fr', { sensitivity: 'base' });
    });
  },

  /**
   * Nettoie toutes les sections d'overflow
   */
  clearOverflowSections: function() {
    // Nettoyer les sections trackées
    this.currentOverflowSections.forEach(section => {
      if (section.parentNode) {
        section.parentNode.removeChild(section);
      }
    });
    this.currentOverflowSections = [];

    // Nettoyer toutes les sections d'overflow orphelines
    document.querySelectorAll('.overflow-section').forEach(el => el.remove());

    // Réinitialiser l’état de traitement au cas où
    this.isProcessing = false;
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
    // CALCULS INITIAUX
    this.calculateDimensions();
    this.processAncestorData();

    // LECTURE DE L'ÉTAT URL
    const state = URLManager.readCurrentState();
    tool = state.tool;
    sortMode = state.sortMode;
    showEvents = state.showEvents;
    isCircularMode = state.isCircular;
    current_angle = state.angle;
    implexMode = state.implexMode;

    // CONSTRUCTION DES DONNÉES
    LocationDataBuilder.buildCompleteLocationData();

    // INTERFACE DES LIEUX
    if (document.querySelector('.places-panel')) {
      PlacesInterface.initialize();
      PlacesPanelControls.initialize();
    }

    // MISE À JOUR DES BOUTONS
    AngleManager.updateAngleButtons();
    if (isCircularMode) {
      const circularBtn = document.getElementById('b-circular-mode');
      if (circularBtn) circularBtn.classList.add('active');
    }

    // INITIALISATION DES ÉVÉNEMENTS
    DOMCache.preload();
    this.initializeEvents();
    this.initializeAngleEvents();
    ColorManager.initializeColorEvents();
    if (document.getElementById('age-legend')) {
      AgeHighlighter.initialize();
    }

    // RENDU ET FINALISATION
    this.renderFanchart();
    this.updateGenerationTitle();
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
    });

    // Après le nettoyage, mettre à jour les flags globaux
    this.updateGlobalFlags();
  },

  // Nettoyage des lieux
  cleanPersonPlaces: function(person, key) {
    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType); // 'birth_place', etc.
      const flagName = Events.flagProp(eventType); // 'has_b', etc.

      if (person[placeField] !== undefined) {
        ancestor[key][placeField] = this.cleanPlaceName(person[placeField]);
        window[flagName] = true;
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

  // Mise à jour des flags globaux basée sur l'état actuel des données
  updateGlobalFlags: function() {
    // Réinitialiser tous les flags via Events
    Events.types.forEach(eventType => {
      const flagName = Events.flagProp(eventType); // 'has_b', 'has_ba', etc.
      window[flagName] = false;
    });

    // Parcourir les ancêtres pour déterminer quels types sont présents
    Object.values(ancestor).forEach(person => {
      Events.types.forEach(eventType => {
        const placeField = Events.place(eventType);
        const flagName = Events.flagProp(eventType);
        if (person[placeField]) {
          window[flagName] = true;
        }
      });
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
      URLManager.updateCurrentURL();
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

    if (isCircularMode) {
      // Mode circulaire : forcer un carré pour contenir le cercle complet
      const size = 2 * (max_r + margin);
      svg_w = size;
      svg_h = size;
      center_x = size / 2;
      center_y = size / 2;
    } else {
      // Calcul standard pour tous les angles
      center_x = max_r + margin;
      center_y = max_r + margin;
      svg_w = 2 * center_x;

      if (current_angle === 180) {
        // Demi-cercle : hauteur réduite, le centre est positionné en haut
        // MAIS le cercle S1 dépasse vers le bas de CONFIG.a_r[0]
        svg_h = max_r + CONFIG.a_r[0] + 2 * margin; // Ajouter le rayon du cercle central
      } else if (current_angle <= 270) {
        center_y = max_r + margin;
        // Angles standard : calcul avec extension
        const halfAngleRad = Math.PI/180 * (current_angle - 180) / 2;
        const extraHeight = Math.max(CONFIG.a_r[0], Math.round(max_r * Math.sin(halfAngleRad)));
        svg_h = 2 * margin + max_r + extraHeight;
      } else {
        // Angles > 270° : hauteur complète nécessaire
        svg_h = 2 * (max_r + margin);
      }
    }

    if (isNaN(svg_w) || isNaN(svg_h) || svg_w <= 0 || svg_h <= 0) {
      console.error("Dimensions SVG calculées invalides:", { svg_w, svg_h, max_r, center_x, center_y });
      svg_w = 800;
      svg_h = 600;
      center_x = svg_w / 2;
      center_y = svg_h / 2;
    }

    // Dimensions de la fenêtre avec limitation pour la liste des lieux

    this.window_h = window.innerHeight;
    const maxAllowedWidth = window.innerWidth * 0.85; // 85% max de la largeur
    const calculatedWidth = Math.round(this.window_h * svg_w / svg_h);
    this.window_w = Math.min(calculatedWidth, maxAllowedWidth);

    // Si on a dû réduire la largeur, ajuster la hauteur en conséquence
    if (this.window_w < calculatedWidth) {
      this.window_h = Math.round(this.window_w * svg_h / svg_w);
    }

    // Ajuster la position pour les modes carrés (éviter l'empiètement)
    if (current_angle >= 310 || isCircularMode) {
      fanchart.style.marginLeft = '120px';
    } else {
      fanchart.style.marginLeft = '0';
    }

    // Configurer le SVG
    fanchart.setAttribute("height", this.window_h);
    fanchart.setAttribute("width", this.window_w);

    // Mettre à jour la variable CSS pour la largeur de la liste
     const actualListWidth = LayoutCalculator.calculatePlacesListWidth();
     root.style.setProperty('--fc-tool-size', actualListWidth + 'px');

    // Initialisation de la viewbox
    svg_viewbox_x = 0;
    svg_viewbox_y = 0;
    svg_viewbox_w = svg_w;
    svg_viewbox_h = svg_h;
  },

  renderFanchart: function() {
    // Initialiser le texte standard
    const standardInfo = this.initializeStandardText();
    standard = standardInfo.element;
    standard_width = standardInfo.width;

    if (isCircularMode) {
      // Nettoyer complètement le SVG
      while (fanchart.firstChild) {
        fanchart.removeChild(fanchart.firstChild);
      }

      // Réinitialiser après nettoyage
      const newStandardInfo = this.initializeStandardText();
      standard = newStandardInfo.element;
      standard_width = newStandardInfo.width;

      // Sauvegarder l'état
      const savedAngle = current_angle;
      const originalAncestors = ancestor;

      // Forcer l'angle à 180° pour les demi-cercles
      current_angle = 180;

      // Créer les groupes pour les deux hémisphères
      const northGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
      northGroup.setAttribute("id", "north-hemisphere");
      fanchart.appendChild(northGroup);

      const southGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
      southGroup.setAttribute("id", "south-hemisphere");
      southGroup.setAttribute("transform", `rotate(180 ${center_x} ${center_y})`);
      fanchart.appendChild(southGroup);

      // RENDU NORD : Lignée paternelle (S2)
      ancestor = CircularModeRenderer.shiftAncestorsForParent(originalAncestors, 2);
      renderTarget = northGroup; // CRITIQUE : rediriger TOUS les rendus vers le groupe

      // Pas de centre S1 pour les demi-disques
      this.renderAncestorsByGeneration();

      // RENDU SUD : Lignée maternelle (S3)
      ancestor = CircularModeRenderer.shiftAncestorsForParent(originalAncestors, 3);
      renderTarget = southGroup;

      this.renderAncestorsByGeneration();

      // Restaurer l'état
      renderTarget = null;
      ancestor = originalAncestors;
      current_angle = savedAngle;

      // Ajouter le centre en mode couple
      CircularModeRenderer.renderCoupleCenter();

    } else {
      // Mode éventail normal
      this.renderCenterPerson();
      this.renderAncestorsByGeneration();
    }

    this.updateButtonStates();
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
    const target = renderTarget || fanchart;
    const standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
    standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz 0123456789 ’'–-?~/";
    standard.setAttribute("id", "standard");
    standard.setAttribute("x", center_x);
    standard.setAttribute("y", center_y);
    target.append(standard);

    const bbox = standard.getBBox();
    return {
      element: standard,
      width: bbox.width / standard.textContent.length,
      height: bbox.height // Si besoin plus tard
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
      const target = renderTarget || fanchart;
      // Créer le groupe pour cet ancêtre
      const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
      group.setAttribute("id", "S" + sosa);
      target.append(group);

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
    if (max_gen > LocationDataBuilder._generationCache.size) {
      LocationDataBuilder.clearCache();
    }
    DOMCache.invalidate();
    this.calculateDimensions();
    LocationDataBuilder.buildCompleteLocationData(max_gen);
    const fanchart = document.getElementById("fanchart");
    fanchart.innerHTML = "";
    this.renderFanchart();

    const placesPanel = document.querySelector('.places-panel');
    if (placesPanel) {
      PlacesInterface.generatePlacesList();
      PlacesInterface.updateSummarySection();
      PlacesPanelControls.initialize();
      PlacesInterface.setupEventListeners();
    }

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
      this.window_w = window.innerWidth;
      this.window_h = window.innerHeight;
      this.calculateDimensions();
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
        URLManager.updateCurrentURL();
      } else {
        URLManager.navigateToGeneration(max_gen + 1);
      }
    };
    document.getElementById("b-gen-del").onclick = () => {
      if(max_gen > 1) {
        max_gen--;
        FanchartApp.reRenderWithCurrentGenerations();
        URLManager.updateCurrentURL();
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
      URLManager.updateCurrentURL();
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
      // URLManager.updateCurrentURL();
    };

  },

  applyInitialState: function() {
    // Configurer l'état initial des outils
    if (tool == "age") {
      document.body.className = "age";
      const ageButton = document.getElementById("b-age");
      if (ageButton) ageButton.classList.add("active");
    } else {
      document.body.className = "place";
      tool = "place";
      const placesButton = document.getElementById("b-places-colorise");
      if (placesButton) placesButton.classList.add("active");

      ColorManager.applyColorization();
    }
  }
};

// Alias pour la rétrocompatibilité
const R = SVGRenderer;
const T = TextRenderer;

window.toggleFanchartSort = () => PlacesPanelControls.toggleSort();
window.toggleFanchartEventsDisplay = () => PlacesPanelControls.toggleEventsDisplay();
window.filterFanchartPlaces = (query) => PlacesPanelControls.filterPlaces(query);
window.clearFanchartSearch = () => PlacesPanelControls.clearSearch();
window.toggleFanchartPlacesPanel = () => document.body.classList.toggle('place');

// Lancement de l'application
FanchartApp.init();