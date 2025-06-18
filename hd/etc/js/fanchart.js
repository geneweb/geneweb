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
var sortMode = "frequency";
var has_bi = false, has_ba = false, has_ma = false, has_de = false, has_bu = false;
var svg_viewbox_x = 0, svg_viewbox_y = 0, svg_viewbox_w = 0, svg_viewbox_h = 0;

// ========== CONSTANTES D'ÉVÉNEMENTS ==========
// Source unique de vérité - tout le reste est généré automatiquement
const EVENT_CONFIG = {
  eventOrder: ['birth', 'baptism', 'marriage', 'death', 'burial'],
  eventToClass: { 'birth': 'n', 'baptism': 'b', 'marriage': 'm', 'death': 'd', 'burial': 's' },
  eventToLabel: { 'birth': 'N', 'baptism': 'B', 'marriage': 'M', 'death': 'D', 'burial': 'S' }
};

const Events = {
  // Accès direct à la liste complète
  get types() { return EVENT_CONFIG.eventOrder; },

  // Réutilise directement les mappings existants
  cssClass: (type) => EVENT_CONFIG.eventToClass[type],
  label: (type) => EVENT_CONFIG.eventToLabel[type],

  // Générateurs cohérents pour les noms longs
  count: (type) => type + '_count', // 'birth' -> 'birth_count'
  place: (type) => type + '_place', // 'birth' -> 'birth_place'
  svgPrefix: (type) => type.substring(0, 2), // 'birth' → 'bi'
  flagProp: (type) => 'has_' + Events.svgPrefix(type), // 'birth' → 'has_bi'

  // Recherche inverse
  findByClass: (cssClass) => EVENT_CONFIG.eventOrder.find(type => Events.cssClass(type) === cssClass),
  findBySvgPrefix: (prefix) => EVENT_CONFIG.eventOrder.find(type => Events.svgPrefix(type) === prefix),

  // Validation
  isValid: (type) => EVENT_CONFIG.eventOrder.includes(type),

  // Traduction
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
  a_r: [50, 50, 50, 50, 80, 70, 100, 150, 130, 90],
  a_m: ["S1", "C3", "C3", "C3", "R3", "R3", "R2", "R1", "R1", "R1"],
  marriage_length_thresholds: [4, 14, 24, 34, 44, 54],
  text_reduction_factor: 0.9,
  svg_margin: 5
};

let isCircularMode = false;
let renderTarget = null; // Cible de rendu actuelle (null = fanchart direct)
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

const LayoutCalculator = {
  /**
   * Mesure la largeur nécessaire pour afficher confortablement la liste des lieux
   * Utilise un élément temporaire pour mesurer le texte réel
   */
  calculatePlacesListWidth: function() {
    // Si pas de liste de lieux, retourner une largeur minimale
    if (!document.body.classList.contains('place_color')) {
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
  /**
   * Configuration de base pour tous les types d'URL
   * Centralise les paramètres par défaut et les formats
   */
  config: {
    // URL de base héritée du système existant
    basePerson: link_to_person,

    // Paramètres par défaut pour éviter la redondance
    defaultParams: {
      module: 'A',
      template: 'FC'
    },

    // Paramètres spéciaux selon le type de navigation
    specialParams: {
      place: { module: 'MOD_DATA', data: 'place' }
    }
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
    if (person.oc) {
      params.push(`oc=${person.oc}`);
    }

    // État du fanchart si demandé
    if (options.useCurrentState) {
      this._addFanchartState(params, options);
    }

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
      
      // ✅ LECTURE DEPUIS LES VARIABLES GLOBALES
      if (sortMode === 'alphabetical') params.push('sort');
      
      const placesPanel = document.querySelector('.places-panel');
      if (placesPanel?.classList.contains('show-events')) {
        params.push('events');
      }
      
      if (implexMode === 'numbered') params.push('implex=num');
      else if (implexMode === 'full') params.push('implex=full');
      
      if (has_ba) params.push('ba=on');
      if (has_bu) params.push('bu=on');
    }
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
};

// ========== Module de construction des données de lieux ==========
const LocationDataBuilder = {
  /**
   * Fonction principale qui orchestre toute la construction des données de lieux
   * @param {number} maxGeneration - Génération maximum à considérer (optionnel, utilise max_gen par défaut)
   */
  buildCompleteLocationData: function(maxGeneration = null) {
    const targetGeneration = maxGeneration || max_gen;
    console.log(`🏗️ Construction des données de lieux pour ${targetGeneration} génération(s)...`);

    // Réinitialisation complète des structures globales existantes
    this.resetLocationData();

    // Filtrer les ancêtres selon la génération maximum
    const filteredAncestors = this.filterAncestorsByGeneration(targetGeneration);
    console.log(`📊 ${Object.keys(filteredAncestors).length} ancêtres retenus sur ${Object.keys(ancestor).length} disponibles`);

    // Passe unique sur les ancêtres filtrés pour extraire les données de lieux
    Object.values(filteredAncestors).forEach(person => {
      this.processPersonAllLocations(person);
    });

    // Construction de l'array final pour les fonctions de tri et d'affichage
    this.buildFinalLocationArray();

    console.log(`✅ Analyse terminée : ${Object.keys(lieux).length} lieux trouvés, ${this.getTotalEvents()} événements`);
  },

  /**
   * Filtre les ancêtres selon la génération maximum
   */
  filterAncestorsByGeneration: function(maxGeneration) {
    const filteredAncestors = {};

    // Calculer la plage de Sosa pour les générations à inclure
    for (let gen = 1; gen <= maxGeneration + 1; gen++) {
      const startSosa = Math.pow(2, gen - 1);
      const endSosa = Math.pow(2, gen) - 1;

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
   * Préserve la cohérence avec les variables existantes de fanchart.js
   */
  resetLocationData: function() {
    // Structures principales (déjà déclarées globalement dans fanchart.js)
    lieux = {};
    lieux_a = [];

    // Flags globaux existants (les remettre à false, ils seront recalculés)
    has_bi = false;
    has_ba = false;
    has_ma = false;
    has_de = false;
    has_bu = false;
  },

  /**
   * Traite tous les lieux associés à une personne donnée
   * @param {Object} person - Objet personne de l’ancêtre
   */
  processPersonAllLocations: function(person) {
    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType); // 'birth_place', etc.
      if (person[placeField] && person[placeField].trim() !== '') {
        this.processSingleLocation(person[placeField], eventType, person);
      }
    });
  },

  /**
   * Traite un lieu spécifique pour un type d'événement donné
   * @param {string} placeName - Nom du lieu brut
   * @param {string} eventType - Type d'événement (bi, ba, ma, de, bu)
   * @param {Object} person - Référence à la personne (pour de futures extensions)
   */
  processSingleLocation: function(placeName, eventType, person) {
    // Nettoyage du nom de lieu (réutilise la logique existante de fanchart.js)
    const cleanPlaceName = placeName.replace(/^\?, /, "");

    // Analyse de la structure géographique du nom de lieu
    const locationStructure = this.analyzeLocationName(cleanPlaceName);

    // Initialisation de l'entrée si première occurrence de ce lieu
    if (!lieux[cleanPlaceName]) {
      lieux[cleanPlaceName] = this.createLocationEntry(cleanPlaceName, locationStructure);
    }

    // Mise à jour des compteurs et flags pour ce lieu
    this.updateLocationCounters(lieux[cleanPlaceName], eventType);
  },

  /**
   * Analyse la structure géographique d'un nom de lieu
   * Extrait les sous-lieux et précisions géographiques de manière générique
   * @param {string} placeName - Nom du lieu à analyser
   * @returns {Object} Structure analysée du lieu
   */
  analyzeLocationName: function(placeName) {
    const result = {
      fullName: placeName,
      isSubLocation: false,
      subName: null,
      parentLocation: null,
      geographicPrecision: null
    };

    // Première étape : extraction de la précision géographique entre parenthèses
    // Pattern : "Nom du lieu (Précision)" -> garde "Nom du lieu" et extrait "Précision"
    const precisionPattern = /^(.+?)\s*\(([^)]+)\)\s*$/;
    const precisionMatch = placeName.match(precisionPattern);

    let nameWithoutPrecision = placeName;
    if (precisionMatch) {
      nameWithoutPrecision = precisionMatch[1].trim();
      result.geographicPrecision = precisionMatch[2].trim();
    }

    // Deuxième étape : détection des sous-lieux avec séparateurs
    // Pattern : "Sous-lieu – Lieu principal" ou variations avec — et -
    const subLocationPattern = /^(.+?)\s+[–—-]\s+(.+)$/;
    const subMatch = placeName.match(subLocationPattern);

    if (subMatch) {
      result.isSubLocation = true;
      result.subName = subMatch[1].trim();
      result.parentLocation = subMatch[2].trim();
    }

    return result;
  },

  /**
   * Crée une nouvelle entrée de lieu avec toutes les métadonnées nécessaires
   * @param {string} cleanPlaceName - Nom nettoyé du lieu
   * @param {Object} locationStructure - Structure analysée du lieu
   * @returns {Object} Entrée complète pour l'objet lieux
   */
  createLocationEntry: function(cleanPlaceName, locationStructure) {
    const counters = {};
    const flags = {};

    Events.types.forEach(eventType => {
      const eventCount = Events.count(eventType);
      const flagProp = Events.svgPrefix(eventType);

      counters[eventCount] = 0;  // birth_count: 0, baptism_count: 0, etc.
      flags[flagProp] = false;  // n: false, b: false, etc.
    });

    return {
      // Compteurs et flags de présence générés automatiquement
      ...counters,
      ...flags,

      cnt: 0, // Compteur total

      // Métadonnées géographiques extraites
      isSubLocation: locationStructure.isSubLocation,
      subName: locationStructure.subName,
      parentLocation: locationStructure.parentLocation,
      geographicPrecision: locationStructure.geographicPrecision,

      c: null, // ID CSS

      // Données préparées pour le DOM futur
      domAttributes: {
        'data-place': cleanPlaceName,
        'data-is-sublocation': locationStructure.isSubLocation,
        'data-events': []
      }
    };
  },

  /**
   * Met à jour les compteurs et flags pour un lieu donné
   * @param {Object} locationEntry - Entrée de lieu dans l'objet lieux
   * @param {string} eventType - Type d'événement à incrémenter
   */
  updateLocationCounters: function(locationEntry, eventType) {
    const eventCount = Events.count(eventType);
    const flagProp = Events.svgPrefix(eventType);

    // Incrément du compteur spécifique
    locationEntry[eventCount]++;

    // Activation du flag de présence
    locationEntry[flagProp] = true;

    // Incrément du compteur total
    locationEntry.cnt++;

    // Mise à jour des data-attributes pour le DOM futur
    if (!locationEntry.domAttributes['data-events'].includes(eventType)) {
      locationEntry.domAttributes['data-events'].push(eventType);
    }
  },

  /**
   * Construit l'array final lieux_a et assigne les IDs CSS pour la colorisation
   * Cette fonction transforme l'objet lieux en array utilisable par les fonctions de tri
   */
  buildFinalLocationArray: function() {
    lieux_a = Object.entries(lieux);

    // ✅ VALIDATION POST-CRÉATION - corriger les fausses détections
    Object.entries(lieux).forEach(([placeName, locationData]) => {
      if (locationData.isSubLocation && locationData.parentLocation) {
        // Vérifier si le parent existe réellement dans lieux
        if (!lieux[locationData.parentLocation]) {
          console.log(`❌ CORRECTION: "${placeName}" n'est pas un sous-lieu (parent "${locationData.parentLocation}" inexistant)`);
          locationData.isSubLocation = false;
          locationData.subName = null;
          locationData.parentLocation = null;
        }
      }
    });

    // Assignation des IDs CSS
    lieux_a.forEach(([placeName, locationData], index) => {
      locationData.c = "L" + index;
    });
  },

  /**
   * Fonction utilitaire pour obtenir le nombre total d'événements
   * @returns {number} Nombre total d'événements tous lieux confondus
   */
  getTotalEvents: function() {
    return Object.values(lieux).reduce((total, locationData) => total + locationData.cnt, 0);
  },

  /**
   * Fonction utilitaire pour colleter les statistiques par type d’événement
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
  }
};

// ========== Interface du panneau des lieux ==========
const PlacesInterface = {
  // Cache pour les éléments DOM fréquemment utilisés (optimisation)
  elements: {
    panel: null,
    placesList: null,
    template: null,
    summaryPlaces: null,
    summaryEventCounts: null,
    summaryTotal: null
  },

  /**
   * Initialisation du module - à appeler après LocationDataBuilder.buildCompleteLocationData()
   * Cache les références DOM pour éviter les recherches répétées
   */
  initialize: function() {
    console.log('🎨 Initialisation de l’interface des lieux…');

    // Mise en cache des éléments DOM critiques pour la performance
    this.elements.panel = document.querySelector('.places-panel');
    this.elements.placesList = document.querySelector('.places-list');
    this.elements.summaryPlaces = document.querySelector('.summary-places-info');
    this.elements.summaryEventCounts = document.querySelectorAll('.summary-event-count');
    this.elements.summaryTotal = document.querySelector('.summary-total-events');

    // Vérification critique : est-ce que tous les éléments existent ?
    if (!this.elements.placesList) {
      console.error('❌ Éléments HTML requis manquants pour l’interface des lieux');
      return false;
    }

    // Générer l'interface complète
    this.generatePlacesList();
    this.updateSummarySection();
    this.initializeEventListeners();

    console.log('✅ Interface des lieux initialisée');
    return true;
  },

  /**
   * Met à jour la section de résumé avec les statistiques calculées
   * Utilise les données de LocationDataBuilder pour éviter les recalculs
   */
  updateSummarySection: function() {
    // Mise à jour du nombre de génération avec traduction
    const generationElement = this.elements.panel.querySelector('.generation-count');
    if (generationElement) {
      const generationLabel = window.FC_TRANSLATIONS?.[max_gen > 1 ? 'generations' : 'generation'] || 'génération';
      generationElement.textContent = `${max_gen} ${generationLabel}`;
    }

    // Mise à jour du nombre de lieux avec traduction
    const placeCount = Object.keys(lieux).length;
    const placeLabel = window.FC_TRANSLATIONS?.[placeCount > 1 ? 'places' : 'place'] || 'lieu';
    this.elements.summaryPlaces.textContent = `${placeCount} ${placeLabel}`;

    // Utilisation de nos statistiques cohérentes
    const stats = LocationDataBuilder.getEventStatistics();

    // Injection des compteurs dans l'ordre défini par Events.types
    // Chaque compteur utilise maintenant les traductions appropriées pour les tooltips
    Events.types.forEach((eventType, index) => {
      const countElement = this.elements.summaryEventCounts[index];
      if (countElement) {
        const count = stats[eventType] || 0;
        countElement.textContent = count;

        // Enrichissement avec tooltip traduit et contextualisé
        const eventLabel = Events.translate(eventType, count);
        countElement.title = count > 0 ? `${count} ${eventLabel}` : `Aucun ${eventLabel.toLowerCase()}`;
      }
    });

    // Total général avec traduction contextuelle
    const totalEvents = Events.types.reduce((sum, eventType) => {
      return sum + (stats[eventType] || 0);
    }, 0);

    this.elements.summaryTotal.textContent = totalEvents;

    // Tooltip informatif pour le total avec traduction appropriée
    const eventLabel = window.FC_TRANSLATIONS?.[totalEvents > 1 ? 'events' : 'event'] || 'événement';
    this.elements.summaryTotal.title = `Total : ${totalEvents} ${eventLabel}`;

    //Mise à jour du compteur de personnes
    this.updatePersonsCounter();
  },

  updatePersonsCounter: function() {
    const personsElement = this.elements.panel.querySelector('.summary-persons-count');
    if (!personsElement) return;

    const filteredAncestors = LocationDataBuilder.filterAncestorsByGeneration(max_gen);

    // Compter les personnes uniques avec au moins un lieu
    let personsWithPlaces = 0;

    Object.values(filteredAncestors).forEach(person => {
      let hasPlace = false;
      Events.types.forEach(eventType => {
        const placeField = Events.place(eventType);
        if (person[placeField] && person[placeField].trim() !== '') {
          hasPlace = true;
        }
      });
      if (hasPlace) personsWithPlaces++;
    });

    personsElement.textContent = personsWithPlaces;

    // Tooltip avec traduction
    const personLabel = window.FC_TRANSLATIONS?.[personsWithPlaces > 1 ? 'persons' : 'person'] || 'personne';
    personsElement.title = `${personsWithPlaces} ${personLabel} avec lieux`;
  },

 /* ========== ENRICHISSEMENT DES RÉFÉRENCES DOM ==========
   TODO: write doc */
  enrichLocationDataWithDOMReferences: function() {
    console.log('🔗 Enrichissement des références DOM...');

    let enrichedCount = 0;
    let notFoundCount = 0;

    // Pour chaque lieu dans l'objet lieux
    Object.entries(lieux).forEach(([placeName, placeData]) => {
      // Chercher l'élément DOM avec ce data-place
      const element = document.querySelector(`.place-content[data-place="${CSS.escape(placeName)}"]`);

      if (element) {
        // Enrichir avec les références DOM
        placeData.domElement = element;

        // Obtenir l'index depuis data-index du parent
        const row = element.closest('.place-row');
        if (row) {
          placeData.visualIndex = parseInt(row.dataset.index) || 0;

          // Trouver l'élément indicateur
          const indicatorElement = row.querySelector('.place-indicators');
          if (indicatorElement) {
            placeData.indicatorElement = indicatorElement;
          }
        }

        enrichedCount++;
      } else {
        notFoundCount++;
        console.warn(`❌ Élément DOM non trouvé pour : "${placeName}"`);
      }
    });

    console.log(`✅ Enrichissement terminé : ${enrichedCount} lieux enrichis, ${notFoundCount} non trouvés`);

    return {
      enriched: enrichedCount,
      notFound: notFoundCount,
      total: Object.keys(lieux).length
    };
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
   * Génère la liste complète des lieux en utilisant le template
   * Question critique : le clonage est-il vraiment plus performant que innerHTML ?
   * Réponse : Pour <150 éléments, la différence est négligeable, mais la maintenabilité est meilleure
   */
  generatePlacesList: function() {
    console.log('📝 Génération de la liste des lieux...');

    const container = document.querySelector('.places-list');
    if (!container) {
      console.error("❌ Container places-list introuvable");
      return;
    }

    // Nettoyer le conteneur
    container.innerHTML = "";

    // Obtenir les lieux triés selon le mode actuel
    const sortedPlaces = this.getSortedPlaces();

    // Générer le HTML pour chaque lieu
    let htmlBuffer = [];
    sortedPlaces.forEach(([placeName, placeData], index) => {
      const rowHtml = this.placeRowTemplate(placeName, placeData, index);
      htmlBuffer.push(rowHtml);
    });

    // Injecter tout le HTML d'un coup (plus performant)
    container.innerHTML = htmlBuffer.join('');

    console.log(`✅ ${sortedPlaces.length} lieux générés dans le DOM`);

    // Enrichir les références DOM maintenant que le HTML existe
    this.enrichLocationDataWithDOMReferences();
  },

  /**
   * Crée un élément de lieu en clonant et remplissant le template
   * @param {Array} placeData - [placeName, data] du format lieux_a
   * @param {number} index - Index pour l'attribution des couleurs
   * @returns {Element} Élément DOM prêt à insérer
   */
  createPlaceElement: function(placeData, index) {
    const [placeName, data] = placeData;

    // Clonage du template (deep clone pour copier tous les enfants)
    const element = this.elements.template.cloneNode(true);

    // Transformation en élément visible
    element.className = 'place-row';
    element.style.display = '';

    // Injection des totaux
    const eventsContainer = element.querySelector('.place-events');
    if (eventsContainer) {
      eventsContainer.innerHTML = this.generateEventItemsHTML(data);
    }

    // Injection des données de base
    element.querySelector('.place-name').textContent = placeName;
    element.querySelector('.place-count').textContent = data.cnt;

    // Attribution de la couleur cyclique (réutilise la logique existante)
    const colorIndex = (index % 12) + 1;
    element.querySelector('.place-color').className = `place-color color-${colorIndex}`;

    // Attribution des données pour les interactions futures
    element.dataset.place = placeName;
    element.dataset.index = index;

    // Gestion des sous-lieux (utilise les données pré-calculées)
    if (data.isSubLocation && sortMode === 'alphabetical') {
      element.querySelector('.place-content').classList.add('sublocation');
    }

    return element;
    this.enrichLocationDataWithDOMReferences();
  },

  /**
   * Retourne les lieux triés selon le mode actuel
   * Réutilise la variable globale sortMode existante pour la cohérence
     */
  getSortedPlaces: function() {
    if (sortMode === 'alphabetical') {
      const main = new Map();
      const subs = [];

    // Debug : voir ce qui se passe
    console.log('=== DEBUG TRI ALPHABÉTIQUE ===');
    
    Object.entries(lieux).forEach(([name, data]) => {
      if (data.isSubLocation && data.parentLocation) {
        console.log(`SOUS-LIEU: "${name}" → parent: "${data.parentLocation}"`);
        subs.push({ name, data });
      } else {
        console.log(`LIEU PRINCIPAL: "${name}"`);
        main.set(name, { name, data, subs: [] });
      }
    });

    // Rattacher les sous-lieux à leurs parents
    subs.forEach(({ name, data }) => {
      const group = main.get(data.parentLocation);
      if (group) {
        console.log(`✅ RATTACHEMENT: "${name}" → "${data.parentLocation}"`);
        group.subs.push({ name, data });
      } else {
        console.log(`❌ PARENT INTROUVABLE: "${name}" cherche "${data.parentLocation}"`);
        main.set(name, { name, data, subs: [] });
      }
    });

      // Construire l'array trié avec métadonnées d'affichage
      const sorted = [];
      Array.from(main.values())
        .sort((a, b) => a.name.localeCompare(b.name, 'fr', { sensitivity: 'base' }))
        .forEach(group => {
          // Lieu principal
          sorted.push([group.name, group.data, { isIndented: false, displayName: group.name }]);

          // Sous-lieux indentés
          group.subs
            .sort((a, b) => a.name.localeCompare(b.name, 'fr', { sensitivity: 'base' }))
            .forEach(sub => {
              sorted.push([sub.name, sub.data, { isIndented: true, displayName: sub.data.subName }]);
            });
        });

      return sorted;
    }

    // Mode fréquence : noms complets, pas d'indentation
    return Object.entries(lieux)
      .sort((a, b) => b[1].cnt - a[1].cnt)
      .map(([name, data]) => [name, data, { isIndented: false, displayName: name }]);
  },

  handlePlaceClick: function(placeName, event) {
    // Navigation vers la recherche de lieu
    if (placeName && URLManager.navigateToPlace) {
      const newTab = event.ctrlKey || event.metaKey;
      URLManager.navigateToPlace(placeName, newTab);
    }
  },

  /**
   * Initialise les écouteurs d'événements de base
   * Commencer simple, enrichir selon les besoins réels
   */
  initializeEventListeners: function() {
    if (!this.elements.placesList) return;

    // Écouteur pour les survols de lieux (délégation d'événements)
    this.elements.placesList.addEventListener('mouseenter', (e) => {
      const placeRow = e.target.closest('.place-row');
      if (placeRow) {
        placeRow.classList.add('hovered');
        
        const placeContent = placeRow.querySelector('.place-content');
        if (placeContent) {
          const placeName = placeContent.dataset.place;
          PlacesHighlighter.expandPlaceNameIfNeeded(placeContent, placeName, null, true);
          PlacesHighlighter.highlightSVGSectorsForPlace(placeName, true);
          // TODO: FIX intégration avec le highlighting SVG existant
        }
      }
    }, true);

    this.elements.placesList.addEventListener('mouseleave', (e) => {
      const placeRow = e.target.closest('.place-row');
      if (placeRow) {
        placeRow.classList.remove('hovered');

        const placeContent = placeRow.querySelector('.place-content');
        if (placeContent) {
          const placeName = placeContent.dataset.place;
          PlacesHighlighter.restorePlaceNameIfNeeded(placeContent);
          PlacesHighlighter.highlightSVGSectorsForPlace(placeName, false);
        }
        // TODO: FIX ME
      }
    }, true);

    this.elements.placesList.addEventListener('click', (e) => {
      const placeRow = e.target.closest('.place-content');
      if (placeRow) {
        const placeName = placeRow.dataset.place;
        if (placeName && URLManager.navigateToPlace) {
          const newTab = e.ctrlKey || e.metaKey;
          URLManager.navigateToPlace(placeName, newTab);
        }
      }
    });
  }
};

// ========== Interface utilisateur pour le panneau des lieux ==========
const PlacesPanelControls = {
  /**
   * Initialise les contrôles du panneau
   * Cette méthode doit être appelée après l’initialisation de PlacesInterface
   */
  initialize: function() {
    console.log('🎛️ Initialisation des contrôles du panneau des lieux...');

    if (!PlacesInterface.elements.panel) {
      console.error('❌ PlacesInterface doit être initialisé avant PlacesPanelControls');
      return false;
    }

    // Initialisation des states par défaut
    this.initializeDefaultStates();

    // Configuration des event listeners modernes
    this.setupEventListeners();

    console.log('✅ Contrôles du panneau initialisés');
    return true;
  },

  /**
   * Configure tous les event listeners du panneau en utilisant la délégation d'événements
   * Cette approche est plus maintenable et performante que les onclick inline
   */
  setupEventListeners: function() {
    const panel = PlacesInterface.elements.panel;

    // Délégation d'événements sur le panneau entier
    // Cette technique permet de gérer tous les clics d'un seul endroit
    panel.addEventListener('click', (e) => {
      // Identification de l'élément cliqué par ses classes CSS
      if (e.target.matches('.panel-close')) {
        e.preventDefault();
        this.togglePanel();
      }
      else if (e.target.matches('.sort-toggle') || e.target.closest('.sort-toggle')) {
        e.preventDefault();
        this.toggleSort();
      }
      else if (e.target.matches('.events-toggle') || e.target.closest('.events-toggle')) {
        e.preventDefault();
        this.toggleEventsDisplay();
      }
      else if (e.target.matches('.search-clear')) {
        e.preventDefault();
        this.clearSearch();
      }
    });

    // Event listener spécifique pour le champ de recherche
    // L'événement 'input' est plus approprié que 'oninput' pour la réactivité
    const searchInput = panel.querySelector('.search-input');
    if (searchInput) {
      searchInput.addEventListener('input', (e) => {
        this.filterPlaces(e.target.value);
      });
    }
  },

  /**
   * Configure les états par défaut des contrôles
   * Centralise la logique d'initialisation pour éviter les incohérences
   */
  initializeDefaultStates: function(showEvents = false) {
    console.log('🔍 initializeDefaultStates appelé avec showEvents:', showEvents); // ← AJOUTER ÇA

    // État initial du tri (cohérent avec la variable globale sortMode)
    this.updateSortButtonIcon();

    // État initial des événements - NE PAS écraser si vient de l'URL
    const panel = PlacesInterface.elements.panel;

    if (showEvents) {
      panel.classList.add('show-events');
      console.log('✅ Classe show-events AJOUTÉE'); // ← ET ÇA

      // Mettre à jour l'icône du bouton
      const icon = document.querySelector('.events-toggle i');
      if (icon) {
        icon.className = 'far fa-eye-slash';
      }
    } else {
      panel.classList.remove('show-events');
      console.log('❌ Classe show-events SUPPRIMÉE'); // ← ET ÇA
      // Icône par défaut
      const icon = document.querySelector('.events-toggle i');
      if (icon) {
        icon.className = 'far fa-eye';
      }
    }

    // Réinitialisation du champ de recherche
    const searchInput = panel.querySelector('.search-input');
    if (searchInput) searchInput.value = '';
  },

  /**
   * Bascule l'affichage du panneau des lieux
   * Fonction globale référencée par le HTML onclick="togglePlacesPanel()"
   */
  togglePanel: function() {
    const panel = PlacesInterface.elements.panel;
    const isCurrentlyVisible = panel.style.display !== 'none';

    panel.style.display = isCurrentlyVisible ? 'none' : 'block';

    // Log pour le débogage en développement
    console.log(`📋 Panneau des lieux ${isCurrentlyVisible ? 'masqué' : 'affiché'}`);
  },

  /**
   * Bascule le mode de tri entre fréquence et alphabétique
   * Met à jour l'interface et régénère la liste
   */
  toggleSort: function() {
    // Modification de la variable globale (maintient la cohérence avec le reste du système)
    sortMode = sortMode === 'frequency' ? 'alphabetical' : 'frequency';

    // Mise à jour visuelle du bouton
    this.updateSortButtonIcon();

    // Régénération de la liste avec le nouveau tri
    PlacesInterface.generatePlacesList();
    
    URLManager.updateCurrentURL();
    console.log(`🔄 Tri basculé vers: ${sortMode}`);
  },

  /**
   * Met à jour l'icône du bouton de tri selon le mode actuel
   * Centralise la logique d'affichage pour éviter les incohérences
   */
  updateSortButtonIcon: function() {
    const icon = document.querySelector('.sort-toggle i');
    if (icon) {
      icon.className = sortMode === 'alphabetical'
        ? 'fas fa-arrow-down-wide-short'
        : 'fas fa-arrow-down-a-z';
    }

    // Mise à jour du title pour l'accessibilité
    const button = document.querySelector('.sort-toggle');
    if (button) {
      button.title = sortMode === 'alphabetical'
        ? 'Trier par fréquence'
        : 'Trier par ordre alphabétique';
    }
  },

  /**
   * Bascule l'affichage détaillé des événements
   * Active/désactive le mode d'affichage étendu du panneau
   */
  toggleEventsDisplay: function() {
    const panel = PlacesInterface.elements.panel;
    const isShowingEvents = panel.classList.contains('show-events');

    // Basculement de la classe CSS qui contrôle l'affichage
    panel.classList.toggle('show-events', !isShowingEvents);

    // Mise à jour de l'icône du bouton
    const icon = document.querySelector('.events-toggle i');
    if (icon) {
      icon.className = !isShowingEvents ? 'far fa-eye-slash' : 'far fa-eye';
    }

    URLManager.updateCurrentURL();
    console.log(`👁️ Affichage des événements ${!isShowingEvents ? 'activé' : 'désactivé'}`);
  },

  /**
   * Filtre la liste des lieux selon une requête de recherche
   * @param {string} query - Terme de recherche saisi par l'utilisateur
   */
  filterPlaces: function(query) {
    const rows = document.querySelectorAll('.place-row');
    const normalizedQuery = query.toLowerCase().trim();

    // Application du filtrage sur chaque élément de lieu
    rows.forEach(row => {
      const placeName = row.querySelector('.place-name').textContent.toLowerCase();
      const shouldShow = !normalizedQuery || placeName.includes(normalizedQuery);
      row.style.display = shouldShow ? '' : 'none';
    });

    // Gestion de l'affichage de la croix de nettoyage
    this.updateClearButtonVisibility(normalizedQuery);

    // Statistiques de filtrage pour le débogage
    const visibleCount = document.querySelectorAll('.place-row[style=""]').length;
    console.log(`🔍 Filtrage: ${visibleCount} lieux affichés sur ${rows.length}`);
  },

  /**
   * Gère l'affichage conditionnel du bouton de nettoyage de recherche
   * @param {string} query - Requête de recherche actuelle
   */
  updateClearButtonVisibility: function(query) {
    const clearBtn = document.querySelector('.search-clear');
    if (clearBtn) {
      clearBtn.style.display = query ? 'block' : 'none';
    }
  },

  /**
   * Efface la recherche et réaffiche tous les lieux
   * Fonction globale référencée par le HTML onclick="clearSearch()"
   */
  clearSearch: function() {
    const input = document.querySelector('.search-input');
    if (input) {
      input.value = '';
    }

    // Réapplication du filtrage avec une requête vide
    this.filterPlaces('');

    console.log('🧹 Recherche effacée, tous les lieux réaffichés');
  }
};

// ========== Module de surlignage bidirectionnel pour les lieux ==========
const PlacesHighlighter = {
  // Stockage des éléments actuellement surlignés
  currentHighlights: [],
  currentIndicators: [],

  /**
   * Simule le survol d'une personne - appelé depuis SVGRenderer
   * @param {Array<string>} placeNames - Noms des lieux à surligner
   * @param {Array<string>} eventTypes - Types d'événements pour chaque lieu
   */
  simulatePersonHover: function(placeNames, eventTypes) {
    console.log('🎯 Surlignage pour:', placeNames, eventTypes);

    // Nettoyage préalable
    this.clearAllHighlights();

    if (!placeNames || placeNames.length === 0) return;

    const highlightedPlaces = new Set(placeNames);
    const matchingElements = [];

    // Traiter chaque lieu
    placeNames.forEach((placeName, index) => {
      const placeData = lieux[placeName];
      if (!placeData || !placeData.domElement) {
        console.warn(`⚠️ Lieu non trouvé ou sans élément DOM: ${placeName}`);
        return;
      }

      // Surligner l'élément de la liste
      placeData.domElement.classList.add('person-match');
      this.currentHighlights.push(placeData.domElement);

      // Ajouter les indicateurs d'événements
      if (placeData.indicatorElement) {
        const events = placeNames.length === 1 ? eventTypes : [eventTypes[index]].filter(Boolean);
        if (events.length > 0) {
          this.addIndicatorsForPlace(placeData.indicatorElement, events);
        }
     }

     this.expandPlaceNameIfNeeded(placeData.domElement, placeName, highlightedPlaces);

      matchingElements.push({
        element: placeData.domElement,
        index: placeData.visualIndex,
        placeName: placeName
      });
    });

    // Griser les autres lieux
    this.grayOutOtherPlaces();

    if (matchingElements.length > 0) {
      setTimeout(() => {
        ModernOverflowManager.handleOverflow(matchingElements);
      }, 100);
    }
  },

  /**
   * Affiche le nom complet d'un sous-lieu si nécessaire
   */
  expandPlaceNameIfNeeded: function(placeElement, placeName, highlightedPlaces = null, forceExpand = false) {
      console.log('🚀 expandPlaceNameIfNeeded appelé avec:', {
    placeName, 
    forceExpand, 
    'placeData.isSubLocation': lieux[placeName]?.isSubLocation
  }); 
    const placeData = lieux[placeName];
    const placeNameElement = placeElement.querySelector('.place-name');
    
    if (placeNameElement && placeData) {
      let shouldExpand = false;
      
      if (forceExpand && placeData.isSubLocation) {
        // Cas survol liste : toujours afficher pour les sous-lieux uniquement
        shouldExpand = true;
      } else if (sortMode === 'alphabetical' && placeData.isSubLocation) {
        // Cas survol secteur : vérifier le parent
        shouldExpand = !highlightedPlaces || !highlightedPlaces.has(placeData.parentLocation);
      }
      
      if (shouldExpand) {
        if (!placeNameElement.dataset.originalHtml) {
          placeNameElement.dataset.originalHtml = placeNameElement.innerHTML;
        }
        // Remplacer par le nom complet sans indicateur de sous-lieu
        placeNameElement.innerHTML = placeName;
      }
    }
  },

  restorePlaceNameIfNeeded: function(placeElement) {
    const placeNameElement = placeElement.querySelector('.place-name');
    if (placeNameElement && placeNameElement.dataset.originalHtml) {
      placeNameElement.innerHTML = placeNameElement.dataset.originalHtml;
      delete placeNameElement.dataset.originalHtml;
    }
  },

  /**
   * Ajoute les indicateurs visuels d'événements pour un lieu
   * @param {HTMLElement} container - Conteneur des indicateurs
   * @param {Array<string>} events - Types d'événements à afficher
   */
addIndicatorsForPlace: function(container, events) {
  if (!container) return;

  console.log('🔍 addIndicatorsForPlace appelé avec:', events.length, 'événements:', events);

  events.forEach((event, index) => {
    console.log(`  → Index ${index}: ${event}`);
    
    const indicator = document.createElement('div');
    const shortClass = Events.cssClass(event);
    const label = Events.label(event);

    indicator.className = `indicator ${shortClass}`;
    indicator.textContent = label;
    container.appendChild(indicator);
    this.currentIndicators.push(indicator);

    // DEBUG : Line-break après le 2e si 4+ événements
    if (index === 1 && events.length >= 4) {
      console.log('✅ AJOUT LINE-BREAK car index=1 et length=', events.length);
      const breaker = document.createElement('div');
      breaker.className = 'line-break';
      container.appendChild(breaker);
      this.currentIndicators.push(breaker);
    } else {
      console.log('❌ PAS de line-break car index=', index, 'et length=', events.length);
    }
  });

  // Ajouter tall-row seulement si 4+ événements
  if (events.length >= 4) {
    console.log('✅ AJOUT tall-row car length=', events.length);
    const row = container.closest('.place-row');
    if (row) row.classList.add('tall-row');
  }
},

  /**
   * Nettoie tous les surlignages et indicateurs
   */
  clearAllHighlights: function() {
    // Nettoyer les surlignages
    this.currentHighlights.forEach(element => {
      element.classList.remove('person-match');
    });
    this.currentHighlights = [];

    // Nettoyer les indicateurs
    this.currentIndicators.forEach(indicator => {
      indicator.remove();
    });
    this.currentIndicators = [];

    // Retirer le grisage
    document.querySelectorAll('.place-content.grayed-out').forEach(element => {
      element.classList.remove('grayed-out');
    });

    //Restaurer la hauteur de liste et nettoyer overflow
    const list = document.querySelector('.places-list');
    if (list) {
      list.style.maxHeight = 'none';
      list.style.height = 'auto';
    }

    // Nettoyer les sections d'overflow
    document.querySelectorAll('.overflow-section').forEach(section => {
      section.remove();
    });

    // Retirer toutes les classes tall-row
    document.querySelectorAll('.place-row.tall-row').forEach(row => {
      row.classList.remove('tall-row');
    });

    this.currentHighlights.forEach(element => {
      element.classList.remove('person-match');
      this.restorePlaceNameIfNeeded(element); // ← Nouveau
    });
  },

  /**
   * Grise tous les lieux non surlignés
   */
  grayOutOtherPlaces: function() {
    document.querySelectorAll('.place-content').forEach(element => {
      if (!element.classList.contains('person-match')) {
        element.classList.add('grayed-out');
      }
    });
  },

  /**
   * Surligne les secteurs SVG pour un lieu donné (Liste → SVG)
   * @param {string} placeName - Nom du lieu
   * @param {boolean} highlight - true pour surligner, false pour nettoyer
   */
  highlightSVGSectorsForPlace: function(placeName, highlight) {
    if (!placeName || !lieux[placeName]) return;

    const placeData = lieux[placeName];
    const placeClass = placeData.c; // Ex: "L0"

    // Trouver tous les secteurs SVG avec cette classe
    const sectors = document.querySelectorAll(`svg .${placeClass}`);

    sectors.forEach(sector => {
      if (highlight) {
        // Utiliser une couleur différente pour distinguer la direction du highlighting
        sector.style.fill = 'lightblue';
        sector.dataset.listHighlighted = 'true';
      } else {
        // Nettoyer seulement si c'était un highlighting depuis la liste
        if (sector.dataset.listHighlighted === 'true') {
          sector.style.fill = '';
          delete sector.dataset.listHighlighted;
        }
      }
    });
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
      title.textContent = `(Sosa 1) ${p.fn} ${p.sn}${age}\nCtrl+clic pour la fiche individuelle`;
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
      Events.types.forEach(eventType => {
        const placeField = Events.place(eventType);
        const svgPrefix = Events.svgPrefix(eventType); // 'bi', 'ba', 'ma', 'de', 'bu'

        if (p[placeField] && lieux[p[placeField]]) {
          classes.push(`${svgPrefix}-${lieux[p[placeField]].c}`);
        }
      });

      if (p.death_age) classes.push(Utils.deathAgeClass(p.death_age));

    } else if (type === 'marriage') {
      const marriagePlaceField = Events.place('marriage');
      if (p[marriagePlaceField] && lieux[p[marriagePlaceField]]) {
        const svgPrefix = Events.svgPrefix('marriage'); // 'ma'
        classes.push(`${svgPrefix}-${lieux[p[marriagePlaceField]].c}`);
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

  togglePlaceHighlights: function(p, show, type) {
    // Nettoyer d'abord si on désactive
    if (!show) {
      PlacesHighlighter.clearAllHighlights();
      return;
    }

    // Fonction utilitaire pour vérifier et highlighter un lieu de façon robuste
    const safeHighlightPlace = (placeName, eventType) => {
      if (!placeName || !lieux[placeName]) return false;

      const placeData = lieux[placeName];
      const className = `${Events.svgPrefix(eventType)}-${placeData.c}`;
      const elements = document.getElementsByClassName(className);

      if (elements.length > 0) {
        Array.from(elements).forEach(el => {
          el.classList.toggle("hidden", !show);
        });
        return true;
      }
      return false;
    };

    if (type === 'marriage') {
      // Pour les mariages, traiter seulement le lieu de mariage
      const placeName = p[Events.place('marriage')];
      const highlighted = safeHighlightPlace(placeName, 'marriage');

      if (highlighted && show && PlacesInterface.elements.panel) {
        PlacesHighlighter.simulatePersonHover([placeName], ['marriage']);
      }
    } else {
      // Pour les personnes, GROUPER par lieu
      const placeEventMap = new Map();

      Events.types.forEach(eventType => {
        const placeName = p[Events.place(eventType)];
        if (safeHighlightPlace(placeName, eventType)) {
          if (show) {
            if (!placeEventMap.has(placeName)) {
              placeEventMap.set(placeName, []);
            }
            placeEventMap.get(placeName).push(eventType);
          }
        }
      });
      
      if (show && placeEventMap.size > 0 && PlacesInterface.elements.panel) {
        const placesToHighlight = Array.from(placeEventMap.keys());
        const eventsToShow = Array.from(placeEventMap.values()).flat();
        PlacesHighlighter.simulatePersonHover(placesToHighlight, eventsToShow);
      }
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
        `body.place_color svg .${eventType}-L${index} { fill: var(--fc-color-${index}); }`,
        sheet.cssRules.length
      );
    });
    // Règle générique sans préfixe
    sheet.insertRule(
      `body.place_color svg .L${index} { fill: var(--fc-color-${index}); }`,
      sheet.cssRules.length
    );
  },

  setColorMode: function(newMode) {
    // Nettoyer l'état précédent
    document.body.classList.remove('place_color', 'death-age');


    // Désactiver tous les toggles NMBDS
    this.EVENT_TYPES.forEach(id => {
      const checkbox = document.getElementById(id);
      if (checkbox) checkbox.checked = false;
    });

    // Appliquer le nouveau mode
    if (newMode === 'place_color') {
      document.body.className = "place_color";
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
    URLManager.updateCurrentURL();
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
        document.body.className = "place_color";
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
      URLManager.updateCurrentURL();
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

      URLManager.updateCurrentURL();
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

// ========== GESTIONNAIRE D'ÉVÉNEMENTS GLOBAL ==========
const FanchartPlacesEventManager = {
  /**
   * Initialise tous les événements liés au panneau des lieux
   * Cette fonction centralise l'initialisation pour éviter les doublons
   */
  initializeAll: function() {
    console.log('🎮 Initialisation des événements du panneau des lieux...');

    // Vérifier que les modules sont chargés
    if (!PlacesInterface.elements.panel) {
      console.error('❌ Le panneau des lieux doit être initialisé d\'abord');
      return;
    }

    // Initialiser les événements de base
    this.initializeHoverEvents();
    this.initializeClickEvents();
    this.initializeKeyboardShortcuts();

    console.log('✅ Événements initialisés');
  },

  /**
   * Événements de survol (délégation)
   */
  initializeHoverEvents: function() {
    const placesList = PlacesInterface.elements.placesList;
    if (!placesList) return;

    // Survol entrée
    placesList.addEventListener('mouseenter', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        const placeName = placeContent.dataset.place;
        if (placeName) {
          placeContent.classList.add('hovered');
          PlacesHighlighter.expandPlaceNameIfNeeded(placeContent, placeName, null, true);
        }
      }
    }, true);

    // Survol sortie
    placesList.addEventListener('mouseleave', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        const placeName = placeContent.dataset.place;
        if (placeName) {
          placeContent.classList.remove('hovered');
          PlacesHighlighter.restorePlaceNameIfNeeded(placeContent);
          PlacesHighlighter.highlightSVGSectorsForPlace(placeName, false);
        }
      }
    }, true);
  },

  /**
   * Événements de clic
   */
  initializeClickEvents: function() {
    const placesList = PlacesInterface.elements.placesList;
    if (!placesList) return;

    placesList.addEventListener('click', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        const placeName = placeContent.dataset.place;
        if (placeName) {
          PlacesInterface.handlePlaceClick(placeName, e);
        }
      }
    });
  },

  /**
   * Raccourcis clavier (optionnel)
   */
  initializeKeyboardShortcuts: function() {
    document.addEventListener('keydown', (e) => {
      // Ctrl/Cmd + L : Focus sur la recherche de lieux
      if ((e.ctrlKey || e.metaKey) && e.key === 'l') {
        e.preventDefault();
        const searchInput = document.getElementById('fanchart-search-input');
        if (searchInput) searchInput.focus();
      }

      // Escape : Nettoyer les surlignages
      if (e.key === 'Escape') {
        PlacesHighlighter.clearAllHighlights();
      }
    });
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
    console.log('📏 Hauteur liste initiale:', this.originalListHeight);

    return true;
  },

  /**
   * Gère l'overflow après un surlignage
   * @param {Array} matchingItems - Éléments surlignés à garder visibles
   */
  handleOverflow: function(matchingItems) {
    // Prévenir les appels multiples simultanés
    if (this.isProcessing) {
      console.log('⚠️ Overflow déjà en cours, ignoré');
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
      console.log('✅ Aucun overflow détecté');
      return;
    }

    console.log(`📊 Overflow détecté: ${currentOverflow.above?.length || 0} au-dessus, ${currentOverflow.below?.length || 0} en-dessous`);

    // ✅ FIXÉ - ne pas passer de reservedSpace
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

    console.log(`📐 Espace: ${requiredSpace}px → ${finalSpace}px (max: ${maxAllowedSpace}px)`);
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
      aboveSection.style.top = '0px'; // ✅ FIXÉ - en haut !
      container.appendChild(aboveSection);
      this.currentOverflowSections.push(aboveSection);
    }

    // Overflow en-dessous - utiliser les classes CSS (bottom: 0)
    if (overflowData.below?.length > 0) {
      const belowSection = this.createOverflowSection('below', overflowData.below);
      // ✅ FIXÉ - laisser le CSS gérer avec .overflow-section.below
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
    // ========== PHASE 1: CALCULS DE BASE ==========
    this.calculateDimensions();
    this.processAncestorData();
    
    // ========== PHASE 2: LECTURE CENTRALISÉE DE L'ÉTAT ==========
    const state = URLManager.readCurrentState();
    
    // Application directe aux variables globales (une seule fois)
    tool = state.tool;
    sortMode = state.sortMode;
    isCircularMode = state.isCircular;
    current_angle = state.angle;
    implexMode = state.implexMode;
    
    // ========== PHASE 3: CONSTRUCTION DES DONNÉES ==========
    LocationDataBuilder.buildCompleteLocationData();
    
    // ========== PHASE 4: INTERFACE DES LIEUX ==========
    if (document.querySelector('.places-panel')) {
      if (PlacesInterface.initialize()) {
        PlacesInterface.generatePlacesList();
        PlacesInterface.updateSummarySection();
        // Passage propre de l'état events (pas de re-lecture)
        PlacesPanelControls.initialize(state.showEvents);
        PlacesInterface.initializeEventListeners();
      }
    }
    
    // ========== PHASE 5: MISE À JOUR DES BOUTONS ==========
    AngleManager.updateAngleButtons();
    if (isCircularMode) {
      const circularBtn = document.getElementById('b-circular-mode');
      if (circularBtn) circularBtn.classList.add('active');
    }
    
    // ========== PHASE 6: INITIALISATION DES ÉVÉNEMENTS ==========
    DOMCache.preload();
    this.initializeEvents();
    this.initializeAngleEvents();
    ColorManager.initializeColorEvents();
    LegendManager.initializeAllEvents();
    
    // ========== PHASE 7: RENDU ET FINALISATION ==========
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
      this.cleanPersonAge(person, key);
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
    standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
    standard.setAttribute("id", "standard");
    standard.setAttribute("x", center_x);
    standard.setAttribute("y", center_y);
    target.append(standard);

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
      PlacesInterface.initializeEventListeners();
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
    if (tool == "death-age") {
      document.body.className = "death-age";
      const ageButton = document.getElementById("b-death-age");
      if (ageButton) ageButton.classList.add("active");
    } else {
      document.body.className = "place_color";
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

window.toggleFanchartSort = () => PlacesPanelControls.toggleSort();
window.toggleFanchartEventsDisplay = () => PlacesPanelControls.toggleEventsDisplay();
window.filterFanchartPlaces = (query) => PlacesPanelControls.filterPlaces(query);
window.clearFanchartSearch = () => PlacesPanelControls.clearSearch();
window.toggleFanchartPlacesPanel = () => document.body.classList.toggle('place_color');