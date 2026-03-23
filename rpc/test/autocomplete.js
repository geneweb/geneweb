/**
 * autocomplete.js (geneweb_autocomplete)
 * =======================================
 * Progressive enhancement pour les templates GeneWeb.
 *
 * Remplace les <datalist> natifs par un widget d'autocomplétion
 * à 3 colonnes alimenté par le serveur RPC via WebSocket.
 *
 * Protocole serveur (service.ml) :
 *   - "info"   (arity 0) → list (string * int)
 *              Retourne les paires (nom_index, cardinal) pour tous
 *              les index chargés. Noms = basename sans extension
 *              des fichiers .gz (ex: "HenriT_fnames").
 *   - "lookup" (arity 3) : name:string, query:string, size:int
 *              → list string (résultats plats, ordre priorité :
 *                exact → prefix → fuzzy/Levenshtein)
 *              La déduplication n'est pas garantie côté serveur.
 *
 * Le widget découpe la liste plate en 3 colonnes selon la position
 * du résultat par rapport à la requête :
 *   col1 : résultats commençant par la requête  (prefix exact)
 *   col2 : résultats contenant la requête ailleurs  (exact interne)
 *   col3 : les autres  (fuzzy / Levenshtein)
 *
 * Index format côté serveur : <basename>.gz
 * Nom d'index dans les appels RPC : basename sans extension
 * (ex: "HenriT_fnames")
 *
 * Ce fichier définit la classe GenewebAutocomplete (exportée globalement).
 * L'initialisation se fait dans js.txt via :
 *
 *   GenewebAutocomplete.init({ rpcUrl: ..., indexMap: { ... } });
 *
 * Si le serveur RPC n'est pas disponible, les datalists natifs
 * sont conservés. Aucune dépendance externe (vanilla JS).
 */
;(function (root) {
  'use strict';

  // ==========================================================
  // Configuration par défaut
  // ==========================================================
  const DEFAULTS = {
    rpcUrl: 'ws://localhost:8080/search',
    // Sélecteur CSS des inputs à enrichir
    inputSelector: 'input[list]',
    // Mapping datalist id → nom d'index RPC (basename sans .gz)
    // Ex: { 'list_fn': 'HenriT_fnames', 'list_sn': 'HenriT_snames', ... }
    indexMap: {},
    // Nombre min de caractères avant recherche
    minChars: 2,
    // Délai de debounce en ms
    debounceMs: 250,
    // Nombre max de résultats demandés au serveur (liste plate)
    maxResults: 90,
    // -------------------------------------------------------
    // Libellés des colonnes — SEUL ENDROIT À MODIFIER
    // -------------------------------------------------------
    // Le serveur retourne une liste plate; le widget la découpe en 3 colonnes :
    //   col1 : résultats dont le début correspond à la requête (préfixe)
    //   col2 : résultats contenant la requête non en début (exact interne)
    //   col3 : les autres résultats (fuzzy / Levenshtein)
    //   col4 : réservé / non utilisé
    columnLabels: {
      col1: 'Préfixe exact',
      col2: 'Préfixe interne',
      col3: 'Approx.',
      col4: ''
    },
    // Timeout connexion WS en ms
    connectTimeout: 3000,
    // Tentatives de reconnexion
    maxReconnect: 2,
    // Active le petit badge vert/rouge sur l'input
    showRpcBadge: false,
    // Callback quand un item est sélectionné
    onSelect: null,
    // Log debug dans la console
    debug: false
  };

  // ==========================================================
  // Classe WebSocket RPC (miniature, spécialisée lookup)
  // ==========================================================
  class RpcWsClient {
    constructor(url, opts) {
      this.url = url;
      this.opts = opts;
      this.ws = null;
      this.reqId = 0;
      this.pending = new Map();
      this._connected = false;
      this._connecting = false;
    }

    log(...args) {
      if (this.opts.debug) console.log('[GW-AC RPC]', ...args);
    }

    isConnected() {
      return this._connected && this.ws && this.ws.readyState === WebSocket.OPEN;
    }

    connect() {
      if (this._connected || this._connecting) {
        return this._connectPromise || Promise.resolve();
      }
      this._connecting = true;
      this._connectPromise = new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          this._connecting = false;
          reject(new Error('WS connect timeout'));
        }, this.opts.connectTimeout);

        try {
          this.ws = new WebSocket(this.url);
        } catch (e) {
          clearTimeout(timer);
          this._connecting = false;
          reject(e);
          return;
        }

        this.ws.onopen = () => {
          clearTimeout(timer);
          this._connected = true;
          this._connecting = false;
          this.log('Connecté à', this.url);
          resolve();
        };

        this.ws.onerror = (e) => {
          clearTimeout(timer);
          this._connecting = false;
          this.log('Erreur WS', e);
          reject(new Error('WS connection error'));
        };

        this.ws.onclose = () => {
          this._connected = false;
          this._connecting = false;
          // Rejeter toutes les requêtes en attente
          for (const [id, p] of this.pending) {
            p.reject(new Error('WS closed'));
          }
          this.pending.clear();
          this.log('Déconnecté');
        };

        this.ws.onmessage = (event) => {
          try {
            const msg = JSON.parse(event.data);
            if (msg.id != null && this.pending.has(msg.id)) {
              const p = this.pending.get(msg.id);
              this.pending.delete(msg.id);
              if (msg.error) {
                p.reject(new Error(msg.error.message || 'RPC error'));
              } else {
                p.resolve(msg.result);
              }
            }
          } catch (e) {
            this.log('Parse error', e);
          }
        };
      });
      return this._connectPromise;
    }

    disconnect() {
      if (this.ws) {
        this.ws.close();
        this.ws = null;
      }
      this._connected = false;
    }

    /**
     * Appel RPC générique.
     * params doit être un Array (params positionnels) car le serveur
     * OCaml utilise Desc.eval qui consomme une liste JSON ordonnée.
     */
    call(method, params) {
      return new Promise((resolve, reject) => {
        if (!this.isConnected()) {
          reject(new Error('Not connected'));
          return;
        }
        const id = ++this.reqId;
        const msg = JSON.stringify({
          jsonrpc: '2.0',
          id,
          method,
          params: Array.isArray(params) ? params : []
        });
        this.pending.set(id, { resolve, reject });
        this.ws.send(msg);

        // Timeout par requête
        setTimeout(() => {
          if (this.pending.has(id)) {
            this.pending.delete(id);
            reject(new Error('RPC call timeout'));
          }
        }, 5000);
      });
    }

    /**
     * Recherche : "lookup" (name, query, size) → list string (liste plate).
     *
     * Le serveur retourne les résultats dans l'ordre de priorité :
     *   1. correspondances exactes (commencent par la requête)
     *   2. correspondances préfixe
     *   3. correspondances fuzzy (Levenshtein ≤ 1)
     * La déduplication n'est pas garantie côté serveur.
     *
     * Le widget découpe ensuite cette liste plate en colonnes côté JS
     * (voir _splitResults dans AutocompleteWidget).
     */
    async lookup(indexName, query, size) {
      // Params positionnels : [name, query, size]
      const result = await this.call('lookup', [indexName, query, size || 90]);
      // Le serveur renvoie un array plat de strings
      return Array.isArray(result) ? result : [];
    }

    /**
     * Info sur les index disponibles.
     * Méthode "info" (arity 0) → list (string * int)
     * Retourne des paires [nom_index, cardinal].
     * Noms = basename du fichier .gz sans extension
     * (ex: "HenriT_fnames", "HenriT_snames", ...).
     */
    async info() {
      const result = await this.call('info', []);
      // result est une liste de [name, count] (tuple2 sérialisé en array JSON)
      if (!Array.isArray(result)) return [];
      return result.map(pair => ({
        name:  Array.isArray(pair) ? pair[0] : pair.name  || '',
        count: Array.isArray(pair) ? pair[1] : pair.count || 0
      }));
    }
  }

  // ==========================================================
  // Classe Widget Autocomplete (une instance par input)
  // ==========================================================
  class AutocompleteWidget {
    constructor(input, rpc, opts) {
      this.input = input;
      this.rpc = rpc;
      this.opts = opts;

      // Trouver l'index RPC pour cet input
      this.datalistId = input.getAttribute('list');
      this.indexName = this._resolveIndex();

      // État
      this.isOpen = false;
      // col4 est vide, on travaille sur 3 colonnes de résultats
      this.results = { col1: [], col2: [], col3: [], col4: [] };
      this.selectedCol = 0;
      this.selectedRow = -1;
      this.debounceTimer = null;

      // Sauvegarder le datalist original
      this.originalDatalist = this.datalistId
        ? document.getElementById(this.datalistId)
        : null;

      // Construire le DOM
      this._buildDOM();
      this._bindEvents();

      this.log('Widget créé pour', this.input.name || this.input.id,
               '→ index:', this.indexName);
    }

    log(...args) {
      if (this.opts.debug) console.log('[GW-AC Widget]', ...args);
    }

    /**
     * Résout le nom d'index RPC à partir du datalist id.
     *
     * Stratégie :
     * 1. Mapping explicite dans opts.indexMap (valeur = basename sans .gz)
     * 2. Recherche par pattern dans les index du serveur
     * 3. Convention GeneWeb (datalist id → suffixe de basename)
     */
    _resolveIndex() {
      // 1. Mapping explicite
      if (this.datalistId && this.opts.indexMap[this.datalistId]) {
        const mapped = this.opts.indexMap[this.datalistId];
        // Vérifier si ce nom existe sur le serveur (correspondance exacte)
        const serverNames = this.opts._serverIndexNames || [];
        if (serverNames.indexOf(mapped) !== -1) {
          return mapped;
        }
        // Sinon, recherche floue parmi les index du serveur
        const found = this._findServerIndex(mapped, serverNames);
        if (found) return found;
        // Retourner le mapping tel quel
        return mapped;
      }

      // 2. Convention GeneWeb : datalist id → suffixe connu
      const conventionMap = {
        'list_fn':     'fnames',
        'list_sn':     'snames',
        'list_pl':     'places',
        'list_src':    'sources',
        'list_occu':   'occupations',
        'list_qual':   'qualifiers',
        'list_titl':   'titles',
        'list_estate': 'estates',
        'list_ali':    'aliases',
        'list_pub':    'pub_names'
      };

      const suffix = this.datalistId ? conventionMap[this.datalistId] : null;
      const serverNames = this.opts._serverIndexNames || [];

      if (suffix) {
        const found = this._findServerIndex(suffix, serverNames);
        if (found) return found;
      }

      // 3. Recherche directe du datalist id dans les noms serveur
      if (this.datalistId) {
        const found = this._findServerIndex(this.datalistId, serverNames);
        if (found) return found;
      }

      return this.datalistId || 'default';
    }

    /**
     * Cherche un index serveur qui correspond à un pattern.
     * Ex: pattern "fnames" matche "HenriT_fnames"
     *     pattern "sname"  matche "HenriT_snames"
     */
    _findServerIndex(pattern, serverNames) {
      if (!serverNames || serverNames.length === 0) return null;
      const lowerPattern = pattern.toLowerCase();

      // Correspondance exacte
      for (var i = 0; i < serverNames.length; i++) {
        if (serverNames[i] === pattern) return serverNames[i];
      }

      // Correspondance partielle : le nom serveur contient le pattern
      for (var i = 0; i < serverNames.length; i++) {
        if (serverNames[i].toLowerCase().indexOf(lowerPattern) !== -1)
          return serverNames[i];
      }

      // Essai avec pluriel : "fname" → chercher "fnames"
      const pluralPattern = lowerPattern + 's';
      for (var i = 0; i < serverNames.length; i++) {
        if (serverNames[i].toLowerCase().indexOf(pluralPattern) !== -1)
          return serverNames[i];
      }

      return null;
    }

    /**
     * Découpe la liste plate retournée par "lookup" en 3 colonnes.
     *
     * Le serveur retourne : exact → prefix → fuzzy (sans déduplication garantie).
     * On classe chaque entrée selon sa relation à la requête normalisée :
     *   col1 : commence par la requête (préfixe)
     *   col2 : contient la requête ailleurs (exact interne)
     *   col3 : ne contient pas la requête (fuzzy / Levenshtein)
     *   col4 : toujours vide
     *
     * La déduplication est effectuée ici côté JS.
     */
    _splitResults(flatList, query) {
      const col1 = [], col2 = [], col3 = [];
      const seen = new Set();
      const normQ = this._normalize(query);

      for (const entry of flatList) {
        if (seen.has(entry)) continue;
        seen.add(entry);

        const normE = this._normalize(entry);
        if (normE.startsWith(normQ)) {
          col1.push(entry);
        } else if (normE.indexOf(normQ) !== -1) {
          col2.push(entry);
        } else {
          col3.push(entry);
        }
      }

      return { col1, col2, col3, col4: [] };
    }

    /**
     * Construction du DOM du widget
     */
    _buildDOM() {
      // Wrapper autour de l'input
      this.wrapper = document.createElement('div');
      this.wrapper.className = 'gw-ac-wrapper';
      this.input.parentNode.insertBefore(this.wrapper, this.input);
      this.wrapper.appendChild(this.input);

      // Supprimer l'attribut list pour désactiver le datalist natif
      this.input.removeAttribute('list');
      if (this.datalistId) {
        this.input.setAttribute('data-gw-ac-list', this.datalistId);
      }
      this.input.setAttribute('autocomplete', 'off');

      // Badge RPC (optionnel)
      if (this.opts.showRpcBadge) {
        this.badge = document.createElement('span');
        this.badge.className = 'gw-ac-rpc-badge gw-ac-rpc-ok';
        this.wrapper.appendChild(this.badge);
      }

      // Dropdown — attaché au <body> pour échapper aux styles de la page hôte.
      this.dropdown = document.createElement('div');
      this.dropdown.className = 'gw-ac-dropdown';
      this.dropdown.setAttribute('role', 'listbox');
      Object.assign(this.dropdown.style, {
        display:       'none',
        position:      'fixed',
        zIndex:        '2147483647',
        background:    '#fff',
        border:        '1px solid #b0b0b0',
        borderRadius:  '0 0 6px 6px',
        boxShadow:     '0 8px 24px rgba(0,0,0,0.18)',
        minWidth:      '500px',
        maxWidth:      '900px',
        overflow:      'hidden',
        fontFamily:    'inherit',
        fontSize:      '13px',
        boxSizing:     'border-box'
      });

      // Grille — 3 colonnes actives + col4 cachée pour la compatibilité layout
      this.grid = document.createElement('div');
      this.grid.className = 'gw-ac-grid';
      Object.assign(this.grid.style, {
        display:             'grid',
        gridTemplateColumns: '1fr 1fr 1fr',
        maxHeight:           '350px',
        overflow:            'hidden',
        boxSizing:           'border-box'
      });

      const labels    = this.opts.columnLabels;
      const types     = ['col1', 'col2', 'col3'];
      const colColors = { col1: '#2e7d32', col2: '#1565c0', col3: '#616161' };
      this.columns = [];

      types.forEach((type, i) => {
        const col = document.createElement('div');
        col.className = 'gw-ac-col';
        Object.assign(col.style, {
          display:       'flex',
          flexDirection: 'column',
          borderRight:   i < 2 ? '1px solid #e0e0e0' : 'none',
          maxHeight:     '350px',
          boxSizing:     'border-box',
          overflow:      'hidden'
        });

        const header = document.createElement('div');
        header.className = 'gw-ac-col-header gw-ac-' + type;
        Object.assign(header.style, {
          padding:        '6px 10px',
          fontWeight:     '600',
          fontSize:       '11px',
          textTransform:  'uppercase',
          letterSpacing:  '0.4px',
          position:       'sticky',
          top:            '0',
          zIndex:         '5',
          display:        'flex',
          justifyContent: 'space-between',
          alignItems:     'center',
          borderBottom:   '2px solid rgba(0,0,0,0.08)',
          flexShrink:     '0',
          background:     colColors[type],
          color:          '#fff',
          boxSizing:      'border-box'
        });
        const labelSpan = document.createElement('span');
        labelSpan.textContent = labels[type] || type;
        Object.assign(labelSpan.style, { margin: '0', padding: '0', listStyle: 'none' });
        const countSpan = document.createElement('span');
        countSpan.className = 'gw-ac-count';
        countSpan.textContent = '0';
        Object.assign(countSpan.style, {
          background:   'rgba(255,255,255,0.3)',
          padding:      '1px 6px',
          borderRadius: '8px',
          fontSize:     '10px',
          fontWeight:   'bold',
          margin:       '0',
          listStyle:    'none'
        });
        header.appendChild(labelSpan);
        header.appendChild(countSpan);

        const items = document.createElement('ul');
        items.className = 'gw-ac-items';
        Object.assign(items.style, {
          overflowY: 'auto',
          flex:      '1',
          margin:    '0',
          padding:   '0',
          listStyle: 'none',
          boxSizing: 'border-box'
        });

        col.appendChild(header);
        col.appendChild(items);
        this.grid.appendChild(col);

        this.columns.push({ el: col, header, items, type });
      });

      // col4 : colonne fantôme pour conserver la compatibilité éventuelle
      // (non affichée — gridTemplateColumns est à 3 colonnes)
      this.columns.push({ el: null, header: null, items: null, type: 'col4' });

      this.dropdown.appendChild(this.grid);

      // Footer
      this.footer = document.createElement('div');
      this.footer.className = 'gw-ac-footer';
      Object.assign(this.footer.style, {
        padding:         '4px 10px',
        background:      '#fafafa',
        borderTop:       '1px solid #e0e0e0',
        fontSize:        '11px',
        color:           '#888',
        display:         'flex',
        justifyContent:  'space-between',
        boxSizing:       'border-box',
        margin:          '0',
        listStyle:       'none'
      });
      const footerLeft = document.createElement('span');
      footerLeft.style.cssText = 'margin:0;padding:0;list-style:none';
      ['↑↓ naviguer ', '←→ colonnes ', 'Enter sélectionner ', 'Esc fermer'].forEach((label, idx) => {
        const keys = ['↑↓', '←→', 'Enter', 'Esc'];
        const kbd = document.createElement('kbd');
        kbd.textContent = keys[idx];
        Object.assign(kbd.style, {
          display: 'inline-block', padding: '0 4px', background: '#e0e0e0',
          borderRadius: '3px', fontSize: '10px', fontFamily: 'inherit',
          margin: '0 2px 0 0', listStyle: 'none'
        });
        footerLeft.appendChild(kbd);
        footerLeft.appendChild(document.createTextNode(' ' + label.trim() + ' '));
      });
      const footerInfo = document.createElement('span');
      footerInfo.className = 'gw-ac-footer-info';
      footerInfo.style.cssText = 'margin:0;padding:0;list-style:none';
      this.footer.appendChild(footerLeft);
      this.footer.appendChild(footerInfo);
      this.dropdown.appendChild(this.footer);

      document.body.appendChild(this.dropdown);
    }

    /**
     * Binding des événements
     */
    _bindEvents() {
      this.input.addEventListener('input', () => {
        const query = this.input.value.trim();
        clearTimeout(this.debounceTimer);

        if (query.length < this.opts.minChars) {
          this.close();
          return;
        }

        this.debounceTimer = setTimeout(() => {
          this._doSearch(query);
        }, this.opts.debounceMs);
      });

      this.input.addEventListener('focus', () => {
        if (this._hasResults() && this.input.value.length >= this.opts.minChars) {
          this.open();
        }
      });

      this.input.addEventListener('keydown', (e) => this._onKeyDown(e));

      document.addEventListener('click', (e) => {
        if (!this.wrapper.contains(e.target)) {
          this.close();
        }
      });

      this._scrollHandler = () => { if (this.isOpen) this._repositionDropdown(); };
      window.addEventListener('scroll', this._scrollHandler, true);
      window.addEventListener('resize', this._scrollHandler);

      // Clic sur un item (délégation)
      this.grid.addEventListener('click', (e) => {
        const item = e.target.closest('.gw-ac-item');
        if (item) {
          this._selectItem(item.getAttribute('data-value'));
        }
      });

      // Hover sur un item
      this.grid.addEventListener('mouseover', (e) => {
        const item = e.target.closest('.gw-ac-item');
        if (item) {
          this._clearSelection();
          item.classList.add('gw-ac-selected');
          this.selectedCol = parseInt(item.getAttribute('data-col'), 10);
          this.selectedRow = parseInt(item.getAttribute('data-row'), 10);
        }
      });
    }

    /**
     * Recherche via RPC
     */
    async _doSearch(query) {
      if (!this.rpc.isConnected()) {
        this.log('RPC non connecté, recherche annulée');
        return;
      }

      try {
        // Le serveur retourne une liste plate
        const flat = await this.rpc.lookup(
          this.indexName,
          query,
          this.opts.maxResults
        );
        // Découpe côté JS en 3 colonnes
        this.results = this._splitResults(flat, query);
        this._renderResults(query);
        if (this._hasResults()) {
          this.open();
        } else {
          this.close();
        }
      } catch (err) {
        this.log('Erreur lookup:', err.message);
      }
    }

    /**
     * Rendu des résultats dans les 3 colonnes
     */
    _renderResults(query) {
      const activeTypes = ['col1', 'col2', 'col3'];
      const normQuery = this._normalize(query);

      activeTypes.forEach((type, colIdx) => {
        const col = this.columns[colIdx];
        const items = this.results[type] || [];
        const count = col.header.querySelector('.gw-ac-count');
        count.textContent = items.length;

        col.items.innerHTML = '';

        if (items.length === 0) {
          const empty = document.createElement('li');
          empty.className = 'gw-ac-empty';
          empty.textContent = '—';
          col.items.appendChild(empty);
          return;
        }

        items.forEach((value, rowIdx) => {
          const li = document.createElement('li');
          li.className = 'gw-ac-item';
          li.setAttribute('data-value', value);
          li.setAttribute('data-col', colIdx);
          li.setAttribute('data-row', rowIdx);
          li.setAttribute('role', 'option');

          li.innerHTML = this._highlight(value, normQuery);

          col.items.appendChild(li);
        });
      });

      // Info footer
      const total = activeTypes.reduce((s, t) => s + (this.results[t] || []).length, 0);
      const info = this.footer.querySelector('.gw-ac-footer-info');
      info.textContent = total + ' résultat' + (total > 1 ? 's' : '');

      this.selectedCol = 0;
      this.selectedRow = -1;
    }

    /**
     * Normalise une chaîne : supprime les accents et met en minuscules.
     * "Éric" → "eric", "Ångström" → "angstrom"
     */
    _normalize(text) {
      return text
        .normalize('NFD')
        .replace(/[\u0300-\u036f]/g, '')
        .toLowerCase();
    }

    /**
     * Highlight de la sous-chaîne matchée,
     * insensible à la casse ET aux accents.
     */
    _highlight(text, normQuery) {
      if (!normQuery) return this._escapeHtml(text);
      const normText = this._normalize(text);
      const idx = normText.indexOf(normQuery);
      if (idx === -1) return this._escapeHtml(text);

      const before = this._escapeHtml(text.substring(0, idx));
      const match  = this._escapeHtml(text.substring(idx, idx + normQuery.length));
      const after  = this._escapeHtml(text.substring(idx + normQuery.length));
      return before + '<span class="gw-ac-match">' + match + '</span>' + after;
    }

    _escapeHtml(text) {
      const div = document.createElement('div');
      div.textContent = text;
      return div.innerHTML;
    }

    /**
     * Gestion du clavier
     */
    _onKeyDown(e) {
      if (!this.isOpen) {
        if ((e.key === 'ArrowDown' || e.key === 'Enter') &&
            this.input.value.length >= this.opts.minChars) {
          e.preventDefault();
          this._doSearch(this.input.value.trim());
        }
        return;
      }

      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          this._moveSelection(0, 1);
          break;

        case 'ArrowUp':
          e.preventDefault();
          this._moveSelection(0, -1);
          break;

        case 'ArrowRight':
          e.preventDefault();
          this._moveSelection(1, 0);
          break;

        case 'ArrowLeft':
          e.preventDefault();
          this._moveSelection(-1, 0);
          break;

        case 'Enter':
          e.preventDefault();
          if (this.selectedRow >= 0) {
            const item = this._getSelectedItem();
            if (item) {
              this._selectItem(item.getAttribute('data-value'));
            }
          }
          break;

        case 'Escape':
          e.preventDefault();
          this.close();
          break;

        case 'Tab':
          if (this.selectedRow >= 0) {
            const item = this._getSelectedItem();
            if (item) {
              this._selectItem(item.getAttribute('data-value'));
            }
          }
          this.close();
          break;
      }
    }

    /**
     * Déplacement de la sélection clavier (3 colonnes actives)
     */
    _moveSelection(dx, dy) {
      const activeTypes = ['col1', 'col2', 'col3'];
      const maxCols = activeTypes.length;
      let newCol = this.selectedCol + dx;
      if (newCol < 0) newCol = maxCols - 1;
      if (newCol >= maxCols) newCol = 0;

      // Chercher une colonne non-vide
      let attempts = 0;
      while ((this.results[activeTypes[newCol]] || []).length === 0 && attempts < maxCols) {
        newCol += (dx || 1);
        if (newCol < 0) newCol = maxCols - 1;
        if (newCol >= maxCols) newCol = 0;
        attempts++;
      }

      const colItems = this.results[activeTypes[newCol]] || [];
      if (colItems.length === 0) return;

      let newRow = this.selectedRow + dy;
      if (newRow < 0) newRow = colItems.length - 1;
      if (newRow >= colItems.length) newRow = 0;

      if (dx !== 0) {
        newRow = Math.min(this.selectedRow, colItems.length - 1);
        if (newRow < 0) newRow = 0;
      }

      this._clearSelection();
      this.selectedCol = newCol;
      this.selectedRow = newRow;

      const item = this._getSelectedItem();
      if (item) {
        item.classList.add('gw-ac-selected');
        item.scrollIntoView({ block: 'nearest' });
      }
    }

    _getSelectedItem() {
      return this.grid.querySelector(
        '.gw-ac-item[data-col="' + this.selectedCol +
        '"][data-row="' + this.selectedRow + '"]'
      );
    }

    _clearSelection() {
      this.grid.querySelectorAll('.gw-ac-item.gw-ac-selected')
        .forEach(el => el.classList.remove('gw-ac-selected'));
    }

    _selectItem(value) {
      this.input.value = value;
      this.close();

      this.input.dispatchEvent(new Event('input', { bubbles: true }));
      this.input.dispatchEvent(new Event('change', { bubbles: true }));

      if (typeof this.opts.onSelect === 'function') {
        this.opts.onSelect(value, this.indexName, this.input);
      }

      this.log('Sélectionné:', value);
    }

    open() {
      this._repositionDropdown();
      this.dropdown.style.display = 'block';
      this.dropdown.classList.add('gw-ac-visible');
      this.isOpen = true;
    }

    _repositionDropdown() {
      const rect = this.input.getBoundingClientRect();
      Object.assign(this.dropdown.style, {
        top:   (rect.bottom) + 'px',
        left:  (rect.left)   + 'px',
        width: Math.max(rect.width, 500) + 'px'
      });
    }

    close() {
      this.dropdown.style.display = 'none';
      this.dropdown.classList.remove('gw-ac-visible');
      this.isOpen = false;
      this._clearSelection();
      this.selectedRow = -1;
    }

    _hasResults() {
      return (this.results.col1.length +
              this.results.col2.length +
              this.results.col3.length) > 0;
    }

    restoreDatalist() {
      if (this.originalDatalist && this.datalistId) {
        this.input.setAttribute('list', this.datalistId);
        this.input.removeAttribute('data-gw-ac-list');
        this.input.setAttribute('autocomplete', '');
      }
      if (this._scrollHandler) {
        window.removeEventListener('scroll', this._scrollHandler, true);
        window.removeEventListener('resize', this._scrollHandler);
      }
      if (this.dropdown && this.dropdown.parentNode) {
        this.dropdown.parentNode.removeChild(this.dropdown);
      }
      if (this.badge && this.badge.parentNode) {
        this.badge.parentNode.removeChild(this.badge);
      }
      if (this.wrapper && this.wrapper.parentNode) {
        this.wrapper.parentNode.insertBefore(this.input, this.wrapper);
        this.wrapper.parentNode.removeChild(this.wrapper);
      }
    }
  }

  // ==========================================================
  // Classe principale GenewebAutocomplete
  // ==========================================================
  class GenewebAutocomplete {
    constructor() {
      this.rpc = null;
      this.widgets = [];
      this.opts = {};
      this._initialized = false;
      // Liste des index disponibles : [{ name, count }]
      this.serverIndexes = [];
    }

    /**
     * Initialisation principale.
     * Tente de se connecter au serveur RPC.
     * Si OK : découvre les index via "info", enrichit les inputs.
     * Si KO : ne touche à rien, les datalists natifs restent.
     */
    async init(userOpts) {
      if (this._initialized) {
        this.log('Déjà initialisé');
        return;
      }

      this.opts = Object.assign({}, DEFAULTS, userOpts || {});
      this.log('Initialisation avec', this.opts);

      this.rpc = new RpcWsClient(this.opts.rpcUrl, this.opts);

      let connected = false;
      let attempts = 0;
      while (!connected && attempts < this.opts.maxReconnect) {
        try {
          await this.rpc.connect();
          connected = true;
        } catch (e) {
          attempts++;
          this.log('Connexion échouée (tentative ' + attempts + '):', e.message);
          if (attempts < this.opts.maxReconnect) {
            await this._sleep(1000);
          }
        }
      }

      if (!connected) {
        console.info(
          '[GeneWeb Autocomplete] Serveur RPC non disponible à ' +
          this.opts.rpcUrl + '. Les datalists natifs sont conservés.'
        );
        return;
      }

      this.log('Connexion RPC établie');

      // Découvrir les index disponibles via la méthode "info"
      try {
        this.serverIndexes = await this.rpc.info();
        this.log('Index disponibles:', this.serverIndexes);
      } catch (e) {
        this.log('Impossible d\'obtenir les infos des index:', e.message);
        this.serverIndexes = [];
      }

      // Extraire uniquement les noms pour la résolution d'index
      const serverIndexNames = this.serverIndexes.map(idx => idx.name);

      const inputs = document.querySelectorAll(this.opts.inputSelector);
      this.log('Inputs trouvés:', inputs.length);

      inputs.forEach(input => {
        try {
          const widgetOpts = Object.assign({}, this.opts, {
            _serverIndexNames: serverIndexNames
          });
          const widget = new AutocompleteWidget(input, this.rpc, widgetOpts);
          this.widgets.push(widget);
        } catch (e) {
          this.log('Erreur création widget pour', input, e);
        }
      });

      this._initialized = true;
      this.log('Initialisation terminée:', this.widgets.length, 'widgets créés');
    }

    /**
     * Enrichir un input spécifique ajouté dynamiquement au DOM.
     */
    enhance(input, indexName) {
      if (!this.rpc || !this.rpc.isConnected()) {
        this.log('RPC non connecté, enhance ignoré');
        return null;
      }

      const opts = Object.assign({}, this.opts, {
        _serverIndexNames: this.serverIndexes.map(idx => idx.name)
      });
      if (indexName) {
        const datalistId = input.getAttribute('list');
        if (datalistId) {
          opts.indexMap[datalistId] = indexName;
        }
      }

      const widget = new AutocompleteWidget(input, this.rpc, opts);
      this.widgets.push(widget);
      return widget;
    }

    /**
     * Détruire tous les widgets et restaurer les datalists.
     */
    destroy() {
      this.widgets.forEach(w => w.restoreDatalist());
      this.widgets = [];
      if (this.rpc) {
        this.rpc.disconnect();
        this.rpc = null;
      }
      this._initialized = false;
      this.serverIndexes = [];
    }

    isConnected() {
      return this.rpc && this.rpc.isConnected();
    }

    log(...args) {
      if (this.opts.debug) console.log('[GW-AC]', ...args);
    }

    _sleep(ms) {
      return new Promise(r => setTimeout(r, ms));
    }
  }

  // ==========================================================
  // Export global
  // ==========================================================
  root.GenewebAutocomplete = new GenewebAutocomplete();

  // Auto-init si un attribut data est présent sur le body
  // <body data-gw-ac-rpc="ws://localhost:8080/search">
  document.addEventListener('DOMContentLoaded', () => {
    const body = document.body;
    const autoRpcUrl = body.getAttribute('data-gw-ac-rpc');
    if (autoRpcUrl) {
      root.GenewebAutocomplete.init({ rpcUrl: autoRpcUrl });
    }
  });

})(typeof window !== 'undefined' ? window : this);
