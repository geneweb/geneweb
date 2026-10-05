/**
 * autocomplete.js — GeneWeb RPC autocomplete
 * ===========================================
 * Progressive enhancement of GeneWeb's <input list="datalist_xxx"> fields.
 * When the RPC search server (rpc_server / service.ml) is reachable, each
 * field gets a three-column dropdown fed over a WebSocket; otherwise the
 * page is left untouched and the native datalists keep working.
 *
 * Server protocol (JSON-RPC 2.0, positional params):
 *   info   ()                      → [[index_name, count], ...]
 *   lookup (index, query, size)    → [string, ...]   (flat, may contain duplicates)
 * Index names are the dictionary file basenames, e.g. "roglo_fnames".
 *
 * Results are split client-side into three columns:
 *   prefix   : the entry starts with the query
 *   internal : the entry contains the query elsewhere
 *   fuzzy    : everything else (approximate matches)
 * Matching ignores case and diacritics.
 *
 * ---------------------------------------------------------------------------
 * Usage from a GeneWeb template (self-configuring, no inline JavaScript):
 *
 *   <script src=".../autocomplete.js"
 *     data-rpc="/search"            path behind the proxy, host[:port]/path,
 *                                   or a full ws:// / wss:// URL
 *     data-base="roglo"             index names become <base>_<type>
 *     data-debug="1"                optional: console tracing
 *     data-labels="A|B|C"></script> optional: column titles
 *
 * Fallback hook for when the server is unreachable or goes away:
 *
 *   GenewebAutocomplete.onUnavailable(() => populateDatalists());
 *
 * Programmatic use (e.g. the standalone demo page):
 *
 *   await GenewebAutocomplete.init({ rpcUrl, indexMap: { list_fn: 'HenriT_fnames' } });
 *
 * All static styling lives in autocomplete.css.
 */
;(function (root) {
  'use strict';

  // ==========================================================================
  // Configuration
  // ==========================================================================

  const DEFAULTS = {
    rpcUrl: 'ws://127.0.0.1:8080/search',
    // Index names are <base>_<suffix>, suffix = datalist id without "datalist_"
    base: '',
    // Explicit datalist id → index name; takes precedence over `base`
    indexMap: {},
    inputSelector: 'input[list]',
    minChars: 2,
    debounceMs: 250,
    maxResults: 90,
    connectTimeout: 1500,
    requestTimeout: 5000,
    maxReconnect: 1,
    // "off" is ignored by browser address autofill; a valid non-address
    // token keeps the browser's own popup away from the dropdown
    autocompleteToken: 'one-time-code',
    showRpcBadge: false,
    labels: { prefix: 'Préfixe exact', internal: 'Préfixe interne', fuzzy: 'Approx.' },
    hints: { navigate: 'naviguer', columns: 'colonnes', select: 'sélectionner', close: 'fermer' },
    resultLabel: n => n + ' résultat' + (n > 1 ? 's' : ''),
    onSelect: null,
    debug: false
  };

  // Column definitions: order matters, the first test that passes wins.
  // Tests receive folded (lower-case, diacritic-free) strings.
  const COLUMNS = [
    { key: 'prefix',   test: (entry, q) => entry.startsWith(q) },
    { key: 'internal', test: (entry, q) => entry.includes(q) },
    { key: 'fuzzy',    test: () => true }
  ];

  const DROPDOWN_ID = 'gw-ac-dropdown';
  const LIST_MAX = 350;   // preferred list height (px)
  const LIST_MIN = 120;   // never shrink lists below this
  const EDGE = 8;         // gap kept from the window edges
  const MIN_WIDTH = 500;

  // ==========================================================================
  // Utilities
  // ==========================================================================

  const COMBINING = /[\u0300-\u036f]/g;

  /** Lower-case and strip diacritics: "Éric" → "eric". */
  function fold(s) {
    return s.normalize('NFD').replace(COMBINING, '').toLowerCase();
  }

  /**
   * Fold a string and keep, for each code unit of the result, the index of
   * the original character it came from. Needed because folding can change
   * the length ("İ" → "i̇", decomposed input, …), so positions found in the
   * folded string cannot be used directly on the original.
   */
  function foldWithMap(text) {
    let folded = '';
    const map = [];
    for (let i = 0; i < text.length; ) {
      const ch = String.fromCodePoint(text.codePointAt(i));
      const f = fold(ch);
      for (let k = 0; k < f.length; k++) map.push(i);
      folded += f;
      i += ch.length;
    }
    map.push(text.length);
    return { folded, map };
  }

  /** Deduplicate a flat result list and split it into COLUMNS. */
  function classify(values, query) {
    const q = fold(query);
    const cols = COLUMNS.map(() => []);
    const seen = new Set();
    for (const v of values) {
      if (typeof v !== 'string' || seen.has(v)) continue;
      seen.add(v);
      const f = fold(v);
      cols[COLUMNS.findIndex(c => c.test(f, q))].push(v);
    }
    return cols;
  }

  /** Text with the matched part wrapped in <span class="gw-ac-match">. */
  function highlight(text, query) {
    const frag = document.createDocumentFragment();
    const q = fold(query);
    const { folded, map } = foldWithMap(text);
    const at = q ? folded.indexOf(q) : -1;
    if (at < 0) {
      frag.append(text);
      return frag;
    }
    const start = map[at];
    const end = map[at + q.length];
    const mark = el('span', 'gw-ac-match', text.slice(start, end));
    frag.append(text.slice(0, start), mark, text.slice(end));
    return frag;
  }

  /**
   * Build the WebSocket URL from a template setting:
   *   "ws://…" / "wss://…"  → used as is
   *   "/search"             → same host as the page (behind a proxy)
   *   "host:port/search"    → that host
   * The scheme follows the page: wss on https (required), ws on http.
   */
  function buildUrl(cfg) {
    if (!cfg) return DEFAULTS.rpcUrl;
    if (/^wss?:\/\//i.test(cfg)) return cfg;
    const scheme = location.protocol === 'https:' ? 'wss://' : 'ws://';
    return scheme + (cfg.charAt(0) === '/' ? location.host + cfg : cfg);
  }

  function el(tag, className, text) {
    const e = document.createElement(tag);
    if (className) e.className = className;
    if (text != null) e.textContent = text;
    return e;
  }

  function sleep(ms) {
    return new Promise(r => setTimeout(r, ms));
  }

  function makeLog(enabled, tag) {
    return enabled ? (...args) => console.log(tag, ...args) : () => {};
  }

  // ==========================================================================
  // RPC client (JSON-RPC 2.0 over WebSocket)
  // ==========================================================================

  class RpcClient {
    constructor(url, opts, log) {
      this.url = url;
      this.opts = opts;
      this.log = log;
      this.ws = null;
      this.nextId = 0;
      this.pending = new Map();
      this.onclose = null;
      this._connecting = null;
    }

    get connected() {
      return !!this.ws && this.ws.readyState === WebSocket.OPEN;
    }

    /** Open the connection; concurrent callers share one attempt. */
    connect() {
      if (this.connected) return Promise.resolve();
      if (this._connecting) return this._connecting;

      this._connecting = new Promise((resolve, reject) => {
        let ws;
        try {
          ws = new WebSocket(this.url);
        } catch (e) {
          reject(e);
          return;
        }
        const timer = setTimeout(() => {
          ws.close();
          reject(new Error('connection timeout'));
        }, this.opts.connectTimeout);

        ws.onopen = () => {
          clearTimeout(timer);
          this.ws = ws;
          this.log('connected to', this.url);
          resolve();
        };
        ws.onerror = () => {
          clearTimeout(timer);
          reject(new Error('connection error'));
        };
        ws.onclose = () => {
          clearTimeout(timer);
          if (this.ws === ws) this._closed();
          else reject(new Error('connection closed'));
        };
        ws.onmessage = ev => this._receive(ev.data);
      }).finally(() => { this._connecting = null; });

      return this._connecting;
    }

    disconnect() {
      const ws = this.ws;
      this.ws = null;
      this.onclose = null;
      this._rejectPending('disconnected');
      if (ws) ws.close();
    }

    _closed() {
      this.ws = null;
      this._rejectPending('connection closed');
      this.log('connection closed');
      if (this.onclose) this.onclose();
    }

    _rejectPending(reason) {
      for (const p of this.pending.values()) {
        clearTimeout(p.timer);
        p.reject(new Error(reason));
      }
      this.pending.clear();
    }

    _receive(data) {
      let msg;
      try {
        msg = JSON.parse(data);
      } catch (e) {
        this.log('invalid JSON from server', e);
        return;
      }
      const p = this.pending.get(msg.id);
      if (!p) return;
      this.pending.delete(msg.id);
      clearTimeout(p.timer);
      if (msg.error) p.reject(new Error(msg.error.message || 'RPC error'));
      else p.resolve(msg.result);
    }

    /** Positional params: the OCaml side consumes an ordered JSON list. */
    call(method, params) {
      if (!this.connected) return Promise.reject(new Error('not connected'));
      const id = ++this.nextId;
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          this.pending.delete(id);
          reject(new Error(method + ': timeout'));
        }, this.opts.requestTimeout);
        this.pending.set(id, { resolve, reject, timer });
        this.ws.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }));
      });
    }

    async lookup(index, query, size) {
      const r = await this.call('lookup', [index, query, size]);
      return Array.isArray(r) ? r : [];
    }

    async info() {
      const r = await this.call('info', []);
      if (!Array.isArray(r)) return [];
      return r.map(p => Array.isArray(p)
        ? { name: p[0], count: p[1] }
        : { name: p.name || '', count: p.count || 0 });
    }
  }

  // ==========================================================================
  // Dropdown — a single instance shared by all fields
  // ==========================================================================

  class Dropdown {
    constructor(opts) {
      this.field = null;
      this.cols = [];
      this.sel = { col: 0, row: -1 };

      this.el = el('div', 'gw-ac-dropdown');
      this.el.id = DROPDOWN_ID;
      this.el.hidden = true;

      const grid = el('div', 'gw-ac-grid');
      COLUMNS.forEach(c => {
        const label = opts.labels[c.key] || c.key;
        const col = el('div', 'gw-ac-col gw-ac-col-' + c.key);
        const header = el('div', 'gw-ac-col-header');
        const count = el('span', 'gw-ac-count', '0');
        header.append(el('span', 'gw-ac-col-label', label), count);
        const list = el('ul', 'gw-ac-items');
        list.setAttribute('role', 'listbox');
        list.setAttribute('aria-label', label);
        col.append(header, list);
        grid.append(col);
        this.cols.push({ count, list, values: [] });
      });

      const hints = el('span', 'gw-ac-hints');
      [['↑↓', opts.hints.navigate], ['←→', opts.hints.columns],
       ['Enter', opts.hints.select], ['Esc', opts.hints.close]].forEach(([key, text]) => {
        const hint = el('span', 'gw-ac-hint');
        hint.append(el('kbd', null, key), ' ' + text);
        hints.append(hint);
      });
      this.info = el('span', 'gw-ac-info');
      const footer = el('div', 'gw-ac-footer');
      footer.append(hints, this.info);

      this.el.append(grid, footer);
      document.body.append(this.el);
      this.resultLabel = opts.resultLabel;

      // Keep focus in the input when the dropdown is clicked, so the
      // input's blur handler can close the dropdown on any other click.
      this._onMouseDown = e => e.preventDefault();
      this._onClick = e => {
        const item = e.target.closest('.gw-ac-item');
        if (item && this.field) this.field.select(item.dataset.value);
      };
      this._onMouseOver = e => {
        const item = e.target.closest('.gw-ac-item');
        if (item) this._mark(+item.dataset.col, +item.dataset.row);
      };
      this._onViewport = e => {
        if (e && e.target instanceof Node && this.el.contains(e.target)) return;
        if (this.isOpen) this.position();
      };
      this.el.addEventListener('mousedown', this._onMouseDown);
      this.el.addEventListener('click', this._onClick);
      this.el.addEventListener('mouseover', this._onMouseOver);
      window.addEventListener('scroll', this._onViewport, true);
      window.addEventListener('resize', this._onViewport);
    }

    get isOpen() {
      return !this.el.hidden;
    }

    isFor(field) {
      return this.isOpen && this.field === field;
    }

    hasSelection() {
      return this.sel.row >= 0;
    }

    selectedValue() {
      const c = this.cols[this.sel.col];
      return this.sel.row >= 0 && c ? c.values[this.sel.row] : undefined;
    }

    show(field, columns, query) {
      if (this.field && this.field !== field) this._setExpanded(false);
      this.field = field;
      this._render(columns, query);
      this.el.hidden = false;
      this._setExpanded(true);
      this.position();
    }

    hide() {
      if (this.el.hidden) return;
      this.el.hidden = true;
      this._setExpanded(false);
      this.sel = { col: 0, row: -1 };
    }

    /** Move the keyboard selection; dx changes column, dy changes row. */
    move(dx, dy) {
      const lens = this.cols.map(c => c.values.length);
      let { col, row } = this.sel;
      if (row < 0) {
        if (!lens[col]) col = lens.findIndex(n => n > 0);
        if (col < 0) return;
        row = dy < 0 ? lens[col] - 1 : 0;
      } else if (dx) {
        let c = col;
        do { c += dx; } while (c >= 0 && c < lens.length && !lens[c]);
        if (c < 0 || c >= lens.length) return;
        col = c;
        row = Math.min(row, lens[c] - 1);
      } else {
        row = Math.max(0, Math.min(lens[col] - 1, row + dy));
      }
      this._mark(col, row);
    }

    /**
     * Place the dropdown under the input, or above it when there is more
     * room there, clamp it horizontally to the window, and cap the list
     * heights to the available space (lists scroll internally).
     */
    position() {
      const input = this.field && this.field.input;
      if (!input) return;
      const r = input.getBoundingClientRect();
      const vw = document.documentElement.clientWidth;
      const vh = document.documentElement.clientHeight;

      if (r.bottom < 0 || r.top > vh) {   // input scrolled out of view
        this.hide();
        return;
      }

      const s = this.el.style;
      const width = Math.min(Math.max(r.width, MIN_WIDTH), vw - 2 * EDGE);
      s.width = width + 'px';
      s.left = Math.max(EDGE, Math.min(r.left, vw - width - EDGE)) + 'px';

      // Measure at the preferred size, then fit to the space available
      this.cols.forEach(c => { c.list.style.maxHeight = LIST_MAX + 'px'; });
      const listH = Math.max(...this.cols.map(c => c.list.offsetHeight));
      const chrome = this.el.offsetHeight - listH;   // headers, footer, borders

      const below = vh - r.bottom - EDGE;
      const above = r.top - EDGE;
      const up = below < listH + chrome && above > below;
      const cap = Math.max(LIST_MIN, Math.min(LIST_MAX, (up ? above : below) - chrome));
      this.cols.forEach(c => { c.list.style.maxHeight = cap + 'px'; });

      this.el.classList.toggle('gw-ac-above', up);
      if (up) {
        s.top = 'auto';
        s.bottom = (vh - r.top) + 'px';
      } else {
        s.bottom = 'auto';
        s.top = r.bottom + 'px';
      }
    }

    destroy() {
      window.removeEventListener('scroll', this._onViewport, true);
      window.removeEventListener('resize', this._onViewport);
      this.el.remove();
      this.field = null;
    }

    _render(columns, query) {
      let total = 0;
      columns.forEach((values, ci) => {
        const c = this.cols[ci];
        c.values = values;
        c.count.textContent = values.length;
        total += values.length;
        if (!values.length) {
          c.list.replaceChildren(el('li', 'gw-ac-empty', '—'));
          return;
        }
        const frag = document.createDocumentFragment();
        values.forEach((v, ri) => {
          const li = el('li', 'gw-ac-item');
          li.id = 'gw-ac-opt-' + ci + '-' + ri;
          li.setAttribute('role', 'option');
          li.dataset.value = v;
          li.dataset.col = ci;
          li.dataset.row = ri;
          li.append(highlight(v, query));
          frag.append(li);
        });
        c.list.replaceChildren(frag);
      });
      this.info.textContent = this.resultLabel(total);
      this.sel = { col: 0, row: -1 };
    }

    _mark(col, row) {
      const old = this.el.querySelector('.gw-ac-selected');
      if (old) old.classList.remove('gw-ac-selected');
      this.sel = { col, row };
      const li = this.cols[col] && this.cols[col].list.children[row];
      if (!li || !li.classList.contains('gw-ac-item')) return;
      li.classList.add('gw-ac-selected');
      if (li.scrollIntoView) li.scrollIntoView({ block: 'nearest' });
      if (this.field) this.field.input.setAttribute('aria-activedescendant', li.id);
    }

    _setExpanded(open) {
      if (!this.field) return;
      const input = this.field.input;
      input.setAttribute('aria-expanded', open ? 'true' : 'false');
      if (!open) input.removeAttribute('aria-activedescendant');
    }
  }

  // ==========================================================================
  // Field — one per enhanced input
  // ==========================================================================

  class Field {
    constructor(input, indexName, mgr) {
      this.input = input;
      this.indexName = indexName;
      this.mgr = mgr;
      this.datalistId = input.getAttribute('list');
      this.cols = COLUMNS.map(() => []);
      this.query = '';
      this.seq = 0;
      this.timer = null;
      this.selecting = false;
      this.wrapper = null;
      this._saved = {
        autocomplete: input.getAttribute('autocomplete'),
        role: input.getAttribute('role')
      };

      // Disable the native datalist (restored by destroy)
      input.removeAttribute('list');
      if (this.datalistId) input.dataset.gwAcList = this.datalistId;
      input.setAttribute('autocomplete', mgr.opts.autocompleteToken);
      input.setAttribute('role', 'combobox');
      input.setAttribute('aria-autocomplete', 'list');
      input.setAttribute('aria-expanded', 'false');
      input.setAttribute('aria-controls', DROPDOWN_ID);

      // The wrapper only exists to position the optional status badge
      if (mgr.opts.showRpcBadge) {
        this.wrapper = el('div', 'gw-ac-wrapper');
        input.parentNode.insertBefore(this.wrapper, input);
        this.wrapper.append(input, el('span', 'gw-ac-rpc-badge gw-ac-rpc-ok'));
      }

      this._handlers = {
        input:   () => this._onInput(),
        focus:   () => this._onFocus(),
        blur:    () => { if (mgr.dropdown && mgr.dropdown.isFor(this)) mgr.dropdown.hide(); },
        keydown: e => this._onKeyDown(e)
      };
      for (const [ev, fn] of Object.entries(this._handlers)) input.addEventListener(ev, fn);
    }

    hasResults() {
      return this.cols.some(c => c.length > 0);
    }

    async search(query) {
      const seq = ++this.seq;
      let values;
      try {
        values = await this.mgr.lookup(this.indexName, query);
      } catch (err) {
        if (seq === this.seq && this.mgr.state !== 'unavailable') {
          console.warn('[GW-AC] search failed for', this.indexName, '-', err.message);
        }
        return;
      }
      // Ignore replies to an older query, or for text that has changed since
      if (seq !== this.seq || this.input.value.trim() !== query) return;

      this.cols = classify(values, query);
      this.query = query;
      const dd = this.mgr.dropdown;
      if (!dd) return;
      if (this.hasResults() && document.activeElement === this.input) dd.show(this, this.cols, query);
      else if (dd.isFor(this)) dd.hide();
    }

    select(value) {
      clearTimeout(this.timer);
      this.seq++;                                   // drop any reply still in flight
      this.input.value = value;
      this.cols = COLUMNS.map(() => []);
      if (this.mgr.dropdown) this.mgr.dropdown.hide();

      // Notify other scripts; our own input handler ignores these events
      this.selecting = true;
      try {
        this.input.dispatchEvent(new Event('input', { bubbles: true }));
        this.input.dispatchEvent(new Event('change', { bubbles: true }));
      } finally {
        this.selecting = false;
      }
      const cb = this.mgr.opts.onSelect;
      if (typeof cb === 'function') cb(value, this.indexName, this.input);
    }

    destroy() {
      clearTimeout(this.timer);
      this.seq++;
      for (const [ev, fn] of Object.entries(this._handlers)) this.input.removeEventListener(ev, fn);
      const input = this.input;
      if (this.datalistId) {
        input.setAttribute('list', this.datalistId);
        delete input.dataset.gwAcList;
      }
      restoreAttr(input, 'autocomplete', this._saved.autocomplete);
      restoreAttr(input, 'role', this._saved.role);
      ['aria-autocomplete', 'aria-expanded', 'aria-controls', 'aria-activedescendant']
        .forEach(a => input.removeAttribute(a));
      if (this.wrapper) {
        this.wrapper.parentNode.insertBefore(input, this.wrapper);
        this.wrapper.remove();
      }
    }

    _onInput() {
      if (this.selecting) return;
      const q = this.input.value.trim();
      clearTimeout(this.timer);
      if (q.length < this.mgr.opts.minChars) {
        this.seq++;
        this.cols = COLUMNS.map(() => []);
        const dd = this.mgr.dropdown;
        if (dd && dd.isFor(this)) dd.hide();
        return;
      }
      this.timer = setTimeout(() => this.search(q), this.mgr.opts.debounceMs);
    }

    _onFocus() {
      const dd = this.mgr.dropdown;
      if (dd && this.hasResults() && this.input.value.trim() === this.query) {
        dd.show(this, this.cols, this.query);
      }
    }

    _onKeyDown(e) {
      const dd = this.mgr.dropdown;
      if (!dd) return;

      if (!dd.isFor(this)) {
        const q = this.input.value.trim();
        if (e.key === 'ArrowDown' && q.length >= this.mgr.opts.minChars) {
          e.preventDefault();
          if (this.hasResults() && q === this.query) dd.show(this, this.cols, q);
          else this.search(q);
        }
        return;
      }

      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          dd.move(0, 1);
          break;
        case 'ArrowUp':
          e.preventDefault();
          dd.move(0, -1);
          break;
        case 'ArrowRight':
        case 'ArrowLeft':
          // Without a selection, arrows keep moving the caret
          if (dd.hasSelection()) {
            e.preventDefault();
            dd.move(e.key === 'ArrowRight' ? 1 : -1, 0);
          }
          break;
        case 'Enter': {
          const v = dd.selectedValue();
          if (v !== undefined) {
            e.preventDefault();          // also suppresses keypress → no submit
            this.select(v);
          } else {
            dd.hide();                   // let the form submit normally
          }
          break;
        }
        case 'Tab': {
          const v = dd.selectedValue();
          if (v !== undefined) this.select(v);   // focus still moves on
          else dd.hide();
          break;
        }
        case 'Escape':
          e.preventDefault();
          dd.hide();
          break;
      }
    }
  }

  function restoreAttr(node, name, value) {
    if (value == null) node.removeAttribute(name);
    else node.setAttribute(name, value);
  }

  // ==========================================================================
  // Public API
  // ==========================================================================

  class GenewebAutocomplete {
    constructor() {
      this.opts = Object.assign({}, DEFAULTS);
      this.state = 'idle';     // idle | connecting | ready | disconnected | unavailable
      this.rpc = null;
      this.dropdown = null;
      this.widgets = [];
      this.serverIndexes = [];
      this.log = () => {};
      this._initPromise = null;
      this._unavailableCallbacks = [];
      this._warned = new Set();
    }

    /**
     * Connect, discover the server's indexes and enhance matching inputs.
     * Resolves to true on success, false if the server is unreachable
     * (the page is then left untouched). Safe to call more than once.
     */
    init(userOpts) {
      if (!this._initPromise) this._initPromise = this._init(userOpts || {});
      return this._initPromise;
    }

    /** Enhance one input, e.g. added to the page after init. */
    enhance(input, indexName) {
      if (this.state !== 'ready' && this.state !== 'disconnected') return null;
      if (this.widgets.some(w => w.input === input)) return null;
      const id = input.getAttribute('list');
      const wanted = indexName || this._indexFor(id);
      const index = wanted && this._resolveIndex(wanted);
      if (!index) {
        this.log('no index for list="' + id + '", field left native');
        return null;
      }
      this._checkIndex(index, id);
      const field = new Field(input, index, this);
      this.widgets.push(field);
      return field;
    }

    /** Run cb if/when autocompletion becomes unavailable (at most once). */
    onUnavailable(cb) {
      if (this.state === 'unavailable') cb();
      else this._unavailableCallbacks.push(cb);
    }

    /** Remove all widgets, restore the native datalists, disconnect. */
    destroy() {
      this._teardown();
      this.state = 'idle';
      this.serverIndexes = [];
      this._initPromise = null;
      this._warned.clear();
    }

    isConnected() {
      return !!(this.rpc && this.rpc.connected);
    }

    /** Search one index, reconnecting once if the connection was lost. */
    async lookup(index, query) {
      if (!this.isConnected()) {
        if (this.state === 'unavailable' || !this.rpc) throw new Error('unavailable');
        if (!(await this._connect(1))) {
          this._fail();
          throw new Error('unavailable');
        }
        this.state = 'ready';
      }
      return this.rpc.lookup(index, query, this.opts.maxResults);
    }

    async _init(userOpts) {
      const o = this.opts = Object.assign({}, DEFAULTS, userOpts, {
        labels: Object.assign({}, DEFAULTS.labels, userOpts.labels),
        hints: Object.assign({}, DEFAULTS.hints, userOpts.hints),
        indexMap: Object.assign({}, userOpts.indexMap)
      });
      this.log = makeLog(o.debug, '[GW-AC]');
      this.log('init', o);

      this.state = 'connecting';
      this.rpc = new RpcClient(o.rpcUrl, o, this.log);
      this.rpc.onclose = () => {
        if (this.state === 'ready') {
          this.state = 'disconnected';
          this.log('connection lost, will reconnect on next search');
        }
      };

      if (!(await this._connect(o.maxReconnect))) {
        this._fail();
        return false;
      }

      try {
        this.serverIndexes = await this.rpc.info();
      } catch (e) {
        this.log('info failed:', e.message);
        this.serverIndexes = [];
      }
      this.log('server indexes:', this.serverIndexes);

      this.state = 'ready';
      this.dropdown = new Dropdown(o);
      document.querySelectorAll(o.inputSelector).forEach(input => this.enhance(input));
      this.log(this.widgets.length + ' fields enhanced');
      return true;
    }

    async _connect(attempts) {
      for (let i = 1; i <= attempts; i++) {
        try {
          await this.rpc.connect();
          return true;
        } catch (e) {
          this.log('connection attempt ' + i + ' failed:', e.message);
          if (i < attempts) await sleep(1000);
        }
      }
      return false;
    }

    /** Map an index name to the server's: exact, or with a file extension
        ("HenriT_fnames" → "HenriT_fnames.cache"). */
    _resolveIndex(index) {
      const names = this.serverIndexes.map(i => i.name);
      if (!names.length || names.includes(index)) return index;
      return names.find(n => n.startsWith(index + '.')) || index;
    }

    _indexFor(datalistId) {
      if (!datalistId) return null;
      if (this.opts.indexMap[datalistId]) return this.opts.indexMap[datalistId];
      if (this.opts.base) return this.opts.base + '_' + datalistId.replace(/^datalist_/, '');
      return null;
    }

    /** Warn (once per index) about a configuration mismatch. */
    _checkIndex(index, datalistId) {
      if (!this.serverIndexes.length || this._warned.has(index)) return;
      if (this.serverIndexes.some(i => i.name === index)) return;
      this._warned.add(index);
      console.warn('[GW-AC] index "' + index + '" (list="' + datalistId + '") is not loaded on the RPC server; ' +
                   'server has: ' + this.serverIndexes.map(i => i.name).join(', '));
    }

    _fail() {
      if (this.state === 'unavailable') return;
      console.info('[GeneWeb Autocomplete] RPC server unavailable at ' + this.opts.rpcUrl +
                   '; native datalists are used.');
      this._teardown();
      this.state = 'unavailable';
      const callbacks = this._unavailableCallbacks;
      this._unavailableCallbacks = [];
      callbacks.forEach(cb => {
        try { cb(); } catch (e) { console.error(e); }
      });
      document.dispatchEvent(new CustomEvent('gw-ac:unavailable'));
    }

    _teardown() {
      this.widgets.forEach(w => w.destroy());
      this.widgets = [];
      if (this.dropdown) {
        this.dropdown.destroy();
        this.dropdown = null;
      }
      if (this.rpc) {
        this.rpc.disconnect();
        this.rpc = null;
      }
    }
  }

  // ==========================================================================
  // Export and self-configuration from the <script> tag
  // ==========================================================================

  const script = document.currentScript;     // only valid during this first run
  const api = new GenewebAutocomplete();
  api.util = { fold, classify, buildUrl };   // handy from the console
  root.GenewebAutocomplete = api;

  if (script && script.dataset.rpc !== undefined) {
    const d = script.dataset;
    const labels = d.labels ? d.labels.split('|') : [];
    const start = () => api.init({
      rpcUrl: buildUrl(d.rpc),
      base: d.base || '',
      inputSelector: d.selector || 'input[list^="datalist_"]',
      debug: !!d.debug,
      labels: labels.length === 3
        ? { prefix: labels[0], internal: labels[1], fuzzy: labels[2] }
        : undefined
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
  }

})(typeof window !== 'undefined' ? window : this);
