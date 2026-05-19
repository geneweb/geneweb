/* cousins.js — renders the cousins matrix, totals and per-cell modals
   from the inline JSON injected by cousmenu.txt. Exposes
   window.Cousins.init(), called by the central DOMContentLoaded
   dispatcher in js.txt under e.m="C". */
window.Cousins = (function () {
  'use strict';

  function init() {
    const root    = document.getElementById('cousins-root');
    const dataEl  = document.getElementById('cousins-data');
    const lexEl   = document.getElementById('cousins-lex');
    if (!root || !dataEl) return;

    let raw, lex;
    try { raw = JSON.parse(dataEl.textContent); }
    catch (e) { console.error('cousins: bad JSON', e); return; }
    try { lex = lexEl ? JSON.parse(lexEl.textContent) : {}; }
    catch (e) { console.warn('cousins: bad lexicon JSON', e); lex = {}; }

    const cs = getComputedStyle(root);
    const get = (v) => cs.getPropertyValue(v).trim();
    const C = {
      aged:       get('--c-aged'),         /* fond cellule, alpha .9 */
      agedStrong: get('--c-aged-strong'),  /* gradient âge,  alpha 1  */
      agedSoft:   get('--c-aged-soft'),    /* gradient âge sans naissance */
      live:       get('--c-live'),
      dead:       get('--c-dead'),
      fade:       get('--c-fade'),
    };

    /* ---- 1. State (merge-friendly maps) ---- */
    const PREFIX  = root.dataset.prefix || '';
    const SELF_IP = raw.self_iper;
    const IS_ABK     = !!raw.abk;
    const IS_WIZ     = !!raw.wizard;
    const LVL     = raw.lvl | 0;

    const cells   = new Map();
    const persons = new Map();
    for (const c of raw.cells) cells.set(c.i + ':' + c.j, c);
    for (const [ip, p] of Object.entries(raw.persons)) persons.set(ip, p);

    /* ---- per-level loading, stable per-level URLs ---- */
    const loaded  = new Set([0]);
    const pending = new Map();

    const levelUrl = (n) =>
      PREFIX + 'i=' + enc(SELF_IP) + '&m=C&json_level=' + n;

    function mergeLevel(resp) {
      if (!resp || !Array.isArray(resp.cells)) return false;
      if (resp.persons)
        for (const [ip, p] of Object.entries(resp.persons))
          persons.set(ip, p);
      for (const c of resp.cells) cells.set(c.i + ':' + c.j, c);
      return resp.cells.length > 0;
    }

    let forceReload = false;
    try {
      if (sessionStorage.getItem('cousinsReload')) {
        forceReload = true;
        sessionStorage.removeItem('cousinsReload');
      }
    } catch (e) { /* ignore */ }

    function fetchLevel(n) {
      if (loaded.has(n)) return Promise.resolve(true);
      if (pending.has(n)) return pending.get(n);
      const pr = fetch(levelUrl(n),
        { headers: { Accept: 'application/json' },
          cache: forceReload ? 'reload' : 'default' })
        .then((r) => {
          if (!r.ok) throw new Error('HTTP ' + r.status);
          return r.json();
        })
        .then((resp) => {
          const nonEmpty = mergeLevel(resp);
          loaded.add(n);
          pending.delete(n);
          return nonEmpty;
        })
        .catch((e) => {
          pending.delete(n);
          return false;
        });
      pending.set(n, pr);
      return pr;
    }

    async function ensureLevels(upTo) {
      for (let n = 1; n <= upTo; n++) {
        const nonEmpty = await fetchLevel(n);
        hydrateMatrix();
        applyDisplayLvl(displayLvl);
        renderTotals();
        if (!nonEmpty) break;
      }
    }

    /* ---- in-page generation navigation ---- */
    let displayLvl = LVL;

    function setActiveButton(n) {
      const bar = document.getElementById('lvl-toolbar');
      if (!bar) return;
      bar.querySelectorAll('button[data-lvl]').forEach((b) => {
        b.classList.toggle('lvl-active', (b.dataset.lvl | 0) === n);
      });
    }

    function getMatrixMax() {
      const t = document.getElementById('quickrel');
      return t ? (t.dataset.matrixMax | 0) : 0;
    }

    function lvlTrueMax() {
      let m = getMatrixMax();
      document.querySelectorAll('#lvl-toolbar button[data-lvl]')
        .forEach((b) => { const v = b.dataset.lvl | 0; if (v > m) m = v; });
      return m;
    }

    async function navigateToLvl(n) {
      if (n === displayLvl) return;
      if (n > displayLvl) {
        if (n > getMatrixMax()) await expandMatrixTo(n);
        await withOverlay(ensureLevels(n));
      }
      displayLvl = n;
      applyDisplayLvl(n);
      applyContainerWidth(n);
      setActiveButton(n);
      syncUrl(n);
    }

    function syncUrl(n) {
      const u = new URL(window.location);
      u.searchParams.set('v', n);
      history.replaceState(null, '', u);
    }

    (function initLvlNav() {
      const bar = document.getElementById('lvl-toolbar');
      if (!bar) return;
      bar.addEventListener('click', (e) => {
        const btn = e.target.closest('button[data-lvl]');
        if (!btn) return;
        navigateToLvl(btn.dataset.lvl | 0);
      });
    })();

    /* ---- 2. Helpers ---- */
    const L = (k) => lex[k] || '';
    const enc = encodeURIComponent;

    function pUrl(p, ip) {
      if (IS_ABK && p) {
        const oc = p.oc ? '&oc=' + p.oc : '';
        return PREFIX + 'p=' + enc(p.fn_key) + '&n=' + enc(p.sn_key) + oc;
      }
      return PREFIX + 'i=' + ip;
    }
    function pUrlNav(p, ip) { return pUrl(p, ip) + '&m=C&v=' + LVL; }
    function rUrl(ip)     {
      return PREFIX + 'em=R&et=A&ei=' + SELF_IP + '&i=' + ip;
    }
    function rlmUrl(ip, path, lvlA, lvlD) {
      const anc = path.a1 || path.a2 || SELF_IP;
      const i1 = lvlA === 0 ? SELF_IP : anc;
      const dag = (path.nbr | 0) > 1 ? '&dag=on' : '';
      return PREFIX + 'm=RL&i=' + i1
           + '&l1=' + lvlA + '&i1=' + SELF_IP
           + '&l2=' + lvlD + '&i2=' + ip + dag;
    }

    function sexClass(s) {
      return s === 0 ? 'primary' : s === 1 ? 'danger' : 'dark';
    }

    /* Replace a missing translation marker [cousins.i.k] by a compact i/k.
       The server inserts "[cousins.i.k]" when the lexicon entry is absent;
       we render the bare degree pair instead, which is more legible at high
       v than verbose labels and avoids unwrapped long words breaking the
       column layout. */
    const MISSING_LABEL = /^\[cousins\.(\d+)\.(\d+)\]+$/;
    function legibleLabel(s) {
      const m = MISSING_LABEL.exec(s);
      return m ? m[1] + '/' + m[2] : s;
    }

    function thousandsSep(n) {
      return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ' ');
    }

    function decorateBorders(btn, i, k, selfHasChildren) {
      const td = btn.closest('td');
      if (!td) return;
      const next = cells.get(i + ':' + (k + 1));
      const hasNext = !!(next && next.cnt && next.cnt.paths > 0);
      td.querySelectorAll('.border-next').forEach(function (el) {
        const z = parseInt(el.dataset.z, 10);
        let on;
        if (i !== 0 && k === 0) {
          on = hasNext ? (z % 2 === 0) : (z === 0);
        } else {
          on = z === 0 && hasNext;
        }
        if (on) el.classList.add('border-dark', 'border-end');
      });
    }

    function nameOf(ip) {
      const p = persons.get(ip);
      return p ? p.fn + ' ' + p.sn : '';
    }
    function gIdx(s) { return (s === 0 || s === 1) ? s : 2; }
    function relTip(j, a1, a2, idx) {
      const t = lex.rel && lex.rel[j];
      const noun = t ? (t[idx] || t[2] || '') : '';
      let ancs = nameOf(a1);
      if (a2) ancs += ' ' + (L('and') || '&') + ' ' + nameOf(a2);
      return (raw.ttfmt || '\u0001N \u0001A')
        .split('\u0001N').join(noun)
        .split('\u0001A').join(ancs);
    }

    /* ---- 3. Matrix hydration ---- */
    function kinshipCoeff(i, j) {
      return (i === 0 || j === 0)
        ? (100 / Math.pow(2, i + j)).toFixed(2)
        : (100 / Math.pow(2, i + j - 1)).toFixed(2);
    }

    const tipTpl = document.getElementById('cell-tip-tpl');

    function buildTip(labelHtml, i, k, ttHtml) {
      const node = tipTpl.content.firstElementChild.cloneNode(true);
      node.querySelector('.tip-label').innerHTML = labelHtml;
      if (i + k === 0) {
        node.querySelector('.tip-gen-block').remove();
      } else {
        node.querySelector('.tip-i').textContent   = i;
        node.querySelector('.tip-k').textContent   = k;
        node.querySelector('.tip-rel').textContent = kinshipCoeff(i, k);
      }
      if (ttHtml) node.querySelector('.tip-tt').innerHTML = ttHtml;
      else        node.querySelector('.tip-tt-block').remove();
      return node.innerHTML;
    }

    function hydrateMatrix() {
      const selfP = persons.get(SELF_IP);
      const selfHasChildren = !!(selfP && selfP.has_child);

      const buttons = root.querySelectorAll('#quickrel button[data-cell]');
      buttons.forEach(function (btn) {
        const key = btn.dataset.cell;
        const [i, k] = key.split(':').map(Number);
        const cell = cells.get(key);
        const wrap = btn.parentElement;
        if (!cell || !cell.cnt || cell.cnt.paths === 0) {
          if (wrap) wrap.classList.add('invisible');
          return;
        }
        if (wrap) wrap.classList.remove('invisible');
        const cnt = cell.cnt;
        const isSelf = i + k === 0;

        const labelHtml = isSelf
          ? L('him_her')
          : (cnt.dist === 1 ? legibleLabel(cell.label_s)
                            : legibleLabel(cell.label_p));
        const tip = buildTip(labelHtml, i, k, cell.tt);
        btn.removeAttribute('title');
        btn.setAttribute('data-bs-original-title', tip);

        const lblEl = btn.querySelector('.cell-label');
        if (lblEl) {
          lblEl.innerHTML = isSelf ? L('him_her') : legibleLabel(cell.label_p);
        }
        const cntEl = btn.querySelector('.cell-count');
        if (cntEl) cntEl.textContent = cnt.dist;

        const pctDead = cnt.dist > 0
          ? Math.round(100 - (cnt.alive * 100) / cnt.dist) : 0;
        const inner = btn.querySelector('.stretched-link');
        if (pctDead === 0) {
          btn.classList.add('border-0');
        } else if (pctDead === 100) {
          btn.style.backgroundColor = C.aged;
        } else if (inner) {
          const a = pctDead > 1 ? pctDead - 1 : 1;
          const b = pctDead < 99 ? pctDead + 1 : 100;
          inner.style.background =
            'linear-gradient(270deg,'
            + C.aged + ' ' + a + '%,'
            + C.fade + ' ' + b + '%)';
        }

        decorateBorders(btn, i, k, selfHasChildren);
      });
    }

    /* Hide entire <tr> rows whose every matrix <td> is beyond the ceiling.
       Without this pass, descending leaves the generation-number cell
       (which has no data-lvl-i) standing alone on an otherwise empty row. */
    function applyDisplayLvl(displayLvl) {
      const grid = root.querySelector('#quickrel');
      if (!grid) return;
      grid.querySelectorAll('td[data-lvl-i]').forEach((td) => {
        const i = td.dataset.lvlI | 0;
        td.classList.toggle('beyond-lvl', i > displayLvl);
      });
      grid.querySelectorAll('tr').forEach((tr) => {
        const cells = tr.querySelectorAll('td[data-lvl-i]');
        if (cells.length === 0) return;
        let allBeyond = true;
        cells.forEach((td) => {
          if (!td.classList.contains('beyond-lvl')) allBeyond = false;
        });
        tr.classList.toggle('beyond-lvl', allBeyond);
      });
    }

    function applyContainerWidth(n) {
      const c = document.getElementById('cousins-container');
      if (!c) return;
      if (n > 6) {
        c.classList.remove('container');
        c.classList.add('container-fluid');
      } else {
        c.classList.remove('container-fluid');
        c.classList.add('container');
      }
    }

    /* ---- 4. Modal (single reusable shell) ---- */
    const modalEl = document.getElementById('cousins-modal');
    const listEl  = document.getElementById('cousins-modal-list');
    const colsEl  = document.getElementById('cousins-modal-cols');

    modalEl.addEventListener('show.bs.modal', function (ev) {
      const trig = ev.relatedTarget;
      const host = trig && trig.closest('[data-cell]');
      if (!host) return;
      const cell = cells.get(host.dataset.cell);
      if (cell) populateModal(cell);
    });

    function cellExists(ti, tj) {
      if (ti < 0 || tj < 0) return false;
      const c = cells.get(ti + ':' + tj);
      return !!(c && c.cnt && c.cnt.paths > 0);
    }

    function cloneNav(tplId, container, ti, tk, val, enabled) {
      const tpl = document.getElementById(tplId);
      if (!tpl) return;
      const node = tpl.content.firstElementChild.cloneNode(true);
      const btn = node.matches('button') ? node : node.querySelector('button');
      btn.dataset.cell = ti + ':' + tk;
      if (!enabled) btn.classList.add('disabled');
      const tip = node.querySelector('[data-bs-toggle="tooltip"]');
      if (tip) tip.setAttribute('data-bs-original-title', ti + '/' + tk);
      const valSpan = node.querySelector('.nav-val');
      if (valSpan && val != null) valSpan.textContent = val;
      container.appendChild(node);
    }

    function disposeNavTooltips() {
      if (!window.bootstrap || !bootstrap.Tooltip) return;
      modalEl
        .querySelectorAll('.mh-nav-up [data-bs-toggle="tooltip"], '
                        + '.mh-nav-down [data-bs-toggle="tooltip"]')
        .forEach(function (el) {
          const t = bootstrap.Tooltip.getInstance(el);
          if (t) t.dispose();
        });
    }

    function populateModal(cell) {
      currentCell = cell;
      const { i, j, cnt } = cell;
      const isSelf = i + j === 0;
      const labelH = isSelf
        ? L('him_her')
        : (cnt.dist === 1 ? cell.label_s : cell.label_p);

      const headerEl = modalEl.querySelector('.mh-header');
      if (headerEl) {
        const dead = (cnt.dist | 0) - (cnt.alive | 0);
        headerEl.classList.toggle('bg-light', dead > 0);
        headerEl.classList.toggle('bg-linen', dead === 0);
      }

      /* LEFT counts */
      modalEl.querySelector('.mh-dist').textContent = cnt.dist;
      modalEl.querySelector('.mh-noun').textContent =
        cnt.dist === 1 ? L('person_s') : L('person_p');

      const aliveRow = modalEl.querySelector('.mh-alive-row');
      if (cnt.alive !== 0 && cnt.alive !== cnt.dist) {
        aliveRow.hidden = false;
        const dead = cnt.dist - cnt.alive;
        const swatch = ' <span class="mh-alive-swatch"></span>';
        aliveRow.querySelector('.mh-alive').innerHTML =
          2 * cnt.alive <= cnt.dist
            ? cnt.alive + ' ' + L('alive')
            : dead + ' ' + L('died') + swatch;
      } else aliveRow.hidden = true;

      const noDescRow = modalEl.querySelector('.mh-nodesc-row');
      if (cnt.no_desc > 0) {
        noDescRow.hidden = false;
        noDescRow.querySelector('.mh-nodesc').textContent = cnt.no_desc;
      } else noDescRow.hidden = true;

      /* CENTER title + extra + span */
      const title = modalEl.querySelector('.mh-title');
      title.innerHTML = labelH;
      title.href = buildRlmUrl(cell);
      title.setAttribute('data-bs-original-title',
                         'RLM c(' + i + ',' + j + ')');

      const extra = cnt.paths - cnt.dist;
      modalEl.querySelector('.mh-extra').textContent =
        extra > 0 ? ' (+ ' + extra + ')' : '';

      const sp = cell.span;
      modalEl.querySelector('.mh-span').textContent =
        (sp && sp.min_yr != null && sp.max_yr != null)
          ? (sp.min_yr === sp.max_yr
              ? String(sp.min_yr)
              : sp.min_yr + '-' + sp.max_yr)
          : '';

      /* RIGHT nav */
      const navUp = modalEl.querySelector('.mh-nav-up');
      const navDn = modalEl.querySelector('.mh-nav-down');
      disposeNavTooltips();
      navUp.replaceChildren();
      navDn.replaceChildren();

      /* Position 1 — diagonal descend-i (only on ancestor column) */
      if (j === 0 && i > 0)
        cloneNav('cousins-nav-diag-tpl', navUp,
                 i - 1, 0, i, cellExists(i - 1, 0));

      /* Position 2 top — ascend (always rendered) */
      if (j === 0)
        cloneNav('cousins-nav-up-anc-tpl', navDn,
                 i + 1, 0, i, cellExists(i + 1, 0));
      else
        cloneNav('cousins-nav-up-cous-tpl', navDn,
                 i, j - 1, i, cellExists(i, j - 1));

      /* Position 2 bottom — descend-j (always rendered) */
      cloneNav('cousins-nav-down-tpl', navDn,
               i, j + 1, j, cellExists(i, j + 1));

      modalEl.querySelector('.mh-rl').href =
        PREFIX + 'i=' + SELF_IP + '&m=C&v1=' + i + '&v2=' + j;

      const rlmBtn = modalEl.querySelector('.mh-rl');
      const rlmTipEl =
        (rlmBtn && rlmBtn.querySelector('[data-bs-toggle="tooltip"]')) || rlmBtn;
      if (rlmTipEl) {
        const n = i + j;
        const txt =
          n + ' '
          + (n < 2 ? L('degree_kinship_s') : L('degree_kinship_p'))
          + ' (' + L('show_all') + ' ' + L('rel_links_p') + ')';

        const tip = bootstrap.Tooltip.getOrCreateInstance(rlmTipEl);

        rlmTipEl.setAttribute('data-bs-original-title', txt);

        if (tip.setContent) {
          tip.setContent({ '.tooltip-inner': txt });
        }
      }

      /* Cards */
      const mode   = sortTypes[sortIdx].type;
      const groups = buildGroups(cell.entries, mode);
      const np = groups.length;
      colsEl.style.columnCount = np > 31 ? 3 : 2;
      listEl.innerHTML = '';
      const tpl    = document.getElementById('cousins-card-tpl');
      const segTpl = document.getElementById('cousins-seg-tpl');
      const memTpl = document.getElementById('cousins-mem-tpl');
      const cmpG   = cmpPaths(mode);
      groups.forEach(function (g) {
        g.rep = g.ips.reduce(function (m, ip) {
          return cmpG({ ip: ip }, { ip: m }) < 0 ? ip : m;
        }, g.ips[0]);
      });
      groups.sort((a, b) => cmpG({ ip: a.rep }, { ip: b.rep }));
      const frag = document.createDocumentFragment();
      for (const group of groups) {
        const node = renderCard(tpl, segTpl, memTpl, group, i, j, cell.span);
        if (node) frag.appendChild(node);
      }
      listEl.replaceChildren(frag);
      applySortLabel();
      ensureTooltips(modalEl);
    }

    const sortBtn = modalEl.querySelector('.sortbtn');
    /* Self-paired (icon/label describe own type). applySortLabel reads
       the NEXT entry; with sortIdx=0 the button advertises 'surname',
       which matches the initial HTML rendering of the button. */
    const sortTypes = [
      { type: 'relation',
        icon: 'fa-people-roof',
        label: sortBtn.dataset.lblRelation },
      { type: 'birth',
        icon: 'fa-cake-candles',
        label: sortBtn.dataset.lblBirth },
      { type: 'surname',
        icon: 'fa-user',
        label: sortBtn.dataset.lblSurname },
      { type: 'age',
        icon: 'fa-hourglass-half',
        label: sortBtn.dataset.lblAge },
    ];
    let sortIdx = 0;
    let currentCell = null;

    function applySortLabel() {
      const cur  = sortTypes[sortIdx];
      const next = sortTypes[(sortIdx + 1) % sortTypes.length];
      const ic = sortBtn.querySelector('.sorticon');
      const tp = sortBtn.querySelector('.sorttype');
      if (ic) ic.className = 'fa-solid ' + next.icon + ' fa-fw me-1 sorticon';
      if (tp) tp.textContent = next.label;
      const cic = modalEl.querySelector('.sortcuricon');
      const cla = modalEl.querySelector('.sortcurlabel');
      if (cic) cic.className = 'fa-solid ' + cur.icon + ' fa-fw me-1 sortcuricon';
      if (cla) cla.textContent = cur.label;
    }

    modalEl.addEventListener('click', function (ev) {
      if (ev.target.closest('.sortbtn')) {
        sortIdx = (sortIdx + 1) % sortTypes.length;
        if (currentCell) populateModal(currentCell);
        return;
      }
      const nav = ev.target.closest('.cous-hed-nav-btn');
      if (nav && nav.dataset.cell
          && !nav.classList.contains('disabled')) {
        const c = cells.get(nav.dataset.cell);
        if (c) populateModal(c);
      }
    });

    listEl.addEventListener('click', function (ev) {
      if (ev.target.closest('a, button')) return;
      const li = ev.target.closest('li[data-nav]');
      if (li) window.location.href = li.dataset.nav;
    });

    function rlmUrlOf(ips, label) {
      let s = PREFIX + 'm=RLM&i1=' + SELF_IP;
      ips.forEach((ip, k) => { s += '&i' + (k + 2) + '=' + ip; });
      if (label) s += '&t1=' + encodeURIComponent(label);
      return s;
    }

    function buildRlmUrl(cell) {
      const label = (cell.i + cell.j !== 0)
        ? (cell.label_p || '').replace(/<[^>]*>/g, '')
        : null;
      return rlmUrlOf(cell.entries.map((e) => e.ip), label);
    }

    /* Sort primitives over JSON person objects. fn_key/sn_key are
       Already Name.lower-normalized server-side; birth/death are
       { y, m, d, p } or null. */
    const collator = new Intl.Collator(
      document.documentElement.lang || undefined,
      { sensitivity: 'base', numeric: true });

    function cmpStr(a, b) { return collator.compare(a || '', b || ''); }
    function cmpNum(a, b) {
      if (a === b) return 0;
      if (a == null) return 1;
      if (b == null) return -1;
      return a - b;
    }
    function cmpDate(a, b) {
      a = a || {}; b = b || {};
      return cmpNum(a.y, b.y) || cmpNum(a.m, b.m)
          || cmpNum(a.d, b.d) || cmpStr(a.p, b.p);
    }

    const PERSON_CMP = {
      birth: function (a, b) {
        return cmpDate(a.birth, b.birth)
            || cmpStr(a.sn_key, b.sn_key)
            || cmpStr(a.fn_key, b.fn_key)
            || cmpNum(a.oc | 0, b.oc | 0);
      },
      surname: function (a, b) {
        return cmpStr(a.sn_key, b.sn_key)
            || cmpStr(a.fn_key, b.fn_key)
            || cmpDate(a.birth, b.birth)
            || cmpNum(a.oc | 0, b.oc | 0);
      },
      age: function (a, b) {
        const aa = a.age_d, bb = b.age_d;
        if (aa == null || bb == null) {
          if (aa != null) return -1;
          if (bb != null) return 1;
          return cmpNum(a.birth && a.birth.y, b.birth && b.birth.y)
              || cmpStr(a.sn_key, b.sn_key)
              || cmpStr(a.fn_key, b.fn_key)
              || cmpNum(a.oc | 0, b.oc | 0);
        }
        return bb - aa
            || cmpNum(a.birth && a.birth.y, b.birth && b.birth.y)
            || cmpNum(a.death && a.death.y, b.death && b.death.y)
            || cmpStr(a.sn_key, b.sn_key)
            || cmpStr(a.fn_key, b.fn_key)
            || cmpNum(a.oc | 0, b.oc | 0);
      },
      relation: function (a, b) {
        return cmpStr(a.sn_key, b.sn_key)
            || cmpDate(a.birth, b.birth)
            || cmpStr(a.fn_key, b.fn_key)
            || cmpNum(a.oc | 0, b.oc | 0);
      },
    };

    function cmpBy(mode, getIp) {
      const f = PERSON_CMP[mode];
      return (a, b) => {
        const pa = persons.get(getIp(a)), pb = persons.get(getIp(b));
        return !pa || !pb ? 0 : f(pa, pb);
      };
    }
    const cmpPaths = (m) => cmpBy(m, x => x.ip);

    /* ---- 5. Card ---- */
    /* Two distinct multiplicities — do not conflate:
         - group.mult: total paths reaching this person across all
           ancestor pairs of the cell. Drives the nbr_N filter class
           and the data-multi badge (per-person aggregate).
         - path.nbr: chain multiplicity of ONE (ancestor-pair) path.
           Drives the per-line RLM superscript only. */
    function renderCard(tpl, segTpl, memTpl, group, lvlA, lvlD, span) {
      const mode  = sortTypes[sortIdx].type;
      const isRel = mode === 'relation';
      const ips   = group.ips;
      const rep   = persons.get(ips[0]);
      if (!rep) return null;
      const nbr = group.mult || 1;

      const node = tpl.content.firstElementChild.cloneNode(true);
      node.classList.add('nbr_' + nbr);
      if (nbr > 1) node.dataset.multi = String(nbr);

      const cmp = cmpPaths(mode);
      let sortedIps;
      if (isRel) {
        const sibMin = new Map();
        for (const ip of ips) {
          const p = persons.get(ip);
          if (!p) continue;
          const s = group.sib.get(ip);
          const y = (p.birth && p.birth.y) ? p.birth.y : 99999;
          if (!sibMin.has(s) || y < sibMin.get(s)) sibMin.set(s, y);
        }
        sortedIps = ips.slice().sort(function (a, b) {
          const pa = persons.get(a), pb = persons.get(b);
          const sa = group.sib.get(a), sb = group.sib.get(b);
          return cmpStr(pa.sn_key, pb.sn_key)
              || cmpNum(sibMin.get(sa), sibMin.get(sb))
              || cmpStr(sa, sb)
              || cmpDate(pa.birth, pb.birth)
              || cmpStr(pa.fn_key, pb.fn_key)
              || cmpNum(pa.oc | 0, pb.oc | 0);
        });
      } else {
        sortedIps = ips.slice().sort((a, b) => cmp({ ip: a }, { ip: b }));
      }
      node.dataset.ip = sortedIps[0];

      const segsBox = node.querySelector('.card-segs');
      const segs = [];
      let prevSib = null, seg = null;
      for (const ip of sortedIps) {
        const sib = group.sib.get(ip);
        if (sib !== prevSib) { seg = { ips: [] }; segs.push(seg); }
        prevSib = sib;
        seg.ips.push(ip);
      }

      const ico = (q) => q
        ? '<i class="fa-solid fa-user fa-sm text-'
          + sexClass(q.sex) + '"></i>'
        : '';

      segs.forEach(function (s, si) {
        if (si > 0) {
          const hr = document.createElement('hr');
          hr.className = 'p-0 m-0';
          segsBox.appendChild(hr);
        }
        const segNode = segTpl.content.firstElementChild.cloneNode(true);
        const ul = segNode.querySelector('.seg-mem');

        for (const ip of s.ips) {
          const p = persons.get(ip);
          if (!p) continue;
          const dead = !p.alive;
          const row = memTpl.content.firstElementChild.cloneNode(true);
          row.classList.add('border-' + sexClass(p.sex));
          if (!p.has_child) row.classList.add('border-double');
          const bg = ageGradient(p, span, dead);
          if (bg) row.style.background = bg;
          row.dataset.nav = pUrlNav(p, ip);
          row.title = L('navigation');
          const nm = row.querySelector('.card-name');
          nm.href = pUrl(p, ip);
          nm.textContent = p.sn + ' ' + p.fn;
          nm.title = ageTitle(p);
          const ed = row.querySelector('.card-edit');
          if (IS_WIZ) {
            const a = document.createElement('a');
            a.href = PREFIX + 'm=MOD_IND&i=' + ip;
            a.title = L('modify');
            a.textContent = p.dates || (dead ? '†' : '°');
            ed.appendChild(a);
          } else {
            ed.textContent = p.dates || (dead ? '†' : '°');
          }
          const rA = row.querySelector('.card-r');
          rA.href  = rUrl(ip);
          rA.title = L('rel_links_p');
          ul.appendChild(row);
        }

        if (lvlD !== 0) {
          const single = s.ips.length === 1;
          const rlm = segNode.querySelector('.seg-rlm');
          rlm.hidden = false;
          if (isRel) {
            const ancs = new Set();
            group.paths.forEach((p) => {
              if (p.a1) ancs.add(p.a1);
              if (p.a2) ancs.add(p.a2);
            });
            rlm.href = rlmUrlOf([...ancs, ...s.ips]);
          } else {
            rlm.href = rlmUrl(s.ips[0], group.paths[0], lvlA, lvlD);
          }
          const idx = single
            ? gIdx((persons.get(s.ips[0]) || {}).sex)
            : 2;
          rlm.title = group.paths
            .map((pp) => relTip(lvlD, pp.a1, pp.a2, idx))
            .join(' ; ');
          rlm.innerHTML = group.paths.map(function (path) {
            const a1P = path.a1 ? persons.get(path.a1) : null;
            const a2P = path.a2 ? persons.get(path.a2) : null;
            let primP = a1P, secP = a2P;
            if (a1P && a2P && a2P.sex === 0 && a1P.sex !== 0) {
              primP = a2P; secP = a1P;
            }
            const c = path.nbr | 0;
            return '<span class="d-block">'
              + (c > 1 ? '<small class="me-1">' + c + '</small>' : '')
              + ico(primP) + ico(secP) + '</span>';
          }).join('');
        }
        segsBox.appendChild(segNode);
      });
      return node;
    }

    function entrySig(e) {
      return e.paths
        .map((p) => p.a1 + '|' + p.a2 + '|' + p.nbr + '|' + p.lvl)
        .sort()
        .join(';');
    }

    function buildGroups(entries, mode) {
      if (mode !== 'relation')
        return entries.map((e) => ({
          ips: [e.ip], mult: e.mult, paths: e.paths,
          sib: new Map([[e.ip, e.sib]]),
        }));
      const h = new Map();
      const order = [];
      for (const e of entries) {
        const key = entrySig(e);
        let g = h.get(key);
        if (!g) {
          g = { ips: [], mult: e.mult, paths: e.paths,
                sib: new Map() };
          h.set(key, g);
          order.push(key);
        }
        g.ips.push(e.ip);
        g.sib.set(e.ip, e.sib);
      }
      return order.map((k) => h.get(k));
    }

    function ageTitle(p) {
      if (p.age_d == null) return '';
      const d = p.age_d;
      if (d > 364) {
        const yrs = Math.floor(d / 365);
        return yrs + ' ' + L('years_old')
             + ' (' + thousandsSep(d) + ' ' + L('days_old') + ')';
      }
      if (d > 1)   return d + ' ' + L('days_old');
      if (d === 1) return L('one_day');
      return '0 ' + L('ymd0');
    }

    /* Precision-aware bicolor age gradient over the cell's date span.
       Port of the legacy cousmenu template, driven by the JSON
       precision marker (p.birth.p / p.death.p):
         ""            exact   -> hard edge at the year
         "<" (birth)   before  -> aged starts at 0%, fades into life
                                  over [0, birth%]  ("born no later")
         ">" (death)   after   -> life fades into aged over
                                  [death%, 100%]    ("died no earlier")
         "~" "?" "|" ".." or no year -> imprecise: the side is the
           "soft" colour fading into life, never a misleading edge
       life = C.live (alive) / C.dead (dead); aged = C.agedStrong;
       soft (uncertain) = C.agedSoft. */
    function ageGradient(p, span, dead) {
      if (!span || span.min_yr == null || span.max_yr == null
          || span.max_yr <= span.min_yr) return '';
      const range = span.max_yr - span.min_yr;
      const life = dead ? C.dead : C.live;
      const aged = C.agedStrong;
      const soft = C.agedSoft;
      const pos = (y) => {
        const v = 100 - Math.floor(100 * (span.max_yr - y) / range);
        return v < 0 ? 0 : v > 100 ? 100 : v;
      };
      const bp = (p.birth && p.birth.p) || '';
      const dp = (p.death && p.death.p) || '';
      const by = p.birth && p.birth.y;
      const dy = p.death && p.death.y;
      /* a year is a usable marker only when exact or one-sided */
      let bA = (by != null && (bp === '' || bp === '<')) ? pos(by) : null;
      let dA = (dy != null && (dp === '' || dp === '>')) ? pos(dy) : null;
      if (bA != null && dA != null) {
        if (dA < bA) dA = bA;                 /* guard reversed dates */
        if (dA === bA && bA > 0)              /* <1y: keep a sliver */
          dA = bA + 1 > 100 ? 100 : bA + 1;
      }
      const stop = (c, n) => c + ' ' + n + '%';
      const g = [];
      if (!dead) {
        if (bA != null) {
          g.push(stop(aged, bp === '<' ? 0 : bA), stop(life, bA));
        } else if (by != null || dy != null) {
          g.push(stop(soft, 0), stop(life, 100));
        } else return '';
        return 'linear-gradient(90deg,' + g.join(',') + ')';
      }
      if (bA != null) {
        g.push(stop(aged, bp === '<' ? 0 : bA), stop(life, bA));
      } else if (dA != null) {
        g.push(stop(soft, 0));
      } else return '';
      if (dA != null) {
        g.push(stop(life, dA), stop(aged, dp === '>' ? 100 : dA));
      } else {
        g.push(stop(soft, 100));
      }
      return 'linear-gradient(90deg,' + g.join(',') + ')';
    }

    /* ---- 6. Totals (derived from merged state, dedup by iper) ---- */
    function renderTotals() {
      const tot = document.getElementById('cousins-totals');
      if (!tot) return;
      const anc = new Set(), desc = new Set();
      const dist = new Set(), alive = new Set();
      for (const [key, cell] of cells.entries()) {
        const [i, j] = key.split(':').map(Number);
        for (const entry of cell.entries) {
          const ip = entry.ip;
          dist.add(ip);
          if (i >= 1 && j === 0) anc.add(ip);
          else if (i === 0 && j >= 1) desc.add(ip);
          const p = persons.get(ip);
          if (p && p.alive) alive.add(ip);
        }
      }
      tot.querySelector('.tot-anc').textContent   = anc.size + 'A';
      tot.querySelector('.tot-desc').textContent  = '/' + desc.size + 'D';
      tot.querySelector('.tot-alive').textContent = alive.size + 'L';
      tot.querySelector('.tot-dist').textContent  = '/' + dist.size + 'P';
    }

    /* ---- overlay around fetches that may take server time ---- */
    let overlayTimer = null;
    let overlayDepth = 0;

    function showOverlayDelayed(delayMs) {
      if (overlayTimer) return;
      overlayTimer = setTimeout(() => {
        overlayTimer = null;
        if (overlayDepth > 0 && typeof window.showOverlay === 'function')
          window.showOverlay();
      }, delayMs);
    }

    function hideOverlayIfDone() {
      if (overlayDepth > 0) return;
      if (overlayTimer) { clearTimeout(overlayTimer); overlayTimer = null; }
      if (typeof window.hideOverlay === 'function') window.hideOverlay();
    }

    async function withOverlay(promise, delayMs) {
      overlayDepth++;
      showOverlayDelayed(delayMs == null ? 200 : delayMs);
      try { return await promise; }
      finally { overlayDepth--; hideOverlayIfDone(); }
    }

    /* ---- matrix expansion by tier (matrix_v), data-driven guard ---- */
    async function expandMatrixTo(target) {
      const want = Math.min(target, lvlTrueMax());
      if (want <= getMatrixMax()) return;       /* already covered */
      const u = new URL(window.location.href);
      u.searchParams.set('matrix_v', String(want));
      try {
        await withOverlay((async () => {
          const r = await fetch(u.toString(),
            { headers: { Accept: 'text/html' } });
          if (!r.ok) throw new Error('HTTP ' + r.status);
          const html = await r.text();
          const doc = new DOMParser().parseFromString(html, 'text/html');
          const newTable = doc.getElementById('quickrel');
          const oldTable = document.getElementById('quickrel');
          if (!newTable || !oldTable) throw new Error('table not found');
          oldTable.replaceWith(newTable);
          hydrateMatrix();
          applyDisplayLvl(displayLvl);
          ensureTooltips(document);
          newTable.classList.remove('pre-hydrate');
        })(), 0);
      } catch (e) {
        console.error('expandMatrixTo failed', e);
      }
    }

    function initToggleLvl() {
      document.querySelectorAll('.toggle-lev .btn').forEach(function (btn) {
        btn.addEventListener('click', function () {
          document.querySelectorAll('.high-lev, .toggle-lev .btn')
            .forEach(function (el) { el.classList.toggle('d-none'); });
        });
      });
      const tl = document.querySelector('.toggle-lev');
      if (!tl) return;
      const caution = tl.dataset.caution || '';
      document.querySelectorAll('.high-lev .btn').forEach(function (btn) {
        btn.title = btn.title + caution;
      });
    }

    const resetBtn = document.querySelector('.cacheresetbtn');
    if (resetBtn)
      resetBtn.addEventListener('click', async function () {
        resetBtn.disabled = true;
        try {
          await withOverlay(fetch(
            PREFIX + 'm=C&i=' + SELF_IP
            + '&v=' + (raw.lvl || 0) + '&reset=on',
            { cache: 'no-store' }), 0);
        } catch (e) { /* ignore */ }
        try { sessionStorage.setItem('cousinsReload', '1'); } catch (e) {}
        location.reload();
      });

    /* ---- 7. Tooltips page-wide + toggle-lvl ---- */
    const TOOLTIP_ALLOWLIST = (() => {
      if (!window.bootstrap || !bootstrap.Tooltip) return null;
      const al = Object.assign({}, bootstrap.Tooltip.Default.allowList);
      al.div = ['class', 'style'];
      al.hr  = ['class'];
      al.i   = ['class', 'style'];
      return al;
    })();

    function ensureTooltips(scope) {
      if (!TOOLTIP_ALLOWLIST) return;
      scope.querySelectorAll('[data-bs-toggle="tooltip"]').forEach(el => {
        if (bootstrap.Tooltip.getInstance(el)) return;
        new bootstrap.Tooltip(el, {
          html: true, allowList: TOOLTIP_ALLOWLIST,
          trigger: 'hover focus',
          delay: { show: 80, hide: 30 }, container: 'body'
        });
      });
    }

    /* ---- 8. Entry ---- */
    hydrateMatrix();
    applyDisplayLvl(LVL);
    renderTotals();
    ensureTooltips(document);
    initToggleLvl();
    setActiveButton(LVL);
    document.getElementById('quickrel')?.classList.remove('pre-hydrate');
    if (LVL > 0) withOverlay(ensureLevels(LVL), 0);
  }

  return { init };
})();