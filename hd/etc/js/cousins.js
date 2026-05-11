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

    /* ---- 1. State (merge-friendly maps for Phase D upserts) ---- */
    const PREFIX  = root.dataset.prefix || '';
    const SELF_IP = raw.self_iper;
    const IS_ABK     = !!raw.abk;
    const IS_WIZ     = !!raw.wizard;
    const LVL     = raw.lvl | 0;

    const cells   = new Map();
    const persons = new Map();
    for (const c of raw.cells) cells.set(c.i + ':' + c.j, c);
    for (const [ip, p] of Object.entries(raw.persons)) persons.set(ip, p);

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
    function rlmUrl(path, lvlA, lvlD) {
      const anc = path.a1 || path.a2 || SELF_IP;
      const i1 = lvlA === 0 ? SELF_IP : anc;
      const dag = (path.nbr | 0) > 1 ? '&dag=on' : '';
      return PREFIX + 'm=RL&i=' + i1
           + '&l1=' + lvlA + '&i1=' + SELF_IP
           + '&l2=' + lvlD + '&i2=' + path.ip + dag;
    }

    function sexClass(s) {
      return s === 0 ? 'primary' : s === 1 ? 'danger' : 'dark';
    }
    /* per-cell ip → multiplicity, used for nbr_N filter classes */
    function pathMult(cell) {
      const m = new Map();
      for (const p of cell.paths) m.set(p.ip, (m.get(p.ip) || 0) + (p.nbr | 0 || 1));
      return m;
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
        const cnt = cell.cnt;
        const isSelf = i + k === 0;

        const labelHtml = isSelf
          ? L('him_her')
          : (cnt.dist === 1 ? cell.label_s : cell.label_p);
        const tip = buildTip(labelHtml, i, k, cell.tt);
        btn.removeAttribute('title');
        btn.setAttribute('data-bs-original-title', tip);

        const lblEl = btn.querySelector('.cell-label');
        if (lblEl) {
          lblEl.innerHTML = isSelf ? L('him_her') : cell.label_p;
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

    /* ---- 4. Modal (single reusable shell) ---- */
    const modalEl = document.getElementById('cousins-modal');
    const listEl  = document.getElementById('cousins-modal-list');
    const colsEl  = document.getElementById('cousins-modal-cols');

    modalEl.addEventListener('show.bs.modal', function (ev) {
      const trig = ev.relatedTarget;
      if (!trig) return;
      const key = trig.dataset.cell;
      const cell = cells.get(key);
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
      const { i, j, cnt, paths } = cell;
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
                 i - 1, 0, null, cellExists(i - 1, 0));

      /* Position 2 top — ascend (always rendered) */
      if (j === 0)
        cloneNav('cousins-nav-up-anc-tpl', navDn,
                 i + 1, 0, i, cellExists(i + 1, 0));
      else
        cloneNav('cousins-nav-up-cous-tpl', navDn,
                 i, j - 1, j, cellExists(i, j - 1));

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
      const np = paths.length;
      colsEl.style.columnCount = np > 31 ? 3 : 2;

      const mult = pathMult(cell);
      listEl.innerHTML = '';
      const tpl = document.getElementById('cousins-card-tpl');
      const sorted =
        paths.slice().sort(cmpPaths(sortTypes[sortIdx].type));
      for (const path of sorted) {
        const node = renderCard(tpl, path, i, j, cell.span, mult);
        if (node) listEl.appendChild(node);
      }
      applySortLabel();
      ensureTooltips(modalEl);
    }

    const sortBtn = modalEl.querySelector('.sortbtn');
    /* Self-paired (icon/label describe own type). applySortLabel reads
       the NEXT entry; with sortIdx=0 the button advertises 'surname',
       which matches the initial HTML rendering of the button. */
    const sortTypes = [
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

    function sortList() {
      const items = Array.from(listEl.querySelectorAll('li'));
      items.sort(cmpListItems(sortTypes[sortIdx].type));
      items.forEach(function (li) { listEl.appendChild(li); });
    }

    modalEl.addEventListener('click', function (ev) {
      if (ev.target.closest('.sortbtn')) {
        sortIdx = (sortIdx + 1) % sortTypes.length;
        sortList();
        applySortLabel();
        return;
      }
      const nav = ev.target.closest('.cous-hed-nav-btn');
      if (nav && nav.dataset.cell
          && !nav.classList.contains('disabled')) {
        const c = cells.get(nav.dataset.cell);
        if (c) populateModal(c);
      }
    });

    function buildRlmUrl(cell) {
      const params = ['m=RLM', 'i1=' + SELF_IP];
      cell.paths.forEach((p, k) => params.push('i' + (k + 2) + '=' + p.ip));
      if (cell.i + cell.j !== 0) {
        const label = (cell.label_p || '').replace(/<[^>]*>/g, '');
        params.push('t1=' + encodeURIComponent(label));
      }
      return PREFIX + params.join('&');
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
    };

    function cmpBy(mode, getIp) {
      const f = PERSON_CMP[mode];
      return (a, b) => {
        const pa = persons.get(getIp(a)), pb = persons.get(getIp(b));
        return !pa || !pb ? 0 : f(pa, pb);
      };
    }
    const cmpPaths = (m) => cmpBy(m, x => x.ip);
    const cmpListItems   = (m) => cmpBy(m, x => x.dataset.ip);

    /* ---- 5. Card ---- */
    function renderCard(tpl, path, lvlA, lvlD, span, mult) {
      const p = persons.get(path.ip);
      if (!p) return null;
      const dead   = !p.alive;
      const noDesc = !p.has_child;
      const nbr    = mult.get(path.ip) || 1;

      const node = tpl.content.firstElementChild.cloneNode(true);
      node.classList.add('nbr_' + nbr);
      node.classList.add('border-' + sexClass(p.sex));
      if (noDesc) node.classList.add('border-double');
      if (dead)   node.classList.add('border-right-5-black');
      if (nbr > 1) node.dataset.multi = String(nbr);

      const bg = ageGradient(p, span, dead);
      if (bg) node.style.background = bg;

      node.dataset.ip = path.ip;

      const nav = node.querySelector('.card-nav');
      nav.href = pUrlNav(p, path.ip);
      nav.title = L('navigation');

      const rA = node.querySelector('.card-r');
      rA.href  = rUrl(path.ip);
      rA.title = L('rel_links_p');

      const rlm = node.querySelector('.card-rlm');
      if (lvlD !== 0) {
        const a1P = path.a1 ? persons.get(path.a1) : null;
        const a2P = path.a2 ? persons.get(path.a2) : null;
        let primP = a1P, secP = a2P;
        if (a1P && a2P && a2P.sex === 0 && a1P.sex !== 0) {
          primP = a2P; secP = a1P;
        }
        rlm.href = rlmUrl(path, lvlA, lvlD);
        rlm.title = path.tt || '';
        rlm.hidden = false;
        const k = path.nbr | 0;
        const sup = k > 1 ? '<small class="me-1">' + k + '</small>' : '';
        const ico = function (p) {
          return p
            ? '<i class="fa-solid fa-user fa-sm text-'
              + sexClass(p.sex) + '"></i>'
            : '';
        };
        rlm.innerHTML = sup + ico(primP) + ico(secP);
      }

      const nm = node.querySelector('.card-name');
      nm.href = pUrl(p, path.ip);
      nm.textContent = p.sn + ' ' + p.fn;
      nm.title = ageTitle(p);

      const ed = node.querySelector('.card-edit');
      if (IS_WIZ) {
        const a = document.createElement('a');
        a.href = PREFIX + 'm=MOD_IND&i=' + path.ip;
        a.title = L('modify');
        a.textContent = p.dates || (dead ? '†' : '°');
        ed.appendChild(a);
      } else {
        ed.textContent = p.dates || (dead ? '†' : '°');
      }
      return node;
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

    /* ---- 6. Totals ---- */
    function renderTotals() {
      const t = raw.totals;
      const tot = document.getElementById('cousins-totals');
      if (!tot) return;
      tot.querySelector('.tot-anc').textContent   = t.anc + 'A';
      tot.querySelector('.tot-desc').textContent  = '/' + t.desc + 'D';
      tot.querySelector('.tot-alive').textContent = t.alive + 'L';
      tot.querySelector('.tot-dist').textContent  = '/' + t.dist  + 'P';
    }

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

    /* ---- 8. Entry ---- */
    hydrateMatrix();
    renderTotals();
    ensureTooltips(document);
    initToggleLvl();
  }

  return { init };
})();