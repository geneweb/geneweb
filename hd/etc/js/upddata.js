(() => {
  const { notify, toast } = InplaceEdit;
  const GEO = 'https://data.geopf.fr/geocodage/search';
  const DICOTOPO = 'https://dicotopo.cths.fr';
  const WIKIDATA = 'https://www.wikidata.org/w/api.php';
  const GIVEN = 'P31=Q202444|P31=Q12308941|P31=Q11879590|P31=Q3409032|P31=Q3409027';
  // the classes of the Wikidata items of a book (haswbstatement filter)
  const CLASSES = {
    fn: GIVEN, fna: GIVEN, sn: 'P31=Q101352', sna: 'P31=Q101352',
    occu: 'P31=Q28640|P31=Q12737077', title: 'P31=Q355567'
  };
  const getJSON = (url, params) =>
    fetch(`${url}?${new URLSearchParams(params)}`, { referrerPolicy: 'no-referrer' })
      .then(r => r.ok ? r.json() : Promise.reject(r.status));
  const fold = s => s.normalize('NFD').replace(/\p{M}/gu, '').toLowerCase()
    .replace(/[^\p{L}\p{N}]+/gu, ' ').trim();
  const paren = t => {
    const m = /^(.+?)\s*\(([^()]+)\)$/.exec(t);
    return m ? [m[1], m[2]] : [t];
  };
  // a place: its suburb, its town (the first term, the last one when the
  // places of the base are inverted) and all its terms
  const placeParts = (value, inverted) => {
    const m = /^\[([^\]]*)\]\s*[-\u2013\u2014]\s*/.exec(value);
    const terms = (m ? value.slice(m[0].length) : value).split(',')
      .map(t => t.trim()).filter(Boolean);
    return {
      suburb: m ? m[1].trim() : '',
      town: paren((inverted ? terms.at(-1) : terms[0]) ?? '')[0],
      terms: terms.flatMap(paren)
    };
  };
  const text = html => html
    ? new DOMParser().parseFromString(html, 'text/html').body.textContent.trim() : '';
  // "Crotoy (Le)" written "Le Crotoy"
  const article = s => s.replace(/^(.+) \((L[’']|Le|La|Les)\)$/,
    (_, n, a) => /[’']$/.test(a) ? a + n : `${a} ${n}`);
  // the places and old forms of the Dictionnaire topographique de la France
  // named as [q], exact words first, of the department [dep] if known
  const dicotopo = (q, dep) => {
    const words = fold(q).split(' ').filter(Boolean);
    if (!words.length) return Promise.resolve([]);
    const query = words
      .map(w => `label.folded:${w.length > 3 ? `(${w}^2 OR ${w}~1)` : w}`)
      .concat(dep ? [`dep-id:${dep}`] : []).join(' ');
    const seen = new Set();
    return getJSON(`${DICOTOPO}/api/1.0/search`, { query, 'page[size]': 12 })
      .then(r => r.data
        .filter(({ attributes: a }) => !seen.has(a['place-id']) && seen.add(a['place-id']))
        .map(({ type, attributes: a }) => ({
          name: article(a['place-label']),
          picks: [a['commune-label'], a.region], details: [a.dpt],
          note: text((a.descriptions ?? a['place-desc'] ?? [])[0]),
          forms: type === 'place-old-label'
            ? [text(a['rich-date'] ? `${a['rich-label']} (${a['rich-date']})` : a['rich-label'])]
            : (a['old-labels'] ?? []).map(text),
          link: { href: `${DICOTOPO}/places/${a['place-id']}`, title: 'DicoTopo', icon: 'fa fa-book' }
        })));
  };
  const geo = (q, type) => q.length < 3 ? Promise.resolve([])
    : getJSON(GEO, { q, type, limit: 6 }).then(r => r.features.map(f => f.properties));
  // one request at a time, retried once a second later when refused for
  // too many requests (HTTP 429)
  let queue = Promise.resolve();
  const wdJSON = params => {
    const run = () => getJSON(WIKIDATA, params);
    queue = queue.catch(() => {}).then(run).catch(e => e === 429
      ? new Promise(ok => setTimeout(ok, 1000)).then(run) : Promise.reject(e));
    return queue;
  };
  const wikidata = (q, cls, lang) => {
    if (!q) return Promise.resolve([]);
    const items = cls
      ? wdJSON({
          action: 'query', list: 'search', srsearch: `${q} haswbstatement:${cls}`,
          srnamespace: 0, srlimit: 8, format: 'json', origin: '*'
        }).then(r => {
          const ids = r.query.search.map(e => e.title);
          return ids.length ? wdJSON({
            action: 'wbgetentities', ids: ids.join('|'), props: 'labels|descriptions',
            languages: lang, languagefallback: 1, format: 'json', origin: '*'
          }).then(r => ids.map(id => ({
            id,
            label: r.entities[id].labels?.[lang]?.value,
            description: r.entities[id].descriptions?.[lang]?.value
          }))) : [];
        })
      : wdJSON({
          action: 'wbsearchentities', search: q, language: lang, uselang: lang,
          type: 'item', limit: 8, format: 'json', origin: '*'
        }).then(r => r.search);
    return items.then(l => l.filter(e => e.label).map(e => ({
      name: e.label, details: [e.description],
      link: { href: `https://www.wikidata.org/wiki/${e.id}`, title: 'Wikidata', icon: 'fab fa-wikipedia-w' }
    })));
  };
  const BookEdit = {
    a: null,
    init() {
      const form = document.getElementById('book-edit');
      if (!form) return;
      this.form = form;
      this.field = form.elements.nx_input;
      this.text = form.querySelector('.book-input-text');
      this.persons = document.getElementById('book-persons');
      this.personsHref = this.persons.href;
      this.extra = form.querySelector('.book-extra');
      this.warn = form.querySelector('.book-warn');
      this.ref = form.querySelector('.book-ref');
      this.refButton = document.getElementById('book-ref');
      this.refButton.addEventListener('click', () => this.lookup());
      this.ref.addEventListener('click', ev => {
        const v = ev.target.closest('[data-v]')?.dataset.v;
        if (v) this.insert(v);
      });
      this.ref.addEventListener('keydown', ev => {
        if (ev.key !== 'Escape') return;
        this.hideRef();
        this.field.focus();
      });
      addEventListener('resize', () => { if (this.a) this.fit(); });
      this.field.addEventListener('input', () => {
        this.changed();
        this.fit();
      });
      if (this.field.tagName === 'TEXTAREA' && typeof autosize === 'function') {
        autosize(this.field);
      }
      document.addEventListener('click', ev => {
        const a = ev.target.closest('#main-content a[data-entry]');
        if (!a || ev.button || ev.ctrlKey || ev.metaKey || ev.shiftKey) return;
        ev.preventDefault();
        this.open(a, null, this.caret(a, ev));
      });
      form.addEventListener('submit', ev => {
        ev.preventDefault();
        this.save();
      });
      form.querySelector('.btn-close').addEventListener('click', () => this.close());
      this.field.addEventListener('keydown', ev => {
        if (ev.key === 'Enter' && !ev.isComposing) {
          ev.preventDefault();
          this.save();
        } else if (ev.key === 'Escape') {
          ev.preventDefault();
          this.close();
        } else if (ev.altKey && (ev.key === 'ArrowLeft' || ev.key === 'ArrowRight')) {
          ev.preventDefault();
          this.side(ev.key === 'ArrowLeft' ? -1 : 1);
        } else if (ev.key === 'ArrowDown' || ev.key === 'ArrowUp') {
          ev.preventDefault();
          this.move(ev.key === 'ArrowUp' ? -1 : 1);
        }
      });
      const filter = document.getElementById('book-filter');
      this.prefix = filter?.value;
      filter?.addEventListener('input', () => this.filter(filter.value));
      const p = new URLSearchParams(location.search);
      const a = document.getElementById('k' + p.get('key'))
        ?.querySelector('a[data-entry]');
      if (a) this.open(a, p.get('s2'));
    },
    entries() {
      return [...document.querySelectorAll(
        '#main-content li:not([hidden]) a[data-entry]')];
    },
    move(step) {
      const all = this.entries();
      const next = all[all.indexOf(this.a) + step];
      if (next) this.open(next);
    },
    side(step) {
      const r = this.a.closest('li').getBoundingClientRect();
      let best = null;
      let bx = Infinity;
      let by = Infinity;
      for (const a of this.entries()) {
        const q = a.closest('li').getBoundingClientRect();
        const dx = step * (q.left - r.left);
        if (dx < r.width / 2) continue;
        const dy = Math.abs(q.top - r.top);
        if (dx < bx - 1 || (dx <= bx + 1 && dy < by)) {
          best = a;
          bx = dx;
          by = dy;
        }
      }
      if (best) this.open(best);
    },
    caret(a, ev) {
      let r = document.caretRangeFromPoint?.(ev.clientX, ev.clientY);
      const p = !r && document.caretPositionFromPoint?.(ev.clientX, ev.clientY);
      if (p) {
        r = document.createRange();
        r.setStart(p.offsetNode, p.offset);
      }
      if (!r || !a.contains(r.startContainer)) return null;
      const pre = document.createRange();
      pre.selectNodeContents(a);
      pre.setEnd(r.startContainer, r.startOffset);
      return pre.toString().trimStart().length;
    },
    changed() {
      this.extra.hidden = this.field.value.trim() === this.original;
      this.warn.hidden = true;
    },
    open(a, value, pos) {
      this.a?.closest('li').classList.remove('book-current');
      this.a = a;
      const li = a.closest('li');
      li.classList.add('book-current');
      this.original = a.dataset.v ?? a.dataset.entry;
      this.form.elements.key.value = li.id.slice(1);
      this.persons.href = `${this.personsHref}&key=${li.id.slice(1)}`;
      const n = a.dataset.n;
      const label = this.persons.dataset[n === '1' ? 'one' : 'many'].replace('{n}', n);
      this.persons.textContent = n;
      this.persons.title = label;
      this.persons.setAttribute('aria-label', label);
      this.text.textContent = this.original;
      this.hideRef();
      this.field.value = value || this.original;
      this.changed();
      bootstrap.Tooltip.getInstance(a)?.hide();
      li.scrollIntoView({ block: 'nearest' });
      this.place(li.querySelector('.book-src') ?? a);
      const at = !value && pos !== null && pos !== undefined
        && a.textContent.trim() === this.original ? pos : this.field.value.length;
      this.field.focus({ preventScroll: true });
      this.field.setSelectionRange(at, at);
    },
    place(anchor) {
      const { form, field } = this;
      const rem = parseFloat(getComputedStyle(document.documentElement).fontSize);
      form.style.width = innerWidth - 2 * rem + 'px';
      form.hidden = false;
      this.min = Math.max(anchor.closest('li').offsetWidth
        + form.offsetWidth - field.offsetWidth, innerWidth / 3);
      const cs = getComputedStyle(field);
      const fr = form.getBoundingClientRect();
      const ir = field.getBoundingClientRect();
      const ar = anchor.getBoundingClientRect();
      this.left = ar.left - (ir.left - fr.left)
        - parseFloat(cs.borderLeftWidth) - parseFloat(cs.paddingLeft);
      const top = ar.top - (ir.top - fr.top)
        - parseFloat(cs.borderTopWidth) - parseFloat(cs.paddingTop);
      form.style.top = scrollY + top + 'px';
      this.fit();
      form.scrollIntoView({ block: 'nearest' });
    },
    fit() {
      const { form, field } = this;
      const rem = parseFloat(getComputedStyle(document.documentElement).fontSize);
      const cs = getComputedStyle(field);
      const ctx = (this.ctx ??= document.createElement('canvas').getContext('2d'));
      ctx.font = `${cs.fontWeight} ${cs.fontSize} ${cs.fontFamily}`;
      const need = Math.ceil(ctx.measureText(field.value).width) + 2
        + parseFloat(cs.paddingLeft) + parseFloat(cs.paddingRight)
        + parseFloat(cs.borderLeftWidth) + parseFloat(cs.borderRightWidth)
        + form.offsetWidth - field.offsetWidth;
      const w = Math.min(Math.max(this.min, need), innerWidth - 2 * rem);
      form.style.width = w + 'px';
      form.style.left = scrollX + Math.min(Math.max(this.left, rem), innerWidth - w - rem) + 'px';
      if (field.tagName === 'TEXTAREA' && typeof autosize === 'function') {
        autosize.update(field);
      }
      this.spread();
    },
    // the reference sources under the field, as wide as the editor, across
    // the width of the window when they hold more than a few lines
    spread() {
      if (this.ref.hidden) return;
      const rem = parseFloat(getComputedStyle(document.documentElement).fontSize);
      const style = this.ref.style;
      style.left = -this.form.clientLeft + 'px';
      style.width = this.form.offsetWidth + 'px';
      if (this.ref.scrollHeight <= 12 * rem) return;
      const left = this.form.getBoundingClientRect().left + this.form.clientLeft;
      style.left = rem - left + 'px';
      style.width = document.documentElement.clientWidth - 2 * rem + 'px';
    },
    hideRef() {
      this.ref.hidden = true;
      this.ref.replaceChildren();
      this.refButton.setAttribute('aria-expanded', 'false');
    },
    close() {
      const a = this.a;
      if (!a) return;
      this.a = null;
      a.closest('li').classList.remove('book-current');
      this.form.hidden = true;
      if (a.isConnected) a.focus();
    },
    save() {
      const a = this.a;
      const form = this.form;
      const value = this.field.value.trim();
      if (!value || value === this.original) {
        this.move(1);
        return;
      }
      if (form.dataset.busy) return;
      form.dataset.busy = '1';
      const body = new URLSearchParams(new FormData(form));
      body.set('nx_input', value);
      body.set('ajax', '1');
      if (!this.warn.hidden) body.set('confirm', 'on');
      fetch(form.action, {
        method: 'POST',
        headers: { 'Accept': 'application/json' },
        body })
        .then(r => r.ok ? r.json() : Promise.reject(r.status))
        .then(data => {
          delete form.dataset.busy;
          if (data.confirm) {
            if (this.a !== a) {
              notify('danger', data.message);
              return;
            }
            this.warn.textContent = data.message;
            this.warn.hidden = false;
            return;
          }
          if (!data.success) {
            notify('danger', data.message || form.dataset.error);
            return;
          }
          notify('success', toast(data.label, data.before, data.after));
          const here = this.a === a;
          const all = this.entries();
          const next = all[all.indexOf(a) + 1];
          this.update(a, data);
          if (!here) return;
          if (next?.isConnected) this.open(next);
          else this.close();
        })
        .catch(err => {
          delete form.dataset.busy;
          console.error('m=MOD_DATA_OK', err);
          notify('danger', form.dataset.error);
        });
    },
    update(a, data) {
      const li = a.closest('li');
      const ul = li.parentElement;
      const head = li.previousElementSibling;
      const other = document.getElementById('k' + data.key);
      if (data.place === null || (other && other !== li)) {
        if (other) {
          other.querySelector('a[data-entry]').dataset.n = data.records;
          other.querySelector('.book-n').textContent = data.records;
        }
        li.remove();
      } else if (this.insert(li, data.place)) {
        li.id = 'k' + data.key;
        const u = new URL(a.href);
        u.searchParams.set('key', data.key);
        a.href = u;
        a.dataset.entry = data.entry;
        a.dataset.n = data.records;
        li.querySelector('.book-n').textContent = data.records;
        if (data.after === data.entry) delete a.dataset.v;
        else a.dataset.v = data.after;
        (a.nextElementSibling ?? a).textContent = data.entry;
      } else {
        location.reload();
        return;
      }
      if (head?.classList.contains('book-head')
        && !head.nextElementSibling?.querySelector('a[data-entry]')) {
        head.remove();
      }
      if (!ul.querySelector('a[data-entry]')) {
        ul.previousElementSibling.remove();
        ul.remove();
      }
      if (!document.querySelector('#main-content a[data-entry]')) {
        location.href = data.url;
      }
    },
    insert(li, { ini, prev, head }) {
      const ul = document.getElementById('_' + ini)?.nextElementSibling;
      let at = prev && document.getElementById('k' + prev);
      if (!ul || (prev && !at)) return false;
      if (head) {
        const next = at ? at.nextElementSibling : ul.firstElementChild;
        let h = at;
        while (h && !h.classList.contains('book-head')) h = h.previousElementSibling;
        if (next?.classList.contains('book-head') && next.textContent === head) at = next;
        else if (h?.textContent !== head) {
          const n = document.createElement('li');
          n.className = 'book-head';
          n.textContent = head;
          if (at) at.after(n);
          else ul.prepend(n);
          at = n;
        }
      }
      if (at) at.after(li);
      else ul.prepend(li);
      return true;
    },
    lookup() {
      const a = this.a;
      const value = this.field.value.trim();
      if (!a || !value) return;
      if (!this.ref.hidden && value === this.searched) {
        this.hideRef();
        return;
      }
      this.searched = value;
      const lang = document.documentElement.lang || 'en';
      const { cities, places } = this.ref.dataset;
      const data = this.form.elements.data.value;
      let terms = [value];
      let groups;
      if (data === 'place' || data === 'domain') {
        const p = placeParts(value, 'inverted' in this.ref.dataset);
        const context = e => (e.context ?? '').split(', ').slice(1);
        terms = [p.suburb, ...p.terms].filter(Boolean);
        const folded = new Set(terms.map(fold));
        const communes = geo(p.town, 'municipality');
        // the department of the town: the one named in the entry, else the
        // one of the only municipality of this name
        const topo = communes.catch(() => []).then(c => {
          const same = c.filter(e => fold(e.name) === fold(p.town));
          const dep = c.find(e => folded.has(fold(context(e)[0] ?? '')))
            ?? (same.length === 1 ? same[0] : null);
          return dicotopo(p.suburb || p.town, dep?.depcode);
        });
        groups = Promise.allSettled([
          topo,
          communes,
          geo(p.suburb ? `${p.suburb} ${p.town}` : p.town, 'locality'),
          wikidata(p.town, null, lang)
        ]).then(r => {
          if (r.every(e => e.status === 'rejected')) throw r[0].reason;
          return r.map(e => e.value ?? []);
        }).then(([d, c, l, w]) => [
          { head: 'DicoTopo', items: d },
          { head: cities, items: c.map(e => ({
            name: e.name, picks: context(e),
            details: [e.postcode, `Insee ${e.citycode}`]
          })) },
          { head: places, items: l.map(e => ({
            name: e.name, picks: [e.city, ...context(e)], details: [e.postcode]
          })) },
          { head: 'Wikidata', items: w }
        ]);
      } else {
        // each first name, each occupation without its dates, apart
        const parts = data === 'fn' || data === 'fna' ? value.split(/\s+/)
          : data === 'occu' ? value.replace(/\s*\([^()]*\d[^()]*\)/g, '').split(',')
          : [value];
        terms = [...new Set(parts.map(t => t.trim()).filter(Boolean))];
        groups = Promise.allSettled(terms.map(t => wikidata(t, CLASSES[data], lang)
          .then(items => ({ head: `Wikidata · ${t}`, items })))).then(r => {
          if (r.every(e => e.status === 'rejected')) throw r[0].reason;
          return r.filter(e => e.value).map(e => e.value);
        });
      }
      const icon = this.refButton.firstElementChild;
      icon.className = 'spinner-border spinner-border-sm';
      groups
        .then(g => { if (this.a === a) this.show(g, terms); })
        .catch(err => {
          console.error('reference sources', err);
          if (this.a === a) notify('danger', this.form.dataset.error);
        })
        .finally(() => { icon.className = 'fa fa-globe'; });
    },
    show(groups, terms) {
      const { same, near, use, none } = this.ref.dataset;
      const fold1 = c => {
        const f = c.normalize('NFD').replace(/\p{M}/gu, '').toLowerCase();
        return f.length !== 1 ? c : /[\p{L}\p{N}]/u.test(f) ? f : ' ';
      };
      // folded code unit by code unit, so that offsets are those of [s]
      const fold0 = s => Array.prototype.map.call(s, fold1).join('');
      const keys = terms.filter(t => t.length > 2).map(t => [t, fold0(t)])
        .filter(([, k]) => k.trim());
      const el = (tag, cls = '', ...kids) => {
        const e = document.createElement(tag);
        e.className = cls;
        e.append(...kids);
        return e;
      };
      // [s] with the terms of the entry in bold, waved when written otherwise
      const hl = s => {
        const f = fold0(s);
        const hits = [];
        for (const [t, k] of keys) {
          for (let i = f.indexOf(k); i >= 0; i = f.indexOf(k, i + k.length)) {
            hits.push([i, i + k.length, s.slice(i, i + k.length) === t]);
          }
        }
        hits.sort((x, y) => x[0] - y[0] || y[1] - x[1]);
        const out = [];
        let at = 0;
        for (const [i, j, exact] of hits) {
          if (i < at) continue;
          const b = el('b', exact ? '' : 'book-near', s.slice(i, j));
          b.title = exact ? same : near;
          out.push(s.slice(at, i), b);
          at = j;
        }
        out.push(s.slice(at));
        return out;
      };
      const pick = (tag, cls, s, v = s) => {
        const e = el(tag, cls, ...hl(s));
        e.dataset.v = v;
        e.title = use;
        if (tag === 'button') e.type = 'button';
        return e;
      };
      const nodes = groups.filter(g => g.items.length).flatMap(g => {
        const ul = el('ul', 'list-unstyled mb-0');
        for (const c of g.items) {
          const li = el('li', '',
            pick('button', 'btn btn-link btn-sm p-0 align-baseline', c.name),
            ...(c.picks ?? []).filter(Boolean).flatMap(s => [' · ', pick('span', 'book-pick', s)]),
            ...c.details.filter(Boolean).flatMap(s => [' · ', ...hl(s)]));
          if (c.link) {
            const w = el('a', 'ms-2', el('i', c.link.icon));
            w.href = c.link.href;
            w.target = '_blank';
            w.rel = 'noreferrer noopener';
            w.title = c.link.title;
            li.append(w);
          }
          if (c.note) li.append(el('div', 'text-body-secondary', ...hl(c.note)));
          if (c.forms?.length) {
            li.append(el('div', 'fst-italic', ...c.forms.flatMap((s, i) =>
              [i ? ', ' : '', pick('span', 'book-pick', s, paren(s)[0])])));
          }
          ul.append(li);
        }
        return [el('div', 'fw-semibold mt-1', g.head), ul];
      });
      this.ref.replaceChildren(el('div', 'book-ref-cols', ...(nodes.length ? nodes : [none])));
      this.ref.hidden = false;
      this.refButton.setAttribute('aria-expanded', 'true');
      this.spread();
      this.ref.scrollIntoView({ block: 'nearest' });
    },
    insert(v) {
      const f = this.field;
      f.setRangeText(v, f.selectionStart, f.selectionEnd, 'end');
      f.dispatchEvent(new Event('input'));
      f.focus();
    },
    filter(q) {
      const norm = s => s.normalize('NFD').replace(/\p{M}/gu, '').toLowerCase();
      const n = norm(q.trim());
      const base = norm(this.prefix ?? '');
      for (const ul of document.querySelectorAll('#main-content ul')) {
        let shown = false;
        let head = null;
        for (const li of ul.children) {
          const a = li.querySelector('a[data-entry]');
          if (!a) {
            head = li;
            head.hidden = true;
            continue;
          }
          li.hidden = n !== base && !norm(a.dataset.v ?? a.dataset.entry).includes(n);
          if (li.hidden) continue;
          shown = true;
          if (head) head.hidden = false;
        }
        ul.hidden = ul.previousElementSibling.hidden = !shown;
      }
    }
  };

  const updateData = {
    init() {
      try {
        this.keyboardNavigation();
        this.stableTooltips();
        this.removeLoadingOverlay();
        this.autoSelectLink();
        this.helpPopover();
        this.searchFocus();
        this.balanceIndex();
        BookEdit.init();
      } catch (e) {
        console.error('updateData init failed:', e);
      }
    },

    keyboardNavigation() {
      const navbar = document.getElementById('fixed-nav');
      const book = document.getElementById('book');
      if (navbar && book) {
        book.style.scrollPaddingTop = (navbar.offsetHeight + 4) + 'px';
      }

      const linkTrm = document.querySelectorAll('.sub-char');
      const linkAdd = document.querySelectorAll('.sup-char');
      let iTrm = -1, iAdd = -1;

      const keyActions = new Map([
        ['t', () => {
          const len = linkTrm.length;
          if (len === 0) return;
          if (iTrm === -1) {
            iTrm = len - 1;
          } else {
            linkTrm[iTrm].blur();
            iTrm = (iTrm + len - 1) % len;
          }
          linkTrm[iTrm].focus();
        }],
        ['d', () => {
          const len = linkAdd.length;
          if (len === 0) return;
          if (iAdd === -1) {
            iAdd = 0;
          } else {
            linkAdd[iAdd].blur();
            iAdd = (iAdd + 1) % len;
          }
          linkAdd[iAdd].focus();
        }]
      ]);

      const inputTags = new Set(['input', 'textarea']);
      document.addEventListener('keydown', (event) => {
        const action = keyActions.get(event.key);
        if (action && !event.ctrlKey && !event.metaKey
          && (event.altKey || !inputTags.has(event.target.tagName.toLowerCase()))) {
          event.preventDefault();
          action();
        }
      });
    },

    stableTooltips() {
      document.querySelectorAll('[data-bs-toggle="tooltip"]').forEach(el => {
        if (!bootstrap.Tooltip.getInstance(el)) {
          new bootstrap.Tooltip(el, {
            html: true,
            allowList: {
              ...bootstrap.Tooltip.Default.allowList,
              span: [],
              i: ['class'],
              hr: []
            }
          });
        }
      });
    },

    helpPopover() {
      const btn = document.getElementById('book-help');
      if (!btn) return;
      new bootstrap.Popover(btn, {
        html: true,
        trigger: 'focus',
        placement: 'bottom',
        content: btn.nextElementSibling.innerHTML,
        allowList: { ...bootstrap.Tooltip.Default.allowList, kbd: [] },
        popperConfig: config => ({
          ...config,
          modifiers: [...config.modifiers,
            { name: 'preventOverflow', options: { padding: 16 } }]
        })
      });
    },

    searchFocus() {
      const input = document.getElementById('book-filter');
      input?.addEventListener('focus', () => {
        input.setSelectionRange(input.value.length, input.value.length);
      });
    },

    balanceIndex() {
      const box = document.getElementById('book-index');
      const n = box?.querySelectorAll('a').length;
      if (!n) return;
      const fit = () => {
        box.style.removeProperty('--cols');
        const max = getComputedStyle(box).gridTemplateColumns.split(' ').length;
        box.style.setProperty('--cols', Math.ceil(n / Math.ceil(n / max)));
      };
      fit();
      addEventListener('resize', fit);
    },

    removeLoadingOverlay() {
      const urlParams = new URLSearchParams(window.location.search);
      if (urlParams.has('key') || !urlParams.has('s1')) {
        const overlay = document.getElementById('loading-overlay');
        if (overlay) overlay.style.display = 'none';
      }
    },

    // localStorage may throw in private browsing.
    _lsGet(key) {
      try { return localStorage.getItem(key); } catch { return null; }
    },
    _lsSet(key, value) {
      try { localStorage.setItem(key, value); } catch { /* ignore */ }
    },
    _lsRemove(key) {
      try { localStorage.removeItem(key); } catch { /* ignore */ }
    },

    _safeDecode(value) {
      try { return decodeURIComponent(value); } catch { return value; }
    },

    // Auto-select link based on URL parameters.
    // Fallback on book view if s1 not found, with warning.
    autoSelectLink() {
      const urlParams = new URLSearchParams(window.location.search);

      if (urlParams.has('s1') || urlParams.has('s2')) {
        if (urlParams.has('key')) return; // already editing an entry

        const s1Param = urlParams.get('s1');
        const s2Param = urlParams.get('s2');
        const sParam  = urlParams.get('s');
        const value   = s1Param || sParam;

        if (value && value.trim()) {
          const decodedValue = this._safeDecode(value);
          const link = Array.from(
            document.querySelectorAll('a[data-entry]')
          ).find(
            a => a.getAttribute('data-entry') === decodedValue && a.href.includes('key=')
          );

          if (link) {
            window.location.href = s2Param
              ? `${link.href}&s2=${encodeURIComponent(this._safeDecode(s2Param))}`
              : link.href;
          } else {
            if (decodedValue.trim()) {
              this._lsSet('notFoundValue', decodedValue);
            }
            urlParams.delete('s1');
            urlParams.delete('s2');
            // s1 takes precedence over s
            if (s1Param) urlParams.set('s', s1Param);
            window.location.href = window.location.pathname + '?' + urlParams.toString();
          }
        } else {
          urlParams.delete('s1');
          urlParams.delete('s2');
          window.location.href = window.location.pathname + '?' + urlParams.toString();
        }
      } else {
        const notFoundValue = this._lsGet('notFoundValue');
        if (notFoundValue && notFoundValue.trim()) {
          const valueSpan   = document.getElementById('notfound-value');
          const notFoundDiv = document.getElementById('notfound-warning');
          if (valueSpan && notFoundDiv) {
            valueSpan.textContent = notFoundValue;
            notFoundDiv.classList.remove('d-none');
          }
        }
        this._lsRemove('notFoundValue');
      }
    }
  };
  document.addEventListener('DOMContentLoaded', () => updateData.init());
})();
