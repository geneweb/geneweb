(() => {
  function notify(type, content) {
    document.querySelectorAll('.ajax-notification').forEach(n => n.remove());
    const n = document.createElement('div');
    n.className = 'alert alert-' + (type === 'success' ? 'success' : 'danger')
      + ' ajax-notification';
    const c = document.createElement('div');
    c.className = 'd-flex align-items-start';
    const s = document.createElement('div');
    s.className = 'flex-grow-1';
    if (content instanceof Node) s.appendChild(content);
    else s.textContent = content;
    const b = document.createElement('button');
    b.type = 'button';
    b.className = 'btn-close ms-2';
    c.appendChild(s);
    c.appendChild(b);
    n.appendChild(c);
    const remove = () => {
      n.classList.add('removing');
      n.addEventListener('animationend', () => n.remove(), { once: true });
    };
    const to = setTimeout(remove, 5000);
    b.onclick = () => { clearTimeout(to); remove(); };
    document.body.appendChild(n);
  }
  function diffParts(a, b) {
    const max = Math.min(a.length, b.length);
    let p = 0;
    while (p < max && a.charAt(p) === b.charAt(p)) p++;
    let s = 0;
    while (s < max - p
      && a.charAt(a.length - 1 - s) === b.charAt(b.length - 1 - s)) s++;
    return {
      prefix: a.slice(0, p),
      aMid: a.slice(p, a.length - s),
      bMid: b.slice(p, b.length - s),
      suffix: a.slice(a.length - s)
    };
  }
  function diffLine(d, mid, cls) {
    const row = document.createElement('div');
    if (d.prefix) row.append(d.prefix);
    const m = document.createElement('span');
    m.className = cls;
    m.textContent = mid;
    row.append(m);
    if (d.suffix) row.append(d.suffix);
    return row;
  }
  function toast(label, before, after) {
    const wrap = document.createElement('div');
    const title = document.createElement('div');
    title.className = 'fw-bold';
    title.textContent = label;
    const hr = document.createElement('hr');
    hr.className = 'my-1';
    const d = diffParts(before, after);
    wrap.append(
      title, hr,
      diffLine(d, d.aMid, 'text-decoration-line-through'),
      diffLine(d, d.bMid, 'bg-warning-subtle')
    );
    return wrap;
  }
  const InplaceEdit = {
    active: false,
    initialized: false,
    init() {
      if (this.initialized) return;
      this.initialized = true;
      document.addEventListener('click', ev => {
        const el = ev.target.closest('[data-updfield]');
        if (!el || !this.active) return;
        ev.preventDefault();
        this.open(el);
      });
      document.querySelectorAll('.upd-occ').forEach(occ => {
        const submit = () => {
          let v = parseInt(occ.value, 10);
          if (isNaN(v)) v = 0;
          v = Math.min(99999, Math.max(0, v));
          occ.value = v;
          this.save(null, 'occ', occ.defaultValue, String(v),
            text => { occ.value = text; });
        };
        occ.addEventListener('change', submit);
        occ.addEventListener('keydown', ev => {
          if (ev.key === 'Escape') {
            ev.preventDefault();
            occ.value = occ.defaultValue;
            occ.blur();
          }
        });
      });
    },
    toggle() {
      this.active = !this.active;
      document.body.classList.toggle('upd-edit', this.active);
      document.querySelectorAll('[data-updfield]').forEach(el => {
        if (this.active) el.title = UPD.editField;
        else el.removeAttribute('title');
      });
      document.querySelectorAll('.upd-occ').forEach(o =>
        o.classList.toggle('d-none', !this.active));
      document.querySelectorAll('.upd-wand').forEach(w => {
        w.classList.toggle('text-success', this.active);
        w.classList.toggle('text-muted', !this.active);
      });
    },
    open(el) {
      const next = el.nextElementSibling;
      if (next && next.classList.contains('upd-input')) return;
      const field = el.dataset.updfield;
      const raw = el.dataset.updraw;
      const abbr = el.querySelector('abbr');
      const link =
        raw !== undefined ? null : el.matches('a') ? el : el.querySelector('a');
      const original = raw !== undefined ? raw
        : abbr ? abbr.title.trim() : el.textContent.trim();
      const original_html = el.innerHTML;
      const area =
        field === 'occu' || field === 'psources' || field === 'fsources'
        || field.endsWith('_src');
      const input = document.createElement(area ? 'textarea' : 'input');
      input.className = 'form-control upd-input';
      input.value = original;
      input.style.font = 'inherit';
      if (area) {
        input.rows = 1;
      } else {
        input.type = 'text';
        input.placeholder = original;
      }
      el.style.display = 'none';
      el.after(input);
      input.style.display = 'inline-block';
      input.style.verticalAlign = area ? 'top' : 'middle';
      input.style.width = '1px';
      const box = input.closest('.col, [class*="col-"], .container, .container-fluid')
        || document.body;
      const cs = getComputedStyle(box);
      const r = box.getBoundingClientRect();
      const right = r.right - parseFloat(cs.paddingRight);
      const full = Math.floor(right - r.left - parseFloat(cs.paddingLeft)) - 1;
      const room = Math.floor(right - input.getBoundingClientRect().left) - 1;
      const min = 15 * parseFloat(getComputedStyle(input).fontSize);
      const head = input.closest('h1, h2, h3, h4, h5, h6');
      if (head) {
        const rg = document.createRange();
        rg.selectNodeContents(head);
        const slack = Math.floor(right - rg.getBoundingClientRect().right);
        input.style.width = Math.max(input.scrollWidth, slack) + 'px';
      } else {
        input.style.width = (room >= min ? room : full) + 'px';
      }
      if (area && typeof autosize === 'function') autosize(input);
      const restore = (text, htmlText) => {
        if (area && typeof autosize === 'function') autosize.destroy(input);
        input.removeEventListener('blur', onBlur);
        if (text === original) {
          el.innerHTML = original_html;
        } else if (htmlText) {
          el.innerHTML = htmlText;
        } else if (link) {
          link.textContent = text;
          try {
            const u = new URL(link.href);
            if (u.searchParams.has('v')) {
              u.searchParams.set('v', text);
              link.href = u.toString();
            }
          } catch (e) { /* unparsable href: leave it unchanged */ }
        } else {
          el.textContent = text;
        }
        el.style.display = '';
        input.remove();
      };
      const onBlur = () => restore(original);
      input.addEventListener('blur', onBlur);
      input.focus();
      input.setSelectionRange(original.length, original.length);
      input.addEventListener('keydown', ev => {
        if (ev.key === 'Enter' && !ev.shiftKey) {
          ev.preventDefault();
          this.save(el, field, original, input.value.trim(), restore);
        } else if (ev.key === 'Escape') {
          ev.preventDefault();
          restore(original);
        }
      });
    },
    save(el, field, original, value, restore) {
      if (value === '' || value === original) { restore(original); return; }
      const iper = (el && el.dataset.index) || UPD.iper;
      const ifam = (el && el.dataset.ifam) || '';
      const qs = UPD.prefix.indexOf('?');
      const action = qs < 0 ? UPD.prefix : UPD.prefix.slice(0, qs);
      const henv = qs < 0 ? '' : UPD.prefix.slice(qs + 1);
      let body = henv + 'm=UPD&i=' + encodeURIComponent(iper)
        + (ifam ? '&f=' + encodeURIComponent(ifam) : '')
        + '&' + field + '=' + encodeURIComponent(value)
        + '&prev=' + encodeURIComponent(original);
      fetch(action, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
          'Accept': 'application/json'
        },
        body })
        .then(r => r.ok ? r.json() : Promise.reject(r.status))
        .then(data => {
          if (data.success) {
            const txt = data.after !== undefined ? data.after : value;
            restore(txt, data.after_html || '');
            if (el && !data.reload && data.raw !== undefined) {
              el.dataset.updraw = data.raw;
            }
            if (typeof reducePlaces === 'function') reducePlaces();
            notify('success', toast(data.label, data.before, data.raw));
            if (data.reload) {
              if (el && el.dataset.index) {
                const box = el.closest('div');
                const a = box && box.querySelector('a[href*="p="]');
                if (a) a.href = data.reload;
              } else {
                setTimeout(() => { window.location.href = data.reload; }, 1200);
              }
            }
          } else {
            restore(original);
            if (data.message) notify('danger', data.message);
          }
        })
        .catch(err => {
          console.error('m=UPD', err);
          restore(original);
          notify('danger', UPD.error);
        });
    }
  };
  const BookEdit = {
    a: null,
    init() {
      const form = document.getElementById('book-edit');
      if (!form) return;
      this.form = form;
      this.field = form.elements.nx_input;
      this.text = form.querySelector('.book-input-text');
      this.extra = form.querySelector('.book-extra');
      this.field.addEventListener('input', () => this.changed());
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
    },
    open(a, value, pos) {
      this.a?.closest('li').classList.remove('book-current');
      this.a = a;
      const li = a.closest('li');
      li.classList.add('book-current');
      this.original = a.dataset.v ?? a.dataset.entry;
      this.form.elements.key.value = li.id.slice(1);
      this.text.textContent = this.original;
      this.field.value = value || this.original;
      this.changed();
      bootstrap.Tooltip.getInstance(a)?.hide();
      li.scrollIntoView({ block: 'nearest' });
      const form = this.form;
      const rem = parseFloat(getComputedStyle(document.documentElement).fontSize);
      const w = Math.min(Math.max(li.offsetWidth + 8 * rem, 24 * rem),
        innerWidth - 2 * rem);
      form.style.width = w + 'px';
      form.hidden = false;
      if (this.field.tagName === 'TEXTAREA' && typeof autosize === 'function') {
        autosize.update(this.field);
      }
      const cs = getComputedStyle(this.field);
      const fr = form.getBoundingClientRect();
      const ir = this.field.getBoundingClientRect();
      const ar = a.getBoundingClientRect();
      const left = ar.left - (ir.left - fr.left)
        - parseFloat(cs.borderLeftWidth) - parseFloat(cs.paddingLeft);
      const top = ar.top - (ir.top - fr.top)
        - parseFloat(cs.borderTopWidth) - parseFloat(cs.paddingTop);
      form.style.left = scrollX + Math.min(Math.max(left, rem), innerWidth - w - rem) + 'px';
      form.style.top = scrollY + top + 'px';
      form.scrollIntoView({ block: 'nearest' });
      const at = !value && pos !== null && pos !== undefined
        && a.textContent.trim() === this.original ? pos : this.field.value.length;
      this.field.focus({ preventScroll: true });
      this.field.setSelectionRange(at, at);
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
      fetch(form.action, {
        method: 'POST',
        headers: { 'Accept': 'application/json' },
        body })
        .then(r => r.ok ? r.json() : Promise.reject(r.status))
        .then(data => {
          delete form.dataset.busy;
          if (!data.success) {
            notify('danger', data.message || form.dataset.error);
            return;
          }
          notify('success', toast(data.label, data.before, data.after));
          if (!data.complete) return;
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
      const other = document.getElementById('k' + data.key);
      if (data.entry === null || (other && other !== li)) {
        const ul = li.parentElement;
        li.remove();
        if (!ul.querySelector('a[data-entry]')) {
          ul.previousElementSibling.remove();
          ul.remove();
        }
        if (!document.querySelector('#main-content a[data-entry]')) {
          location.href = data.url;
        }
        return;
      }
      li.id = 'k' + data.key;
      const u = new URL(a.href);
      u.searchParams.set('key', data.key);
      a.href = u;
      a.dataset.entry = data.entry;
      if (data.after === data.entry) delete a.dataset.v;
      else a.dataset.v = data.after;
      (a.nextElementSibling ?? a).textContent = data.entry;
    },
    filter(q) {
      const norm = s => s.normalize('NFD').replace(/\p{M}/gu, '').toLowerCase();
      const n = norm(q.trim());
      const base = norm(this.prefix ?? '');
      for (const ul of document.querySelectorAll('#main-content ul')) {
        let shown = false;
        for (const li of ul.children) {
          const a = li.querySelector('a[data-entry]');
          li.hidden = n !== base && !norm(a.dataset.v ?? a.dataset.entry).includes(n);
          shown ||= !li.hidden;
        }
        ul.hidden = ul.previousElementSibling.hidden = !shown;
      }
    }
  };

  window.InplaceEdit = InplaceEdit;
  window.BookEdit = BookEdit;
})();