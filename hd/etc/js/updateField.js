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
      input.className = area
        ? 'form-control upd-input w-100'
        : 'form-control form-control-inline upd-input';
      input.value = original;
      input.style.font = 'inherit';
      if (area) {
        input.rows = 1;
      } else {
        input.type = 'text';
        input.placeholder = original;
        input.size = original.length + 1;
      }
      el.style.display = 'none';
      el.after(input);
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
  window.InplaceEdit = InplaceEdit;
})();