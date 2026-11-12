(() => {
  const { notify, toast } = InplaceEdit;
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
      this.count = document.getElementById('book-count');
      this.extra = form.querySelector('.book-extra');
      this.warn = form.querySelector('.book-warn');
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
      this.count.textContent = a.dataset.n;
      this.text.textContent = this.original;
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
      const other = document.getElementById('k' + data.key);
      if (data.entry === null || (other && other !== li)) {
        if (other) other.querySelector('a[data-entry]').dataset.n = data.records;
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
      a.dataset.n = data.records;
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
        this.alignBar();
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
        allowList: { ...bootstrap.Tooltip.Default.allowList, kbd: [] }
      });
    },

    searchFocus() {
      const input = document.getElementById('book-filter');
      input?.addEventListener('focus', () => {
        input.setSelectionRange(input.value.length, input.value.length);
      });
    },

    alignBar() {
      const list = document.getElementById('main-content');
      const bar = document.getElementById('book-bar');
      if (!list || !bar) return;
      const fit = () => { bar.style.maxWidth = `max(40rem, ${list.offsetWidth}px)`; };
      fit();
      addEventListener('resize', fit);
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
