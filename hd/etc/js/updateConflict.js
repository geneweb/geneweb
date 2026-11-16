// Edit conflicts of the person, family and note forms (UpdateConflict,
// upd_review.txt). While a form is open, checks on return to its tab and
// every ten minutes whether its record was saved meanwhile (m=CHK_DIGEST),
// and then offers to reload the form or to add the changes to the entry. On a
// form redisplayed on an edit conflict, lists the changes saved in the record
// and the differences to check, marks the fields of these differences, and
// holds the form until each one is checked.
(() => {
  'use strict';
  const d = document.currentScript.dataset;
  const form = document.getElementById('updind') || document.getElementById('updfam')
    || document.getElementById('updnotes');
  if (!form) return;

  const field = name => form.elements.namedItem(name);
  const key = d.kind === 'note' ? 'f' : 'i';
  const id = () => (field(key) ? field(key).value : '');
  if (d.kind && field('snapshot') && field('digest') && (d.kind === 'note' || field('i'))) {
    const url = d.prefix + 'm=CHK_DIGEST&t=' + d.kind + '&' + key + '=' + encodeURIComponent(id())
      + '&digest=' + encodeURIComponent(field('digest').value);
    const entry = () => new URLSearchParams(new FormData(form)).toString();
    let initial = entry();
    document.addEventListener('DOMContentLoaded', () => { initial = entry(); });
    let last = Date.now();
    let warned = false;
    const warn = () => {
      warned = true;
      const alert = document.createElement('div');
      alert.className = 'alert alert-warning sticky-top d-flex flex-wrap align-items-center gap-2';
      alert.setAttribute('role', 'alert');
      alert.append(d.changed);
      const button = document.createElement('button');
      button.type = 'button';
      button.className = 'btn btn-sm btn-primary ms-auto';
      const edited = () => entry() !== initial;
      const label = () => { button.textContent = edited() ? d.merge : d.reload; };
      label();
      form.addEventListener('input', label);
      form.addEventListener('change', label);
      button.addEventListener('click', () => {
        if (edited()) {
          const redisplay = document.createElement('input');
          redisplay.type = 'hidden';
          redisplay.name = 'return';
          redisplay.value = 'on';
          form.append(redisplay);
          HTMLFormElement.prototype.submit.call(form);
        } else {
          const ip = field('ip');
          location.href = d.prefix + 'm=' + d.modify + '&' + key + '=' + encodeURIComponent(id())
            + (ip && ip.value ? '&ip=' + encodeURIComponent(ip.value) : '');
        }
      });
      alert.append(button);
      form.prepend(alert);
    };
    const check = () => {
      if (warned || document.visibilityState !== 'visible' || Date.now() - last < 60000) return;
      last = Date.now();
      fetch(url, { headers: { Accept: 'application/json' } })
        .then(r => (r.ok ? r.json() : null))
        .then(data => { if (data && data.changed && !warned) warn(); })
        .catch(() => {});
    };
    document.addEventListener('visibilitychange', check);
    window.addEventListener('focus', check);
    setInterval(check, 600000);
  }

  const box = document.getElementById('review');
  if (!box) return;
  const t = box.dataset;
  const review = JSON.parse(t.review);

  const add = (parent, tag, className, text) => {
    const el = document.createElement(tag);
    if (className) el.className = className;
    if (text !== undefined) el.textContent = text;
    parent.appendChild(el);
    return el;
  };
  const line = (parent, label, text) => {
    const div = add(parent, 'div', 'small');
    add(div, 'span', 'text-body-secondary', label + t.colon + ' ');
    return add(div, 'span', 'review-value', text || '—');
  };
  const norm = v => (v || '').replace(/\r\n/g, '\n');
  const shows = pairs => pairs.every(([name, value]) => {
    const input = form.elements.namedItem(name);
    return !input || norm(input.value) === norm(value);
  });
  const current = pairs => pairs.map(([name]) => {
    const input = form.elements.namedItem(name);
    if (!input) return '';
    if (input instanceof RadioNodeList) {
      const radio = Array.from(input).find(r => r.checked);
      return radio ? (radio.labels[0]?.textContent.trim() || radio.value) : '';
    }
    if (input.tagName === 'SELECT') return input.selectedOptions[0]?.textContent.trim() || '';
    return input.value;
  }).filter(Boolean).join(', ') || '—';
  const changed = target => {
    target.dispatchEvent(new Event('input', { bubbles: true }));
    target.dispatchEvent(new Event('change', { bubbles: true }));
  };
  const fill = pairs => pairs.forEach(([name, value]) => {
    const input = form.elements.namedItem(name);
    if (!input) return;
    input.value = value;
    changed(input instanceof RadioNodeList ? input[0] : input);
  });
  // A difference of input values, or of lines in a text area: the lines of
  // each side come with their unchanged neighbours, to be found and swapped.
  const view = (c, fields) => {
    if (!fields.length) return null;
    if (c.text) {
      const area = fields[0];
      const { saved, mine } = c.text;
      const swap = (from, to) => {
        const value = norm(area.value);
        if (!value.includes(from)) return;
        area.value = value.replace(from, () => to);
        changed(area);
      };
      return {
        state: () => {
          const value = norm(area.value);
          const onSaved = value.includes(saved);
          const onMine = value.includes(mine);
          if (onSaved && onMine) return saved.includes(mine) ? 'saved' : 'mine';
          return onSaved ? 'saved' : onMine ? 'mine' : 'edited';
        },
        toSaved: () => swap(mine, saved),
        toMine: () => swap(saved, mine),
        current: null,
      };
    }
    if (!c.restore.length) return null;
    return {
      state: () => (shows(c.saved) ? 'saved' : shows(c.restore) ? 'mine' : 'edited'),
      toSaved: () => fill(c.saved),
      toMine: () => fill(c.restore),
      current: () => current(c.restore),
    };
  };

  add(box, 'p', 'mt-2 mb-0', review.kind === 'families' ? t.families
    : t.kept + ' ' + (review.conflicts.length ? t.red : t.reread));
  if (!review.snapshot) add(box, 'p', 'mt-2 mb-0', t.nosnap);

  if (review.changes.length) {
    add(box, 'h6', 'mt-3', t.saved);
    const list = add(box, 'ul', 'mb-0');
    review.changes.forEach(c => {
      const item = add(list, 'li');
      add(item, 'span', 'fw-bold', c.what + t.colon + ' ');
      if (c.before) add(item, 'del', 'text-body-secondary review-value', c.before);
      if (c.before && c.after) item.append(' → ');
      if (c.after) add(item, 'ins', 'review-value', c.after);
    });
  }

  if (review.conflicts.length) add(box, 'h6', 'mt-3', t.check);
  const rows = review.conflicts.map((c, i) => {
    const fields = c.selector ? Array.from(form.querySelectorAll(c.selector)) : [];
    fields.forEach(f => f.classList.add('border-danger'));
    const row = add(box, 'div', 'border-top pt-2 mt-2');
    add(row, 'div', 'fw-bold', c.what);
    const saved = line(row, t.saved, c.base);
    const mine = line(row, t.mine, c.mine);
    const edited = line(row, t.current, '');
    edited.parentNode.classList.add('d-none');
    const hand = add(row, 'div', 'small fst-italic d-none', t.hand);
    const actions = add(row, 'div', 'd-flex flex-wrap align-items-center gap-2 mt-1');
    const v = view(c, fields);
    const buttons = v ? [[t.restoreSaved, 'saved', v.toSaved], [t.restore, 'mine', v.toMine]]
      .map(([text, state, apply]) => {
        const button = add(actions, 'button', 'btn btn-sm btn-outline-primary', text);
        button.type = 'button';
        button.addEventListener('click', apply);
        return [button, state];
      }) : [];
    const check = add(actions, 'div', 'form-check mb-0 ms-2');
    const input = add(check, 'input', 'form-check-input');
    input.type = 'checkbox';
    input.id = 'review_check' + i;
    const label = add(check, 'label', 'form-check-label', t.checked);
    label.htmlFor = input.id;
    input.addEventListener('change', () => {
      fields.forEach(f => f.classList.toggle('border-danger', !input.checked));
    });
    const strike = ['text-decoration-line-through', 'opacity-50'];
    const refresh = () => {
      if (!v) return;
      const state = v.state();
      // Lines edited by hand can no longer be found to be swapped.
      const blocked = state === 'edited' && !v.current;
      buttons.forEach(([button, shown]) => { button.disabled = blocked || state === shown; });
      hand.classList.toggle('d-none', !blocked);
      strike.forEach(s => {
        saved.classList.toggle(s, state !== 'saved');
        mine.classList.toggle(s, state !== 'mine');
      });
      edited.parentNode.classList.toggle('d-none', state !== 'edited' || !v.current);
      if (state === 'edited' && v.current) edited.textContent = v.current();
    };
    refresh();
    return { input, refresh };
  });
  if (!rows.length) return;

  const refresh = () => rows.forEach(row => row.refresh());
  form.addEventListener('input', refresh);
  form.addEventListener('change', refresh);

  const pending = add(box, 'p', 'text-danger fw-bold mt-3 mb-0 d-none', t.pending);
  const ready = () => {
    const done = rows.every(row => row.input.checked);
    pending.classList.toggle('d-none', done);
    if (!done) box.scrollIntoView({ behavior: 'smooth', block: 'start' });
    return done;
  };
  form.addEventListener('submit', e => {
    if (!ready()) e.preventDefault();
  });
  // The forms also submit on Enter through form.submit(), which skips the
  // submit event.
  const submit = form.submit.bind(form);
  form.submit = () => {
    if (ready()) submit();
  };
})();
