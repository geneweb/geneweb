// Reorder window (updateOrder.txt): the [data-key] items of .reorder-list
// move by drag and drop (html5sortable, loaded just before by js.txt) or
// with their up and down buttons. Saving posts the keys in the new order
// (o) and in the order read at opening (prev) to data-reorder, then
// reloads the page, where js_notif.txt shows the notice kept in
// sessionStorage; a failure is shown in the window, which stays open.
(() => {
  'use strict';
  document.querySelectorAll('.modal[data-reorder]').forEach(modal => {
    const d = modal.dataset;
    const list = modal.querySelector('.reorder-list');
    const save = modal.querySelector('.reorder-save');
    const box = modal.querySelector('.reorder-alert');
    const items = () => list.querySelectorAll(':scope > [data-key]');
    const order = () => Array.from(items(), it => it.dataset.key).join(',');
    const start = Array.from(items());
    const prev = order();
    const refresh = () => {
      const all = items();
      all.forEach((it, i) => {
        it.querySelector('.reorder-up').disabled = i === 0;
        it.querySelector('.reorder-down').disabled = i === all.length - 1;
      });
      save.disabled = order() === prev;
    };
    const fail = msg => {
      box.innerHTML = msg || d.failed;
      box.classList.remove('d-none');
      refresh();
    };
    list.addEventListener('click', ev => {
      const btn = ev.target.closest('.reorder-up, .reorder-down');
      if (!btn) return;
      const it = btn.closest('[data-key]');
      const up = btn.classList.contains('reorder-up');
      const sib = up ? it.previousElementSibling : it.nextElementSibling;
      if (!sib) return;
      if (up) it.after(sib); else it.before(sib);
      refresh();
      if (btn.disabled) {
        it.querySelector(up ? '.reorder-down' : '.reorder-up').focus();
      }
    });
    list.addEventListener('sortupdate', refresh);
    modal.addEventListener('hidden.bs.modal', () => {
      list.append(...start);
      box.classList.add('d-none');
      refresh();
    });
    save.addEventListener('click', () => {
      const q = d.prefix.indexOf('?');
      save.disabled = true;
      fetch(q < 0 ? d.prefix : d.prefix.slice(0, q), {
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
          'Accept': 'application/json'
        },
        body: (q < 0 ? '' : d.prefix.slice(q + 1)) + d.reorder
          + '&o=' + encodeURIComponent(order())
          + '&prev=' + encodeURIComponent(prev)
      })
        .then(r => r.ok ? r.json() : Promise.reject(r.status))
        .then(data => {
          if (!data.success) {
            fail(data.message);
            return;
          }
          try {
            sessionStorage.setItem('gw_notif', JSON.stringify(
              [{ level: 'success', message: data.message, autohide: true }]));
          } catch (e) { /* no storage: reload without the notice */ }
          location.reload();
        })
        .catch(() => fail(''));
    });
    if (typeof sortable === 'function') {
      sortable(list, {
        items: '[data-key]',
        placeholderClass: 'bg-warning-subtle border border-warning',
        draggingClass: 'opacity-50',
        forcePlaceholderSize: true
      });
    }
    refresh();
  });
})();