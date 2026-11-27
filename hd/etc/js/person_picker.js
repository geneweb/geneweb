(function () {
    'use strict';
    if (typeof window.PersonPicker !== 'undefined') return;

    function debounce(fn, ms) {
        let t;
        return function () {
            const ctx = this, args = arguments;
            clearTimeout(t);
            t = setTimeout(() => fn.apply(ctx, args), ms);
        };
    }

    function url(qs) {
        const base = window.GW && GW.prefix || '';
        const sep = base.endsWith('?') || base.endsWith('&') ? '' :
                    base.indexOf('?') >= 0 ? '&' : '?';
        return base + sep + qs;
    }

    function endpoint(qs) {
        return fetch(url('m=PNOC_LOOKUP&' + qs))
            .then(r => r.ok && (r.headers.get('content-type') || '').indexOf('json') >= 0
                ? r.json() : [])
            .catch(() => []);
    }

    function lookup(q, limit) {
        return endpoint('q=' + encodeURIComponent(q) +
            '&n=' + (limit || 20))
            .then(r => Array.isArray(r) ? { persons: [], truncated: false } : r);
    }

    function checkExact(fn, sn, oc) {
        if (!fn || !sn) return Promise.resolve(null);
        return endpoint('exact=1&fn=' + encodeURIComponent(fn) +
            '&sn=' + encodeURIComponent(sn) + '&oc=' + (oc || 0))
            .then(arr => arr.length > 0 ? arr[0] : null);
    }

    function wikiKey(p) {
        return '[[' + p.fn + '/' + p.sn + (p.oc ? '/' + p.oc : '') + ']]';
    }

    // Search dialog of person_picker.txt
    let dialog = null;
    let onPicked = null;
    let picked = null;
    let seq = 0;

    function ind(p) {
        const node = document.getElementById('pp-ind')
            .content.firstElementChild.cloneNode(true);
        const name = node.querySelector('.pp-name');
        name.textContent = p.name;
        name.href = url(p.acc);
        const dates = node.querySelector('.pp-dates');
        if (!p.dates) dates.remove();
        else {
            dates.textContent = p.dates;
            if (dates.tagName === 'A') dates.href = url('m=MOD_IND&i=' + p.i);
        }
        return node;
    }

    function render(res, short) {
        const row = document.getElementById('pp-row').content.firstElementChild;
        dialog.querySelector('tbody').replaceChildren(...res.persons.map(p => {
            const tr = row.cloneNode(true);
            tr.querySelector('.pp-self').append(ind(p));
            if (p.father) tr.querySelector('.pp-father').append(ind(p.father));
            if (p.mother) tr.querySelector('.pp-mother').append(ind(p.mother));
            tr.querySelector('.pp-spouses').append(...p.spouses.map(ind));
            tr.querySelector('.pp-pick').addEventListener('click', () => {
                picked = p;
                bootstrap.Modal.getOrCreateInstance(dialog).hide();
            });
            return tr;
        }));
        dialog.querySelector('.pp-results').classList.toggle('d-none', short);
        dialog.querySelector('.pp-none').classList.toggle('d-none',
            short || res.persons.length > 0);
        dialog.querySelector('.pp-truncated').classList.toggle('d-none',
            !res.truncated);
    }

    function search() {
        const q = dialog.querySelector('.pp-query').value.trim();
        const n = ++seq;
        // a link key is searched once complete
        if (q.length < 2 || q.startsWith('[[') && !q.endsWith(']]')) {
            render({ persons: [], truncated: false }, true);
            return;
        }
        lookup(q, 200).then(res => { if (n === seq) render(res, false); });
    }

    function open(query, onPick) {
        if (!dialog) {
            dialog = document.getElementById('pp-modal');
            if (!dialog) return;
            const input = dialog.querySelector('.pp-query');
            input.addEventListener('input', debounce(search, 250));
            input.addEventListener('keydown', e => {
                if (e.key === 'Enter') {
                    e.preventDefault();
                    search();
                }
            });
            dialog.addEventListener('shown.bs.modal', () => input.select());
            // after the focus trap of the dialog is released
            dialog.addEventListener('hidden.bs.modal', () => {
                if (picked && onPicked) onPicked(picked);
                picked = null;
            });
        }
        onPicked = onPick;
        picked = null;
        if (query !== undefined) dialog.querySelector('.pp-query').value = query;
        search();
        bootstrap.Modal.getOrCreateInstance(dialog).show();
    }

    function flashValid(el) {
        el.classList.add('row-valid-flash');
        setTimeout(() => el.classList.remove('row-valid-flash'), 900);
    }

    function bindGroup(opts) {
        const { fn, sn, oc, gate, link } = opts;
        if (!fn || !sn || !oc) return;
        if (gate && gate.dataset.pnocBound) return;
        if (gate) gate.dataset.pnocBound = '1';
        const inputs = [fn, sn, oc];
        const isActive = () => !gate || gate.value === 'link';

        function paint(state) {
            const invalid = state === 'invalid';
            const flash = state === 'flash';
            inputs.forEach(el => {
                el.classList.toggle('row-invalid', invalid);
                if (flash) flashValid(el);
            });
            if (link) {
                link.classList.toggle('disabled', invalid);
                link.classList.toggle('opacity-50', invalid);
                if (invalid) {
                    link.setAttribute('aria-disabled', 'true');
                    link.setAttribute('tabindex', '-1');
                } else {
                    link.removeAttribute('aria-disabled');
                    link.removeAttribute('tabindex');
                }
            }
        }

        function run(flash) {
            const f = fn.value.trim();
            const s = sn.value.trim();
            const o = (oc.value || '0').trim() || '0';
            if (!isActive() || !f || !s) {
                paint('neutral');
                if (gate) delete gate.dataset.pnocChecked;
                return Promise.resolve();
            }
            const sig = f + '|' + s + '|' + o;
            if (gate && gate.dataset.pnocChecked === sig) {
                return Promise.resolve();
            }
            return checkExact(f, s, o).then(p => {
                const ok = !!p;
                if (gate) gate.dataset.pnocChecked = sig;
                paint(ok ? (flash ? 'flash' : 'valid') : 'invalid');
            });
        }

        inputs.forEach(el =>
            el.addEventListener('focusout', () => run(true)));
        if (gate) gate.addEventListener('change', e => run(e.isTrusted));
        run(false);
    }

    function autoBindGroups() {
        document.querySelectorAll('select[id$="_p_selct"]').forEach(gate => {
            const pre = gate.id.slice(0, -'_p_selct'.length);
            bindGroup({
                fn: document.getElementById(pre + '_fn'),
                sn: document.getElementById(pre + '_sn'),
                oc: document.getElementById(pre + '_occ'),
                gate,
                link: document.querySelector(
                    '#' + pre + '_p_selct_mod a')
            });
        });
    }

    /**
     * Parses a GeneWeb key string. Three formats:
     *  - "fn.occ sn"  (canonical)
     *  - "fn.occ.sn"  (name of the key directory of the carrousel)
     *  - "fn sn"      (last-space split, oc=0)
     * The bare fallback does not consult base particles, so
     * "de la Croix" parses as fn="de la" sn="Croix". Users with
     * particle surnames should use the canonical form.
     */
    function parseGeneWebKey(s) {
        if (!s) return null;
        const t = s.trim();
        if (!t) return null;
        const m = t.match(/^(.+)\.(\d+)\s+(.+)$/);
        if (m) return {
            fn: m[1].trim(), oc: parseInt(m[2], 10), sn: m[3].trim()
        };
        const d = t.match(/^(.+)\.(\d+)\.(\S.*)$/);
        if (d) return {
            fn: d[1].trim(), oc: parseInt(d[2], 10), sn: d[3].trim()
        };
        const sp = t.lastIndexOf(' ');
        if (sp < 0) return null;
        const fn = t.substring(0, sp).trim();
        const sn = t.substring(sp + 1).trim();
        if (!fn || !sn) return null;
        return { fn, sn, oc: 0 };
    }

    window.PersonPicker = {
        open, url, lookup, checkExact, wikiKey, parseGeneWebKey, bindGroup
    };

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', autoBindGroups);
    } else {
        autoBindGroups();
    }
})();