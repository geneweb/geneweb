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

    function endpoint(qs) {
        const base = (window.GW && GW.prefix || '').replace(/\?$/, '?');
        const sep = base.endsWith('?') || base.endsWith('&') ? '' :
                    base.indexOf('?') >= 0 ? '&' : '?';
        return fetch(base + sep + 'm=PNOC_LOOKUP&' + qs)
            .then(r => r.ok && (r.headers.get('content-type') || '').indexOf('json') >= 0
                ? r.json() : [])
            .catch(() => []);
    }

    function lookup(q, limit) {
        return endpoint('q=' + encodeURIComponent(q) +
            '&n=' + (limit || 20));
    }

    function checkExact(fn, sn, oc) {
        if (!fn || !sn) return Promise.resolve(null);
        return endpoint('exact=1&fn=' + encodeURIComponent(fn) +
            '&sn=' + encodeURIComponent(sn) + '&oc=' + (oc || 0))
            .then(arr => arr.length > 0 ? arr[0] : null);
    }

    function flashValid(el) {
        el.classList.add('row-valid-flash');
        setTimeout(() => el.classList.remove('row-valid-flash'), 900);
    }

    function create(el, opts) {
        if (!el || typeof TomSelect === 'undefined') return null;
        opts = opts || {};
        return new TomSelect(el, {
            valueField: 'key',
            labelField: 'label',
            searchField: ['label'],
            create: opts.create !== false,
            maxItems: opts.maxItems || 1,
            preload: false,
            load: debounce((q, cb) => {
                if (!q || q.length < 2) { cb([]); return; }
                lookup(q, opts.limit)
                    .then(arr => arr.map(p => Object.assign({}, p, {
                        key: p.fn + '/' + p.sn + '/' + (p.oc || 0)
                    })))
                    .then(cb).catch(() => cb([]));
            }, 200),
            render: {
                option: (data, escape) =>
                    '<div>' + escape(data.label) + '</div>',
                no_results: () =>
                    '<div class="no-results">' +
                    ((window.GW && GW.i18n && GW.i18n.noResult)
                        || 'No results found') + '</div>'
            }
        });
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
            if (!isActive()) { paint('neutral'); return Promise.resolve(); }
            const f = fn.value.trim();
            const s = sn.value.trim();
            const o = (oc.value || '0').trim() || '0';
            if (!f || !s) { paint('invalid'); return Promise.resolve(); }
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
        if (gate) gate.addEventListener('change', () => run(false));
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
     * Parses a GeneWeb key string. Two formats:
     *  - "fn.occ sn"  (canonical)
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
        const sp = t.lastIndexOf(' ');
        if (sp < 0) return null;
        const fn = t.substring(0, sp).trim();
        const sn = t.substring(sp + 1).trim();
        if (!fn || !sn) return null;
        return { fn, sn, oc: 0 };
    }

    function bindInput(el, options) {
        if (!el || el.dataset.pnocBound) return;
        el.dataset.pnocBound = '1';
        options = options || {};
        const parse = options.parse || parseGeneWebKey;

        function run(flash) {
            const k = parse(el.value);
            if (!k || !k.fn || !k.sn) {
                el.classList.remove('row-invalid');
                delete el.dataset.pnocChecked;
                return Promise.resolve();
            }
            const sig = k.fn + '|' + k.sn + '|' + (k.oc || 0);
            if (el.dataset.pnocChecked === sig) return Promise.resolve();
            return checkExact(k.fn, k.sn, k.oc || 0).then(p => {
                const ok = !!p;
                el.dataset.pnocChecked = sig;
                el.classList.toggle('row-invalid', !ok);
                if (ok && flash) flashValid(el);
            });
        }

        el.addEventListener('focusout', () => run(true));
        if (el.value) run(false);
    }

    window.PersonPicker = {
        create, lookup, checkExact, bindInput, parseGeneWebKey, bindGroup
    };

    function autoBind() {
        document.querySelectorAll('.pnoc-input').forEach(bindInput);
        autoBindGroups();
    }
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', autoBind);
    } else {
        autoBind();
    }
})();