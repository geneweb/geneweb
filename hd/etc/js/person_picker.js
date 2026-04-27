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

    function lookup(q, limit) {
        return fetch(GW.prefix + 'm=PNOC_LOOKUP&q=' +
            encodeURIComponent(q) + '&n=' + (limit || 20))
            .then(r => r.ok ? r.json() : []);
    }

    function checkExact(fn, sn, oc) {
        if (!fn || !sn) return Promise.resolve(false);
        return fetch(GW.prefix + 'm=PNOC_LOOKUP&exact=1' +
            '&fn=' + encodeURIComponent(fn) +
            '&sn=' + encodeURIComponent(sn) +
            '&oc=' + (oc || 0))
            .then(r => r.ok ? r.json() : [])
            .then(arr => arr.length > 0);
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
                    ((GW.i18n && GW.i18n.noResult) || 'No results found') +
                    '</div>'
            }
        });
    }

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
        if (!el) return;
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
            return checkExact(k.fn, k.sn, k.oc || 0).then(ok => {
                el.dataset.pnocChecked = sig;
                el.classList.toggle('row-invalid', !ok);
                if (ok && flash) {
                    el.classList.add('row-valid-flash');
                    setTimeout(
                        () => el.classList.remove('row-valid-flash'), 900);
                }
            });
        }
        el.addEventListener('focusout', () => run(true));
        if (el.value) run(false);
    }

    window.PersonPicker = {
        create, lookup, checkExact, bindInput, parseGeneWebKey
    };

    function autoBind() {
        document.querySelectorAll('.pnoc-input').forEach(bindInput);
    }
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', autoBind);
    } else {
        autoBind();
    }
})();