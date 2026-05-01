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

function bindGroup(opts) {
        const { fn, sn, oc, gate, link } = opts;
        if (!fn || !sn || !oc) return;
        const inputs = [fn, sn, oc];
        const isActive = () => !gate || gate.value === 'link';

        function paint(state) {
            inputs.forEach(el => {
                el.classList.toggle('row-invalid', state === 'invalid');
                if (state === 'flash') {
                    el.classList.add('row-valid-flash');
                    setTimeout(
                        () => el.classList.remove('row-valid-flash'), 900);
                }
            });
            if (link) {
                if (state === 'invalid') {
                    link.classList.add('disabled', 'opacity-50');
                    link.setAttribute('aria-disabled', 'true');
                    link.setAttribute('tabindex', '-1');
                } else if (state === 'valid' || state === 'flash') {
                    link.classList.remove('disabled', 'opacity-50');
                    link.removeAttribute('aria-disabled');
                    link.removeAttribute('tabindex');
                }
            }
        }

        function clear() {
            inputs.forEach(el => el.classList.remove('row-invalid'));
        }

        function run(flash) {
            if (!isActive()) { clear(); return Promise.resolve(); }
            const f = fn.value.trim();
            const s = sn.value.trim();
            const o = (oc.value || '0').trim() || '0';
            if (!f || !s) {
                paint('invalid');
                return Promise.resolve();
            }
            const sig = f + '|' + s + '|' + o;
            if (gate && gate.dataset.pnocChecked === sig) return Promise.resolve();
            return checkExact(f, s, parseInt(o, 10)).then(ok => {
                if (gate) gate.dataset.pnocChecked = sig;
                paint(ok ? (flash ? 'flash' : 'valid') : 'invalid');
            });
        }

        inputs.forEach(el => el.addEventListener('focusout', () => run(true)));
        if (gate) {
            gate.addEventListener('change', () => run(false));
        }
        run(false);
    }

    function autoBindGroups() {
        document.querySelectorAll('select[id$="_p_selct"]').forEach(gate => {
            const pre = gate.id.slice(0, -'_p_selct'.length);
            bindGroup({
                fn:   document.getElementById(pre + '_fn'),
                sn:   document.getElementById(pre + '_sn'),
                oc:   document.getElementById(pre + '_occ'),
                gate,
                link: document.querySelector(
                    '#' + pre + '_p_selct_mod a')
            });
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