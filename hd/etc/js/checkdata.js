const CheckData = (() => {
  'use strict';
  
  // Cache DOM et constantes
  const CACHE = new WeakMap();
  const VALIDATING = new Set();
  const RAF = requestAnimationFrame;
  
  // Cache des éléments fréquemment accédés
  let _container = null;
  let _okTitle = '';
  
  // Optimisation: pré-compiler les sélecteurs
  const SELECTORS = {
    err: '.err',
    editContainer: '.edit-container',
    s2: 'a.s2',
    button: 'button:not([data-action])',
    inputField: 'input, textarea',
    disabled: 'disabled',
    validated: 'validated',
    editing: 'editing'
  };

  // Fonctions utilitaires optimisées (éviter confusion avec jQuery)
  const q = (sel, ctx) => (ctx || document).querySelector(sel);
  const qa = (sel, ctx) => Array.from((ctx || document).querySelectorAll(sel));
  
  const isVisible = el => {
    if (!el) return false;
    const s = getComputedStyle(el);
    return s.visibility !== 'hidden' && s.display !== 'none';
  };

  // Gestion des champs d'édition
  const createField = (isTextarea, value) => {
    const f = document.createElement(isTextarea ? 'textarea' : 'input');
    f.className = 'form-control';
    f.value = value;
    if (!isTextarea) f.type = 'text';
    return f;
  };

  const createContainer = (btn, field) => {
    const c = document.createElement('div');
    c.className = SELECTORS.editContainer.slice(1);
    
    const orig = document.createElement('div');
    orig.className = 'original-content';
    orig.innerHTML = btn.innerHTML;
    
    c.appendChild(orig);
    c.appendChild(field);
    CACHE.set(c, { btn });
    
    return c;
  };

  // Handlers optimisés
  const handleClick = e => {
    const t = e.target;
    const err = t.closest(SELECTORS.err);
    
    if (err?.classList.contains(SELECTORS.disabled) && !t.closest(SELECTORS.s2)) {
      e.preventDefault();
      return;
    }
    
    const btn = t.closest(SELECTORS.button);
    if (btn?.closest(SELECTORS.err)) {
      e.preventDefault();
      showEditInput(btn);
      return;
    }
    
    const s2 = t.closest(SELECTORS.s2);
    if (s2?.closest(SELECTORS.err)) {
      e.preventDefault();
      validateEntry(s2, s2.closest(SELECTORS.err));
    }
  };

  const handleKeydown = e => {
    const key = e.key;
    if (key !== 'ArrowDown' && key !== 'ArrowUp') return;
    
    const err = document.activeElement?.closest(SELECTORS.err);
    if (err && !$(SELECTORS.editContainer, err)) {
      e.preventDefault();
      navigate(err, key === 'ArrowUp');
    }
  };

  const showEditInput = btn => {
    const err = btn.closest(SELECTORS.err);
    if (err.classList.contains(SELECTORS.editing)) return;
    
    const val = err.dataset.ori || '';
    const s2 = q(SELECTORS.s2, err);
    const hidden = s2?.style.visibility === 'hidden';
    
    if (s2 && !s2.dataset.origHidden) {
      s2.dataset.origHidden = hidden ? '1' : '0';
    }
    
    err.classList.add(SELECTORS.editing);
    
    const field = createField(val.length > 115 || val.includes('\n'), val);
    const container = createContainer(btn, field);
    
    btn.replaceWith(container);
    
    RAF(() => {
      field.focus();
      field.setSelectionRange(val.length, val.length);
      if (field.tagName === 'TEXTAREA' && window.autosize) autosize(field);
    });
    
    setupField(field, container, s2, val, hidden);
  };

  const setupField = (field, container, s2, origVal, wasHidden) => {
    // Cache des attributs
    const attrs = { href: s2?.href, title: s2?.title || '' };
    let stored = false;
    
    // Input handler optimisé
    const handleInput = () => {
      if (!s2) return;
      
      const newVal = field.value.trim();
      const changed = newVal !== origVal && newVal !== '';
      
      if (!stored) {
        s2.dataset.origHref = attrs.href;
        s2.dataset.origTitle = attrs.title;
        stored = true;
      }
      
      RAF(() => {
        if (changed) {
          const h = s2.dataset.origHref || attrs.href;
          s2.href = h.includes('&s2=') 
            ? h.replace(/(&s2=)[^&]*/, `$1${encodeURIComponent(newVal)}`)
            : `${h}&s2=${encodeURIComponent(newVal)}`;
          s2.className = 's2 btn btn-warning';
          s2.style.visibility = 'visible';
          s2.title = _okTitle;
        } else {
          s2.href = attrs.href;
          s2.className = 's2 btn btn-success';
          s2.style.visibility = wasHidden ? 'hidden' : 'visible';
          s2.title = attrs.title;
        }
      });
    };
    
    // Keyboard handler optimisé
    const handleKey = e => {
      const key = e.key;
      const err = container.closest(SELECTORS.err);
      
      if (key === 'Enter') {
        e.preventDefault();
        if (s2?.style.visibility !== 'hidden') {
          s2.focus();
          validateEntry(s2, err);
        }
      } else if (key === 'Escape') {
        e.preventDefault();
        cancelEdit(container, s2, wasHidden);
      } else if (key === 'ArrowDown' || key === 'ArrowUp') {
        e.preventDefault();
        navigate(err, key === 'ArrowUp');
      }
    };
    
    // Blur handler
    const handleBlur = e => {
      if (e.relatedTarget === s2) return;
      setTimeout(() => {
        if (document.activeElement !== field && document.activeElement !== s2) {
          cancelEdit(container, s2, wasHidden);
        }
      }, 200);
    };
    
    field.addEventListener('input', handleInput);
    field.addEventListener('keydown', handleKey);
    field.addEventListener('blur', handleBlur);
  };

  const cancelEdit = (container, s2, wasHidden) => {
    const err = container?.closest(SELECTORS.err);
    if (!err) return;
    
    err.classList.remove(SELECTORS.editing);
    
    const cache = CACHE.get(container);
    if (cache?.btn && container.parentNode) {
      try {
        container.replaceWith(cache.btn.cloneNode(true));
      } catch {}
    }
    
    if (s2 && !s2.classList.contains('btn-info')) {
      s2.className = 's2 btn btn-success';
      if (s2.dataset.origHref) {
        s2.href = s2.dataset.origHref;
        s2.title = s2.dataset.origTitle || '';
      }
    }
  };

  const navigate = (current, goUp = false) => {
    if (!current) return;
    
    if (!current.classList.contains(SELECTORS.validated)) {
      const c = q(SELECTORS.editContainer, current);
      if (c) {
        const s2 = q(SELECTORS.s2, current);
        cancelEdit(c, s2, s2?.dataset.origHidden === '1');
      }
    }
    
    const all = qa(SELECTORS.err);
    const idx = all.indexOf(current);
    const step = goUp ? -1 : 1;
    
    let next;
    for (let i = idx + step; goUp ? i >= 0 : i < all.length; i += step) {
      if (!all[i].classList.contains(SELECTORS.disabled)) {
        next = all[i];
        break;
      }
    }
    
    if (!next) return;
    
    const s2 = q(SELECTORS.s2, next);
    const auto = s2?.classList.contains('btn-info');
    
    RAF(() => {
      if (auto && isVisible(s2)) {
        s2.focus();
        s2.scrollIntoView({ behavior: 'smooth', block: 'center' });
      } else {
        const btn = q(SELECTORS.button, next);
        if (btn) {
          btn.click();
          btn.scrollIntoView({ behavior: 'smooth', block: 'center' });
        }
      }
    });
  };

  const validateEntry = async (s2, errEl) => {
    if (!s2 || !errEl) return;
    
    const key = s2.href;
    if (VALIDATING.has(key)) return;
    VALIDATING.add(key);
    
    const orig = s2.innerHTML;
    s2.innerHTML = '<i class="fa fa-spinner fa-spin"></i>';
    s2.className = 's2 btn btn-info';
    
    let val = null;
    const c = q(SELECTORS.editContainer, errEl);
    if (c) {
      const f = q(SELECTORS.inputField, c);
      if (f) val = f.value.trim();
    }
    
    if (!val) {
      const p = new URLSearchParams(s2.href.split('?')[1]);
      val = p.get('s2');
    }
    
    try {
      const r = await fetch(`${s2.href}&ajax`, {
        method: 'GET',
        headers: {
          'Accept': 'application/json',
          'X-Requested-With': 'XMLHttpRequest'
        }
      });
      
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      
      const res = await r.json();
      
      if (res.success) {
        notify('success', `✓ ${res.message}`);
        completeValidation(errEl, s2, res.after || val);
        setTimeout(() => navigate(errEl), 300);
      } else {
        notify('error', `✗ ${res.message}`);
        s2.innerHTML = '<i class="fa fa-exclamation-triangle"></i>';
        s2.className = 's2 btn btn-danger';
        s2.title = res.message;
        
        setTimeout(() => {
          s2.innerHTML = orig;
          s2.className = 's2 btn btn-warning';
        }, 2000);
      }
    } catch (e) {
      console.error('AJAX error:', e);
      s2.innerHTML = orig;
      s2.className = 's2 btn btn-warning';
      window.open(s2.href, '_blank');
    } finally {
      VALIDATING.delete(key);
    }
  };

  const completeValidation = (errEl, s2, val) => {
    RAF(() => {
      s2.innerHTML = '<i class="fa fa-spell-check"></i>';
      s2.className = 's2 btn btn-success';
      s2.style.pointerEvents = 'none';
      s2.style.visibility = 'visible';
      
      let c = q(SELECTORS.editContainer, errEl);
      const btn = q(SELECTORS.button, errEl);
      
      if (c) {
        const f = q(SELECTORS.inputField, c);
        if (f && val) {
          const txt = document.createElement('div');
          txt.className = 'text-muted';
          txt.textContent = val;
          f.replaceWith(txt);
        }
      } else if (btn && val) {
        c = document.createElement('div');
        c.className = SELECTORS.editContainer.slice(1);
        
        const orig = document.createElement('div');
        orig.className = 'original-content';
        orig.innerHTML = btn.innerHTML;
        c.appendChild(orig);
        
        const txt = document.createElement('div');
        txt.className = 'text-muted';
        txt.textContent = val;
        c.appendChild(txt);
        
        btn.replaceWith(c);
      }
      
      errEl.classList.add(SELECTORS.disabled, SELECTORS.validated);
      errEl.classList.remove(SELECTORS.editing);
    });
  };

  const notify = (type, msg, duration = 4000) => {
    qa('.ajax-notification').forEach(n => n.remove());
    
    const n = document.createElement('div');
    n.className = `alert alert-${type === 'success' ? 'success' : 'danger'} ajax-notification`;
    n.innerHTML = `
      <div class="d-flex align-items-center">
        <span class="flex-grow-1">${msg}</span>
        <button type="button" class="close ml-2">&times;</button>
      </div>`;
    
    const remove = () => {
      n.classList.add('removing');
      n.addEventListener('animationend', () => n.remove(), { once: true });
    };
    
    const t = setTimeout(remove, duration);
    q('.close', n).onclick = () => {
      clearTimeout(t);
      remove();
    };
    
    document.body.appendChild(n);
  };

  // Fonctions publiques
  return {
    init() {
      // Initialiser les fonctions qui doivent marcher même sans résultats
      this.initToggles();
      this.initMaxValidation();
      this.preserveScroll();
      
      // Initialiser le container et les handlers seulement s'il existe
      _container = q('#cd');
      if (!_container) return;
      
      _okTitle = _container.dataset.okTitle || '';
      
      // Event delegation pour les résultats
      _container.addEventListener('click', handleClick, { passive: false });
      _container.addEventListener('keydown', handleKeydown, { passive: false });
    },
    
    initToggles() {
      const setupToggle = (action, prefix) => {
        const btn = q(`[data-action="${action}"]`);
        if (btn && !btn.dataset.init) {
          btn.dataset.init = '1';
          btn.addEventListener('click', e => {
            e.preventDefault();
            const boxes = qa(`[name^="${prefix}"]`);
            const all = boxes.every(b => b.checked);
            boxes.forEach(b => b.checked = !all);
          });
        }
      };
      
      setupToggle('toggle-dicts', 'd_');
      setupToggle('toggle-errors', 'e_');
      
      const submit = q('[data-action="validate-submit"]');
      if (submit && !submit.dataset.init) {
        submit.dataset.init = '1';
        submit.addEventListener('click', () => {
          if (typeof showOverlay === 'function') showOverlay();
        });
      }
    },
    
    initMaxValidation() {
      const max = q('input[name="max"]');
      if (!max) return;
      
      const limit = max.getAttribute('max');
      if (!limit) return;
      
      max.type = 'number';
      max.min = '1';
      max.addEventListener('input', e => {
        const v = parseInt(e.target.value);
        const l = parseInt(limit);
        e.target.setCustomValidity(v > l ? `Limité à ${l} résultats` : '');
        e.target.reportValidity();
      });
    },
    
    preserveScroll() {
      window.addEventListener('beforeunload', () => {
        sessionStorage.setItem('checkDataScroll', window.scrollY);
      });
      
      const saved = sessionStorage.getItem('checkDataScroll');
      if (saved) {
        RAF(() => {
          window.scrollTo(0, parseInt(saved));
          sessionStorage.removeItem('checkDataScroll');
        });
      }
    }
  };
})();