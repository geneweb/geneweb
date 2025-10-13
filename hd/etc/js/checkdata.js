const CheckData = (() => {
  'use strict';
  
  const CACHE = new WeakMap();
  const VALIDATING = new Set();
  const RAF = requestAnimationFrame;
  
  let _container = null;
  let _okTitle = '';
  let _errorsCache = null;
  let _cacheInvalid = true;
  
  const CONFIG = {
    TEXTAREA_THRESHOLD: 115,
    NAV_DELAY: 300,
    POLL_INTERVAL: 500,
    MAX_POLLS: 120,
    NOTIFY_DURATION: 4000
  };
  
  const SELECTORS = {
    err: '.err',
    editContainer: '.edit-container',
    bk: '.bk',
    pl: ".pl",
    s2: '.s2',
    button: 'button:not([data-action])',
    inputField: 'input, textarea',
    disabled: 'disabled',
    validated: 'validated',
    editing: 'editing'
  };

  const q = (sel, ctx) => (ctx || document).querySelector(sel);
  const qa = (sel, ctx) => Array.from((ctx || document).querySelectorAll(sel));
  
  const sanitize = str => {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
  };
  
  const isVisible = el => {
    if (!el) return false;
    const s = getComputedStyle(el);
    return s.visibility !== 'hidden' && s.display !== 'none';
  };

  const getUrlParams = href => {
    const url = new URL(href);
    return {
      k: url.searchParams.get('k'),
      s: url.searchParams.get('s'),
      s2: url.searchParams.get('s2')
    };
  };

  const invalidateCache = () => { _cacheInvalid = true; };

  const getAllErrors = () => {
    if (_cacheInvalid) {
      _errorsCache = _container ? qa(SELECTORS.err, _container) : [];
      _cacheInvalid = false;
    }
    return _errorsCache;
  };

  const initButtons = () => {
    if (!_container) return;
    
    const buttonConfigs = [
      { sel: SELECTORS.bk, cls: 'btn btn-primary', 
        icon: 'fa-book fa-xs' },
      { sel: SELECTORS.pl, cls: 'btn btn-success', 
        icon: 'fa-users fa-xs', target: true },
      { sel: SELECTORS.s2, cls: 'btn btn-info', 
        icon: 'fa-check', target: true }
    ];
    
    buttonConfigs.forEach(cfg => {
      qa(cfg.sel, _container).forEach(btn => {
        if (btn.dataset.init) return;
        btn.className = cfg.sel.slice(1) + ' ' + cfg.cls;
        if (!btn.innerHTML.includes('<i')) {
          btn.innerHTML = `<i class="fa ${cfg.icon}"></i>`;
        }
        if (cfg.target) btn.target = '_blank';
        btn.dataset.init = '1';
      });
    });
  };

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
    orig.textContent = btn.textContent;
    
    c.appendChild(orig);
    c.appendChild(field);
    CACHE.set(c, { btn: btn.cloneNode(true) });
    
    return c;
  };

  const handleClick = e => {
    const t = e.target;
    const err = t.closest(SELECTORS.err);
    
    if (err?.classList.contains(SELECTORS.disabled) && 
        !t.closest(SELECTORS.s2)) {
      e.preventDefault();
      return;
    }
    
    const pl = t.closest(SELECTORS.pl);
    if (pl?.closest(SELECTORS.err)) {
      e.preventDefault();
      handlePersonList(pl);
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
    if (err && !q(SELECTORS.editContainer, err)) {
      e.preventDefault();
      navigate(err, key === 'ArrowUp');
    }
  };

  const handlePersonList = btn => {
    const err = btn.closest(SELECTORS.err);
    const s2Btn = q(SELECTORS.s2, err);
    const url = new URL(s2Btn.href);
    const d = url.searchParams.get('d');
    const s = url.searchParams.get('s');
    const k = url.searchParams.get('k');
    const baseUrl = `${url.origin}${url.pathname}`;
    const newUrl = `${baseUrl}?m=CHK_DATA_L&data=${d}&k=${s}&key=${k}`;
    window.open(newUrl, '_blank');
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
    
    const useTextarea = val.length > CONFIG.TEXTAREA_THRESHOLD || 
                        val.includes('\n');
    const field = createField(useTextarea, val);
    const container = createContainer(btn, field);
    
    btn.replaceWith(container);
    
    RAF(() => {
      field.focus();
      field.setSelectionRange(val.length, val.length);
      if (useTextarea && window.autosize) autosize(field);
    });
    
    setupField(field, container, s2, val, hidden);
  };

  const setupField = (field, container, s2, origVal, wasHidden) => {
    const attrs = { href: s2?.href, title: s2?.title || '' };
    let stored = false;
    
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
    
    const handleBlur = e => {
      if (e.relatedTarget === s2) return;
      setTimeout(() => {
        if (document.activeElement !== field && 
            document.activeElement !== s2) {
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
        container.replaceWith(cache.btn);
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
    
    const all = getAllErrors();
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

  const handleValidationResult = (result, errEl, s2, orig, val) => {
    if (!result) {
      console.error('Invalid validation result:', result);
      notify('error', 'Erreur: données de validation invalides');
      return;
    }
    if (result.success) {
      notify('success', `✓ ${result.message}`);
      completeValidation(errEl, s2, result.after || val);
      
      if (result.nb_modified !== null && result.elapsed_time !== null) {
        const statsDiv = document.createElement('div');
        statsDiv.className = 'small text-center mr-2';
        statsDiv.textContent = `+${result.nb_modified}`;
        statsDiv.insertAdjacentHTML('beforeend', '<br>');
        statsDiv.insertAdjacentText('beforeend', 
                                    `${result.elapsed_time.toFixed(1)} s`);
        
        const s2Btn = errEl.querySelector('.s2');
        if (s2Btn) errEl.insertBefore(statsDiv, s2Btn);
      }
      
      setTimeout(() => navigate(errEl), CONFIG.NAV_DELAY);
    } else {
      notify('error', `✗ ${result.message}`);
      s2.innerHTML = '<i class="fa fa-exclamation-triangle"></i>';
      s2.className = 's2 btn btn-danger';
      s2.title = result.message;
      
      setTimeout(() => {
        s2.innerHTML = orig;
        s2.className = 's2 btn btn-warning';
      }, 2000);
    }
  };

  const validateEntry = async (s2, errEl) => {
    if (!s2 || !errEl) return;
    
    const { k, s, s2: s2Val } = getUrlParams(s2.href);
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
      handleValidationResult(res, errEl, s2, orig, val);
      
    } catch (e) {
      console.error('AJAX error:', e);
      s2.innerHTML = orig;
      s2.className = 's2 btn btn-warning';
      
      const validationKey = `chk_validation_${k}_${s}_${s2Val}`;
      localStorage.removeItem(validationKey);
      
      const popup = window.open(s2.href, '_blank');
      
      let pollCount = 0;
      const checkInterval = setInterval(() => {
        pollCount++;
        const result = localStorage.getItem(validationKey);
        
        if (result) {
          clearInterval(checkInterval);
          localStorage.removeItem(validationKey);
          
          try {
            const data = JSON.parse(result);
            handleValidationResult(data, errEl, s2, orig, val);
            
            if (popup && !popup.closed) popup.close();
          } catch (parseError) {
            console.error('Parse error:', parseError);
          }
        } else if (pollCount >= CONFIG.MAX_POLLS) {
          clearInterval(checkInterval);
          localStorage.removeItem(validationKey);
          notify('error', 'Timeout: validation non reçue');
        }
      }, CONFIG.POLL_INTERVAL);
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
      
      const c = q(SELECTORS.editContainer, errEl);
      const btn = q(SELECTORS.button, errEl);
      
      const createValidatedText = () => {
        const txt = document.createElement('div');
        txt.className = 'text-muted';
        txt.textContent = val;
        return txt;
      };
      
      if (c) {
        const f = q(SELECTORS.inputField, c);
        if (f && val) f.replaceWith(createValidatedText());
      } else if (btn && val) {
        const newC = document.createElement('div');
        newC.className = SELECTORS.editContainer.slice(1);
        
        const orig = document.createElement('div');
        orig.className = 'original-content';
        orig.textContent = btn.textContent;
        newC.appendChild(orig);
        newC.appendChild(createValidatedText());
        
        btn.replaceWith(newC);
      }
      
      errEl.classList.add(SELECTORS.disabled, SELECTORS.validated);
      errEl.classList.remove(SELECTORS.editing);
      invalidateCache();
    });
  };

  const notify = (type, msg, duration = CONFIG.NOTIFY_DURATION) => {
    qa('.ajax-notification').forEach(n => n.remove());
    
    const n = document.createElement('div');
    n.className = 
      `alert alert-${type === 'success' ? 'success' : 'danger'} ajax-notification`;
    
    const content = document.createElement('div');
    content.className = 'd-flex align-items-center';
    
    const msgSpan = document.createElement('span');
    msgSpan.className = 'flex-grow-1';
    msgSpan.innerHTML = msg;
    
    const closeBtn = document.createElement('button');
    closeBtn.type = 'button';
    closeBtn.className = 'close ml-2';
    closeBtn.innerHTML = '&times;';
    
    content.appendChild(msgSpan);
    content.appendChild(closeBtn);
    n.appendChild(content);
    
    const remove = () => {
      n.classList.add('removing');
      n.addEventListener('animationend', () => n.remove(), { once: true });
    };
    
    const t = setTimeout(remove, duration);
    closeBtn.onclick = () => {
      clearTimeout(t);
      remove();
    };
    
    document.body.appendChild(n);
  };

  return {
    init() {
      this.initToggles();
      this.initMaxValidation();
      this.preserveScroll();
      
      _container = q('#cd');
      if (!_container) return;
      
      _okTitle = _container.dataset.okTitle || '';
      
      initButtons();
      
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