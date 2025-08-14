const CheckDataEditor = {
  init() {
    this.initToggleFunctions();
    this.initErrorHandlers();
    this.initMaxValidation();
    this.preserveScrollPosition();
  },

  initErrorHandlers() {
    const container = document.querySelector('#cd');
    if (!container) return;

    container.addEventListener('click', e => {
      const err = e.target.closest('.err');
      if (err?.classList.contains('disabled') && 
          !e.target.closest('a.s2')) {
        e.preventDefault();
        return;
      }

      const btn = e.target.closest('button:not([data-action])');
      if (btn?.closest('.err')) {
        e.preventDefault();
        this.showEditInput(btn);
      }

      const s2 = e.target.closest('a.s2');
      if (s2?.closest('.err')) {
        e.preventDefault();
        this.validateEntry(s2, s2.closest('.err'));
      }
    });

    container.addEventListener('keydown', e => {
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        const activeEl = document.activeElement;
        const err = activeEl?.closest('.err');
        
        if (err && !err.querySelector('.edit-container')) {
          e.preventDefault();
          this.navigate(err, e.key === 'ArrowUp');
        }
      }
    });
  },

  showEditInput(btn) {
    const err = btn.closest('.err');
    if (err.classList.contains('editing')) return;
    
    const val = err.getAttribute('data-ori') || '';
    const s2 = err.querySelector('.s2');
    const wasHidden = s2?.style.visibility === 'hidden';
    
    if (s2 && !s2.hasAttribute('data-orig-hidden')) {
      s2.setAttribute('data-orig-hidden', wasHidden ? 'true' : 'false');
    }
    
    err.classList.add('editing');
    
    const isTextarea = val.length > 115 || val.includes('\n');
    const field = document.createElement(isTextarea ? 'textarea' : 'input');
    field.className = 'form-control';
    field.value = val;
    if (!isTextarea) field.type = 'text';
    
    const container = document.createElement('div');
    container.className = 'edit-container';
    container.originalButton = btn;
    
    const orig = document.createElement('div');
    orig.className = 'original-content';
    orig.innerHTML = btn.innerHTML;
    container.appendChild(orig);
    container.appendChild(field);
    
    btn.replaceWith(container);
    field.focus();
    field.setSelectionRange(val.length, val.length);
    
    if (isTextarea && window.autosize) autosize(field);
    
    this.setupField(field, container, s2, val, wasHidden);
  },

  setupField(field, container, s2, origVal, wasHidden) {
    const isTextarea = field.tagName === 'TEXTAREA';
    
    field.addEventListener('input', () => {
      if (!s2) return;
      const newVal = field.value.trim();
      const changed = newVal !== origVal && newVal !== '';
      
      if (!s2.hasAttribute('data-orig-href')) {
        s2.setAttribute('data-orig-href', s2.href);
        s2.setAttribute('data-orig-title', s2.title || '');
      }
      
      if (changed) {
        const href = s2.getAttribute('data-orig-href');
        s2.href = href.includes('&s2=') 
          ? href.replace(/(&s2=)[^&]*/, `$1${encodeURIComponent(newVal)}`)
          : href + `&s2=${encodeURIComponent(newVal)}`;
        s2.classList.remove('btn-success', 'btn-info');
        s2.classList.add('btn-warning');
        s2.style.visibility = 'visible';
        s2.title = document.querySelector('#cd')?.getAttribute('data-ok-title') || '';
      } else {
        s2.href = s2.getAttribute('data-orig-href');
        s2.classList.remove('btn-warning', 'btn-info');
        s2.classList.add('btn-success');
        s2.style.visibility = wasHidden ? 'hidden' : 'visible';
        s2.title = s2.getAttribute('data-orig-title');
      }
    });
    
    field.addEventListener('keydown', e => {
      const err = container.closest('.err');
      
      if (e.key === 'Enter') {
        e.preventDefault();
        if (s2?.style.visibility !== 'hidden') {
          s2.focus();
          const err = container.closest('.err');
          this.validateEntry(s2, err);
        }
      }

      else if (e.key === 'Escape') {
        e.preventDefault();
        this.cancelEdit(container, s2, wasHidden);
      }
      else if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        this.navigate(err, e.key === 'ArrowUp');
      }
    });
    
    field.addEventListener('blur', e => {
      if (e.relatedTarget === s2) return;
      setTimeout(() => {
        if (document.activeElement !== field && 
            document.activeElement !== s2) {
          this.cancelEdit(container, s2, wasHidden);
        }
      }, 200);
    });
  },

  cancelEdit(container, s2, wasHidden) {
    const err = container?.closest('.err');
    if (!err) return;
    
    err.classList.remove('editing');
    const origBtn = container.originalButton;
    if (origBtn) {
      container.replaceWith(origBtn.cloneNode(true));
    }
    
    if (s2) {
      if (!s2.classList.contains('btn-info')) {
        s2.classList.remove('btn-warning');
        s2.classList.add('btn-success');
      }
      if (s2.hasAttribute('data-orig-href')) {
        s2.href = s2.getAttribute('data-orig-href');
        s2.title = s2.getAttribute('data-orig-title');
      }
    }
  },

  navigate(current, goUp = false) {
    if (!current) return;
    
    if (!current.classList.contains('validated')) {
      const container = current.querySelector('.edit-container');
      if (container) {
        const s2 = current.querySelector('.s2');
        const wasHidden = s2?.getAttribute('data-orig-hidden') === 'true';
        this.cancelEdit(container, s2, wasHidden);
      }
    }
    
    const allErrors = Array.from(document.querySelectorAll('.err'));
    const currentIdx = allErrors.indexOf(current);
    
    let next = null;
    const direction = goUp ? -1 : 1;
    for (let i = currentIdx + direction; 
         goUp ? i >= 0 : i < allErrors.length; 
         i += direction) {
      if (!allErrors[i].classList.contains('disabled')) {
        next = allErrors[i];
        break;
      }
    }
    
    if (!next) return;
    
    const s2 = next.querySelector('a.s2');
    const isAutoFix = s2?.classList.contains('btn-info');
    
    if (isAutoFix && this.isVisible(s2)) {
      requestAnimationFrame(() => {
        s2.focus();
        s2.scrollIntoView({ behavior: 'smooth', block: 'center' });
      });
    } else {
      const btn = next.querySelector('button:not([data-action])');
      if (btn) {
        requestAnimationFrame(() => {
          btn.click();
          btn.scrollIntoView({ behavior: 'smooth', block: 'center' });
        });
      }
    }
  },

  _validating: new Set(),

  async validateEntry(s2, errElement) {
    if (!s2 || !errElement) return;
    
    const key = s2.href;
    if (this._validating.has(key)) return;
    this._validating.add(key);
    
    const originalContent = s2.innerHTML;
    s2.innerHTML = '<i class="fa fa-spinner fa-spin"></i>';
    s2.classList.add('btn-info');
    s2.classList.remove('btn-warning', 'btn-success', 'btn-danger');
    
    // Récupérer la valeur S2 depuis l'input s'il existe
    let s2Value = null;
    const container = errElement.querySelector('.edit-container');
    if (container) {
      const field = container.querySelector('input, textarea');
      if (field) {
        s2Value = field.value.trim();
      }
    }
    
    // Si pas d'input, extraire S2 de l'URL
    if (!s2Value) {
      const urlParams = new URLSearchParams(s2.href.split('?')[1]);
      s2Value = urlParams.get('s2');
    }
    
    try {
      const ajaxUrl = s2.href + '&ajax';
      
      const response = await fetch(ajaxUrl, {
        method: 'GET',
        headers: {
          'Accept': 'application/json',
          'X-Requested-With': 'XMLHttpRequest'
        }
      });
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }
      
      const result = await response.json();
      
      if (result.success) {
        this.showNotification('success', `✓ ${result.message}`);
        
        const finalValue = result.after || s2Value;
        
        this.completeValidation(errElement, s2, finalValue);
        
        setTimeout(() => {
          this.navigate(errElement, false);
        }, 300);
        
      } else {
        this.showNotification('error', `✗ ${result.message}`);
        
        s2.innerHTML = '<i class="fa fa-exclamation-triangle"></i>';
        s2.classList.remove('btn-info', 'btn-warning', 'btn-success');
        s2.classList.add('btn-danger');
        s2.title = result.message;
        
        setTimeout(() => {
          s2.innerHTML = originalContent;
          s2.classList.remove('btn-danger');
          s2.classList.add('btn-warning');
          s2.title = '';
        }, 2000);
      }
      
    } catch (error) {
      console.error('Erreur AJAX:', error);
      
      s2.innerHTML = originalContent;
      s2.classList.remove('btn-info', 'btn-success', 'btn-danger');
      s2.classList.add('btn-warning');
      
      window.open(s2.href, '_blank');
      
    } finally {
      this._validating.delete(key);
    }
  },

  completeValidation(errElement, s2, correctedValue) {
    s2.innerHTML = '<i class="fa fa-spell-check"></i>';
    s2.classList.remove('btn-info', 'btn-warning', 'btn-danger');
    s2.classList.add('btn-success');
    s2.style.pointerEvents = 'none';
    s2.style.visibility = 'visible';
    
    let container = errElement.querySelector('.edit-container');
    const btn = errElement.querySelector('button:not([data-action])');
    
    if (container) {
      const field = container.querySelector('input, textarea');
      if (field && correctedValue) {
        const s2Text = document.createElement('div');
        s2Text.className = 'text-muted';
        s2Text.textContent = correctedValue;
        field.replaceWith(s2Text);
      }
    } else if (btn && correctedValue) {
      container = document.createElement('div');
      container.className = 'edit-container';
      
      const orig = document.createElement('div');
      orig.className = 'original-content';
      orig.innerHTML = btn.innerHTML;
      container.appendChild(orig);
      
      const s2Text = document.createElement('div');
      s2Text.className = 'text-muted';
      s2Text.textContent = correctedValue;
      container.appendChild(s2Text);
      
      btn.replaceWith(container);
    }
    
    errElement.classList.add('disabled', 'validated');
    errElement.classList.remove('editing');
  },

  showNotification(type, message, duration = 4000) {
    document.querySelectorAll('.ajax-notification')
      .forEach(n => n.remove());
    
    const notification = document.createElement('div');
    const isSuccess = type === 'success';
    notification.className = 
      `alert alert-${isSuccess ? 'success' : 'danger'} ajax-notification`;
    notification.style.cssText = `
      position: fixed; top: 20px; right: 20px; z-index: 9999;
      min-width: 300px; animation: slideInRight 0.3s ease-out;`;
    
    notification.innerHTML = `
      <div class="d-flex align-items-center">
        <span class="flex-grow-1">${message}</span>
        <button type="button" class="close ml-2">&times;</button>
      </div>`;
    
    const remove = () => {
      notification.style.animation = 'slideOutRight 0.3s ease-in';
      notification.addEventListener('animationend', () => 
        notification.remove());
    };
    
    const timeout = setTimeout(remove, duration);
    notification.querySelector('.close').onclick = () => {
      clearTimeout(timeout);
      remove();
    };
    
    document.body.appendChild(notification);
  },

  isVisible(el) {
    if (!el) return false;
    const style = getComputedStyle(el);
    return style.visibility !== 'hidden' && style.display !== 'none';
  },

  initToggleFunctions() {
    const toggleDicts = document.querySelector('[data-action="toggle-dicts"]');
    if (toggleDicts && !toggleDicts.hasAttribute('data-initialized')) {
      toggleDicts.setAttribute('data-initialized', 'true');
      toggleDicts.addEventListener('click', e => {
        e.preventDefault();
        const boxes = document.querySelectorAll('[name^="d_"]');
        const allOn = Array.from(boxes).every(cb => cb.checked);
        boxes.forEach(cb => cb.checked = !allOn);
      });
    }

    const toggleErrors = document.querySelector('[data-action="toggle-errors"]');
    if (toggleErrors && !toggleErrors.hasAttribute('data-initialized')) {
      toggleErrors.setAttribute('data-initialized', 'true');
      toggleErrors.addEventListener('click', e => {
        e.preventDefault();
        const boxes = document.querySelectorAll('[name^="e_"]');
        const allOn = Array.from(boxes).every(cb => cb.checked);
        boxes.forEach(cb => cb.checked = !allOn);
      });
    }

    const validateSubmit = document.querySelector('[data-action="validate-submit"]');
    if (validateSubmit && !validateSubmit.hasAttribute('data-initialized')) {
      validateSubmit.setAttribute('data-initialized', 'true');
      validateSubmit.addEventListener('click', e => {
        showOverlay();
      });
    }
  },

  initMaxValidation() {
    const max = document.querySelector('input[name="max"]');
    if (!max) return;
    
    const limit = parseInt(max.getAttribute('max') || '150');
    max.type = 'number';
    max.min = '1';
    max.max = limit.toString();
    
    max.addEventListener('input', e => {
      const val = parseInt(e.target.value);
      e.target.setCustomValidity(
        val > limit ? `Limité à ${limit} résultats` : ''
      );
      e.target.reportValidity();
    });
  },

  preserveScrollPosition() {
    window.addEventListener('beforeunload', () => {
      sessionStorage.setItem('checkDataScroll', window.scrollY);
    });
    
    const saved = sessionStorage.getItem('checkDataScroll');
    if (saved) {
      requestAnimationFrame(() => {
        window.scrollTo(0, parseInt(saved));
        sessionStorage.removeItem('checkDataScroll');
      });
    }
  }
};

document.addEventListener('DOMContentLoaded', () => {
  if (typeof CheckDataEditor !== 'undefined' && CheckDataEditor.init) {
    if (document.readyState === 'complete') {
      CheckDataEditor.init();
    } else {
      window.addEventListener('load', () => CheckDataEditor.init());
    }
  }
});