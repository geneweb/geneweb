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

    // Gestion des clics
    container.addEventListener('click', e => {
      const err = e.target.closest('.err');
      if (err?.classList.contains('disabled') && !e.target.closest('a.s2')) {
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
        // Les liens ont déjà target="_blank" dans le HTML
        // On laisse le comportement natif et on passe au suivant
        requestAnimationFrame(() => {
          this.focusNext(s2.closest('.err'));
        });
      }
    });

    // Navigation globale par flèches (fonctionne aussi sur les boutons s2)
    container.addEventListener('keydown', e => {
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        const activeEl = document.activeElement;
        const err = activeEl?.closest('.err');
        
        // Si on est sur un bouton s2 ou tout autre élément dans .err
        if (err && !err.querySelector('.edit-container')) {
          e.preventDefault();
          this.navigateToNext(err, e.key === 'ArrowUp');
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
    
    // Sauvegarder l'état pour la navigation
    if (s2 && !s2.hasAttribute('data-orig-hidden')) {
      s2.setAttribute('data-orig-hidden', wasHidden ? 'true' : 'false');
    }
    
    err.classList.add('editing');
    
    const field = document.createElement(val.length > 100 || val.includes('\n') ? 'textarea' : 'input');
    field.className = 'form-control';
    field.value = val;
    if (field.tagName === 'INPUT') field.type = 'text';
    
    const container = document.createElement('div');
    container.className = 'edit-container mx-2';
    container.originalButton = btn; // Garder référence au bouton original
    
    const orig = document.createElement('div');
    orig.className = 'original-content';
    orig.innerHTML = btn.innerHTML;
    container.appendChild(orig);
    container.appendChild(field);
    
    btn.replaceWith(container);
    field.focus();
    field.setSelectionRange(val.length, val.length);
    
    if (field.tagName === 'TEXTAREA' && window.autosize) autosize(field);
    
    this.setupField(field, btn, s2, val, wasHidden, container);
  },

  setupField(field, btn, s2, origVal, wasHidden, container) {
    const isTextarea = field.tagName === 'TEXTAREA';
    
    // Mise à jour du bouton s2
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
        s2.classList.remove('btn-success');
        s2.classList.add('btn-warning');
        s2.style.visibility = 'visible';
        s2.title = document.querySelector('#cd')?.getAttribute('data-ok-title') || '';
      } else {
        s2.href = s2.getAttribute('data-orig-href');
        s2.classList.remove('btn-warning');
        s2.classList.add('btn-success');
        s2.style.visibility = wasHidden ? 'hidden' : 'visible';
        s2.title = s2.getAttribute('data-orig-title');
      }
    });
    
    // Raccourcis clavier
    field.addEventListener('keydown', e => {
      // Enter pour valider
      if (e.key === 'Enter' && (!isTextarea || e.shiftKey)) {
        e.preventDefault();
        if (s2?.style.visibility !== 'hidden') {
          // Simuler le clic sur le lien (qui s'ouvre dans target="_blank")
          s2.click();
          // Passer au suivant après un court délai
          requestAnimationFrame(() => {
            this.focusNext(container.closest('.err'));
          });
        }
      }
      // Escape pour annuler
      else if (e.key === 'Escape') {
        e.preventDefault();
        this.cancelEdit(container, btn, s2, wasHidden);
      }
      // Flèches pour naviguer
      else if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        const current = container.closest('.err');
        this.navigateToNext(current, e.key === 'ArrowUp');
      }
    });
    
    // Annulation sur blur
    field.addEventListener('blur', e => {
      if (e.relatedTarget === s2) return;
      setTimeout(() => {
        if (document.activeElement !== field && document.activeElement !== s2) {
          this.cancelEdit(container, btn, s2, wasHidden);
        }
      }, 200);
    });
  },

  cancelEdit(container, btn, s2, wasHidden) {
    const err = container?.closest('.err');
    if (!err) return;
    
    err.classList.remove('editing');
    const origBtn = container.originalButton || btn;
    container.replaceWith(origBtn.cloneNode(true));
    
    if (s2) {
      s2.style.visibility = wasHidden ? 'hidden' : 'visible';
      s2.classList.remove('btn-warning');
      s2.classList.add('btn-success');
      if (s2.hasAttribute('data-orig-href')) {
        s2.href = s2.getAttribute('data-orig-href');
        s2.title = s2.getAttribute('data-orig-title');
      }
    }
  },

  // Fonction unifiée de navigation
  moveToNext(current, goUp = false, fromValidation = false) {
    if (!current) return;
    
    // D'abord trouver le prochain AVANT de modifier l'état
    const allErrors = Array.from(document.querySelectorAll('.err'));
    const currentIdx = allErrors.indexOf(current);
    
    // Trouver le prochain non-désactivé
    let next = null;
    if (goUp) {
      // Chercher vers le haut
      for (let i = currentIdx - 1; i >= 0; i--) {
        if (!allErrors[i].classList.contains('disabled')) {
          next = allErrors[i];
          break;
        }
      }
    } else {
      // Chercher vers le bas
      for (let i = currentIdx + 1; i < allErrors.length; i++) {
        if (!allErrors[i].classList.contains('disabled')) {
          next = allErrors[i];
          break;
        }
      }
    }
    
    // Maintenant on peut modifier l'état de l'élément actuel
    if (fromValidation) {
      current.classList.add('disabled');
      current.classList.remove('editing');
      
      // Marquer visuellement comme validé avec bouton vert
      const s2 = current.querySelector('.s2');
      if (s2) {
        s2.classList.remove('btn-warning', 'btn-info');
        s2.classList.add('btn-success');
        s2.style.visibility = 'visible'; // Toujours visible après validation
        s2.innerHTML = '<i class="fa fa-check"></i>'; // Garder l'icône check
      }
    } else {
      // Si navigation par flèches, fermer proprement l'édition
      const container = current.querySelector('.edit-container');
      if (container) {
        const btn = container.originalButton || document.createElement('button');
        const s2 = current.querySelector('.s2');
        const wasHidden = s2?.getAttribute('data-orig-hidden') === 'true';
        this.cancelEdit(container, btn, s2, wasHidden);
      }
    }
    
    if (!next) return;
    
    // Logique unifiée : s2 visible prioritaire, sinon ouvrir l'input
    const s2 = next.querySelector('a.s2');
    if (s2 && this.isVisible(s2)) {
      requestAnimationFrame(() => {
        s2.focus();
        s2.scrollIntoView({ behavior: 'smooth', block: 'center' });
      });
    } else {
      const btn = next.querySelector('button:not([data-action])');
      if (btn) {
        requestAnimationFrame(() => btn.click());
      }
    }
  },

  // Alias pour la compatibilité
  navigateToNext(current, goUp = false) {
    this.moveToNext(current, goUp, false);
  },

  focusNext(current) {
    this.moveToNext(current, false, true);
  },

  isVisible(el) {
    if (!el) return false;
    const style = getComputedStyle(el);
    return style.visibility !== 'hidden' && style.display !== 'none';
  },

  initToggleFunctions() {
    // Toggle pour les dictionnaires
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

    // Toggle pour les erreurs
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
        val > limit ? `Limité à ${limit} résultats` :
        val < 1 && e.target.value ? 'Minimum 1 résultat' : ''
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