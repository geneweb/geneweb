// $Id: p_mod.js, v7.1 04/04/2026 00:10:03 $

'use strict';

const PMod = (() => {

  // Configuration modules
  const MODULES = {
    individu: ['standard', 'centered', 'two cols'],
    parents: ['simple', 'simple + photos', 'evolved', 'complete', 'complete + photos'],
    unions: ['simple', 'simple + photos', 'evolved', 'complete', 'complete + photos'],
    fratrie: ['simple', 'simple + photos', 'complete', 'complete + photos'],
    relations: ['simple', 'complete'],
    chronologie: ['simple', 'simple + events'],
    notes: ['simple', 'complete'],
    sources: ['simple', 'complete'],
    arbres: ['ascendants', 'horizontal', 'compact', 'descendants'],
    htrees: ['+3-3 gen.', 'famille', '6 gen', '8 gen', 'HI'],
    gr_parents: ['standard', 'three cols'],
    documents: ['standard'],
    ligne: ['standard'],
    trois_col: ['standard'],
    w_mod_perso: ['op_m=1', 'op_m=2', 'op_m=3'],
    x_mod_perso: ['op_m=1', 'op_m=2', 'op_m=3'],
    y_mod_perso: ['op_m=1', 'op_m=2', 'op_m=3'],
    z_test: ['op_m=1', 'op_m=2', 'op_m=3']
  };

  // Map rapide lookup par première lettre
  const MODULE_MAP = new Map();
  for (const [name, opts] of Object.entries(MODULES)) {
    MODULE_MAP.set(name[0], { name, opts });
  }

  // Cache DOM
  const DOM = {
    input: null,
    builder: null,
    alert: null,
    alertMod: null,
    alertOpt: null,
    imgPrefix: null,
    bvar: null,
    buttons: new Map()
  };

  // État
  const state = {
    activeButtons: new Set()
  };

  // Popover instances pour cleanup
  const popovers = [];

  /**
   * Génère le HTML de la table de modules
   */
  function generateTable() {
    const rows = Object.entries(MODULES).map(([module, options]) => {
      const buttons = options.map((opt, idx) => {
        const id = `pm_${module[0]}${idx + 1}`;
        DOM.buttons.set(id, { module, option: opt, index: idx });
        return `<button class="btn btn-outline-primary btn-sm me-1 text-nowrap"
                  type="button" id="${id}" title="${module} ${opt}"
                  data-bs-toggle="popover" data-bs-trigger="hover" data-bs-placement="bottom"
                  data-bs-html="true" data-bs-content="<img src='${DOM.imgPrefix}/${module}_${idx + 1}.jpg'>">${opt}</button>`;
      }).join('');

      return `<tr>
        <td class="align-middle pmod">${module}</td>
        <td><div class="d-inline-flex">${buttons}</div></td>
      </tr>`;
    }).join('');

    return `<table class="table table-sm table-hover mt-2 mb-0">
      <thead>
        <tr><th>Module</th><th>Options</th></tr>
      </thead>
      <tbody>${rows}</tbody>
    </table>`;
  }

  /**
   * Met à jour l'état des boutons
   */
  function updateButtonStates() {
    // Réinitialiser tous les boutons actifs
    state.activeButtons.forEach(id => {
      const btn = document.getElementById(id);
      if (btn) {
        btn.classList.remove('btn-primary');
        btn.classList.add('btn-outline-primary');
      }
    });
    state.activeButtons.clear();

    // Activer les boutons correspondant à la valeur actuelle
    const val = DOM.input.value;
    for (let i = 0; i < val.length - 1; i += 2) {
      const moduleId = val.substr(i, 2);
      const btnId = `pm_${moduleId}`;
      const btn = document.getElementById(btnId);
      if (btn) {
        btn.classList.remove('btn-outline-primary');
        btn.classList.add('btn-primary');
        state.activeButtons.add(btnId);
      }
    }
  }

  /**
   * Affiche une erreur
   */
  function showError(type, data) {
    DOM.alert.classList.remove('d-none');

    if (type === 'module') {
      DOM.alertMod.classList.remove('d-none');
      DOM.alertOpt.classList.add('d-none');
      const el = document.getElementById('alert-module-2');
      if (el) el.textContent = data.module;
    } else {
      DOM.alertOpt.classList.remove('d-none');
      DOM.alertMod.classList.add('d-none');
      const elOpt = document.getElementById('alert-option');
      const elMod = document.getElementById('alert-module');
      if (elOpt) elOpt.textContent = data.option;
      if (elMod) elMod.textContent = data.module;
    }

    // Retirer les 2 derniers caractères
    DOM.input.value = DOM.input.value.slice(0, -2);
  }

  /**
   * Construit la vue des images
   */
  function buildView() {
    const val = DOM.input.value;
    const images = [];
    let hasError = false;

    for (let i = 0; i <= val.length - 2; i += 2) {
      const moduleChar = val[i];
      const optNum = val[i + 1];
      const moduleInfo = MODULE_MAP.get(moduleChar);

      if (!moduleInfo) {
        showError('module', { module: moduleChar });
        hasError = true;
        break;
      }

      const optIdx = parseInt(optNum) - 1;
      if (optIdx < 0 || optIdx >= moduleInfo.opts.length) {
        showError('option', { option: optNum, module: moduleInfo.name });
        hasError = true;
        break;
      }

      const imgSrc = (moduleChar === 'z')
        ? 'zz_1.jpg'
        : `${moduleInfo.name}_${optNum}.jpg`;

      images.push(`<img class="rm" src="${DOM.imgPrefix}/${imgSrc}">`);
    }

    if (!hasError) {
      DOM.builder.innerHTML = images.join('\n');
      updateButtonStates();
    }
  }

  /**
   * Ajoute un module via le bouton
   */
  function addModule(btnId) {
    const buttonInfo = DOM.buttons.get(btnId);
    if (!buttonInfo) return;

    const moduleId = btnId.slice(3);
    const imgPath = `${DOM.imgPrefix}/${buttonInfo.module}_${buttonInfo.index + 1}.jpg`;

    DOM.input.value += moduleId;
    DOM.builder.insertAdjacentHTML('beforeend', `<img class="rm" src="${imgPath}">\n`);

    const btn = document.getElementById(btnId);
    if (btn) {
      btn.classList.remove('btn-outline-primary');
      btn.classList.add('btn-primary');
    }
    state.activeButtons.add(btnId);
  }

  /**
   * Supprime le dernier module
   */
  function removeLastModule() {
    const val = DOM.input.value;
    if (val.length < 2) return;

    const removedId = val.slice(-2);
    const newVal = val.slice(0, -2);

    DOM.input.value = newVal;
    const lastImg = DOM.builder.querySelector('.rm:last-child');
    if (lastImg) lastImg.remove();

    // Restaurer le bouton si plus utilisé
    if (!newVal.includes(removedId)) {
      const btnId = `pm_${removedId}`;
      const btn = document.getElementById(btnId);
      if (btn) {
        btn.classList.remove('btn-primary');
        btn.classList.add('btn-outline-primary');
      }
      state.activeButtons.delete(btnId);
    }
  }

  /**
   * Réinitialise tout
   */
  function clearAll() {
    DOM.input.value = '';
    DOM.builder.innerHTML = '';
    state.activeButtons.forEach(id => {
      const btn = document.getElementById(id);
      if (btn) {
        btn.classList.remove('btn-primary');
        btn.classList.add('btn-outline-primary');
      }
    });
    state.activeButtons.clear();
  }

  /**
   * Configure les event handlers
   */
  function setupEventHandlers() {
    // Input avec debounce léger
    let inputTimer;
    DOM.input.addEventListener('input', () => {
      clearTimeout(inputTimer);
      inputTimer = setTimeout(buildView, 50);
    });

    // Fermer l'alerte
    DOM.alert.addEventListener('click', () => {
      DOM.alert.classList.add('d-none');
      DOM.input.focus();
    });

    // Bouton variable de configuration
    if (DOM.bvar) {
      DOM.bvar.addEventListener('click', () => {
        DOM.input.value = DOM.bvar.value;
        buildView();
      });
    }

    // Bouton supprimer dernier module
    const rmBtn = document.getElementById('p_mod_rm');
    if (rmBtn) rmBtn.addEventListener('click', removeLastModule);

    // Bouton effacer
    const clearBtn = document.getElementById('p_mod_clear');
    if (clearBtn) clearBtn.addEventListener('click', clearAll);

    // Bouton zz (reset template)
    const zzBtn = document.getElementById('zz');
    if (zzBtn) {
      zzBtn.addEventListener('click', () => {
        DOM.input.value = 'zz';
        buildView();
      });
    }

    // Délégation pour les boutons de modules (pm_*)
    document.addEventListener('click', (e) => {
      const btn = e.target.closest('[id^="pm_"]');
      if (btn) {
        e.preventDefault();
        addModule(btn.id);
      }
    });
  }

  /**
   * Initialise les popovers BS5 sur les boutons générés
   */
  function initPopovers() {
    // BS5 Popover avec allowList étendu pour <img>
    const defaultAllowList = bootstrap.Tooltip.Default.allowList;
    const allowList = {
      ...defaultAllowList,
      img: ['src', 'class', 'alt']
    };

    document.querySelectorAll('[data-bs-toggle="popover"]').forEach(el => {
      const pop = new bootstrap.Popover(el, { allowList });
      popovers.push(pop);
    });
  }

  /**
   * Initialisation principale
   */
  function init() {
    // Cache DOM
    DOM.input = document.getElementById('p_mod');
    DOM.builder = document.getElementById('p_mod_builder');
    DOM.alert = document.querySelector('.alert');
    DOM.alertMod = document.querySelector('.alert-mod');
    DOM.alertOpt = document.querySelector('.alert-opt');
    const prfxEl = document.querySelector('.img-prfx');
    DOM.imgPrefix = prfxEl ? prfxEl.dataset.prfx : '';
    DOM.bvar = document.getElementById('p_mod_bvar');

    if (!DOM.input || !DOM.builder) return;

    // Générer et insérer la table
    const tableContainer = document.getElementById('p_mod_table');
    if (tableContainer) {
      tableContainer.outerHTML = generateTable();
    }

    // Initialiser les popovers BS5
    initPopovers();

    // Event handlers
    setupEventHandlers();

    // Vue initiale si valeur présente
    if (DOM.input.value) {
      buildView();
    }
  }

  // Script chargé en lazy → DOM déjà prêt
  init();

  // API publique (pour debug/test)
  return { buildView, clearAll };

})();