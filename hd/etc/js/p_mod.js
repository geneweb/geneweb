// $Id: p_mod.js, v7.1.xx 14/08/2025 20:51:16 $

(function($) {
  'use strict';
  
  // Configuration et cache
  const MODULES = {
    individu: ["standard", "centered", "two cols"],
    parents: ["simple", "simple + photos", "evolved", "complete", "complete + photos"],
    unions: ["simple", "simple + photos", "evolved", "complete", "complete + photos"],
    fratrie: ["simple", "simple + photos", "complete", "complete + photos"],
    relations: ["simple", "complete"],
    chronologie: ["simple", "simple + events"],
    notes: ["simple", "complete"],
    sources: ["simple", "complete"],
    arbres: ["ascendants", "horizontal", "compact", "descendants"],
    htrees: ["+3-3 gen.", "famille", "6 gen", "8 gen", "HI"],
    gr_parents: ["standard", "three cols"],
    ligne: ["standard"],
    data_3col: ["standard"],
    w: ["personnel"],
    x: ["personnel"],
    z: ["personnel"]
  };
  
  // Map rapide pour lookup par première lettre
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
  
  // État de l'application
  const state = {
    currentValue: '',
    activeButtons: new Set()
  };
  
  /**
   * Génère le HTML de la table de modules
   */
  function generateTable() {
    const rows = Object.entries(MODULES).map(([module, options]) => {
      const buttons = options.map((opt, idx) => {
        const id = `pm_${module[0]}${idx + 1}`;
        DOM.buttons.set(id, { module, option: opt, index: idx });
        return `<button class="btn btn-outline-primary btn-sm mr-1 text-nowrap"
                  type="button" id="${id}" title="${module} ${opt}"
                  data-toggle="popover" data-trigger="hover" data-placement="bottom" 
                  data-html="true" data-content="<img src='${DOM.imgPrefix}/${module}_${idx + 1}.jpg'>">${opt}</button>`;
      }).join('');
      
      return `<tr>
        <td class="align-middle pmod">${module}</td>
        <td><div class="d-inline-flex">${buttons}</div></td>
      </tr>`;
    }).join('');
    
    return `<table class="table table-sm table-hover mt-2 mb-0">
      <thead class="thead-default">
        <tr><th>Module</th><th>Options</th></tr>
      </thead>
      <tbody>${rows}</tbody>
    </table>`;
  }
  
  /**
   * Met à jour l'état des boutons
   */
  function updateButtonStates() {
    // Réinitialiser tous les boutons
    state.activeButtons.forEach(id => {
      const btn = $(`#${id}`);
      btn.removeClass('btn-primary').addClass('btn-outline-primary');
    });
    state.activeButtons.clear();
    
    // Activer les boutons correspondant à la valeur actuelle
    const val = DOM.input.val();
    for (let i = 0; i < val.length - 1; i += 2) {
      const moduleId = val.substr(i, 2);
      const btnId = `pm_${moduleId}`;
      const btn = $(`#${btnId}`);
      if (btn.length) {
        btn.removeClass('btn-outline-primary').addClass('btn-primary');
        state.activeButtons.add(btnId);
      }
    }
  }
  
  /**
   * Affiche une erreur
   */
  function showError(type, data) {
    DOM.alert.removeClass('d-none');
    
    if (type === 'module') {
      DOM.alertMod.removeClass('d-none');
      DOM.alertOpt.addClass('d-none');
      $('#alert-module-2').text(data.module);
    } else {
      DOM.alertOpt.removeClass('d-none');
      DOM.alertMod.addClass('d-none');
      $('#alert-option').text(data.option);
      $('#alert-module').text(data.module);
    }
    
    // Retirer les 2 derniers caractères de la valeur
    const newVal = DOM.input.val().slice(0, -2);
    DOM.input.val(newVal);
  }
  
  /**
   * Construit la vue des images
   */
  function buildView() {
    const val = DOM.input.val();
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
        showError('option', { 
          option: optNum, 
          module: moduleInfo.name 
        });
        hasError = true;
        break;
      }
      
      // Image spéciale pour le module z
      const imgSrc = (moduleChar === 'z') 
        ? 'zz_1.jpg' 
        : `${moduleInfo.name}_${optNum}.jpg`;
      
      images.push(`<img class="rm" src="${DOM.imgPrefix}/${imgSrc}">`);
    }
    
    // Mettre à jour le DOM une seule fois
    if (!hasError) {
      DOM.builder.html(images.join('\n'));
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
    
    DOM.input.val(DOM.input.val() + moduleId);
    DOM.builder.append(`<img class="rm" src="${imgPath}">\n`);
    
    $(`#${btnId}`).removeClass('btn-outline-primary').addClass('btn-primary');
    state.activeButtons.add(btnId);
  }
  
  /**
   * Supprime le dernier module
   */
  function removeLastModule() {
    const val = DOM.input.val();
    if (val.length < 2) return;
    
    const removedId = val.slice(-2);
    const newVal = val.slice(0, -2);
    
    DOM.input.val(newVal);
    $('.rm:last-child').remove();
    
    // Restaurer le bouton si plus utilisé
    if (!newVal.includes(removedId)) {
      const btnId = `pm_${removedId}`;
      $(`#${btnId}`).removeClass('btn-primary').addClass('btn-outline-primary');
      state.activeButtons.delete(btnId);
    }
  }
  
  /**
   * Réinitialise tout
   */
  function clearAll() {
    DOM.input.val('');
    DOM.builder.html('');
    state.activeButtons.forEach(id => {
      $(`#${id}`).removeClass('btn-primary').addClass('btn-outline-primary');
    });
    state.activeButtons.clear();
  }
  
  /**
   * Initialisation principale
   */
  function init() {
    // Initialiser le cache DOM
    DOM.input = $('#p_mod');
    DOM.builder = $('#p_mod_builder');
    DOM.alert = $('.alert');
    DOM.alertMod = $('.alert-mod');
    DOM.alertOpt = $('.alert-opt');
    DOM.imgPrefix = $('.img-prfx').attr('data-prfx');
    DOM.bvar = $('#p_mod_bvar');
    
    // Générer et insérer la table
    $('#p_mod_table').replaceWith(generateTable());
    
    // Initialiser les popovers
    $('[data-toggle="popover"]').popover();
    
    // Activer les alertes
    DOM.alert.alert();
    
    // Event handlers optimisés avec délégation
    setupEventHandlers();
    
    // Construire la vue initiale si valeur présente
    if (DOM.input.val()) {
      buildView();
    }
  }
  
  /**
   * Configure tous les event handlers
   */
  function setupEventHandlers() {
    // Input handler avec debounce léger
    let inputTimer;
    DOM.input.on('input', function() {
      clearTimeout(inputTimer);
      inputTimer = setTimeout(buildView, 50);
    });
    
    // Fermer l'alerte
    DOM.alert.on('click', function() {
      $(this).addClass('d-none');
      DOM.input.focus();
    });
    
    // Bouton variable de configuration
    DOM.bvar.on('click', function() {
      DOM.input.val($(this).val());
      buildView();
    });
    
    // Bouton supprimer
    $('#p_mod_rm').on('click', removeLastModule);
    
    // Bouton effacer
    $('#p_mod_clear').on('click', clearAll);
    
    // Bouton zz
    $('#zz').on('click', function() {
      DOM.input.val('zz');
      buildView();
    });
    
    // Délégation pour les boutons de modules
    $(document).on('click', '[id^="pm_"]', function(e) {
      e.preventDefault();
      addModule(this.id);
    });
  }
  
  // Lancer l'initialisation au chargement
  $(document).ready(init);
  
})(jQuery);