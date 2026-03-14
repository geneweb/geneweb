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
    htrees: ["+3-3 gen.", "family", "6 gen", "8 gen", "HI"],
    gr_parents: ["standard", "three cols"],
    ligne: ["standard"],
    data_3col: ["standard"],
    w: ["custom"],
    x: ["custom"],
    z: ["custom"]
  };

  // Module labels are passed from the server-side template via data-labels
  // attribute on #p_mod_table, so they respect the user's language setting.
  let MODULE_LABELS = {};

  // Fast map for first-letter lookup
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
  
  // Application state
  const state = {
    currentValue: '',
    activeButtons: new Set()
  };
  
  /**
   * Generate the HTML module table
   */
  function generateTable() {
    const rows = Object.entries(MODULES).map(([module, options]) => {
      const buttons = options.map((opt, idx) => {
        const id = `pm_${module[0]}${idx + 1}`;
        DOM.buttons.set(id, { module, option: opt, index: idx });
        const label = MODULE_LABELS[module] || module;
        return `<button class="btn btn-outline-primary btn-sm mr-1 text-nowrap"
                  type="button" id="${id}" title="${label} ${opt}"
                  data-toggle="popover" data-trigger="hover" data-placement="bottom"
                  data-html="true" data-content="<img src='${DOM.imgPrefix}/${module}_${idx + 1}.jpg'>">${opt}</button>`;
      }).join('');
      
      return `<tr>
        <td class="align-middle pmod">${MODULE_LABELS[module] || module}</td>
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
   * Update button states
   */
  function updateButtonStates() {
    // Reset all buttons
    state.activeButtons.forEach(id => {
      const btn = $(`#${id}`);
      btn.removeClass('btn-primary').addClass('btn-outline-primary');
    });
    state.activeButtons.clear();
    
    // Activate buttons matching the current value
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
   * Display an error
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
    
    // Remove the last 2 characters from the value
    const newVal = DOM.input.val().slice(0, -2);
    DOM.input.val(newVal);
  }
  
  /**
   * Build the image view
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
      
      // Special image for module z
      const imgSrc = (moduleChar === 'z') 
        ? 'zz_1.jpg' 
        : `${moduleInfo.name}_${optNum}.jpg`;
      
      images.push(`<img class="rm" src="${DOM.imgPrefix}/${imgSrc}">`);
    }
    
    // Update the DOM once
    if (!hasError) {
      DOM.builder.html(images.join('\n'));
      updateButtonStates();
    }
  }
  
  /**
   * Add a module via button click
   */
  function addModule(btnId) {
    const buttonInfo = DOM.buttons.get(btnId);
    if (!buttonInfo) return;

    const moduleId = btnId.slice(3);
    const val = DOM.input.val();

    // Toggle: if already active, remove it
    if (state.activeButtons.has(btnId)) {
      const newVal = val.replace(moduleId, '');
      DOM.input.val(newVal);
      $(`#${btnId}`).removeClass('btn-primary').addClass('btn-outline-primary');
      state.activeButtons.delete(btnId);
      // Rebuild images from scratch
      buildView();
      return;
    }

    const imgPath = `${DOM.imgPrefix}/${buttonInfo.module}_${buttonInfo.index + 1}.jpg`;

    DOM.input.val(val + moduleId);
    DOM.builder.append(`<img class="rm" src="${imgPath}">\n`);

    $(`#${btnId}`).removeClass('btn-outline-primary').addClass('btn-primary');
    state.activeButtons.add(btnId);
  }
  
  /**
   * Remove the last module
   */
  function removeLastModule() {
    const val = DOM.input.val();
    if (val.length < 2) return;
    
    const removedId = val.slice(-2);
    const newVal = val.slice(0, -2);
    
    DOM.input.val(newVal);
    $('.rm:last-child').remove();
    
    // Restore the button if no longer used
    if (!newVal.includes(removedId)) {
      const btnId = `pm_${removedId}`;
      $(`#${btnId}`).removeClass('btn-primary').addClass('btn-outline-primary');
      state.activeButtons.delete(btnId);
    }
  }
  
  /**
   * Reset everything
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
   * Main initialization
   */
  function init() {
    // Initialize DOM cache
    DOM.input = $('#p_mod');
    DOM.builder = $('#p_mod_builder');
    DOM.alert = $('.alert');
    DOM.alertMod = $('.alert-mod');
    DOM.alertOpt = $('.alert-opt');
    DOM.imgPrefix = $('.img-prfx').attr('data-prfx');
    DOM.bvar = $('#p_mod_bvar');
    
    // Read translated labels from server-rendered data attribute
    const labelData = $('#p_mod_table').attr('data-labels');
    if (labelData) {
      try { MODULE_LABELS = JSON.parse(labelData); } catch(e) {}
    }

    // Generate and insert the table
    $('#p_mod_table').replaceWith(generateTable());
    
    // Initialize popovers
    $('[data-toggle="popover"]').popover();
    
    // Enable alerts
    DOM.alert.alert();
    
    // Optimized event handlers with delegation
    setupEventHandlers();
    
    // Build initial view if value is present
    if (DOM.input.val()) {
      buildView();
    }
  }
  
  /**
   * Set up all event handlers
   */
  function setupEventHandlers() {
    // Input handler with light debounce
    let inputTimer;
    DOM.input.on('input', function() {
      clearTimeout(inputTimer);
      inputTimer = setTimeout(buildView, 50);
    });
    
    // Close alert
    DOM.alert.on('click', function() {
      $(this).addClass('d-none');
      DOM.input.focus();
    });
    
    // Configuration variable button
    DOM.bvar.on('click', function() {
      DOM.input.val($(this).val());
      buildView();
    });
    
    // Remove button
    $('#p_mod_rm').on('click', removeLastModule);
    
    // Clear button
    $('#p_mod_clear').on('click', clearAll);
    
    // zz button
    $('#zz').on('click', function() {
      DOM.input.val('zz');
      buildView();
    });
    
    // Delegation for module buttons
    $(document).on('click', '[id^="pm_"]', function(e) {
      e.preventDefault();
      addModule(this.id);
    });
  }
  
  // Launch initialization on page load
  $(document).ready(init);
  
})(jQuery);