// $Id: p_mod.js, v7.1 18/12/2023 19:57:44 $

// Définitions des modules et options
const modulesEtOptions = {
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
  z: ["personnel"],
};
const imgPrfx = $('.img-prfx').attr('data-prfx');

// Générer la table HTML pour les modules et options
let p_mod_table = `
<table class="table table-sm table-hover mt-2 mb-0">
  <thead class="thead-default">
    <tr>
      <th>Module</th>
      <th>Options</th>
    </tr>
  </thead>
  <tbody>`;

// Parcourir les modules et options pour construire la table
Object.entries(modulesEtOptions).forEach(([module, options]) => {
  p_mod_table += `
    <tr>
      <td class="align-middle pmod">${module}</td>
      <td>
        <div class="d-inline-flex">`;
  // Générer des boutons et des images pour chaque option
  options.forEach((option, index) => {
    const button = `<button class="btn btn-outline-primary btn-sm mr-1 text-nowrap"
                      type="button" id="pm_${module[0]}${index + 1}" title="${module} ${option}"
                      data-toggle="popover" data-trigger="hover" data-placement="bottom" data-html="true"
                      data-content="<img src='${imgPrfx}/${module}_${index + 1}.jpg'>">${option}</button>`;
    p_mod_table += button;
  });
  p_mod_table += `
        </div>
      </td>
    </tr>`;
});

// Enable dismissal of an alert via JavaScript
$('.alert').alert()

// Initialiser la structure HTML
var p_mod_init = '<div id="p_mod_builder">\n</div>\n';

// Remplacer la structure existante par l’initialisation
$('#p_mod_builder').replaceWith(p_mod_init);

// Fonction pour construire la vue en fonction de la valeur de #p_mod
function buildView() {
  // Obtenir la valeur de #p_mod
  var p_mod = $('#p_mod').val();
  // Initialiser la structure de vue
  var p_mod_builder = '<div id="p_mod_builder">';
  // Parcourir la valeur de #p_mod pour construire la vue
  for (var i = 0; i <= p_mod.length - 2; i += 2) {
    // Initialiser la variable module
    var mod = "";
    // Rechercher le module correspondant dans modulesEtOptions
    for (const [nomModule, optionsModule] of Object.entries(modulesEtOptions)) {
      if (p_mod[i] == nomModule[0]) {
        mod = nomModule;
        break;
      }
    }
    // Obtenir la valeur de l’option
    var opt = p_mod[i + 1];
    // Vérifier si le module saisi est valide
    if (modulesEtOptions.hasOwnProperty(mod)) {
      // Vérifier si l’option saisie est valide pour le module
      if (opt >= 1 && opt <= modulesEtOptions[mod].length) {
        // Déterminer la source de l’image en fonction des conditions
        var imgSrc = (p_mod[0] == "z") ? 'zz_1.jpg' : mod + '_' + opt + '.jpg';
        // Ajouter la balise ’'image à la structure de vue
        p_mod_builder += '<img class="rm" src="' + imgPrfx + '/' + imgSrc + '">\n';
        // Mettre à jour le bouton du module avec la classe btn-primary
        var buttonElement = $('#pm_' + mod[0] + (parseInt(opt, 10)));
        if (buttonElement.hasClass('btn-outline-primary')) {
          buttonElement.removeClass('btn-outline-primary').addClass('btn-primary');
        }
      } else {
        $('.alert').removeClass('d-none');
        $('.alert-opt').removeClass('d-none');
        if (!$('.alert-mod').hasClass('d-none')) {
          $('.alert-mod').addClass('d-none');
        }
        $('#alert-option').text(opt);
        $('#alert-module').text(mod);
        $('#p_mod').val($('#p_mod').val().slice(0, -2));
      }
    } else {
      $('.alert').removeClass('d-none');
      $('.alert-mod').removeClass('d-none');
      if (!$('.alert-opt').hasClass('d-none')) {
        $('.alert-opt').addClass('d-none');
      }
      $('#alert-module-2').text(p_mod[i]);
      $('#p_mod').val($('#p_mod').val().slice(0, -2));
    }
  }
  // Fermer la structure de vue
  p_mod_builder += '</div>';
  // Remplacer la vue existante par la nouvelle
  $('#p_mod_builder').replaceWith(p_mod_builder);
}

// Remplacer la table existante par la nouvelle
$('#p_mod_table').replaceWith(p_mod_table);

// Masquer toute l’alerte  en cliquant dessus et placer le focus sur p_mod
$('.alert').on('click', function () {
  $(this).addClass('d-none');
  $('#p_mod').focus();
});

// Activer les popovers Bootstrap
$('[data-toggle="popover"]').popover();

// Gérer les événements dans le champ de saisie pmod
$('#p_mod').on('input', function () {
  buildView();
  const p_mod_value = $('#p_mod').val();
  const lastModuleId = p_mod_value.slice(-2);
  // Parcourir tous les boutons de module en sens inverse
  $('[id^="pm_"]').each(function () {
    // Obtenir l’identifiant du module à partir de l’ID du bouton
    const moduleId = this.id.slice(3);
    // Vérifier si le bouton correspond au dernier module saisi
    if (moduleId === lastModuleId) {
      // Mettre à jour la classe du bouton correspondant
      $(this).removeClass('btn-outline-primary').addClass('btn-primary');
    } else {
      return false; // Sortir de la boucle une fois le bouton trouvé
    }
  });
});

// Gérer le bouton de la variable de configuration
$('#p_mod_bvar').on('click', function () {
  $('#p_mod').val($('#p_mod_bvar').val())
});

// Gérer le bouton de suppression
$('#p_mod_rm').on('click', function () {
  // Obtenir la valeur actuelle de #p_mod
  const p_mod_value = $('#p_mod').val();
  // Vérifier si la valeur n’est pas vide et a au moins deux caractères
  if (p_mod_value.length > 1) {
    // Obtenir les deux derniers caractères représentant le module supprimé
    const removedModuleId = p_mod_value.slice(-2);
    // Supprimer les deux derniers caractères de la saisie
    const new_p_mod_value = p_mod_value.slice(0, -2);
    $('#p_mod').val(new_p_mod_value);
    // Supprimer l’élément d'image correspondant
    $('.rm:last-child').remove();
    // Restaurer la classe du bouton correspondant si le module n’est plus présent
    if (!new_p_mod_value.includes(removedModuleId)) {
      $('#pm_' + removedModuleId).removeClass('btn-primary').addClass('btn-outline-primary');
    }
  }
});

// Gérer le bouton de nettoyage
$('#p_mod_clear').on('click', function () {
  $('#p_mod').val('');
  $('#p_mod_builder').replaceWith('<div id="p_mod_builder">\n</div>\n');
  $('[id^="pm_"]').removeClass('btn-primary').addClass('btn-outline-primary');
});

// Gérer le bouton zz
$('#zz').on('click', function () {
  $('#p_mod').val('zz');
});

// Gérer les clics pour mettre à jour la zone d'affichage des images
$(document).on('click', '[id^="pm_"]', function () {
  const itemId = this.id.slice(3);
  const imgData = $(this).attr('data-content');
  const imgPath = $(imgData).attr('src');
  $('#p_mod').val($('#p_mod').val() + itemId);
  $('#p_mod_builder').append(`<img class="rm" src="${imgPath}">\n`);
  // Mettre à jour le bouton de module correspondant avec la classe btn-primary
  $('#pm_' + itemId).removeClass('btn-outline-primary').addClass('btn-primary');
});