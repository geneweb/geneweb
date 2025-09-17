/* relationMatrix.js - Gestion des modals pour le tableau de parenté */
const RelationMatrix = (() => {
  'use strict';

  /* Détection du mode CGI et stockage du paramètre base */
  const urlParams = new URLSearchParams(window.location.search);
  const baseParam = urlParams.get('b');

  function getPersonName(iper) {
    if (!iper) return 'N/A';
    if (window.rmData && window.rmData.names && window.rmData.names[iper]) {
      return window.rmData.names[iper];
    }
    return 'iper:' + iper;
  }

  function makePersonUrl(iper) {
    return '?' + (baseParam ? 'b=' + baseParam + '&' : '') + 'i=' + iper;
  }

  function makeEditUrl(iper) {
    return '?' + (baseParam ? 'b=' + baseParam + '&' : '') +
           'm=MOD_IND&i=' + iper;
  }

  /* URL vers le graphe de parenté m=RL (dag=on affiche tous les chemins) */

  function makeRelationLinkUrl(ancIper, l1, p1Iper, l2, p2Iper) {
    const l1Str = Array.isArray(l1) ? l1.join(',') : l1;
    const l2Str = Array.isArray(l2) ? l2.join(',') : l2;
    return '?' + (baseParam ? 'b=' + baseParam + '&' : '') +
           'm=RL&i=' + ancIper +
           '&l1=' + l1Str + '&i1=' + p1Iper +
           '&l2=' + l2Str + '&i2=' + p2Iper +
           '&dag=on';
  }

  /* Regroupe les ancêtres par famille commune */
  function groupAncestorsByCouples(ancestors) {
    const groups = [];
    const processed = new Set();

    ancestors.forEach((anc1, idx1) => {
      if (processed.has(idx1)) return;

      let partner = null;
      let partnerIdx = -1;

      if (anc1.f && anc1.f.length > 0) {
        for (let idx2 = idx1 + 1; idx2 < ancestors.length; idx2++) {
          if (processed.has(idx2)) continue;

          const anc2 = ancestors[idx2];
          if (anc2.f && anc2.f.length > 0) {
            const commonFamily = anc1.f.some(f1 => anc2.f.includes(f1));
            if (commonFamily) {
              partner = anc2;
              partnerIdx = idx2;
              break;
            }
          }
        }
      }

      processed.add(idx1);
      if (partnerIdx >= 0) {
        processed.add(partnerIdx);
        groups.push({ type: 'couple', anc1: anc1, anc2: partner });
      } else {
        groups.push({ type: 'single', anc: anc1 });
      }
    });

    return groups;
  }

  /*Formate un groupe d'ancêtres (couple ou isolé) en HTML */
  function formatAncestorGroup(group, l1, p1Iper, l2, p2Iper) {
    if (group.type === 'couple') {
      const name1 = getPersonName(group.anc1.p);
      const url1 = makePersonUrl(group.anc1.p);
      const rlUrl1 = makeRelationLinkUrl(group.anc1.p, l1, p1Iper, l2, p2Iper);

      const name2 = getPersonName(group.anc2.p);
      const url2 = makePersonUrl(group.anc2.p);
      const rlUrl2 = makeRelationLinkUrl(group.anc2.p, l1, p1Iper, l2, p2Iper);

      return '<span class="text-nowrap"><a href="' + url1 + '">' + name1 + '</a>' +
             ' [<a href="' + rlUrl1 + '">' + group.anc1.c + '</a>]</span>' +
             ' <span class="text-nowrap">&amp; <a href="' + url2 + '">' + name2 + '</a>' +
             ' [<a href="' + rlUrl2 + '">' + group.anc2.c + '</a>]</span>';
    } else {
      const name = getPersonName(group.anc.p);
      const url = makePersonUrl(group.anc.p);
      const rlUrl = makeRelationLinkUrl(group.anc.p, l1, p1Iper, l2, p2Iper);

      return '<span class="text-nowrap"><a href="' + url + '">' + name + '</a>' +
             ' [<a href="' + rlUrl + '">' + group.anc.c + '</a>]</span>';
    }
  }

  /* Génère les liens somme pour ancêtres multi-niveaux */
  function generateSumLinks(sortedPaths, p1Iper, p2Iper) {
    const ancestorLevels = new Map();

    // Collecter tous les ancêtres et leurs niveaux
    sortedPaths.forEach(path => {
      if (path.anc && path.anc.length > 0) {
        path.anc.forEach(anc => {
          if (!ancestorLevels.has(anc.p)) {
            ancestorLevels.set(anc.p, {
              name: getPersonName(anc.p),
              levels: [],
              total: 0,
              partner: null
            });
          }
          const entry = ancestorLevels.get(anc.p);
          entry.levels.push({ l1: path.l1, l2: path.l2, count: anc.c });
          entry.total += anc.c;
        });
      }
    });

    // Identifier les couples et filtrer multi-niveaux
    const multiLevel = [];
    const processed = new Set();

    ancestorLevels.forEach((data, iper) => {
      if (data.levels.length > 1 && !processed.has(iper)) {
        processed.add(iper);

        // Chercher un partenaire éventuel
        ancestorLevels.forEach((data2, iper2) => {
          if (iper2 !== iper && data2.levels.length > 1 && !processed.has(iper2)) {
            // Vérifier s'ils sont dans les mêmes paths (couple probable)
            const sameLevels = data.levels.every(l1 =>
              data2.levels.some(l2 => l1.l1 === l2.l1 && l1.l2 === l2.l2)
            );
            if (sameLevels) {
              data.partner = { iper: iper2, name: data2.name, total: data2.total };
              processed.add(iper2);
            }
          }
        });

        multiLevel.push({ iper, ...data });
      }
    });

    if (multiLevel.length === 0) return '';

    // Générer le HTML
    let html = '<hr class="w-75 my-2">';
    html += '<div class="text-center mx-4">';

    const entries = multiLevel.map(anc => {
      const l1s = [...new Set(anc.levels.map(l => l.l1))].sort((a, b) => b - a);
      const l2s = [...new Set(anc.levels.map(l => l.l2))].sort((a, b) => b - a);

      const url = makeRelationLinkUrl(anc.iper, l1s, p1Iper, l2s, p2Iper);
      let result = '<span class="text-nowrap"><a href="' +
                   makePersonUrl(anc.iper) + '">' + anc.name +
                   '</a> [<a href="' + url + '">' + anc.total + '</a>]</span>';

      if (anc.partner) {
        const pUrl = makeRelationLinkUrl(anc.partner.iper, l1s, p1Iper, l2s, p2Iper);
        result += ' <span class="text-nowrap">&amp; <a href="' +
                  makePersonUrl(anc.partner.iper) + '">' + anc.partner.name +
                  '</a> [<a href="' + pUrl + '">' + anc.partner.total +
                  '</a>]</span>';
      }

      return result;
    });

    html += entries.join(', ');
    html += '</div>';

    return html;
  }

  function renderAllLevels(cellData) {
    let html = '';

    const p1Iper = cellData.i1 ? cellData.i1.p : '';
    const p2Iper = cellData.i2 ? cellData.i2.p : '';
    const p1Name = getPersonName(p1Iper);
    const p2Name = getPersonName(p2Iper);
    const sortedPaths = [...cellData.data.paths].sort((a, b) => {
      return (b.l1 + b.l2) - (a.l1 + a.l2);
    });

    html += '<table class="table table-sm mb-1">';
    html += '<tbody>';

    sortedPaths.forEach((path, index) => {
      let ancestorsHtml = '';
      if (path.anc && path.anc.length > 0) {
        const groups = groupAncestorsByCouples(path.anc);
        const formattedGroups = groups.map(group =>
          formatAncestorGroup(group, path.l1, p1Iper, path.l2, p2Iper)
        );
        ancestorsHtml = formattedGroups.join(', ');
      }

      html += '<tr><td>' + path.l1 + '</td>';
      html += '<td colspan="2">' + ancestorsHtml + '</td>';
      html += '<td>' + path.l2 + '</td></tr>';
    });

    html += '<tfoot><td><i class="fa-solid fa-person-arrow-up-from-line"></i></td>';
    html += '<td class="person"><a href="' + makePersonUrl(p1Iper) + '">' + p1Name + '</a></td>';
    html += '<td class="person"><a href="' + makePersonUrl(p2Iper) + '">' + p2Name + '</a></td>';
    html += '<td><i class="fa-solid fa-person-arrow-down-to-line fa-flip-horizontal"></i></td></tfoot>';
    html += '</tbody></table>';

    const table = document.getElementById('rm-table');
    html += '<div class="text-center">';
    html += '<strong><a href="' + cellData.url + '" target="_blank">' + cellData.data.total + ' ' + table.dataset.linksLabel + '</a></strong>';
    html += '<br><span class="text-muted">' + table.dataset.coeffLabel + ' : ' + cellData.data.coeff + '</span>';
    html += '</div>';

    // Ajouter les liens somme
    html += generateSumLinks(sortedPaths, p1Iper, p2Iper);

    return html;
  }

  /* Gestionnaire de clic sur les cellules */
  function handleCellClick(event) {
    const cell = event.target.closest('.rm-cell');
    event.preventDefault();
    const dataId = cell.dataset.id;
    if (!dataId) return;
    const url = cell.dataset.url;
    const parts = dataId.split('_');
    const iper1 = parts[0].replace('§', '');
    const iper2 = parts[1].replace('§', '');
    const cellData = Object.values(window.rmData.cells).find(cell =>
      (cell.i1.p === iper1 && cell.i2.p === iper2) ||
      (cell.i1.p === iper2 && cell.i2.p === iper1)
    );
    cellData.url = url;
    showRelationModal(cellData);
  }

  function showRelationModal(data) {
    const modal = $('#rmModal');
    const modalBody = document.getElementById('rmModalBody');

    let htmlContent = '<div class="rm-modal-container">';
    htmlContent += renderAllLevels(data);

    // Section debug compacte, uniquement si &debug en URL
    if (window.location.search.includes('debug')) {
      htmlContent += '<details class="rm-debug"><summary>Debug JSON</summary>';
      htmlContent += '<pre>' + JSON.stringify(data, null, 2) + '</pre></details>';
    }

    htmlContent += '</div>';

    modalBody.innerHTML = htmlContent;
    modal.modal('show');

    setTimeout(() => {
      modalBody.scrollTop = modalBody.scrollHeight;
    }, 100);
  }

  return {
    init: function() {
      if (!window.rmData || !window.rmData.cells) {
        console.error('rmData non disponible');
        return;
      }

      const rmTable = document.getElementById('rm-table');
      if (rmTable) {
        rmTable.addEventListener('click', handleCellClick);
      }
    }
  };

})();