<!-- $Id: js/rlm_builder.js v7.1 04/04/2026 01:56:46 $ -->

'use strict';

const RLM = document.getElementById('RLM');
if (RLM) {
  const { i, p, n, oc, self, eb: eBase, accessbykey: ak } = RLM.dataset;
  const descInput = document.getElementById('description');
  const saveBtn = document.getElementById('saveButton');
  const clearBtn = document.getElementById('clearGraphButton');
  const graphBtns = document.getElementById('graphButtons');
  const genBtn = document.getElementById('generateGraphButton');
  const editBtn = document.getElementById('editGraphButton');
  const cgibase = eBase ? '&b=' + eBase : '';

  // Prevent dropdown close on interactive elements
  [saveBtn, clearBtn].forEach(el => {
    if (el) el.addEventListener('click', e => e.stopPropagation());
  });

  saveBtn.addEventListener('click', () => {
    const t = descInput.value;
    const ind = { i, p, n, oc, t, self };
    const storedInds = JSON.parse(localStorage.getItem('inds')) || [];
    storedInds.push(ind);
    localStorage.setItem('inds', JSON.stringify(storedInds));
    descInput.value = '';
    updateUI();
  });

  clearBtn.addEventListener('click', () => {
    localStorage.removeItem('inds');
    updateUI();
  });

  function updateUI() {
    const storedInds = JSON.parse(localStorage.getItem('inds')) || [];
    const { url, tooltip } = buildUrlAndTooltip(storedInds);
    const urlRLM = `?m=RLM${cgibase}${url}`;
    const urlEditRLM = `?m=TP&v=updRLM${cgibase}${url}`;
    genBtn.href = urlRLM;
    genBtn.title = tooltip;
    editBtn.href = urlEditRLM;
    const hasData = storedInds.length >= 1;
    clearBtn.classList.toggle('d-none', !hasData);
    graphBtns.classList.toggle('d-none', !hasData);
  }

  function buildUrlAndTooltip(storedInds) {
    let url = '';
    let tooltip = '';
    storedInds.forEach((ind, idx) => {
      const num = idx + 1;
      url += buildUrlParams(ind, num);
      tooltip += `${num}. ${ind.self} (${ind.i ? 'i=' + ind.i : ''}${ind.p ? '/' + ind.p : ''}${ind.n ? '/' + ind.n : ''}${ind.oc && !ind.i ? '/' + ind.oc : ''}${ind.t ? '/' + ind.t : ''})\n`;
    });
    return { url, tooltip: tooltip.trim() };
  }

  function buildUrlParams(ind, num) {
    const useKey = ak === '1' && ind.self !== '? ? ';
    const ident = useKey
      ? `p${num}=${ind.p}&n${num}=${ind.n}${ind.oc ? `&oc${num}=${ind.oc}` : ''}`
      : `i${num}=${ind.i}`;
    const text = ind.t ? `&t${num}=${encodeURIComponent(ind.t)}` : '';
    return `&${ident}${text}`;
  }

  updateUI();
}