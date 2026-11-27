<!-- $Id: js/rlm_builder.js v7.1 28/04/2026 03:36:41 $ -->

'use strict';

const RLM = document.getElementById('saveButton');
if (RLM) {
  const { i, p, n, oc, self, eb: eBase, accessbykey: ak } = RLM.dataset;
  const editBtn = document.getElementById('editGraphButton');
  const count = document.getElementById('graphCount');
  const cgibase = eBase ? '&b=' + eBase : '';

  // the dropdown stays open to show the count
  RLM.addEventListener('click', e => {
    e.stopPropagation();
    const storedInds = JSON.parse(localStorage.getItem('inds')) || [];
    if (storedInds.at(-1)?.i !== i) {
      storedInds.push({ i, p, n, oc, self });
      localStorage.setItem('inds', JSON.stringify(storedInds));
    }
    updateUI();
  });

  function updateUI() {
    const storedInds = JSON.parse(localStorage.getItem('inds')) || [];
    const { url, tooltip } = buildUrlAndTooltip(storedInds);
    editBtn.href = `?m=TP&v=updRLM${cgibase}${url}`;
    editBtn.title = tooltip;
    count.textContent = storedInds.length;
    count.classList.toggle('d-none', !storedInds.length);
  }

  function indLabel(ind) {
    if (ind.p && ind.n) {
      return ind.p + ' ' + ind.n + (ind.oc && ind.oc !== '0' ? '.' + ind.oc : '');
    }
    return ind.self || ('i=' + (ind.i || '?'));
  }

  function buildUrlAndTooltip(storedInds) {
    let url = '';
    let tooltip = '';
    storedInds.forEach((ind, idx) => {
      const num = idx + 1;
      url += buildUrlParams(ind, num);
      tooltip += `${num}. ${indLabel(ind)}${ind.t ? ' — ' + ind.t : ''}\n`;
    });
    return { url, tooltip: tooltip.trim() };
  }

  function buildUrlParams(ind, num) {
    const useKey = (ak === '1' || !ind.i) && ind.self !== '? ? ';
    const ident = useKey
      ? `p${num}=${encodeURIComponent(ind.p)}&n${num}=${encodeURIComponent(ind.n)}${ind.oc ? `&oc${num}=${ind.oc}` : ''}`
      : `i${num}=${ind.i}`;
    const text = ind.t ? `&t${num}=${encodeURIComponent(ind.t)}` : '';
    return `&${ident}${text}`;
  }

  updateUI();
}