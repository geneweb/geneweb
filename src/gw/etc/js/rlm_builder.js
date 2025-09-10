<!-- $Id: js/rlm_builder.js v7.1 10/01/2025 04:15:36 $ -->
$('#saveButton').on('click', saveIndParams);
$('#clearGraphButton').on('click', clearCache);
updateUI();

function saveIndParams() {
  const $RLM = $('#RLM');
  const { i, p, n, oc, self } = $RLM.data();
  const t = $('#description').val();
  const ind = { i, p, n, oc, t, self };
  const storedInds = JSON.parse(localStorage.getItem('inds')) || [];
  storedInds.push(ind);
  localStorage.setItem('inds', JSON.stringify(storedInds));
  $('#description').val('');
  updateUI();
}

function clearCache() {
  localStorage.removeItem('inds');
  updateUI();
}

function updateUI() {
  const storedInds = JSON.parse(localStorage.getItem('inds')) || [];
  const { url, tooltip } = buildUrlAndTooltip(storedInds);
  const eBase = $('#RLM').data('eb') || '';
  const cgibase = eBase ? '&b=' + eBase : '';
  const urlRLM = `?m=RLM${cgibase}${url}`;
  const urlEditRLM = `?m=TP&v=updRLM${cgibase}${url}`;
  $('#generateGraphButton').attr('href', urlRLM).attr('title', tooltip);
  $('#editGraphButton').attr('href', urlEditRLM);
  $('#clearGraphButton, #graphButtons').toggleClass('d-none', storedInds.length < 1);
}

function buildUrlAndTooltip(storedInds) {
  let url = '';
  let tooltip = '';
  storedInds.forEach((ind, i) => {
    url += buildUrlParams(ind, i + 1);
    tooltip += buildTooltipLine(ind, i + 1);
  });
  return { url, tooltip: tooltip.trim() };
}

function buildUrlParams(ind, n) {
  const ak = $('#RLM').data('accessbykey') || '';
  return `&${ak === 1 && ind.self !== '? ? ' ? `p${n}=${ind.p}&n${n}=${ind.n}${ind.oc ? `&oc${n}=${ind.oc}` : ''}` : `i${n}=${ind.i}`}${ind.t ? `&t${n}=${encodeURIComponent(ind.t)}` : ''}`;
}

function buildTooltipLine(ind, n) {
  return `${n}. ${ind.self} (${ind.i ? `i=${ind.i}` : ''}${ind.p ? `/${ind.p}` : ''}${ind.n ? `/${ind.n}` : ''}${ind.oc && !ind.i ? `/${ind.oc}` : ''}${ind.t ? `/${ind.t}` : ''})\n`;
}