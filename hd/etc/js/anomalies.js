// Anomalies lists (m=ANOMA&k=…, m=OK_FILE): a "mark as verified" button
// records the verification at once (m=OK_ADD) and turns into "cancel"
// (m=OK_DEL). Labels and the base URL come from data- attributes of
// #anomaly-list; each button carries its anomaly as JSON.
(function () {
  'use strict';
  var list = document.getElementById('anomaly-list');
  if (!list) return;
  var d = list.dataset;

  function setState(b, verified) {
    var item = b.closest('li');
    b.dataset.verified = verified ? '1' : '0';
    b.textContent = verified ? d.cancel : d.mark;
    b.classList.toggle('btn-outline-secondary', verified);
    b.classList.toggle('btn-outline-success', !verified);
    if (item) item.classList.toggle('list-group-item-success', verified);
    var badge = item && item.querySelector('.anomaly-badge');
    if (verified && !badge && item) {
      badge = document.createElement('span');
      badge.className = 'badge bg-success ms-2 anomaly-badge';
      badge.textContent = d.verified;
      b.before(badge);
    } else if (!verified && badge) {
      badge.remove();
    }
  }

  list.addEventListener('click', function (ev) {
    var b = ev.target.closest('button.anomaly-verify');
    if (!b || b.disabled) return;
    var verified = b.dataset.verified === '1';
    var url = d.submit + 'm=' + (verified ? 'OK_DEL' : 'OK_ADD') +
      '&a=' + encodeURIComponent(b.dataset.anomaly);
    b.disabled = true;
    fetch(url, { credentials: 'same-origin' })
      .then(function (r) {
        if (!r.ok) throw new Error(r.status);
        setState(b, !verified);
      })
      .catch(function () {
        b.textContent = d.failed;
      })
      .finally(function () { b.disabled = false; });
  });
})();
