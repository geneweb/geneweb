// Genealogical map navigator for GeneWeb
// Displays ancestor events on a Leaflet map
// with generational navigation
import L from './leaflet.min.js';

const NOMINATIM_URL =
  'https://nominatim.openstreetmap.org/search'
  + '?format=json&limit=1&q=';
const CACHE_PREFIX = 'gw_geoloc_';
const NOMINATIM_DELAY_MS = 1100;

const GEN_COLORS = [
  '#1054a3', '#1054a3', '#2171b5', '#2171b5',
  '#4292c6', '#4292c6', '#6baed6', '#6baed6',
  '#9ecae1', '#9ecae1', '#c6dbef', '#c6dbef'
];
const GEN_COLOR_DEFAULT = '#deebf7';

function genColor(gen) {
  return GEN_COLORS[gen - 1] || GEN_COLOR_DEFAULT;
}

function markerRadius(count) {
  if (count === 1) return 5;
  if (count <= 3) return 7;
  if (count <= 6) return 11;
  if (count <= 10) return 14;
  if (count <= 25) return 18;
  if (count <= 50) return 22;
  if (count <= 85) return 30;
  if (count <= 160) return 38;
  return 44;
}

// Nominatim-friendly: "(dept)" → ", dept"

function nomClean(s) {
  return s.replace(/\s*\(([^)]+)\)/g, ', $1')
    .trim();
}

// Nominatim HTTP with global rate limiting

// Viewbox bias: narrows Nominatim after 5 hits

const viewbox = {
  hits: 0,
  minLat: Infinity, maxLat: -Infinity,
  minLon: Infinity, maxLon: -Infinity,
  active: false,
  update(lat, lon) {
    this.minLat = Math.min(this.minLat, lat);
    this.maxLat = Math.max(this.maxLat, lat);
    this.minLon = Math.min(this.minLon, lon);
    this.maxLon = Math.max(this.maxLon, lon);
    this.hits++;
    this.active = this.hits >= 5;
  },
  param() {
    if (!this.active) return '';
    const pad = 0.5;
    return '&viewbox='
      + (this.minLon - pad).toFixed(4) + ','
      + (this.maxLat + pad).toFixed(4) + ','
      + (this.maxLon + pad).toFixed(4) + ','
      + (this.minLat - pad).toFixed(4);
  }
};

let lastNomCall = 0;

async function nomFetch(query) {
  const wait = NOMINATIM_DELAY_MS
    - (Date.now() - lastNomCall);
  if (wait > 0)
    await new Promise(r => setTimeout(r, wait));
  lastNomCall = Date.now();
  try {
    const url = NOMINATIM_URL
      + encodeURIComponent(query)
      + viewbox.param();
    const resp = await fetch(url);
    const data = await resp.json();
    if (data.length > 0) {
      const lat = parseFloat(data[0].lat);
      const lon = parseFloat(data[0].lon);
      viewbox.update(lat, lon);
      return [lat, lon];
    }
    return null;
  } catch (err) {
    console.error(
      `Nominatim: "${query}":`, err);
    return null;
  }
}

// Geocode using sub/main from template
// Try "sub, main" for sub-places, plain name
// otherwise. No main-only fallback: better
// unfound than false commune center position.

async function geocode(placeName, sub, main) {
  const key = CACHE_PREFIX + placeName;
  const raw = localStorage.getItem(key);
  if (raw) {
    const v = JSON.parse(raw);
    const coords = Array.isArray(v) ? v
      : (v && v.c !== undefined) ? v.c : v;
    if (coords !== null) {
      viewbox.update(coords[0], coords[1]);
      return { coords, cached: true };
    }
    if (!sub)
      return { coords: null, cached: true };
  }

  let coords = null;
  if (sub && main) {
    coords = await nomFetch(
      nomClean(sub + ', ' + main));
  } else {
    coords = await nomFetch(
      nomClean(placeName));
  }

  localStorage.setItem(key,
    JSON.stringify(coords));
  return { coords, cached: false };
}

// RLM URL builder

function buildRLMUrl(prefix, inds, rootIdx, place) {
  const ps = inds
    .map((v, i) => `i${i + 1}=${v}`).join('&');
  const n = inds.length + 1;
  return `${prefix}spouse=on;m=RLM;${ps}`
    + `&i${n}=${rootIdx}&t${n}=${place}`;
}

// Transform gen-keyed raw data to place-keyed

function transformToPlaceFormat(data, prefix) {
  const places = {};
  for (const [gen, gp] of
       Object.entries(data.gen)) {
    const g = parseInt(gen);
    for (const [name, pd] of
         Object.entries(gp)) {
      if (!places[name]) {
        places[name] = {
          generations: {},
          coordinates: null,
          firstAppearance: g,
          sub: pd.sub || null,
          main: pd.main || null
        };
      }
      places[name].generations[gen] = {
        eventCount: pd.i.length,
        yearStart: pd.s,
        yearEnd: pd.e,
        individuals: pd.i,
        url: buildRLMUrl(
          prefix, pd.i, data.meta.index, name)
      };
      places[name].firstAppearance =
        Math.min(places[name].firstAppearance, g);
    }
  }
  return { meta: data.meta, places };
}

// Compute date ranges per generation

function genDateRanges(rawData) {
  const ranges = {};
  for (const [gen, places] of
       Object.entries(rawData.gen)) {
    let mn = null, mx = null;
    for (const p of Object.values(places)) {
      if (typeof p.s === 'number')
        mn = mn === null ? p.s
          : Math.min(mn, p.s);
      if (typeof p.e === 'number')
        mx = mx === null ? p.e
          : Math.max(mx, p.e);
    }
    ranges[gen] = { min: mn, max: mx };
  }
  return ranges;
}

// Loading overlay helpers

function showLoading(total) {
  const el =
    document.getElementById('loadingPopup');
  const pr =
    document.getElementById('loadingProgress');
  if (el) el.style.display = 'block';
  if (pr) pr.textContent = `0 / ${total}`;
}

function updateLoading(current, total) {
  const pr =
    document.getElementById('loadingProgress');
  if (pr) pr.textContent =
    `${current} / ${total}`;
}

function hideLoading() {
  const el =
    document.getElementById('loadingPopup');
  if (el) el.style.display = 'none';
}

// Error popup: two lists (fallback + unfound)

function showGeoErrors(fallbacks, unfound) {
  if (!fallbacks.length && !unfound.length)
    return;
  const div = document.createElement('div');
  div.className = 'geo-error-popup';
  let html = '<div class="geo-error-hd">'
    + '<span>G\u00e9olocalisation</span>'
    + '<button class="close-btn">'
    + '&times;</button></div>';
  if (fallbacks.length > 0) {
    html +=
      '<div style="padding:8px 15px 0">'
      + '<b>' + fallbacks.length
      + ' lieu(x) amalgam\u00e9(s)'
      + ' \u2192 commune</b></div>'
      + '<ul class="geo-error-list">'
      + fallbacks.map(
        n => `<li>${n}</li>`).join('')
      + '</ul>';
  }
  if (unfound.length > 0) {
    html +=
      '<div style="padding:8px 15px 0">'
      + '<b>' + unfound.length
      + ' lieu(x) non localis\u00e9(s)</b>'
      + '</div>'
      + '<ul class="geo-error-list">'
      + unfound.map(
        n => `<li>${n}</li>`).join('')
      + '</ul>';
  }
  div.innerHTML = html;
  document.body.appendChild(div);
  div.querySelector('.close-btn')
    .addEventListener('click',
      () => div.remove());
}

// Merge src place entry into dst in-place

function mergePlaceInto(
    places, src, dst, meta) {
  const s = places[src], d = places[dst];
  for (const [gen, gd] of
       Object.entries(s.generations)) {
    if (d.generations[gen]) {
      const tg = d.generations[gen];
      const merged = [
        ...tg.individuals, ...gd.individuals];
      tg.individuals = merged;
      tg.eventCount = merged.length;
      if (typeof gd.yearStart === 'number')
        tg.yearStart =
          typeof tg.yearStart === 'number'
            ? Math.min(
                tg.yearStart, gd.yearStart)
            : gd.yearStart;
      if (typeof gd.yearEnd === 'number')
        tg.yearEnd =
          typeof tg.yearEnd === 'number'
            ? Math.max(
                tg.yearEnd, gd.yearEnd)
            : gd.yearEnd;
      tg.url = buildRLMUrl(
        meta.prefix, merged,
        meta.index, dst);
    } else {
      d.generations[gen] = { ...gd,
        url: buildRLMUrl(
          meta.prefix, gd.individuals,
          meta.index, dst)
      };
    }
  }
  d.firstAppearance = Math.min(
    d.firstAppearance, s.firstAppearance);
  delete places[src];
}

// Geocode all: 3 phases
// 1. geocode each place (sub,main or plain)
// 2. unfound sub-places: geocode main, merge
// 3. report fallbacks + truly unfound

async function geocodeAll(mapData) {
  const pl = mapData.places;
  const meta = mapData.meta;
  const entries = Object.entries(pl);
  const phase2 = [];
  const unfound = [];

  // Phase 1: geocode all places
  showLoading(entries.length);
  for (let i = 0; i < entries.length; i++) {
    const [name, pd] = entries[i];
    const res = await geocode(
      name, pd.sub, pd.main);
    pd.coordinates = res.coords;
    updateLoading(i + 1, entries.length);
    if (res.coords === null) {
      if (pd.sub && pd.main)
        phase2.push(name);
      else
        unfound.push(name);
    }
  }
  hideLoading();

  // Phase 2: fallback sub-places to main
  const fallbacks = [];
  if (phase2.length > 0) {
    const mainCache = new Map();
    for (const name of phase2) {
      const pd = pl[name];
      const mainName = pd.main;
      let mainCoords = null;

      if (mainCache.has(mainName)) {
        mainCoords = mainCache.get(mainName);
      } else {
        const existing = Object.entries(pl)
          .find(([pn, ppd]) =>
            pn !== name
            && ppd.coordinates !== null
            && (pn === mainName
              || nomClean(pn)
                === nomClean(mainName)));
        if (existing) {
          mainCoords = existing[1].coordinates;
        } else {
          const res = await geocode(
            mainName, null, null);
          mainCoords = res.coords;
        }
        mainCache.set(mainName, mainCoords);
      }

      if (mainCoords) {
        let target = null;
        for (const [pn, ppd] of
             Object.entries(pl)) {
          if (pn !== name
              && ppd.coordinates !== null
              && (pn === mainName
                || nomClean(pn)
                  === nomClean(mainName))) {
            target = pn;
            break;
          }
        }
        if (target) {
          mergePlaceInto(
            pl, name, target, meta);
        } else {
          pl[mainName] = {
            generations: {},
            coordinates: mainCoords,
            firstAppearance:
              pd.firstAppearance,
            sub: null, main: null
          };
          mergePlaceInto(
            pl, name, mainName, meta);
        }
        fallbacks.push(name);
      } else {
        unfound.push(name);
      }
    }
  }

  showGeoErrors(fallbacks, unfound);
}

// Generational navigator

class GenNav {
  constructor(L, mapData, rawData, map) {
    this.L = L;
    this.data = mapData;
    this.raw = rawData;
    this.map = map;
    this.gen = mapData.meta.maxGen;
    this.markers = new Map();
    this.playing = false;
    this.speed = 3;
    this.timer = null;
    this.dateRanges = genDateRanges(rawData);
    this._bindControls();
    this.showGen(this.gen);
  }

  _bindControls() {
    const slider =
      document.getElementById('generationSlider');
    if (slider) {
      slider.addEventListener('input', e => {
        this.showGen(parseInt(e.target.value));
      });
    }
    const playBtn =
      document.getElementById('playButton');
    if (playBtn) {
      playBtn.addEventListener('click',
        () => this._toggle());
    }
    const spd =
      document.getElementById('speedSlider');
    const spdVal =
      document.getElementById('speedValue');
    if (spd && spdVal) {
      spd.addEventListener('input', e => {
        this.speed = parseFloat(e.target.value);
        spdVal.textContent =
          `${this.speed}\u00d7`;
        if (this.playing) {
          this._stop();
          this._play();
        }
      });
    }
  }

  showGen(gen) {
    this.gen = gen;
    const places = this._collect(gen);
    this._renderMarkers(places);
    this._updateUI(gen, places);
  }

  _collect(gen) {
    const out = [];
    for (const [name, pd] of
         Object.entries(this.data.places)) {
      let total = 0, ys = null, ye = null;
      let url = null;
      for (let g = 0; g <= gen; g++) {
        const gd = pd.generations[g];
        if (gd) {
          total += gd.eventCount;
          ys = ys === null ? gd.yearStart
            : Math.min(ys, gd.yearStart);
          ye = ye === null ? gd.yearEnd
            : Math.max(ye, gd.yearEnd);
          url = gd.url;
        }
      }
      if (total > 0 && pd.coordinates) {
        out.push({
          name, coordinates: pd.coordinates,
          eventCount: total,
          yearStart: ys, yearEnd: ye,
          firstAppearance: pd.firstAppearance,
          url
        });
      }
    }
    return out;
  }

  _renderMarkers(places) {
    this.markers.forEach(m =>
      this.map.removeLayer(m));
    this.markers.clear();
    for (const p of places) {
      const m = new this.L.CircleMarker(
        p.coordinates, {
          radius: markerRadius(p.eventCount),
          fillColor:
            genColor(p.firstAppearance),
          color: '#333', weight: 2,
          fillOpacity: 0.7,
          className: 'generation-marker'
        });
      const period =
        p.yearStart === p.yearEnd
          ? p.yearStart
          : `${p.yearStart}\u2013${p.yearEnd}`;
      m.bindPopup(
        '<strong class="d-flex'
        + ' align-self-center text-center">'
        + `${p.name}</strong>`
        + '<div class="d-flex mt-1">'
        + '<div class="flex-grow-1">'
        + `${period}<br>${p.eventCount}`
        + ' \u00e9v\u00e9nement'
        + `${p.eventCount > 1 ? 's' : ''}`
        + '<div class="legend-item'
        + ' text-muted small mt-1">'
        + `<span>&gt; ${p.firstAppearance}`
        + 'e g\u00e9n.</span>'
        + ' <div class="legend-color mb-1"'
        + ' style="background-color:'
        + genColor(p.firstAppearance)
        + '"></div></div></div>'
        + `<a role="button" href="${p.url}"`
        + ' target="_blank"'
        + ' class="btn btn-sm mt-1 ml-auto">'
        + '<i class="fa fa-user fa-fw mr-1">'
        + '</i>Arbre</a></div>'
      );
      this.markers.set(p.name, m);
      m.addTo(this.map);
    }
  }

  _updateUI(gen, places) {
    const cur = document.querySelector(
      '.current-generation');
    if (cur)
      cur.textContent = `G\u00e9n. ${gen}`;
    const sl = document.getElementById(
      'generationSlider');
    if (sl) sl.value = gen;
    const pc = document.getElementById(
      'placesCount');
    if (pc) pc.textContent = places.length;
    const ec = document.getElementById(
      'eventsCount');
    if (ec) {
      ec.textContent = places.reduce(
        (s, p) => s + p.eventCount, 0);
    }
    const tl = document.getElementById(
      'timelineText');
    if (tl) {
      if (gen === 0) {
        tl.textContent =
          this.data.meta.person
          + ' \u2022 G\u00e9n\u00e9ration'
          + ' actuelle';
      } else {
        const r = this.dateRanges[gen];
        if (r && r.min !== null) {
          const period = r.min === r.max
            ? r.min
            : `${r.min}\u2013${r.max}`;
          tl.textContent =
            `G\u00e9n\u00e9ration ${gen}`
            + ` \u2022 ${period}`;
        }
      }
    }
  }

  _toggle() {
    this.playing ? this._stop() : this._play();
  }

  _play() {
    this.playing = true;
    this.showGen(0);
    const btn =
      document.getElementById('playButton');
    if (btn) {
      btn.classList.add('playing');
      const ico =
        btn.querySelector('.play-icon');
      const txt =
        btn.querySelector('.play-text');
      if (ico) ico.textContent = '\u23f8';
      if (txt) txt.textContent = 'Pause';
    }
    this.timer = setInterval(() => {
      if (this.gen < this.data.meta.maxGen) {
        this.showGen(this.gen + 1);
      } else {
        this._stop();
      }
    }, 2000 / this.speed);
  }

  _stop() {
    this.playing = false;
    const btn =
      document.getElementById('playButton');
    if (btn) {
      btn.classList.remove('playing');
      const ico =
        btn.querySelector('.play-icon');
      const txt =
        btn.querySelector('.play-text');
      if (ico) ico.textContent = '\u25b6';
      if (txt) txt.textContent = 'Lire';
    }
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = null;
    }
  }
}

// Auto-init from window.__mapData (map.txt)

document.addEventListener('DOMContentLoaded',
  () => {
    const rawData = window.__mapData;
    if (rawData) initMap(rawData);
  });

async function initMap(rawData) {
  const mapData = transformToPlaceFormat(
    rawData, rawData.meta.prefix);
  await geocodeAll(mapData);

  const map = new L.Map('map');
  const validCoords =
    Object.values(mapData.places)
      .map(p => p.coordinates)
      .filter(c => c !== null);

  if (validCoords.length > 1) {
    const bounds =
      new L.LatLngBounds(validCoords);
    map.fitBounds(bounds,
      { padding: [40, 40] });
  } else if (validCoords.length === 1) {
    map.setView(validCoords[0], 12);
  } else {
    map.setView([46.5, 2.5], 6);
  }

  new L.TileLayer(
    'https://{s}.tile.openstreetmap.org'
    + '/{z}/{x}/{y}.png',
    { attribution:
        '\u00a9 OpenStreetMap contributors' }
  ).addTo(map);

  new GenNav(L, mapData, rawData, map);
}