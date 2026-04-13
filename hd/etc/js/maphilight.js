/*!
 * maphilight - vanilla JS port for GeneWeb
 * Original: David Lynch's jquery.maphilight (2008-2021), MIT License
 * Ported 2026 for GeneWeb BS5 migration (jQuery removal)
 *
 * Usage:
 *   maphilight(document.querySelectorAll('img.map'));
 *   maphilight('img.map');                  // selector string
 *   maphilight(imgElement, { fillColor: 'ff0000' });
 *
 * Per-area overrides: <area data-maphilight='{"alwaysOn":true}' ...>
 */
(function (global) {
  'use strict';

  const DEFAULTS = {
    fill: true,
    fillColor: '000000',
    fillOpacity: 0.2,
    stroke: true,
    strokeColor: 'ff0000',
    strokeOpacity: 1,
    strokeWidth: 1,
    fade: true,
    alwaysOn: false,
    neverOn: false,
    alwaysOnFade: false,
    groupBy: false,
    wrapClass: true,
    shadow: false,
    shadowX: 0,
    shadowY: 0,
    shadowRadius: 6,
    shadowColor: '000000',
    shadowOpacity: 0.8,
    shadowPosition: 'outside',
    shadowFrom: false
  };

  const OVERLAY_STYLE = 'position:absolute;left:0;top:0;padding:0;border:0;';

  // "ff0000" + 0.5  ->  "rgba(255,0,0,0.5)"
  function hexToRgba(hex, opacity) {
    const clamp = v => Math.max(0, Math.min(parseInt(v, 16), 255));
    return `rgba(${clamp(hex.substr(0, 2))},${clamp(hex.substr(2, 2))},${clamp(hex.substr(4, 2))},${opacity})`;
  }

  // Trace a shape path onto ctx. offsetX/Y used by the shadow trick.
  function tracePath(ctx, shape, coords, dx = 0, dy = 0) {
    ctx.beginPath();
    if (shape === 'rect') {
      ctx.rect(coords[0] + dx, coords[1] + dy, coords[2] - coords[0], coords[3] - coords[1]);
    } else if (shape === 'poly') {
      ctx.moveTo(coords[0] + dx, coords[1] + dy);
      for (let i = 2; i < coords.length; i += 2) {
        ctx.lineTo(coords[i] + dx, coords[i + 1] + dy);
      }
    } else if (shape === 'circ') {
      ctx.arc(coords[0] + dx, coords[1] + dy, coords[2], 0, 2 * Math.PI, false);
    }
    ctx.closePath();
  }

  // Draw one highlighted area on the canvas.
  function drawShape(canvas, shape, coords, opts) {
    const ctx = canvas.getContext('2d');

    // Shadow pass (rarely used but preserved for feature parity)
    if (opts.shadow) {
      ctx.save();
      if (opts.shadowPosition === 'inside') {
        tracePath(ctx, shape, coords);
        ctx.clip();
      }
      // Offscreen trick: draw the shape far outside the canvas and let the
      // shadow fall back into view at the requested offset.
      const offX = 100 * canvas.width;
      const offY = 100 * canvas.height;
      tracePath(ctx, shape, coords, offX, offY);
      ctx.shadowOffsetX = opts.shadowX - offX;
      ctx.shadowOffsetY = opts.shadowY - offY;
      ctx.shadowBlur = opts.shadowRadius;
      ctx.shadowColor = hexToRgba(opts.shadowColor, opts.shadowOpacity);

      const from = opts.shadowFrom || (opts.shadowPosition === 'outside' ? 'fill' : 'stroke');
      if (from === 'stroke') {
        ctx.strokeStyle = 'rgba(0,0,0,1)';
        ctx.stroke();
      } else {
        ctx.fillStyle = 'rgba(0,0,0,1)';
        ctx.fill();
      }
      ctx.restore();

      if (opts.shadowPosition === 'outside') {
        // Punch the shape itself out so the shadow stays "outside".
        ctx.save();
        tracePath(ctx, shape, coords);
        ctx.globalCompositeOperation = 'destination-out';
        ctx.fillStyle = 'rgba(0,0,0,1)';
        ctx.fill();
        ctx.restore();
      }
    }

    // Regular fill + stroke pass
    ctx.save();
    tracePath(ctx, shape, coords);
    if (opts.fill) {
      ctx.fillStyle = hexToRgba(opts.fillColor, opts.fillOpacity);
      ctx.fill();
    }
    if (opts.stroke) {
      ctx.strokeStyle = hexToRgba(opts.strokeColor, opts.strokeOpacity);
      ctx.lineWidth = opts.strokeWidth;
      ctx.stroke();
    }
    ctx.restore();

    if (opts.fade) {
      canvas.style.opacity = '0';
      canvas.animate([{ opacity: 0 }, { opacity: 1 }], { duration: 100, fill: 'forwards' });
    }
  }

  function clearCanvas(canvas) {
    canvas.getContext('2d').clearRect(0, 0, canvas.width, canvas.height);
  }

  // <area shape="rect" coords="1,2,3,4">  ->  ['rect', [1,2,3,4]]
  function parseAreaShape(area) {
    const shape = (area.getAttribute('shape') || 'rect').toLowerCase().substr(0, 4);
    if (shape === 'defa') return null; // default area = fallback, no highlight
    const coords = (area.getAttribute('coords') || '').split(',').map(parseFloat);
    return [shape, coords];
  }

  // Merge global opts + per-area data-maphilight JSON overrides.
  function areaOptions(area, baseOpts) {
    const raw = area.getAttribute('data-maphilight');
    if (!raw) return baseOpts;
    try {
      return Object.assign({}, baseOpts, JSON.parse(raw));
    } catch (e) {
      console.warn('maphilight: invalid data-maphilight JSON on', area, e);
      return baseOpts;
    }
  }

  function makeCanvas(img) {
    const canvas = document.createElement('canvas');
    canvas.width = img.width;
    canvas.height = img.height;
    canvas.style.cssText = OVERLAY_STYLE;
    return canvas;
  }

  function initOne(img, userOpts) {
    const opts = Object.assign({}, DEFAULTS, userOpts);

    // Wait until the image is actually laid out.
    if (!img.complete || img.naturalWidth === 0) {
      img.addEventListener('load', () => initOne(img, userOpts), { once: true });
      return;
    }

    const usemap = img.getAttribute('usemap');
    if (!usemap) return;
    const map = document.querySelector(`map[name="${usemap.substring(1)}"]`);
    if (!map) return;

    // Idempotence: if already wrapped, destroy the previous init cleanly
    // (aborts its listeners, removes its canvases, unwraps the img).
    if (img._maphilight) {
      img._maphilight.destroy();
    }

    // Wrapper div that becomes the positioning context for the canvas.
    const wrap = document.createElement('div');
    wrap.style.cssText =
      `display:block;position:relative;padding:0;` +
      `width:${img.width}px;height:${img.height}px;` +
      `background:url("${img.src}") no-repeat;background-size:contain;`;
    if (opts.wrapClass) {
      wrap.className = opts.wrapClass === true ? img.className : opts.wrapClass;
    }

    img.parentNode.insertBefore(wrap, img);
    // Hide the real img visually but keep it in the DOM for accessibility/map resolution.
    img.style.cssText = OVERLAY_STYLE + 'opacity:0.0000001;';
    wrap.appendChild(img);

    // Main canvas: transient highlights (mouseover/focus).
    const hoverCanvas = makeCanvas(img);
    wrap.insertBefore(hoverCanvas, img);

    // Persistent canvas for alwaysOn areas — lazily created to avoid cost when unused.
    let alwaysCanvas = null;

    function redrawAlwaysOn() {
      if (alwaysCanvas) clearCanvas(alwaysCanvas);
      map.querySelectorAll('area[coords]').forEach(area => {
        const o = areaOptions(area, opts);
        if (!o.alwaysOn) return;
        const parsed = parseAreaShape(area);
        if (!parsed) return;
        if (!alwaysCanvas) {
          alwaysCanvas = makeCanvas(img);
          wrap.insertBefore(alwaysCanvas, hoverCanvas);
        }
        drawShape(alwaysCanvas, parsed[0], parsed[1], Object.assign({}, o, { fade: o.alwaysOnFade }));
      });
    }

    function onAreaEnter(ev) {
      const area = ev.target;
      if (area.tagName !== 'AREA') return;
      const o = areaOptions(area, opts);
      if (o.neverOn || o.alwaysOn) return;
      const parsed = parseAreaShape(area);
      if (!parsed) return;
      drawShape(hoverCanvas, parsed[0], parsed[1], o);

      // groupBy: highlight sibling areas sharing an attribute value or matching a selector.
      if (o.groupBy) {
        const groups = Array.isArray(o.groupBy) ? o.groupBy : [o.groupBy];
        groups.forEach(key => {
          let siblings;
          if (/^[a-zA-Z][-a-zA-Z]+$/.test(key)) {
            const val = area.getAttribute(key);
            siblings = val ? map.querySelectorAll(`area[${key}="${val}"]`) : [];
          } else {
            siblings = map.querySelectorAll(key);
          }
          siblings.forEach(sib => {
            if (sib === area) return;
            const so = areaOptions(sib, opts);
            if (so.neverOn || so.alwaysOn) return;
            const sp = parseAreaShape(sib);
            if (sp) drawShape(hoverCanvas, sp[0], sp[1], so);
          });
        });
      }
    }

    function onAreaLeave() {
      clearCanvas(hoverCanvas);
    }

    // AbortController for clean teardown on re-init or explicit destroy.
    const abortCtrl = new AbortController();
    const signal = abortCtrl.signal;

    map.addEventListener('mouseover', onAreaEnter, { signal });
    map.addEventListener('focusin', onAreaEnter, { signal });
    map.addEventListener('mouseout', onAreaLeave, { signal });
    map.addEventListener('focusout', onAreaLeave, { signal });

    redrawAlwaysOn();
    img.classList.add('maphilighted');

    // Public handle stored on the img element. Callers can reach it
    // via img._maphilight, or via the maphilight.set/clear/refresh helpers.
    const api = {
      redraw: redrawAlwaysOn,
      destroy() {
        abortCtrl.abort();
        if (alwaysCanvas) alwaysCanvas.remove();
        hoverCanvas.remove();
        // Move img back out of wrap, then drop the wrap.
        if (wrap.parentNode) {
          wrap.parentNode.insertBefore(img, wrap);
          wrap.remove();
        }
        img.style.cssText = '';
        img.classList.remove('maphilighted');
        delete img._maphilight;
      }
    };
    img._maphilight = api;
    return api;
  }

  function maphilight(target, opts) {
    let elements;
    if (typeof target === 'string') {
      elements = document.querySelectorAll(target);
    } else if (target instanceof Element) {
      elements = [target];
    } else {
      elements = target; // NodeList / Array
    }
    Array.from(elements).forEach(el => initOne(el, opts));
  }

  // --- Public helpers for dynamic area management -----------------------
  //
  // Typical patterns:
  //
  //   // One-shot persistent highlight on a single area (redraws immediately):
  //   maphilight.set(areaEl, { alwaysOn: true, fillColor: 'ff0000' });
  //
  //   // Remove persistent state from one area (redraws immediately):
  //   maphilight.clear(areaEl);
  //
  //   // Batch update: write data-maphilight on many areas, then one refresh:
  //   areas.forEach(a => a.setAttribute('data-maphilight', JSON.stringify(cfg)));
  //   maphilight.refresh(img);
  //
  //   // Full teardown (unwraps img, removes canvases, aborts listeners):
  //   maphilight.destroy(img);

  function imgFromArea(area) {
    const map = area.closest('map');
    if (!map) return null;
    const name = map.getAttribute('name');
    return name ? document.querySelector(`img[usemap="#${name}"]`) : null;
  }

  function getApi(target) {
    const img = target instanceof HTMLImageElement ? target : imgFromArea(target);
    return img && img._maphilight ? img._maphilight : null;
  }

  maphilight.set = function (area, cfg) {
    if (cfg == null) {
      area.removeAttribute('data-maphilight');
    } else {
      area.setAttribute('data-maphilight', JSON.stringify(cfg));
    }
    const api = getApi(area);
    if (api) api.redraw();
  };

  maphilight.clear = function (area) {
    area.removeAttribute('data-maphilight');
    const api = getApi(area);
    if (api) api.redraw();
  };

  maphilight.refresh = function (img) {
    const api = getApi(img);
    if (api) api.redraw();
  };

  maphilight.destroy = function (img) {
    const api = getApi(img);
    if (api) api.destroy();
  };

  maphilight.defaults = DEFAULTS;
  global.maphilight = maphilight;
})(window);