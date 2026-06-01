/**
 * dagSvg.js — GeneWeb DAG table: overlay clean SVG connectors.
 * Pipes/hr from the HTML are hidden; lines are drawn as SVG. A vertical
 * connector stops at a real portrait but crosses an empty (reserved) image
 * slot to reach the text.
 */
(function () {
  'use strict';

  document.addEventListener('DOMContentLoaded', init);

  function init() {
    const table = document.getElementById('dag');
    if (!table) return;

    table.querySelectorAll('td.dag-collapse').forEach(function (td) {
      td.style.visibility = 'hidden';
      td.style.padding = '0';
    });
    table.querySelectorAll('td.dag-bar').forEach(function (td) {
      td.style.visibility = 'hidden';
    });
    table.querySelectorAll('td hr').forEach(function (hr) {
      hr.style.visibility = 'hidden';
    });

    const parent = table.parentElement;
    if (window.getComputedStyle(parent).position === 'static') {
      parent.style.position = 'relative';
    }
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.id = 'dag-svg-overlay';
    svg.style.cssText =
      'position:absolute;top:0;left:0;width:100%;height:100%;' +
      'overflow:visible;pointer-events:none;z-index:5';
    parent.appendChild(svg);

    requestAnimationFrame(function () { drawConnectors(table, svg); });
  }

  function classifyRow(tr) {
    let hasBar = false, hasBranch = false, hasContent = false, hasHr = false;
    for (const td of tr.cells) {
      if (td.classList.contains('dag-bar')) { hasBar = true; continue; }
      const hr = td.querySelector('hr');
      if (hr) {
        hasHr = true;
        if (hr.className === 'right' || hr.className === 'left') hasBranch = true;
        continue;
      }
      if (td.querySelector('a[id]')) hasContent = true;
    }
    if (hasContent) return 'content';
    if (hasBar) return 'bar';
    if (hasBranch) return 'branch';
    if (hasHr) return 'sibling';
    return 'empty';
  }

  function buildColStarts(tr) {
    const map = new Map();
    let col = 0;
    for (const td of tr.cells) {
      map.set(td, col);
      col += parseInt(td.getAttribute('colspan') || '1', 10);
    }
    return map;
  }

function buildContentIndex(rows, rowType, colStarts, toSVG) {
    const idx = {};
    rows.forEach(function (tr, ri) {
      if (rowType[ri] !== 'content') return;
      const colMap = colStarts[ri];
      idx[ri] = [];
      for (const td of tr.cells) {
        if (!td.querySelector('a[id]')) continue;
        const tdR = toSVG(td.getBoundingClientRect());

        // Connector stops at the outermost visible content — text or a real
        // portrait, wherever the image sits in the cell — but crosses an
        // empty reserved slot (no <img>) to reach the text.
        let top = null, bottom = null;
        td.querySelectorAll('span.text-nowrap, img, bdo').forEach(function (el) {
          const r = toSVG(el.getBoundingClientRect());
          if (r.h < 1) return;
          if (top === null || r.y < top) top = r.y;
          if (bottom === null || r.y + r.h > bottom) bottom = r.y + r.h;
        });

        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);
        idx[ri].push({
          cs, ce, cx: tdR.cx,
          contentTop: top !== null ? top : tdR.y,
          contentBottom: bottom !== null ? bottom : tdR.y + tdR.h,
        });
      }
    });
    return idx;
  }

  function findOverlap(idx, ri, cs, ce) {
    const entries = idx[ri];
    if (!entries) return null;
    for (const e of entries) {
      if (e.cs < ce && e.ce > cs) return e;
    }
    return null;
  }

  function buildBranchIndex(rows, rowType, toSVG) {
    const idx = {};
    rows.forEach(function (tr, ri) {
      if (rowType[ri] !== 'branch' && rowType[ri] !== 'sibling') return;
      let rowY = null;
      for (const td of tr.cells) {
        const hr = td.querySelector('hr');
        if (!hr) continue;
        rowY = toSVG(td.getBoundingClientRect()).cy;
        break;
      }
      idx[ri] = { y: rowY };
    });
    return idx;
  }

  function drawConnectors(table, svg) {
    const pRect = svg.parentElement.getBoundingClientRect();
    function toSVG(r) {
      return {
        x: r.left - pRect.left, y: r.top - pRect.top,
        w: r.width, h: r.height,
        cx: r.left - pRect.left + r.width / 2,
        cy: r.top - pRect.top + r.height / 2,
      };
    }

    const rows = Array.from(table.rows);
    const nRows = rows.length;
    const rowType = rows.map(classifyRow);
    const colStarts = rows.map(buildColStarts);
    const contentIdx = buildContentIndex(rows, rowType, colStarts, toSVG);
    const branchIdx = buildBranchIndex(rows, rowType, toSVG);

    const STROKE = 'var(--color-border-secondary, #999)';

    function nearestSig(ri, dir) {
      for (let r = ri + dir; r >= 0 && r < nRows; r += dir) {
        const t = rowType[r];
        if (t === 'content' || t === 'branch' || t === 'sibling') {
          return { ri: r, type: t };
        }
      }
      return null;
    }

    /* Vertical bars */
    rows.forEach(function (tr, ri) {
      if (rowType[ri] !== 'bar') return;
      const colMap = colStarts[ri];

      for (const td of tr.cells) {
        if (!td.classList.contains('dag-bar')) continue;
        const barR = toSVG(td.getBoundingClientRect());
        if (barR.w < 1) continue;

        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);

        const sigAbove = nearestSig(ri, -1);
        const sigBelow = nearestSig(ri, +1);

        const aboveIsBranch = sigAbove &&
          (sigAbove.type === 'branch' || sigAbove.type === 'sibling');
        const belowIsBranch = sigBelow &&
          (sigBelow.type === 'branch' || sigBelow.type === 'sibling');

        let y1 = barR.y;
        if (aboveIsBranch) {
          const b = branchIdx[sigAbove.ri];
          if (b && b.y !== null) y1 = b.y;
        } else if (sigAbove && sigAbove.type === 'content') {
          const e = findOverlap(contentIdx, sigAbove.ri, cs, ce);
          if (e) y1 = e.contentBottom;
        }

        let y2 = barR.y + barR.h;
        if (belowIsBranch) {
          const b = branchIdx[sigBelow.ri];
          if (b && b.y !== null) y2 = b.y;
        } else if (sigBelow && sigBelow.type === 'content') {
          const e = findOverlap(contentIdx, sigBelow.ri, cs, ce);
          if (e) y2 = e.contentTop;
        }

        let cx = barR.cx;
        if (aboveIsBranch) {
          if (sigBelow && sigBelow.type === 'content') {
            const e = findOverlap(contentIdx, sigBelow.ri, cs, ce);
            if (e) cx = e.cx;
          }
        } else if (sigAbove && sigAbove.type === 'content') {
          const e = findOverlap(contentIdx, sigAbove.ri, cs, ce);
          if (e) cx = e.cx;
        }

        if (y2 > y1 + 1) {
          const link = td.querySelector('a');
          makeBar(svg, cx, y1, y2, STROKE,
            link ? link.getAttribute('href') : null,
            link ? link.getAttribute('title') : null);
        }
      }
    });

    /* Horizontal connectors */
    rows.forEach(function (tr, ri) {
      if (rowType[ri] !== 'branch' && rowType[ri] !== 'sibling') return;

      const colMap = colStarts[ri];
      const bInfo = branchIdx[ri];
      const midY = bInfo && bInfo.y !== null ? bInfo.y : null;

      const hrCells = [];
      for (const td of tr.cells) {
        const hr = td.querySelector('hr');
        if (!hr) continue;
        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);
        hrCells.push({ td, cls: hr.className, cs, ce });
      }
      hrCells.sort(function (a, b) { return a.cs - b.cs; });

      const runs = [];
      let cur = [];
      hrCells.forEach(function (c, i) {
        if (i > 0 && c.cs !== hrCells[i - 1].ce) { runs.push(cur); cur = []; }
        cur.push(c);
      });
      if (cur.length) runs.push(cur);

      runs.forEach(function (run) {
        const dashed = !run.some(function (c) {
          return c.cls === 'right' || c.cls === 'left';
        });
        run.forEach(function (c) {
          const r = toSVG(c.td.getBoundingClientRect());
          if (r.w < 1) return;
          const y = midY !== null ? midY : r.cy;
          if (c.cls === 'full') {
            svg.appendChild(makeLine(r.x, y, r.x + r.w, y, STROKE, '1', dashed));
          } else if (c.cls === 'right') {
            svg.appendChild(makeLine(r.cx, y, r.x + r.w, y, STROKE, '1', false));
          } else if (c.cls === 'left') {
            svg.appendChild(makeLine(r.x, y, r.cx, y, STROKE, '1', false));
          }
        });
      });
    });
  }

function makeBar(svg, cx, y1, y2, stroke, href, title) {
    const visible = makeLine(cx, y1, cx, y2, stroke, '1', false);
    if (!href) { svg.appendChild(visible); return; }
    const SVG = 'http://www.w3.org/2000/svg';
    const a = document.createElementNS(SVG, 'a');
    a.classList.add('dag-link');
    a.setAttribute('href', href);
    a.setAttributeNS('http://www.w3.org/1999/xlink', 'href', href);
    if (title) {
      const t = document.createElementNS(SVG, 'title');
      t.textContent = title;
      a.appendChild(t);
    }
    const hit = makeLine(cx, y1, cx, y2, 'transparent', '12', false);
    hit.style.pointerEvents = 'stroke';
    hit.style.cursor = 'pointer';
    a.appendChild(hit);
    a.appendChild(visible);
    svg.appendChild(a);
  }

  function makeLine(x1, y1, x2, y2, stroke, sw, dashed) {
    const l = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    l.setAttribute('x1', x1); l.setAttribute('y1', y1);
    l.setAttribute('x2', x2); l.setAttribute('y2', y2);
    l.setAttribute('stroke', stroke);
    l.setAttribute('stroke-width', sw);
    l.setAttribute('stroke-linecap', 'round');
    if (dashed) l.setAttribute('stroke-dasharray', '4 3');
    return l;
  }

})();