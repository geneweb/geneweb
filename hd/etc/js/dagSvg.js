/**
 * dagSvg.js — GeneWeb DAG table: overlay clean SVG connectors
 *
 * Geometry rules (derived from HTML structure):
 *
 * Each bar row sits between two "significant" rows. Scanning outward:
 *
 *   ABOVE a bar: either a content row or a branch row
 *   BELOW a bar: either a content row or a branch row
 *
 * y endpoints — all meet at branch CENTER (same Y as the drawn hr line):
 *   - toward content above  → y1 = contentBottom (div.mx-1 bottom)
 *   - toward branch above   → y1 = branch.y (center = where hr line is drawn)
 *   - toward content below  → y2 = contentTop (div.mx-1 top)
 *   - toward branch below   → y2 = branch.y (center = where hr line is drawn)
 *
 * BUT: when sigAbove is content AND sigBelow is branch, the bar spans
 * from contentBottom down to branchY. The td of the content cell may be
 * taller than its div.mx-1 (stretched by a sibling). In that case
 * contentBottom < tdBottom < branchY, so the line still reaches branchY
 * correctly — no gap. Use contentBottom for y1 always.
 *
 * cx — always the content cell this bar belongs to:
 *   - bar below a branch (child bar)  → cx = child content cell below
 *   - bar above a branch (parent bar) → cx = parent content cell above
 *   - bar between two content rows    → cx = content cell above
 * The hr lines position themselves via their own cell geometry.
 *
 * Horizontal lines:
 *   - all drawn at branchY of their row
 *   - runs with no right/left anchors = dashed (sibling reach)
 */

(function () {
  'use strict';

  document.addEventListener('DOMContentLoaded', init);

  function init() {
    const table = document.getElementById('dag');
    if (!table) return;

    table.querySelectorAll('td.dag-collapse').forEach(function (td) {
      td.style.visibility = 'hidden';
      td.style.padding    = '0';
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
    svg.style.cssText = 'position:absolute;top:0;left:0;width:100%;height:100%;' +
                        'overflow:visible;pointer-events:none;z-index:5';
    parent.appendChild(svg);

    requestAnimationFrame(function () { drawConnectors(table, svg); });
  }

  /* ── Row classification ───────────────────────────────────────── */

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
    if (hasBar)     return 'bar';
    if (hasBranch)  return 'branch';
    if (hasHr)      return 'sibling';
    return 'empty';
  }

  /* ── Column helpers ───────────────────────────────────────────── */

  function buildColStarts(tr) {
    const map = new Map();
    let col = 0;
    for (const td of tr.cells) {
      map.set(td, col);
      col += parseInt(td.getAttribute('colspan') || '1', 10);
    }
    return map;
  }

  /* ── Content index ────────────────────────────────────────────── */

  function buildContentIndex(rows, toSVG) {
    const idx = {};
    rows.forEach(function (tr, ri) {
      if (classifyRow(tr) !== 'content') return;
      const colMap = buildColStarts(tr);
      idx[ri] = [];
      for (const td of tr.cells) {
        if (!td.querySelector('a[id]')) continue;
        const tdR = toSVG(td.getBoundingClientRect());
        const mx1 = td.querySelector('div.mx-1');
        const mx1R = mx1 ? toSVG(mx1.getBoundingClientRect()) : null;
        const contentTop    = mx1R ? mx1R.y          : tdR.y;
        const contentBottom = mx1R ? mx1R.y + mx1R.h : tdR.y + tdR.h;
        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);
        idx[ri].push({ cs, ce, cx: tdR.cx, tdTop: tdR.y, contentTop, contentBottom });
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

  /* ── Branch index ─────────────────────────────────────────────── */

  function buildBranchIndex(rows, toSVG) {
    const idx = {};
    rows.forEach(function (tr, ri) {
      const rtype = classifyRow(tr);
      if (rtype !== 'branch' && rtype !== 'sibling') return;
      let rowTop = null, rowBottom = null, rowY = null;
      for (const td of tr.cells) {
        const hr = td.querySelector('hr');
        if (!hr) continue;
        const r = toSVG(td.getBoundingClientRect());
        if (rowY === null) {
          rowY      = r.cy;
          rowTop    = r.y;
          rowBottom = r.y + r.h;
        }
      }
      idx[ri] = { y: rowY, top: rowTop, bottom: rowBottom };
    });
    return idx;
  }

  /* ── Main draw ────────────────────────────────────────────────── */

  function drawConnectors(table, svg) {
    const pRect = svg.parentElement.getBoundingClientRect();
    function toSVG(r) {
      return {
        x: r.left - pRect.left, y: r.top - pRect.top,
        w: r.width, h: r.height,
        cx: r.left - pRect.left + r.width  / 2,
        cy: r.top  - pRect.top  + r.height / 2,
      };
    }

    const rows       = Array.from(table.rows);
    const nRows      = rows.length;
    const rowType    = rows.map(classifyRow);
    const contentIdx = buildContentIndex(rows, toSVG);
    const branchIdx  = buildBranchIndex(rows, toSVG);

    const STROKE = 'var(--color-border-secondary, #999)';

    /* First significant (non-empty, non-bar) row in direction dir */
    function nearestSig(ri, dir) {
      for (let r = ri + dir; r >= 0 && r < nRows; r += dir) {
        const t = rowType[r];
        if (t === 'content' || t === 'branch' || t === 'sibling') {
          return { ri: r, type: t };
        }
      }
      return null;
    }

    /* ── Vertical bars ──────────────────────────────────────────── */
    rows.forEach(function (tr, ri) {
      if (rowType[ri] !== 'bar') return;
      const colMap = buildColStarts(tr);

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

        /* ── y1 ── */
        let y1 = barR.y;
        if (aboveIsBranch) {
          /* Child bar: start at branch center — same Y where the hr line is drawn */
          const b = branchIdx[sigAbove.ri];
          if (b && b.y !== null) y1 = b.y;
        } else if (sigAbove && sigAbove.type === 'content') {
          const e = findOverlap(contentIdx, sigAbove.ri, cs, ce);
          if (e) y1 = e.contentBottom;
        }

        /* ── y2 ── */
        let y2 = barR.y + barR.h;
        if (belowIsBranch) {
          /* Parent bar: end at branch center — same Y where the hr line is drawn */
          const b = branchIdx[sigBelow.ri];
          if (b && b.y !== null) y2 = b.y;
        } else if (sigBelow && sigBelow.type === 'content') {
          const e = findOverlap(contentIdx, sigBelow.ri, cs, ce);
          if (e) y2 = e.contentTop;
        }

        /* ── cx ── always the content cell this bar belongs to:
              child bar (above is branch) → content BELOW
              parent bar (below is branch) → content ABOVE
              direct bar (no branch)       → content ABOVE
           The hr lines position themselves correctly using cell geometry;
           the vertical bar just needs to meet branchY at the right x.  */
        let cx = barR.cx;
        if (aboveIsBranch) {
          /* Child bar: content is below */
          if (sigBelow && sigBelow.type === 'content') {
            const e = findOverlap(contentIdx, sigBelow.ri, cs, ce);
            if (e) cx = e.cx;
          }
        } else if (sigAbove && sigAbove.type === 'content') {
          /* Parent bar or direct bar: content is above */
          const e = findOverlap(contentIdx, sigAbove.ri, cs, ce);
          if (e) cx = e.cx;
        }

        if (y2 > y1 + 1) {
          svg.appendChild(makeLine(cx, y1, cx, y2, STROKE, '1', false));
        }
      }
    });

    /* ── Horizontal connectors ──────────────────────────────────── */
    rows.forEach(function (tr, ri) {
      const rtype = rowType[ri];
      if (rtype !== 'branch' && rtype !== 'sibling') return;

      const colMap  = buildColStarts(tr);
      const bInfo   = branchIdx[ri];
      const midY    = bInfo && bInfo.y !== null ? bInfo.y : null;

      const hrCells = [];
      for (const td of tr.cells) {
        const hr = td.querySelector('hr');
        if (!hr) continue;
        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);
        hrCells.push({ td, cls: hr.className, cs, ce });
      }
      hrCells.sort(function (a, b) { return a.cs - b.cs; });

      /* Group into contiguous runs */
      const runs = [];
      let cur = [];
      hrCells.forEach(function (c, i) {
        if (i > 0 && c.cs !== hrCells[i-1].ce) { runs.push(cur); cur = []; }
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
