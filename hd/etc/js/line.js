/*-------------------------------------------------------------------------------------------------
  Vanilla JS port of jquery.line.js (Tiago do Bem, 2013 — GPLv3)
  Original: https://github.com/tbem/jquery.line
  Ported for GeneWeb — jQuery removed, legacy IE branches dropped, opacity support added.
  Distributed under GNU GPL v3.
  -------------------------------------------------------------------------------------------------
*/
(function (global) {
  'use strict';

  var defaults = {
    zindex:  10000,
    color:   '#000000',
    stroke:  '1',
    style:   'solid',
    class:   'line',
    opacity: 1
  };

  function createLine(x1, y1, x2, y2, options) {
    if (x2 < x1) {
      var tx = x1; x1 = x2; x2 = tx;
      var ty = y1; y1 = y2; y2 = ty;
    }

    var length = Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
    var angle  = Math.atan((y2 - y1) / (x2 - x1));

    var line = document.createElement('div');
    line.className = options.class;
    line.style.position     = 'absolute';
    line.style.zIndex       = options.zindex;
    line.style.opacity      = options.opacity;
    line.style.width        = length + 'px';
    line.style.borderBottom = options.stroke + 'px ' + options.style + ' ' + options.color;
    line.style.top          = (y1 + 0.5 * length * Math.sin(angle)) + 'px';
    line.style.left         = (x1 - 0.5 * length * (1 - Math.cos(angle))) + 'px';
    line.style.transform    = 'rotate(' + angle + 'rad)';
    return line;
  }

  function line(target, x1, y1, x2, y2, options, callback) {
    if (typeof options === 'function') {
      callback = options;
      options  = null;
    }
    var opts = Object.assign({}, defaults, options || {});

    var list;
    if (!target) {
      list = [];
    } else if (target.nodeType === 1) {
      list = [target];
    } else {
      list = Array.prototype.slice.call(target);
    }

    list.forEach(function (el) {
      el.appendChild(createLine(x1, y1, x2, y2, opts));
    });

    if (typeof callback === 'function') {
      callback.call(list[0] || null);
    }
    return target;
  }

  global.GeneWeb = global.GeneWeb || {};
  global.GeneWeb.line = line;
})(window);