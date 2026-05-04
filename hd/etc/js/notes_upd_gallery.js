var table;
var groupTable;
var currentAreaCount = 0;
var albumCurrent = 0;
var albumImages = [];
let fnameTS = null;

// Maphilight configuration for genealogical photos
const MAPHILIGHT_CONFIG = {
    alwaysOn: false,
    stroke: true,
    strokeColor: 'dba76a',
    strokeWidth: 3,
    strokeOpacity: 0.9,
    fillColor: 'ffffff',
    fillOpacity: 0,
    neverOn: false,
    fade: false,
    shadow: false
};

// Utility helpers
const Utils = {
    get: function(v) {
        return (typeof v !== "undefined") ? v : "";
    },
    debounce: (func, wait) => {
        let timeout;
        return function executedFunction(...args) {
            const later = () => {
                clearTimeout(timeout);
                func(...args);
            };
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
        };
    }
};
const get = Utils.get;

// Creates new row in map table and adds corresponding area and legend elements
function addNewArea(x1, y1, x2, y2) {
    if (x2 - x1 > 5 && y2 - y1 > 5) {
        x1 = parseFloat(x1.toFixed(2));
        y1 = parseFloat(y1.toFixed(2));
        x2 = parseFloat(x2.toFixed(2));
        y2 = parseFloat(y2.toFixed(2));
        const areaIndex = ++currentAreaCount;
        const coords = `${x1},${y1},${x2},${y2}`;
        const new_row = table.row.add({ shape: "rect", coords: coords }).draw(false);

        // formatAreaLabel lives in P2; call retained as-is (identical signature)
        const displayText = formatAreaLabel(areaIndex, {});

        const mapEl = document.getElementById('map');
        mapEl.insertAdjacentHTML('beforeend',
            `<area id="area${areaIndex}" shape="rect" coords="${coords}" alt="${areaIndex}" title="${areaIndex}">`);

        new_row.node().id = "row" + areaIndex;

        // Focus first-name input (3rd td) once the row is in the DOM
        setTimeout(() => {
            const firstNameInput = new_row.node().querySelector('td:nth-child(3) input');
            if (firstNameInput) {
                firstNameInput.focus();
                firstNameInput.select();
            }
        }, 0);

        updateStatus(`Added new area ${areaIndex}`);
    }
}


// Selection area drawing and interaction
const SelectionManager = {
    x1: 0,
    y1: 0,
    x2: 0,
    y2: 0,
    selection: false,
    gMOUSEUP: false,
    gMOUSEDOWN: false,
    redrawMode: false,

    // Get rectangular coordinates
    getCoords: function() {
        const x1 = Math.min(this.x1, this.x2);
        const y1 = Math.min(this.y1, this.y2);
        const x2 = Math.max(this.x1, this.x2);
        const y2 = Math.max(this.y1, this.y2);
        return `${x1},${y1},${x2},${y2}`;
    },

    init: function() {
        document.addEventListener('mouseup', () => {
            this.gMOUSEUP = true;
            this.gMOUSEDOWN = false;
        });

        document.addEventListener('mousedown', () => {
            this.gMOUSEUP = false;
            this.gMOUSEDOWN = true;
        });

        this.initFrameHandlers();
    },

    initFrameHandlers: function() {
        const frame = document.getElementById('frame');
        const selectionEl = document.getElementById('selection');
        if (!frame) return;

        frame.addEventListener('mousedown', (event) => {
            this.x1 = Math.round(event.pageX);
            this.y1 = Math.round(event.pageY);
            this.x2 = this.x1;
            this.y2 = this.y1;
            this.selection = true;
        });

        frame.addEventListener('mousemove', (event) => {
            if (this.selection) {
                this.x2 = Math.round(event.pageX);
                this.y2 = Math.round(event.pageY);

                const TOP    = Math.min(this.y1, this.y2);
                const LEFT   = Math.min(this.x1, this.x2);
                const WIDTH  = Math.abs(this.x2 - this.x1);
                const HEIGHT = Math.abs(this.y2 - this.y1);

                if (selectionEl) {
                    selectionEl.style.position = 'absolute';
                    selectionEl.style.zIndex   = '5000';
                    selectionEl.style.left     = LEFT   + 'px';
                    selectionEl.style.top      = TOP    + 'px';
                    selectionEl.style.width    = WIDTH  + 'px';
                    selectionEl.style.height   = HEIGHT + 'px';
                    selectionEl.style.display  = 'block';
                }
            }
        });

        frame.addEventListener('mouseup', () => {
            const pos = frame.getBoundingClientRect();
            const l_x1 = Math.min(this.x1, this.x2) - (pos.left + window.scrollX);
            const l_x2 = Math.max(this.x1, this.x2) - (pos.left + window.scrollX);
            const l_y1 = Math.min(this.y1, this.y2) - (pos.top  + window.scrollY);
            const l_y2 = Math.max(this.y1, this.y2) - (pos.top  + window.scrollY);

            if (this.redrawMode) {
                const redrawTarget = document.getElementById('redraw-target');
                const rowId = redrawTarget?.dataset.currentRow;
                document.getElementById(`area${rowId}_prev`)?.remove();

                if (Math.abs(l_x2 - l_x1) > 5 && Math.abs(l_y2 - l_y1) > 5) {
                    const coords = `${l_x1},${l_y1},${l_x2},${l_y2}`;
                    const rowElement = document.getElementById(`row${rowId}`);
                    if (rowElement) {
                        const row = table.row(rowElement);
                        let rowData = row.data() || {};
                        rowData.coords = coords;
                        row.data(rowData).draw(false);

                        const mapEl = document.getElementById('map');
                        mapEl.insertAdjacentHTML('beforeend',
                            `<area id="area${rowId}" shape="rect" coords="${coords}" alt="${rowId}" title="${rowId}">`);

                        // Remove highlight and focus first input
                        document.querySelector(`#a${rowId} span`)?.classList.remove('legend-highlighted');
                        const firstInput = rowElement.querySelector('input:not(:disabled)');
                        if (firstInput) {
                            setTimeout(() => {
                                firstInput.focus();
                                firstInput.setSelectionRange(0, firstInput.value.length);
                            }, 0);
                        }
                    }
                } else {
                    // Selection too small: restore original area
                    const originalCoords = redrawTarget?.dataset.originalCoords;
                    document.getElementById(`area${rowId}_prev`)?.remove();
                    const mapEl = document.getElementById('map');
                    mapEl.insertAdjacentHTML('beforeend',
                        `<area id="area${rowId}" shape="rect" coords="${originalCoords}" alt="${rowId}" title="${rowId}">`);
                    document.querySelector(`#a${rowId} span`)?.classList.remove('legend-highlighted');
                }
                this.redrawMode = false;
                if (redrawTarget) {
                    Object.keys(redrawTarget.dataset).forEach(k => delete redrawTarget.dataset[k]);
                }
            } else {
                addNewArea(l_x1, l_y1, l_x2, l_y2);
            }

            this.selection = false;
            if (selectionEl) selectionEl.style.display = 'none';
        });

        // Handler for clicking outside the frame to cancel redraw area
        document.addEventListener('mouseup', (e) => {
            const redrawTarget = document.getElementById('redraw-target');
            const rowId = redrawTarget?.dataset.currentRow;
            const target = e.target;

            if (target.closest('.redraw-area')) {
                return;
            }

            const prevExists = rowId && document.getElementById(`area${rowId}_prev`);
            if ((this.redrawMode || prevExists) && !target.closest('#frame')) {
                if (!rowId) return;

                document.getElementById(`area${rowId}_prev`)?.remove();

                const img = document.getElementById('image');
                if (img) maphilight.refresh(img);

                const originalCoords = redrawTarget?.dataset.originalCoords;
                if (originalCoords) {
                    const mapEl = document.getElementById('map');
                    mapEl.insertAdjacentHTML('beforeend',
                        `<area id="area${rowId}" shape="rect" coords="${originalCoords}" alt="${rowId}" title="${rowId}">`);
                    if (img) maphilight.refresh(img);
                }

                document.querySelector(`#a${rowId} span`)?.classList.remove('legend-highlighted');

                this.redrawMode = false;
                this.selection = false;
                if (redrawTarget) {
                    Object.keys(redrawTarget.dataset).forEach(k => delete redrawTarget.dataset[k]);
                }
                if (selectionEl) selectionEl.style.display = 'none';
            }
        });

        frame.addEventListener('mouseenter', () => {
            this.selection = this.gMOUSEDOWN;
        });

        if (selectionEl) {
            selectionEl.addEventListener('mouseenter', () => {
                this.selection = this.gMOUSEDOWN;
            });
        }

        frame.addEventListener('mouseleave', () => {
            this.selection = false;
        });
    }
};

// Create HTML for groups legend with interactive highlighting
function updateGroupsLegend() {
    if (!groupTable) return;
    const groups = groupTable.rows().data().toArray();
    const groupsLegend = document.getElementById('ul_groups_legend');
    if (!groupsLegend) return;

    groupsLegend.innerHTML = '';
    const labeledGroups = groups.filter(g => g.label);
    if (labeledGroups.length === 0) {
        groupsLegend.classList.add('d-none');
        return;
    }
    groupsLegend.classList.remove('d-none');
    const header = (labeledGroups.length === 1)
        ? GW.i18n.group0 + GW.i18n.colon + ' '
        : GW.i18n.group1 + GW.i18n.colon + ' ';
    groupsLegend.insertAdjacentText('beforeend', header);
    groups.forEach(group => {
        if (group.label) {
            groupsLegend.insertAdjacentHTML('beforeend', `
                <li class="group-legend" data-group="${group.name}">
                    <span class="legend-number">${group.name}.</span> ${group.label}
                </li>
            `);
        }
    });
}

// Handles all row movement operations in the data table
// Manages up/down (↓↑) buttons and drag-drop functionality
const TableManager = {
    // Row-related operations
    moveRow: function(direction) {
        return function(event) {
            event.preventDefault();
            // `this` is the clicked button (set by addEventListener below)
            const row = this.closest('tr');
            if (!row) return;
            const targetRow = direction === 'up'
                ? row.previousElementSibling
                : row.nextElementSibling;
            if (targetRow && targetRow.tagName === 'TR') {
                const currentRowData = table.row(row).data();
                const targetRowData  = table.row(targetRow).data();
                table.row(row).data(targetRowData);
                table.row(targetRow).data(currentRowData);
                table.draw(false);
            }
        };
    },

    // Group-related operations: adding, updating, labeling
    // Add new group with next available number, calculating gaps in sequence
    addGroup: function() {
        let nextNumber = 1;
        groupTable.rows().every(function() {
            const num = parseInt(this.data().name);
            if (num === nextNumber) nextNumber++;
        });

        groupTable.row.add({
            name: nextNumber,
            label: ""
        }).draw();
        groupTable.order([0, 'asc']).draw();
        updateGroupsLegend();
    },

    updateGroupLabels: function() {
        groupTable.rows().every(function() {
            const data = this.data();
            const groupNum = this.node().id.replace(/[a-z_]/g, "");
            if (data.label) {
                const gEl = document.getElementById(`g${groupNum}`);
                if (gEl) gEl.innerHTML = `<span>${groupNum} : ${data.label}</span>`;
            }
        });
    },

    init: function() {
        const mapTable = document.getElementById('map_table');
        if (mapTable) {
            mapTable.addEventListener('click', (e) => {
                const upBtn = e.target.closest('.btn-up');
                if (upBtn && mapTable.contains(upBtn)) {
                    this.moveRow('up').call(upBtn, e);
                }
            });
            mapTable.addEventListener('click', (e) => {
                const downBtn = e.target.closest('.btn-down');
                if (downBtn && mapTable.contains(downBtn)) {
                    this.moveRow('down').call(downBtn, e);
                }
            });
        }
        const addGroupBtn = document.getElementById('add-group-btn');
        if (addGroupBtn) {
            addGroupBtn.addEventListener('click', this.addGroup);
        }
    }
};

const UIManager = {
    init: function() {
        const setAttrAll = (sel, attr, val) =>
            document.querySelectorAll(sel).forEach(el => el.setAttribute(attr, val));

        setAttrAll('#map_table', 'role', 'grid');
        setAttrAll('.legend', 'role', 'button');
        setAttrAll('#map_table th, #grp_table th', 'scope', 'col');
        setAttrAll('#map_table tbody tr, #grp_table tbody tr', 'role', 'row');
        setAttrAll('#map_table tbody td, #grp_table tbody td', 'role', 'cell');
        bindLegendKeyboardNavigation();
    }
};

// Utility function to highlight groups
function highlightGroup(groupNum) {
    if (!groupNum) return;

    document.querySelectorAll(`input[value="${groupNum}"]`).forEach(inp => {
        inp.closest('tr')?.classList.add('group-highlight');
    });
    document.getElementById(`g_row${groupNum}`)?.classList.add('group-highlight');

    document.querySelectorAll('area').forEach(areaEl => {
        const rowId = areaEl.id.replace('area', '');
        const rowData = table.row(`#row${rowId}`).data();

        if (rowData && rowData.group === groupNum) {
            const highlightConfig = {
                ...MAPHILIGHT_CONFIG,
                alwaysOn: true,
                strokeColor: 'ffd700',
                strokeOpacity: 0.8,
                fillColor: 'ffd700',
                fillOpacity: 0.1
            };
            areaEl.setAttribute('class', `group${groupNum}`);
            maphilight.set(areaEl, highlightConfig);
        }
    });
}

// Delegated mouseout on any <area>: restore its native config
document.addEventListener('mouseout', (e) => {
    const areaEl = e.target.closest('area');
    if (!areaEl) return;

    const areaId = areaEl.id.replace(/[a-z]/g, '');
    const storedGroupClass = areaEl.dataset.originalGroup;

    if (storedGroupClass) {
        areaEl.classList.add(storedGroupClass);
        const groupConfig = { ...MAPHILIGHT_CONFIG, groupBy: `.${storedGroupClass}` };
        areaEl.setAttribute('data-maphilight', JSON.stringify(groupConfig));
    } else {
        areaEl.removeAttribute('data-maphilight');
    }

    const img = document.getElementById('image');
    if (img) maphilight.refresh(img);

    document.querySelector(`#a${areaId} span`)?.classList.remove('in');
    document.getElementById(`row${areaId}`)?.classList.remove('in');
});

function updateStatus(message) {
    const el = document.getElementById('status-updates');
    if (el) el.textContent = message;
}

// Format text for display in titles and legends, based on type and available data
function formatAreaLabel(areaIndex, data) {
    if (!data || typeof data !== 'object') {
        return `${areaIndex}.`;
    }
    const { fn, sn, alt, t, href } = data;
    let txt = `${areaIndex}. `;

    try {
        if (!t || t === 'p') {
            if (fn || sn) {
                txt += `${fn || ''} ${sn || ''}`.trim();
                if (alt) txt += ` (${alt.trim()})`;
            } else if (alt) {
                txt += alt.trim();
            }
        } else if (t === 'g' || t === 'e') {
            if (href) {
                txt += alt ? `${href.trim()} (${alt.trim()})` : href.trim();
            } else if (alt) {
                txt += alt.trim();
            }
        }
        return txt;
    } catch (error) {
        console.error('Error in formatAreaLabel:', error);
        return `${areaIndex}.`;
    }
}

// Creates HTML markup for legend entries with proper person/URL formatting and links
function createLegendHTML(areaIndex, data) {
    const num = `<span class="legend-number">${areaIndex}.</span> `;
    if (!data) return num;
    const { fn, sn, alt, t, href, oc } = data;

    // Check if the area is being redrawn
    let hl = '';
    const redrawTarget = document.getElementById('redraw-target');
    if (redrawTarget && redrawTarget.dataset.currentRow == areaIndex) {
        hl = ' class="legend-highlighted"';
    }

    // Person
    if (!t || t === 'p') {
        if (fn || sn) {
            const occNum = (get(oc) && oc !== '0') ? `&oc=${oc}` : '';
            const linkText = fn + ' ' + sn;
            return `<span${hl}>${num}<a href="${GW.prefix}&p=${encodeURIComponent(fn)}&n=${encodeURIComponent(sn)}${occNum}">${linkText}</a>${alt ? ` (${alt})` : ''}</span>`;
        }
    }
    // GeneWeb or external link
    else if ((t === 'g' || t === 'e') && href) {
        const baseUrl = t === 'g' ? GW.prefix : '';
        const linkText = alt || href;
        return `<span${hl}>${num}<a href="${baseUrl}${href}">${linkText}</a></span>`;
    }

    // Fallback: alt text only
    return `<span${hl}>${num}${alt || ''}</span>`;
}

/**
 * Updates area map titles and legend entries with group handling
 */
function updateAreaAndLegend(data, areaIndex) {
    const displayText = formatAreaLabel(areaIndex, data);
    const group = get(data.group);
    const groupAttr = group
        ? ` class="group${group}" data-maphilight='{"groupBy":".group${group}"}'`
        : '';

    document.getElementById('map')?.insertAdjacentHTML('beforeend', `
        <area id="area${areaIndex}"
              shape="rect"
              coords="${get(data.coords)}"
              alt="${displayText}"
              title="${displayText}"
              role="button"
              tabindex="0"
              aria-label="${displayText}"${groupAttr}>
    `);

    document.getElementById('ul_legend')?.insertAdjacentHTML('beforeend', `
        <li class="legend" id="a${areaIndex}">
            ${createLegendHTML(areaIndex, data)}
        </li>
    `);
}

// Keyboard navigation for legend items
function bindLegendKeyboardNavigation() {
    const ul = document.getElementById('ul_legend');
    if (!ul) return;

    ul.addEventListener('keydown', (e) => {
        const current = e.target.closest('.legend-button');
        if (!current || !ul.contains(current)) return;

        const items = Array.from(ul.querySelectorAll('.legend-button'));
        const currentIndex = items.indexOf(current);

        switch (e.key) {
            case 'ArrowRight':
            case 'ArrowDown':
                e.preventDefault();
                items[currentIndex + 1]?.focus();
                break;
            case 'ArrowLeft':
            case 'ArrowUp':
                e.preventDefault();
                items[currentIndex - 1]?.focus();
                break;
            case 'Home':
                e.preventDefault();
                items[0]?.focus();
                break;
            case 'End':
                e.preventDefault();
                items[items.length - 1]?.focus();
                break;
            case 'Enter':
            case ' ':
                e.preventDefault();
                current.click();
                break;
        }
    });
}

// Optimize updateLegendLinks with batched DOM updates
const updateLegendLinks = () => {
    table.rows().every(function(rowIdx) {
        const rowData = this.data();
        const row = this.node();
        if (!row) return;

        const areaIndex = row.id.replace(/[a-z]/g, '');
        const displayText = formatAreaLabel(areaIndex, rowData);
        const group = get(rowData.group);

        // Update area attributes
        const area = document.getElementById(`area${areaIndex}`);
        if (area) {
            area.setAttribute('alt', displayText);
            area.setAttribute('title', displayText);

            if (group && group !== '0') {
                area.setAttribute('class', `group${group}`);
                area.setAttribute('data-maphilight', `{"groupBy":".group${group}"}`);
            } else {
                area.removeAttribute('class');
                area.removeAttribute('data-maphilight');
            }
        }

        // Update legend HTML
        const legendEntry = document.getElementById(`a${areaIndex}`);
        if (legendEntry) {
            legendEntry.innerHTML = createLegendHTML(areaIndex, rowData);
        }
    });
};

// Generate GeneWeb wiki-syntax link
function generateGwSyntax(data) {
    if (!data.fn || !data.sn) return null;

    const oc = get(data.oc);
    const oc2 = (oc != '' && oc != 0) ? '/' + oc : '';
    const txt = get(data.alt);
    return '[[' + data.fn + '/' + data.sn + oc2 +
           (txt ? '/' + data.fn + ' ' + data.sn : '') + ']]';
}


// Handles row reordering in the data table, ensuring consistency between
// table data, DOM elements, and visual elements (areas, legends).
// DataTables returns a diff array with cascading position changes; we only
// process the specifically dragged row (edit.triggerRow).
function handleAreaReorder(event, diff, edit) {
    if (!diff.length) return;

    const currentData = table.rows().data().toArray();
    const draggedMove = diff.find(d => d.node.id === edit.triggerRow.node().id);
    if (!draggedMove) return;

    requestAnimationFrame(() => {
        const [movedItem] = currentData.splice(draggedMove.oldPosition, 1);
        currentData.splice(draggedMove.newPosition, 0, movedItem);

        table.clear().rows.add(currentData).draw(false);

        const updates = currentData.map((rowData, index) => {
            const rowNum = index + 1;
            return () => {
                const rowNode = table.row(index).node();
                const areaEl  = document.getElementById(`area${rowNum}`);
                const legendEl = document.getElementById(`a${rowNum}`);

                if (rowNode) rowNode.id = `row${rowNum}`;

                if (areaEl) {
                    const displayText = formatAreaLabel(rowNum, rowData);
                    areaEl.setAttribute('alt', displayText);
                    areaEl.setAttribute('title', displayText);
                }

                if (legendEl) {
                    legendEl.id = `a${rowNum}`;
                    legendEl.innerHTML = createLegendHTML(rowNum, rowData);
                }
            };
        });

        requestAnimationFrame(() => {
            updates.forEach(update => update());
            const img = document.getElementById('image');
            if (img) maphilight.refresh(img);
        });
    });
}

// Utility function for handling legend clicks
function handleLegendClick(targetId, tablePrefix) {
    const row = document.getElementById(`${tablePrefix}${targetId}`);
    if (!row) return;

    row.scrollIntoView({ behavior: 'smooth', block: 'center' });
    const selector = tablePrefix === 'row' ? 'input:not(:disabled)' : 'input';
    const input = row.querySelector(selector);
    if (input) {
        input.focus();
        input.select();
    }
}

// Unified group hover handling for both table and legend
function handleGroupHover(action, groupNum) {
    if (!groupNum) return;

    if (action === 'enter') {
        table.rows().every(function() {
            if (this.data().group == groupNum) {
                const node = this.node();
                node?.classList.add('group-highlight');
                const id = node?.id?.replace('row', '');
                if (id) {
                    document.getElementById(`a${id}`)
                        ?.classList.add('group-highlight');
                }
            }
        });
        document.getElementById(`g_row${groupNum}`)?.classList.add('group-highlight');
        document.querySelectorAll(`.group-legend[data-group="${groupNum}"]`)
            .forEach(el => el.classList.add('group-highlight'));

        // Highlight areas belonging to this group — batch: set attrs, one refresh
        const img = document.getElementById('image');
        document.querySelectorAll('area').forEach(areaEl => {
            const rowId = areaEl.id.replace('area', '');
            const rowData = table.row(`#row${rowId}`).data();

            if (rowData && rowData.group == groupNum) {
                const cfg = {
                    ...MAPHILIGHT_CONFIG,
                    alwaysOn: true,
                    strokeColor: 'ffd700',
                    strokeOpacity: 0.8,
                    fillColor: 'ffd700',
                    fillOpacity: 0.1
                };
                areaEl.setAttribute('data-maphilight', JSON.stringify(cfg));
            }
        });
        if (img) maphilight.refresh(img);
    } else {
        // Remove all highlights
        document.querySelectorAll('.group-highlight')
            .forEach(el => el.classList.remove('group-highlight'));

        // Reset area highlighting — batch: set attrs, one refresh
        const img = document.getElementById('image');
        document.querySelectorAll('area').forEach(areaEl => {
            const rowId = areaEl.id.replace('area', '');
            const rowData = table.row(`#row${rowId}`).data();

            if (rowData?.group) {
                const cfg = { ...MAPHILIGHT_CONFIG, groupBy: `.group${rowData.group}` };
                areaEl.setAttribute('data-maphilight', JSON.stringify(cfg));
            } else {
                areaEl.removeAttribute('data-maphilight');
            }
        });
        if (img) maphilight.refresh(img);
    }
}

function initializeHandlers() {
    if (!table || !groupTable) return;

    const delegate = (root, eventType, selector, fn) => {
        root.addEventListener(eventType, (e) => {
            const match = e.target.closest(selector);
            if (match && root.contains(match)) fn.call(match, e);
        });
    };

    // -------------------------------------------------------------------------
    // map_table (DataTables) handlers
    // -------------------------------------------------------------------------
    table
        .on('change', '.update', function() {
            const td = table.cell(this.closest('td'));
            const tr = this.closest('tr');
            td.data(this.value).draw();

            if (this.nodeName === 'SELECT') {
                const isPersonType = this.value === 'p';
                const rowIndex = table.row(tr).index();
                const rowData = table.row(rowIndex).data();
                if (!isPersonType) {
                    rowData.fn = '';
                    rowData.sn = '';
                    rowData.oc = '';
                    delete rowData.gw;
                    tr.querySelectorAll('.p-input').forEach(i => { i.disabled = true; });
                } else {
                    rowData.href = '';
                    tr.querySelectorAll('.u-input').forEach(i => { i.disabled = true; });
                }
                table.row(rowIndex).data(rowData).draw(false);

                setTimeout(() => {
                    const firstInput = tr.querySelector('td input:not(:disabled)');
                    if (firstInput) firstInput.focus();
                }, 0);
            }
        })
        .on('keydown', 'input, select, button', function(e) {
            if (e.key !== 'Tab' || e.shiftKey) return;
            e.stopPropagation();
            e.preventDefault();

            const currentRow = this.closest('tr');
            const currentCell = this.closest('td');
            const nextRow = currentRow?.nextElementSibling;

            const focusableSel = 'input:not([disabled]), select:not([disabled]), button:not([disabled])';
            const visibleFocusable = (root) => Array.from(root.querySelectorAll(focusableSel))
                .filter(el => el.offsetParent !== null);

            // Cells that contain at least one visible focusable element
            const cells = Array.from(currentRow.querySelectorAll('td'))
                .filter(td => visibleFocusable(td).length > 0);
            const currentIndex = cells.indexOf(currentCell);

            let nextInput;
            if (currentIndex >= 0 && currentIndex < cells.length - 1) {
                nextInput = visibleFocusable(cells[currentIndex + 1])[0];
            } else if (nextRow) {
                nextInput = visibleFocusable(nextRow)[0];
            }

            if (nextInput) {
                nextInput.focus();
                if (nextInput.matches('input[type="text"]')) {
                    nextInput.setSelectionRange(0, nextInput.value.length);
                }
            }
        })
        .on('mouseover', 'tr', function(e) {
            if (e.target.closest('.btn-up, .btn-down')) return;
            if (this.id) {
                const areaId = this.id.replace(/[a-z]/g, '');
                document.getElementById(`area${areaId}`)
                    ?.dispatchEvent(new MouseEvent('mouseover', { bubbles: true }));
                document.querySelector(`#a${areaId} span`)?.classList.add('in');
                this.classList.add('in');
            }
        })
        .on('mouseout', 'tr', function(e) {
            if (e.target.closest('.btn-up, .btn-down')) return;
            if (this.id) {
                const areaId = this.id.replace(/[a-z]/g, '');
                document.getElementById(`area${areaId}`)
                    ?.dispatchEvent(new MouseEvent('mouseout', { bubbles: true }));
                document.querySelector(`#a${areaId} span`)?.classList.remove('in');
                this.classList.remove('in');
            }
        })
        .on('click', '.redraw-area', function(e) {
            e.preventDefault();
            const row = this.closest('tr');
            const rowId = row?.id.replace(/[a-z]/g, '');
            if (!rowId) return;

            const rowData = table.row(row).data();
            const originalCoords = rowData.coords;

            SelectionManager.redrawMode = true;
            SelectionManager.selection = true;

            const redrawTarget = document.getElementById('redraw-target');
            if (redrawTarget) {
                redrawTarget.dataset.currentRow = rowId;
                redrawTarget.dataset.originalCoords = originalCoords;
            }

            document.getElementById(`area${rowId}`)?.remove();
            const mapEl = document.getElementById('map');
            mapEl.insertAdjacentHTML('beforeend',
                `<area id="area${rowId}_prev" shape="rect" coords="${originalCoords}">`);

            const previewArea = document.getElementById(`area${rowId}_prev`);
            if (previewArea) {
                maphilight.set(previewArea, {
                    ...MAPHILIGHT_CONFIG,
                    strokeColor: 'ff9999',
                    strokeOpacity: 0.5,
                    fillColor: 'ff9999',
                    fillOpacity: 0.2,
                    alwaysOn: true
                });
            }

            document.querySelector(`#a${rowId} span`)?.classList.add('legend-highlighted');

            const frameEl = document.getElementById('frame');
            if (frameEl) {
                const top = frameEl.getBoundingClientRect().top + window.scrollY;
                window.scrollTo({ top, behavior: 'smooth' });
            }
        })
        .on('order.dt', function() {
            document.getElementById('map').innerHTML = '';
            document.getElementById('ul_legend').innerHTML = '';

            table.rows({ order: 'current' }).every(function(index) {
                const data = this.data();
                const counter = index + 1;
                const node = this.node();
                if (node) {
                    node.id = `row${counter}`;
                    const handle = node.querySelector('.drag-handle');
                    if (handle) handle.textContent = counter;
                }
                updateAreaAndLegend(data, counter);
            });

            const img = document.getElementById('image');
            if (img) maphilight.refresh(img);
        })
        .on('row-reorder', handleAreaReorder)
        .on('draw.dt', updateLegendLinks)
        
        .on('focusout', 'td input.update', function () {
            const tr = this.closest('tr');
            if (!tr || !table) return;
            validateRow(tr, table.row(tr).data(), { flash: true });
        });

    if (GW.hasFnList || GW.hasSnList) {
        table.on('input', 'input[list]', function(e) {
            const listId = this.getAttribute('list');
            if (!listId || !this.closest('table')?.classList.contains('dataTable')) return;
            DatalistManager.handleDataTableInput(this, listId);
        });
    }

    // -------------------------------------------------------------------------
    // groupTable (DataTables) handlers
    // -------------------------------------------------------------------------
    groupTable
        .on('draw.dt', () => {
            groupTable.columns.adjust();
            updateGroupsLegend();
        })
        .on('change', '.update', function() {
            const row = groupTable.row(this.closest('tr'));
            if (!row.data()) {
                console.error('Unable to update row: Row data not found.');
                return;
            }
            const currentData = row.data();
            currentData.label = this.value;
            row.data(currentData).draw(false);
            updateGroupsLegend();
        })
        .on('mouseover', 'tr', function() {
            if (this.id && this.id.startsWith('g_row')) {
                const groupNum = this.id.replace('g_row', '');
                if (groupNum) handleGroupHover('enter', groupNum);
            }
        })
        .on('mouseout', 'tr', function() {
            if (this.id?.startsWith('g_row')) handleGroupHover('leave');
        });

    // -------------------------------------------------------------------------
    // Both tables: remove row button
    // -------------------------------------------------------------------------
    ['map_table', 'grp_table'].forEach(tblId => {
        const tblEl = document.getElementById(tblId);
        if (!tblEl) return;
        delegate(tblEl, 'click', '.remove', function(event) {
            const tr = this.closest('tr');
            if (tblId === 'grp_table') {
                groupTable.row(tr).remove().draw();
                groupTable.order([0, 'asc']).draw();
                updateGroupsLegend();
            } else {
                const currentData = table.rows().data().toArray();
                const rowIndex = table.row(tr).index();
                currentData.splice(rowIndex, 1);

                document.getElementById('map').innerHTML = '';
                document.getElementById('ul_legend').innerHTML = '';
                table.clear();

                currentData.forEach((data, idx) => {
                    const areaIndex = idx + 1;
                    table.row.add(data);
                    updateAreaAndLegend(data, areaIndex);
                });
                table.draw();
            }
        });
    });

    // -------------------------------------------------------------------------
    // Custom 'clearButton' event from DataTables clear-input widget
    // -------------------------------------------------------------------------
    document.addEventListener('clearButton', (e) => {
        const input = e.target.closest('input');
        if (!input) return;
        e.stopPropagation();

        const row = input.closest('tr');
        const cell = input.closest('td');
        const tableId = row?.closest('table')?.id;

        if (!tableId || (tableId !== 'map_table' && tableId !== 'grp_table')) return;

        const tableApi = tableId === 'map_table' ? window.table : window.groupTable;
        const rowData = tableApi.row(row).data();
        if (!rowData) return;

        const rowIndex = tableApi.row(row).index();
        const cellIndex = Array.from(row.children).indexOf(cell);
        const column = tableApi.cell(cell).index();
        const columnField = tableApi.column(column.column).dataSrc();

        if (columnField) {
            rowData[columnField] = '';
            tableApi.row(row).data(rowData).draw(false);

            if (tableId === 'map_table') {
                const rowNum = row.id.replace('row', '');
                const legendSpan = document.querySelector(`#a${rowNum} span`);
                if (legendSpan) legendSpan.innerHTML = createLegendHTML(rowNum, rowData);
            }

            setTimeout(() => {
                const newRow = tableApi.row(rowIndex).node();
                const newCell = newRow?.children[cellIndex];
                const newInput = newCell?.querySelector('input');
                if (newInput) newInput.focus();
            }, 0);
        }
    });

    // -------------------------------------------------------------------------
    // Hover legend (#ul_legend)
    // -------------------------------------------------------------------------
    const ulLegend = document.getElementById('ul_legend');
    if (ulLegend) {
        delegate(ulLegend, 'mouseover', '.legend', function() {
            const areaId = this.id.replace(/[a-z]/g, '');
            document.getElementById(`area${areaId}`)
                ?.dispatchEvent(new MouseEvent('mouseover', { bubbles: true }));
            document.getElementById(`row${areaId}`)?.classList.add('in');
        });
        delegate(ulLegend, 'mouseout', '.legend', function() {
            const areaId = this.id.replace(/[a-z]/g, '');
            document.getElementById(`area${areaId}`)
                ?.dispatchEvent(new MouseEvent('mouseout', { bubbles: true }));
            document.getElementById(`row${areaId}`)?.classList.remove('in');
        });
        delegate(ulLegend, 'click', '.legend-number', function(e) {
            e.stopPropagation();
            const legend = this.closest('.legend');
            const areaId = legend?.id.replace(/[a-z]/g, '');
            const row = document.getElementById(`row${areaId}`);
            if (row) {
                row.scrollIntoView({ behavior: 'smooth', block: 'center' });
                const input = row.querySelector('input:not(:disabled)');
                if (input) { input.focus(); input.select(); }
            }
        });
    }

    // -------------------------------------------------------------------------
    // Area hover on the image map — highlight + batch refresh
    // -------------------------------------------------------------------------
    const mapEl = document.getElementById('map');
    if (mapEl) {
        delegate(mapEl, 'mouseover', 'area', function() {
            const areaId = this.id.replace(/[a-z]/g, '');
            maphilight.set(this, {
                ...MAPHILIGHT_CONFIG,
                strokeWidth: 2,
                strokeOpacity: 1,
                strokeColor: 'ffaa00'
            });
            document.querySelector(`#a${areaId} span`)?.classList.add('in');
            document.getElementById(`row${areaId}`)?.classList.add('in');
        });
        delegate(mapEl, 'mouseout', 'area', function() {
            const areaId = this.id.replace(/[a-z]/g, '');
            maphilight.clear(this);
            document.querySelector(`#a${areaId} span`)?.classList.remove('in');
            document.getElementById(`row${areaId}`)?.classList.remove('in');
        });
    }

    // -------------------------------------------------------------------------
    // Groups legend (#ul_groups_legend) hover/click
    // -------------------------------------------------------------------------
    const ulGroups = document.getElementById('ul_groups_legend');
    if (ulGroups) {
        delegate(ulGroups, 'mouseover', '.group-legend', function(e) {
            if (this.contains(e.relatedTarget)) return;
            handleGroupHover('enter', this.dataset.group);
        });
        delegate(ulGroups, 'mouseout', '.group-legend', function(e) {
            if (this.contains(e.relatedTarget)) return;
            handleGroupHover('leave');
        });
        delegate(ulGroups, 'click', '.legend-number', function(e) {
            e.stopPropagation();
            const groupLegend = this.closest('.group-legend');
            const groupNum = groupLegend?.dataset.group;
            const row = document.getElementById(`g_row${groupNum}`);
            if (row) {
                row.scrollIntoView({ behavior: 'smooth', block: 'center' });
                const input = row.querySelector('input');
                if (input) { input.focus(); input.select(); }
            }
        });
    }

    const resetGroupHighlights = () => {
        document.querySelectorAll('.group-highlight')
            .forEach(el => el.classList.remove('group-highlight'));

        const img = document.getElementById('image');
        document.querySelectorAll('area').forEach(areaEl => {
            const rowId = areaEl.id.replace('area', '');
            const rowData = table.row(`#row${rowId}`).data();

            if (rowData && rowData.group) {
                const cfg = { ...MAPHILIGHT_CONFIG, groupBy: `.group${rowData.group}` };
                areaEl.setAttribute('data-maphilight', JSON.stringify(cfg));
                areaEl.setAttribute('class', `group${rowData.group}`);
            } else {
                areaEl.removeAttribute('data-maphilight');
                areaEl.removeAttribute('class');
            }
        });
        if (img) maphilight.refresh(img);
    };
    document.getElementById('ul_groups_legend')
        ?.addEventListener('mouseleave', resetGroupHighlights);
    document.querySelector('#grp_table tbody')
        ?.addEventListener('mouseleave', resetGroupHighlights);

    // -------------------------------------------------------------------------
    // Search input handlers
    // -------------------------------------------------------------------------
    document.querySelectorAll('.search-input').forEach(input => {
        const debouncedSearch = Utils.debounce((e) => {
            const value = e.target.value;
            const hasSearch = value.length > 0;
            const handles = document.querySelectorAll('.drag-handle');
            const moveButtons = document.querySelectorAll('.btn-up, .btn-down');

            if (hasSearch) {
                handles.forEach(h => { h.style.cursor = 'default'; h.setAttribute('title', ''); });
                moveButtons.forEach(b => { b.style.visibility = 'hidden'; });
                table.rowReorder.disable();
            } else {
                handles.forEach(h => { h.style.cursor = 'grab'; h.setAttribute('title', GW.i18n.moveDrag); });
                moveButtons.forEach(b => { b.style.visibility = ''; });
                table.rowReorder.enable();
            }
            table.search(value).draw();
        }, 300);

        input.addEventListener('input', debouncedSearch);
        input.addEventListener('search', debouncedSearch);

        input.addEventListener('keyup', (e) => {
            if (e.key === 'Escape') {
                e.target.value = '';
                document.querySelectorAll('.drag-handle').forEach(h => {
                    h.style.cursor = 'grab';
                    h.setAttribute('title', GW.i18n.moveDrag);
                });
                document.querySelectorAll('.btn-up, .btn-down').forEach(b => {
                    b.style.visibility = '';
                });
                table.rowReorder.enable();
                table.search('').draw();
            }
        });
    });

    // Handlers image redraw
    const fname = document.getElementById('fname');
    if (fname) {
        fname.addEventListener('input', HandlerManager.imageChange);
        fname.addEventListener('change', HandlerManager.imageChange);
    }
}

function initializePageContent(data, tableApi) {
    // Setup search functionality
    const searchInput = document.createElement('input');
    searchInput.type = 'text';
    searchInput.placeholder = GW.i18n.search;
    searchInput.className = 'form-control form-control-sm search-input';
    const headerTh = tableApi.table().header().querySelector('th');
    if (headerTh) headerTh.appendChild(searchInput);
    document.getElementById('search-clear')?.remove();

    if (!data) return;

    // Initialize form fields
    const setVal = (id, v) => { const el = document.getElementById(id); if (el) el.value = v; };
    setVal('page_title', data.title || '');
    setVal('album_chronicle', data.chronicle || '');
    setVal('page_desc', data.desc || '');
    if (albumImages[albumCurrent]?.desc) {
        setVal('page_desc', albumImages[albumCurrent].desc);
    }

    // Initialize image if present
    const imgFile = albumImages[albumCurrent]?.img;
    const img = document.getElementById('image');
    if (imgFile && img) {
        fnameSet(imgFile);
        img.src = '';           // clear previous image immediately
        img.hidden = false;
        // Attach listener BEFORE setting src to catch cached-image synchronous load
        img.addEventListener('load', function onLoad() {
            const width = img.width, height = img.height;
            requestAnimationFrame(() => {
                const frame = document.getElementById('frame');
                if (frame) {
                    frame.style.display = '';
                    frame.style.width = width + 'px';
                    frame.style.height = height + 'px';
                    const legendContainer = frame.closest('#div_img_legend');
                    if (legendContainer) legendContainer.style.width = width + 'px';
                }
                maphilight(img, MAPHILIGHT_CONFIG);
            });
        }, { once: true });
        img.onerror = () => {
            img.src = '';
            img.hidden = true;
            const frame = document.getElementById('frame');
            if (frame) frame.style.display = 'none';
            toggleTables(false);
            img.onerror = null;
        };
        // Correct IMA/DOC routing, set LAST
        img.src = imgFile.startsWith('albums/')
            ? GW.prefix + 'm=IMA&s=' + encodeURI(imgFile.substring(7))
            : GW.prefix + 'm=DOC&s=' + encodeURI(imgFile);
    }
    updateAlbumNav();

    // Initialize map and legend once
    const mapData = albumImages[albumCurrent]?.map;
    if (mapData?.length) {
        document.getElementById('map').innerHTML = '';
        document.getElementById('ul_legend').innerHTML = '';
        mapData.forEach(item => {
            const areaIndex = ++currentAreaCount;
            updateAreaAndLegend(item, areaIndex);
        });
    }
}

function toggleTables(show) {
    const fnameEl = document.getElementById('fname');
    const shouldShow = show || (fnameEl && fnameEl.value);
    ['map_table', 'grp_table'].forEach(id => {
        const tbl = document.getElementById(id)?.closest('table');
        if (tbl) tbl.style.display = shouldShow ? '' : 'none';
    });
}

// Initialize both data tables with proper error handling and state management
function initTables() {
    const isNew = new URLSearchParams(window.location.search).has('new');
    // MD5("") matches Mutil.digest "" used server-side for new-note digest check
    const source = isNew
        ? Promise.resolve({
            digest: 'd41d8cd98f00b204e9800998ecf8427e',
            r: { title: '', chronicle: '', images: [] }
          })
        : fetch(GW.url + '&ajax=on').then(r => r.json());
    source
        .then(json => {
            if (!json || !json.digest) {
                console.error('Invalid JSON response');
                return;
            }
            // Set digest IMMEDIATELY, independently of DataTable lifecycle
            const digestEl = document.getElementById('digest');
            if (digestEl) digestEl.value = json.digest;

            if (json.r?.images) {
                albumImages = json.r.images;
            } else if (json.r) {
                albumImages = [{
                    img: json.r.img || '',
                    desc: json.r.desc || '',
                    map: json.r.map || [],
                    groups: json.r.groups || []
                }];
            }

            const p = new URLSearchParams(window.location.search);
            const si = parseInt(p.get('img')) || 0;
            if (si > 0 && si <= albumImages.length) albumCurrent = si - 1;

            const cur = albumImages[albumCurrent];
            const fnameVal = document.getElementById('fname')?.value;
            toggleTables(Boolean(cur?.img || fnameVal));

            // -----------------------------------------------------------------
            // Main table (DataTables 2.x vanilla constructor)
            // -----------------------------------------------------------------
            table = new DataTable('#map_table', {
                data: albumImages[albumCurrent]?.map || [],
                layout: { topStart: null, topEnd: null, bottomStart: null, bottomEnd: null },
                deferRender: false,
                paging: false,
                ordering: true,
                order: [[0, 'asc']],
                searching: true,
                info: false,
                autoWidth: false,
                language: {
                    emptyTable: GW.i18n.emptyTable + '<br>' + GW.i18n.drawSelection
                },
                rowReorder: {
                    selector: 'div.drag-handle',
                    update: false,
                },
                columns: [
                    {
                        data: null, type: 'num', autoWidth: false, orderable: false,
                        className: 'search-container border-end',
                        render: function(data, type, row, meta) {
                            if (type === 'sort') return (Number(meta.row) + 1);
                            return '<div class="d-inline-flex align-items-center w-100">' +
                                   '<button type="button" class="btn btn-sm btn-outline-secondary redraw-area border-0" title="' + GW.i18n.redraw + '"><i class="far fa-pen-to-square"></i></button>' +
                                   '<button type="button" class="btn btn-sm btn-outline-secondary btn-up ms-2" title="' + GW.i18n.moveUp + '">↑</button>' +
                                   '<button type="button" class="btn btn-sm btn-outline-secondary btn-down ms-1" title="' + GW.i18n.moveDown + '">↓</button>' +
                                   '<div class="ms-auto me-1 drag-handle" title="' + GW.i18n.moveDrag + '">' + (Number(meta.row) + 1) + '</div></div>';
                        }
                    },
                    {
                        data: 't', defaultContent: 'p',
                        render: function(data, type, row, meta) {
                            if (type === 'display') {
                                const types = { p: GW.i18n.person, g: 'GeneWeb', e: 'Web' };
                                let html = '<select class="form-select form-select-sm update type" row="' + meta.row + '">';
                                Object.entries(types).forEach(([value, label]) => {
                                    const selected = (data === value || (!data && value === 'p')) ? ' selected' : '';
                                    html += '<option value="' + value + '"' + selected + '>' + label + '</option>';
                                });
                                html += '</select>';
                                return html;
                            }
                            return data;
                        }
                    },
                    {
                        data: 'fn', type: 'string', defaultContent: '',
                        render: function(data, type, row) {
                            if (type === 'sort' || type === 'type') return get(data).toLowerCase();
                            const disabled = (get(row.t) !== '' && row.t !== 'p') ? ' disabled' : '';
                            const dl = GW.hasFnList ? ' list="datalist_fnames" data-book="fn"' : '';
                            return '<input class="form-control update p-input" type="text" value="' + get(data) + '"' + disabled + dl + '>';
                        }
                    },
                    {
                        data: 'sn', type: 'string', defaultContent: '',
                        render: function(data, type, row) {
                            if (type === 'sort' || type === 'type') return get(data).toLowerCase();
                            const disabled = (get(row.t) !== '' && row.t !== 'p') ? ' disabled' : '';
                            const dl = GW.hasSnList ? ' list="datalist_snames" data-book="sn"' : '';
                            return '<input class="form-control update p-input" type="text" value="' + get(data) + '"' + disabled + dl + '>';
                        }
                    },
                    {
                        data: 'oc', defaultContent: '', orderable: false,
                        render: function(data, type, row) {
                            const disabled = (get(row.t) !== '' && row.t !== 'p') ? ' disabled' : '';
                            return '<input class="form-control update p-input clear-button" type="number" value="' + get(data) + '" min="0" step="1"' + disabled + '>';
                        }
                    },
                    {
                        data: 'href', defaultContent: '',
                        render: function(data, type, row) {
                            const disabled = (row.t !== 'e' && row.t !== 'g') ? ' disabled' : '';
                            const placeholder = row.t === 'e' ? ' placeholder="https://…"'
                                             : row.t === 'g' ? ' placeholder="m=…"' : '';
                            return '<input class="form-control update u-input" type="text" value="' + get(data) + '"' + placeholder + disabled + '>';
                        }
                    },
                    {
                        data: 'alt', defaultContent: '',
                        render: function(data) {
                            return '<input class="form-control update" type="text" value="' + get(data) + '">';
                        }
                    },
                    {
                        data: 'group',
                        render: function(data, type) {
                            if (type === 'sort') return parseInt(data) || 0;
                            return '<input class="form-control update clear-button w-100" type="number" value="' + get(data) + '" min="0" step="1">';
                        }
                    },
                    {
                        data: null, orderable: false,
                        defaultContent: '<button type="button" class="btn btn-link text-danger remove px-1" title="' + GW.i18n.del + '"><i class="fa fa-trash-can"></i></button>'
                    },
                ],
                createdRow: function(row, data) {
                    if ((!data.t || data.t === 'p') &&
                        data.fn && data.sn &&
                        typeof data.valid === 'boolean') {
                        if (!data.valid) row.classList.add('row-invalid');
                        row.dataset.pnocChecked =
                            data.fn + '|' + data.sn + '|' + (data.oc || '0');
                    }
                },
                rowCallback: function(row, data, index) {
                    const rowId = row.getAttribute('id')?.replace(/[a-z]/g, '') || '';
                    if (!rowId) return;

                    if (data.t === 'p' || !data.t) {
                        if (data.fn && data.sn) {
                            const oc = get(data.oc);
                            const oc2 = (oc && oc !== '0') ? '/' + oc : '';
                            const txt = get(data.alt);
                            data.gw = '[[' + data.fn + '/' + data.sn + oc2 +
                                      (txt ? '/' + data.fn + ' ' + data.sn : '') + ']]';
                        } else {
                            delete data.gw;
                        }
                    } else {
                        delete data.gw;
                    }
                    row.id = `row${rowId}`;
                },
                initComplete: function() {
                    const digest = document.getElementById('digest');
                    if (digest) digest.value = json.digest;
                    initializePageContent(json.r, this.api());
                },
                preDrawCallback: function(settings) {
                    const activeElement = document.activeElement;
                    if (activeElement && (activeElement.tagName === 'INPUT' || activeElement.tagName === 'SELECT')) {
                        const tr = activeElement.closest('tr');
                        const td = activeElement.closest('td');
                        settings._focusInfo = {
                            rowId: tr?.id,
                            cellIndex: td ? Array.from(tr.children).indexOf(td) : -1,
                            value: activeElement.value,
                            selectionStart: activeElement.selectionStart,
                            selectionEnd: activeElement.selectionEnd
                        };
                    }
                    return true;
                },
                drawCallback: function(settings) {
                    if (!settings || !settings._focusInfo) return;
                    const focusInfo = settings._focusInfo;
                    if (!focusInfo.rowId || focusInfo.cellIndex < 0) return;

                    requestAnimationFrame(() => {
                        try {
                            const row = document.getElementById(focusInfo.rowId);
                            if (!row) return;
                            const cell = row.children[focusInfo.cellIndex];
                            if (!cell) return;
                            const input = cell.querySelector('input, select');
                            if (!input) return;

                            input.focus();
                            if (typeof input.setSelectionRange === 'function' &&
                                typeof focusInfo.selectionStart === 'number' &&
                                typeof focusInfo.selectionEnd === 'number') {
                                input.setSelectionRange(focusInfo.selectionStart, focusInfo.selectionEnd);
                            }
                        } catch (error) {
                            console.warn('Error restoring focus:', error);
                        } finally {
                            delete settings._focusInfo;
                        }
                    });
                }
            });

            // -----------------------------------------------------------------
            // Group table
            // -----------------------------------------------------------------
            groupTable = new DataTable('#grp_table', {
                data: albumImages[albumCurrent]?.groups || [],
                layout: { topStart: null, topEnd: null, bottomStart: null, bottomEnd: null },
                deferRender: false, paging: false, ordering: true,
                searching: false, info: false,
                language: { emptyTable: GW.i18n.emptyTable + '<br>' + GW.i18n.addGroup },
                columns: [
                    {
                        data: 'name', type: 'numeric', orderable: false,
                        render: function(data, type) {
                            if (type === 'sort' || type === 'type') return parseInt(get(data)) || 0;
                            return '<div class="text-center">' + get(data) + '</div>';
                        }
                    },
                    {
                        data: 'label', orderable: false,
                        render: function(data, type, row, meta) {
                            if (type === 'display') {
                                return '<input class="form-control update" type="text" value="' + get(data) + '" data-index="' + meta.row + '">';
                            }
                            return data;
                        }
                    },
                    {
                        data: null, orderable: false,
                        defaultContent: '<button type="button" class="btn btn-link text-danger remove px-1" title="' + GW.i18n.del + '"><i class="fa fa-trash-can"></i></button>'
                    }
                ],
                rowCallback: function(row, data, index) {
                    row.id = `g_row${index + 1}`;
                    const label = get(data.label);
                    if (label) {
                        const gEl = document.getElementById(`g${index + 1}`);
                        if (gEl) gEl.innerHTML = `<span>${index + 1} : ${label}</span>`;
                    }
                },
                initComplete: function() {
                    if (albumImages[albumCurrent]?.groups?.length) {
                        groupTable = this.api();
                        updateGroupsLegend();
                        groupTable.on('draw', updateGroupsLegend);
                    }
                }
            });

            table.rows().every(function(rowIdx) {
                const node = this.node();
                if (node) node.id = `row${rowIdx + 1}`;
            });

            // Initialize all event handlers after tables are ready
            initializeHandlers();
        })
        .catch(error => {
            console.error('Error fetching table data:', error);
        });
}

// Handler for clearing image and resetting form
function resetButtonHandler() {
    const clearBtn = document.getElementById('clear-all');
    if (!clearBtn) return;
    clearBtn.addEventListener('click', () => {
        ['page_title', 'album_chronicle', 'page_desc', 'fname'].forEach(id => {
            const el = document.getElementById(id); if (el) el.value = '';
        });
        const frame = document.getElementById('frame');
        if (frame) { frame.style.width = 'auto'; frame.style.height = 'auto'; }
        const img = document.getElementById('image');
        if (img) {
            img.setAttribute('src', '');
            const parent = img.parentElement;
            if (parent) { parent.style.background = 'none'; parent.style.height = 'auto'; }
        }
        const selectionEl = document.getElementById('selection');
        if (selectionEl) selectionEl.style.display = 'none';
        document.getElementById('map').innerHTML = '';
        document.getElementById('ul_legend').innerHTML = '';
        document.getElementById('ul_groups_legend').innerHTML = '';
        if (img) maphilight.refresh(img);
        const redrawTarget = document.getElementById('redraw-target');
        if (redrawTarget) {
            Object.keys(redrawTarget.dataset).forEach(k => delete redrawTarget.dataset[k]);
        }
        table.clear().draw();
        groupTable.clear().draw();
        toggleTables(false);
        currentAreaCount = 0;
        albumImages = [{ img: '', map: [], groups: [] }];
        albumCurrent = 0;
        updateAlbumNav();
        window.scrollTo({ top: 0, behavior: 'smooth' });
    });
}

function saveAlbumCurrent() {
    albumImages[albumCurrent] = {
        img:    document.getElementById('fname')?.value || '',
        desc:   document.getElementById('page_desc')?.value || '',
        map:    cleanMapEntries(),
        groups: currentGroups()
    };
}

function fnameSet(value) {
    if (!fnameTS) {
        const el = document.getElementById('fname');
        if (el) el.value = value || '';
        return;
    }
    if (!value) { fnameTS.clear(true); return; }
    if (!fnameTS.options[value]) {
        fnameTS.addOption({ value: value, text: value });
    }
    fnameTS.setValue(value, true);
}

function loadAlbumImage(index) {
    albumCurrent = index;
    const imgData = albumImages[index] || { img: '', map: [], groups: [] };

    // Clear current state
    table.clear();
    groupTable.clear();
    ['map', 'ul_legend', 'ul_groups_legend'].forEach(id => {
        const el = document.getElementById(id); if (el) el.innerHTML = '';
    });
    currentAreaCount = 0;

    // Reload map data
    if (imgData.map?.length) table.rows.add(imgData.map);
    table.draw(false);
    if (imgData.groups?.length) groupTable.rows.add(imgData.groups);
    groupTable.draw(false);
    updateGroupsLegend();

    // Update filename and load image
    fnameSet(imgData.img);
    const descEl = document.getElementById('page_desc');
    if (descEl) descEl.value = imgData.desc || '';

    const imgEl = document.getElementById('image');
    const frameEl = document.getElementById('frame');

    if (imgData.img) {
        const src = imgData.img.startsWith('albums/')
            ? GW.prefix + 'm=IMA&s=' + encodeURI(imgData.img.substring(7))
            : GW.prefix + 'm=DOC&s=' + encodeURI(imgData.img);
        // Clear previous image immediately to prevent bleed-through on 404
        imgEl.src = '';
        imgEl.hidden = false;
        // Attach listener BEFORE setting src to catch cached-image synchronous load
        imgEl.addEventListener('load', function onLoad() {
            const w = imgEl.width, h = imgEl.height;
            requestAnimationFrame(() => {
                if (frameEl) {
                    frameEl.style.display = '';
                    frameEl.style.width = w + 'px';
                    frameEl.style.height = h + 'px';
                    const legendContainer = frameEl.closest('#div_img_legend');
                    if (legendContainer) legendContainer.style.width = w + 'px';
                }
                maphilight(imgEl, MAPHILIGHT_CONFIG);
            });
        }, { once: true });
        imgEl.onerror = () => {
            imgEl.src = '';
            imgEl.hidden = true;
            if (frameEl) frameEl.style.display = 'none';
            toggleTables(false);
            imgEl.onerror = null;
        };
        imgEl.src = src;   // set LAST
        toggleTables(true);
    } else {
        imgEl.onerror = null;
        imgEl.src = '';
        imgEl.hidden = true;
        if (frameEl) frameEl.style.display = 'none';
        toggleTables(false);
    }
    updateAlbumNav();
    const u = new URL(window.location);
    u.searchParams.set('img', index + 1);
    history.replaceState(null, '', u);
}

function updateAlbumNav() {
    const n = albumImages.length;
    const cur = document.getElementById('album-current');
    if (cur) cur.textContent = albumCurrent + 1;
    const cnt = document.getElementById('album-count');
    if (cnt) cnt.textContent = n;

    const multi = n > 1;
    const nav = document.getElementById('album-nav');
    if (nav) {
        nav.classList.toggle('d-none', !multi);
        nav.classList.toggle('d-flex', multi);
    }
    const actions = document.getElementById('album-actions');
    const target = document.getElementById(
        multi ? 'album-actions-mount-nav' : 'album-actions-mount-solo');
    if (actions && target && actions.parentElement !== target) {
        target.appendChild(actions);
    }
    const removeBtn = document.getElementById('album-remove');
    if (removeBtn) removeBtn.classList.toggle('d-none', !multi);
    const atStart = albumCurrent <= 0;
    const atEnd = albumCurrent >= n - 1;
    ['album-first', 'album-prev'].forEach(id => {
        const el = document.getElementById(id);
        if (el) {
            el.classList.toggle('disabled', atStart);
            el.classList.toggle('text-body-secondary', atStart);
        }
    });
    ['album-next', 'album-last'].forEach(id => {
        const el = document.getElementById(id);
        if (el) {
            el.classList.toggle('disabled', atEnd);
            el.classList.toggle('text-body-secondary', atEnd);
        }
    });

}

function initAlbumHandlers() {
    const bind = (id, handler) => {
        const el = document.getElementById(id);
        if (el) el.addEventListener('click', (e) => {
            e.preventDefault();
            handler(e);
        });
    };

    const addFn = () => {
        saveAlbumCurrent();
        const insertAt = albumCurrent + 1;
        albumImages.splice(insertAt, 0,
            { img: '', desc: '', map: [], groups: [] });
        loadAlbumImage(insertAt);
    };
    bind('album-add', addFn);

    bind('album-remove', () => {
        if (albumImages.length > 1) {
            albumImages.splice(albumCurrent, 1);
            if (albumCurrent >= albumImages.length) {
                albumCurrent = albumImages.length - 1;
            }
            loadAlbumImage(albumCurrent);
        }
    });

    bind('album-first', () => { saveAlbumCurrent(); loadAlbumImage(0); });
    bind('album-prev', () => {
        if (albumCurrent > 0) {
            saveAlbumCurrent();
            loadAlbumImage(albumCurrent - 1);
        }
    });
    bind('album-next', () => {
        if (albumCurrent < albumImages.length - 1) {
            saveAlbumCurrent();
            loadAlbumImage(albumCurrent + 1);
        }
    });
    bind('album-last', () => {
        saveAlbumCurrent();
        loadAlbumImage(albumImages.length - 1);
    });
}

function initFnameSelect() {
    const fnameEl = document.getElementById('fname');
    if (!fnameEl || typeof TomSelect === 'undefined') return;

    const buildOpts = (listId, optgroup) =>
        [...document.querySelectorAll('#' + listId + ' option')]
            .map(o => ({ value: o.value, text: o.value, optgroup }));

    const folderOpts = buildOpts('src_albums', 'folders');
    const imageOpts = buildOpts('src_images', 'images');

    fnameTS = new TomSelect(fnameEl, {
        options: [...folderOpts, ...imageOpts],
        optgroups: [
            { value: 'folders', label: GW.i18n?.folders || 'folders' },
            { value: 'images', label: GW.i18n?.images || 'images' }
        ],
        optgroupField: 'optgroup',
        labelField: 'text',
        valueField: 'value',
        searchField: ['text'],
        create: true,
        maxItems: 1,
        hideSelected: true,
        placeholder: fnameEl.placeholder,
        render: {
            option: (data, escape) => {
                const icon = data.optgroup === 'folders'
                    ? '<i class="fa-solid fa-folder text-body-secondary me-1"></i>'
                    : '<i class="fa-regular fa-image text-body-secondary me-1"></i>';
                return '<div>' + icon + escape(data.text) + '</div>';
            },
            option_create: (data, escape) =>
                '<div class="create">' +
                (GW.i18n.addItem || 'Add') + ' <strong>' +
                escape(data.input) + '</strong>&hellip;</div>',
            no_results: () =>
                '<div class="no-results">' +
                (GW.i18n.noResult || 'No results found') + '</div>'
        },
        onItemAdd: (value) => {
            const opt = fnameTS.options[value];
            if (opt?.optgroup === 'folders') importFolder(value);
        }
    });
}

function importFolder(folder) {
    fetch(GW.prefix + 'm=FOLDER_IMAGES&folder='
        + encodeURIComponent(folder))
        .then(r => {
            if (!r.ok) throw new Error('HTTP ' + r.status);
            return r.json();
        })
        .then(files => {
            if (!Array.isArray(files) || !files.length) {
                updateStatus('No images found in folder: ' + folder);
                fnameTS?.clear(true);
                return;
            }
            fnameTS?.clear(true);
            const newEntries = files.map(f =>
                ({ img: f, desc: '', map: [], groups: [] }));
            if (!albumImages[albumCurrent].img) {
                albumImages.splice(albumCurrent, 1, ...newEntries);
                loadAlbumImage(albumCurrent);
            } else {
                saveAlbumCurrent();
                const insertAt = albumCurrent + 1;
                albumImages.splice(insertAt, 0, ...newEntries);
                loadAlbumImage(insertAt);
            }
            updateStatus('Added ' + files.length +
                ' image(s) from "' + folder + '"');
        })
        .catch(err => updateStatus(
            'Error loading folder: ' + folder + ' -- ' + err.message));
}

function cleanMapEntries() {
    return table.rows().data().toArray().map(item => {
        const clean = {};
        if (item.shape)  clean.shape  = item.shape;
        if (item.coords) clean.coords = item.coords;
        if (item.t)      clean.t      = item.t;
        if (item.fn)     clean.fn     = item.fn;
        if (item.sn)     clean.sn     = item.sn;
        if (item.oc)     clean.oc     = item.oc;
        if (item.href)   clean.href   = item.href;
        if (item.alt)    clean.alt    = item.alt;
        if (item.group)  clean.group  = item.group;
        if (item.gw)     clean.gw     = item.gw;
        if (clean.t === 'p' || !clean.t) {
            delete clean.misc;
            delete clean.href;
            if (clean.fn && clean.sn) clean.gw = generateGwSyntax(clean);
        } else if (clean.t === 'g' || clean.t === 'e') {
            delete clean.fn;
            delete clean.sn;
            delete clean.oc;
            delete clean.gw;
        }
        return clean;
    });
}

function currentGroups() {
    return groupTable.rows().data().toArray().map((g, i) => ({
        name: i + 1,
        label: g.label
    }));
}

/**
 * Handles form submission and prepares data for saving
 * Formats JSON with proper spacing and validates data integrity
 */
function setupFormHandler() {
    const form = document.getElementById('form');
    if (!form) return;
    form.addEventListener('submit', () => {
        const fnameInput = document.getElementById('fnotes_name');
        const title = document.getElementById('page_title')?.value
            || fnameInput?.value
            || '…';
        if (typeof table !== 'undefined' && table) saveAlbumCurrent();
        const chronicle =
            document.getElementById('album_chronicle')?.value || '';
        const images = Array.isArray(albumImages) ? albumImages : [];
        const res = { title, chronicle, images };
        const jsonString = JSON.stringify(res, null, 2)
            .replace(/\[\{/g, '[\n  {')
            .replace(/\}\]/g, '}\n]')
            .replace(/\}\,\{/g, '},\n  {');
        const notesEl = document.getElementById('notes');
        if (notesEl) {
            notesEl.value =
                'TITLE=' + title + '\nTYPE=gallery\n' + jsonString;
        }
        const newFEl = document.getElementById('new_f');
        if (newFEl && fnameInput) {
            newFEl.value = fnameInput.value.trim();
        }
    });
}

const HandlerManager = {
    updateGwSyntax: function(data) {
        if (data.fn && data.sn) {
            data.gw = generateGwSyntax(data);
        } else {
            delete data.gw;
        }
    },

    handleGroupLabelUpdate: function(e) {
        const input = e.target;
        const tr = input.closest('tr');
        const rowIdx = groupTable.row(tr).index();
        const currentData = groupTable.row(rowIdx).data();

        currentData.label = input.value;
        groupTable.row(rowIdx).data(currentData).draw(false);
        TableManager.updateGroupLabels();
    },

    removal: function(event) {
        // `this` is the clicked button (caller invokes with .call(btn, e))
        const tr = this.closest('tr');
        const tableId = tr?.closest('table')?.id;

        if (tableId === 'grp_table') {
            groupTable.row(tr).remove().draw();
            groupTable.order([0, 'asc']).draw();
            updateGroupsLegend();
        } else {
            const currentData = table.rows().data().toArray();
            const rowIndex = table.row(tr).index();
            currentData.splice(rowIndex, 1);

            document.getElementById('map').innerHTML = '';
            document.getElementById('ul_legend').innerHTML = '';
            table.clear();

            currentData.forEach((data, idx) => {
                const areaIndex = idx + 1;
                table.row.add(data);
                updateAreaAndLegend(data, areaIndex);
            });
            table.draw();
        }
    },

    imageChange: function() {
        // `this` is the #fname input (addEventListener binds it)
        const fname = this.value;
        if (!fname) {
            const frame = document.getElementById('frame');
            if (frame) frame.style.display = 'none';
            toggleTables(false);
            updateAlbumNav();
            return;
        }

        const imgEl = document.getElementById('image');
        if (!imgEl) return;
        imgEl.src = GW.prefix + 'm=DOC&s=' + fname;

        imgEl.addEventListener('load', function onLoad() {
            const frame = document.getElementById('frame');
            const width  = imgEl.width;
            const height = imgEl.height;

            requestAnimationFrame(() => {
                if (frame) {
                    frame.style.display = '';
                    frame.style.width  = width  + 'px';
                    frame.style.height = height + 'px';
                    const legendContainer = frame.closest('#div_img_legend');
                    if (legendContainer) legendContainer.style.width = width + 'px';
                }
                toggleTables(true);
                updateAlbumNav();
                maphilight(imgEl, MAPHILIGHT_CONFIG);
            });
        }, { once: true });
    },
};

function populateReorderModal() {
    const esc = s => {
        const d = document.createElement('div');
        d.textContent = s ?? '';
        return d.innerHTML;
    };
    const tbody = document.querySelector('#reorder_table tbody');
    tbody.innerHTML = '';
    albumImages.forEach((img, i) => {
        const thumbSrc = img.img.startsWith('albums/')
            ? GW.prefix + 'm=IMA&s=' + encodeURI(img.img.substring(7))
            : GW.prefix + 'm=DOC&s=' + encodeURI(img.img);
        tbody.insertAdjacentHTML('beforeend', `
            <tr data-idx="${i}">
              <td class="text-nowrap reorder-btns align-middle">
                <button type="button" class="btn btn-sm btn-outline-secondary reorder-up" draggable="false" title="${GW.i18n.moveUp}">
                  <i class="fa-solid fa-arrow-up" draggable="false"></i></button>
                <button type="button" class="btn btn-sm btn-outline-secondary reorder-down" draggable="false" title="${GW.i18n.moveDown}">
                  <i class="fa-solid fa-arrow-down" draggable="false"></i></button>
              </td>
              <td class="reorder-num ps-2 border-start align-middle" title="${GW.i18n.moveDrag}">${i + 1}</td>
              <td title="${GW.i18n.moveDrag}"><img src="${thumbSrc}" style="max-height:60px"></td>
              <td title="${GW.i18n.moveDrag}">${esc(img.img)}</td>
              <td title="${GW.i18n.moveDrag}">${esc(img.desc || '')}</td>
            </tr>`);
    });

    refreshReorderNumbers();

    // Initialize html5sortable on each populate
    sortable(tbody, {
        items: 'tr',
        placeholderClass: 'reorder-placeholder',
        draggingClass: 'reorder-dragging'
    });
}

function refreshReorderNumbers() {
    const rows = document.querySelectorAll('#reorder_table tbody tr');
    rows.forEach((tr, i) => {
        const cell = tr.querySelector('.reorder-num');
        if (cell) cell.textContent = i + 1;
        tr.querySelector('.reorder-up')?.classList
            .toggle('invisible', i === 0);
        tr.querySelector('.reorder-down')?.classList
            .toggle('invisible', i === rows.length - 1);
    });
}

function initReorderModal() {
    const tbody = document.querySelector('#reorder_table tbody');
    if (!tbody) return;

    // Up/down click (delegation, attached once)
    tbody.addEventListener('click', e => {
        const btn = e.target.closest('.reorder-up, .reorder-down');
        if (!btn) return;
        const tr = btn.closest('tr');
        if (!tr) return;
        const up = btn.classList.contains('reorder-up');
        if (up && tr.previousElementSibling) {
            tbody.insertBefore(tr, tr.previousElementSibling);
        } else if (!up && tr.nextElementSibling) {
            tbody.insertBefore(tr.nextElementSibling, tr);
        }
        refreshReorderNumbers();
    });

    // Sortable triggers this after any drag update
    tbody.addEventListener('sortupdate', refreshReorderNumbers);

    // OK button
    document.getElementById('reorder_ok')?.addEventListener('click', () => {
        const newOrder = [];
        tbody.querySelectorAll('tr').forEach(tr => {
            newOrder.push(albumImages[parseInt(tr.dataset.idx)]);
        });
        const currentImg = albumImages[albumCurrent];
        albumImages = newOrder;
        albumCurrent = albumImages.indexOf(currentImg);
        if (albumCurrent < 0) albumCurrent = 0;
        bootstrap.Modal.getInstance(
            document.getElementById('reorder_modal')).hide();
        loadAlbumImage(albumCurrent);
    });
}

function validateRow(tr, data, opts) {
    if (!tr || !data) return Promise.resolve();
    if ((data.t && data.t !== 'p') || !data.fn || !data.sn) {
        tr.classList.remove('row-invalid');
        delete tr.dataset.pnocChecked;
        return Promise.resolve();
    }
    const sig = data.fn + '|' + data.sn + '|' + (data.oc || '0');
    if (tr.dataset.pnocChecked === sig) return Promise.resolve();
    return PersonPicker.checkExact(data.fn, data.sn, data.oc || 0)
        .then(ok => {
            tr.dataset.pnocChecked = sig;
            tr.classList.toggle('row-invalid', !ok);
            if (ok && opts && opts.flash) {
                tr.classList.add('row-valid-flash');
                setTimeout(() => tr.classList.remove('row-valid-flash'), 900);
            }
        });
}

function validateAllPnocs() {
    if (!table) return;
    const data = table.rows().data().toArray();
    const nodes = table.rows().nodes();
    Promise.all(data.map((d, i) => validateRow(nodes[i], d)));
}

document.addEventListener('DOMContentLoaded', () => {
    initTables();
    initAlbumHandlers();
    SelectionManager.init();
    TableManager.init();
    UIManager.init();
    setupFormHandler();
    resetButtonHandler();
    initFnameSelect();

    if (typeof addClearButtonToInputs === 'function') addClearButtonToInputs();
    if (typeof inputToBook !== 'undefined' && inputToBook.addNavigation) {
        inputToBook.addNavigation();
    }
    if (typeof populateDatalists === 'function') populateDatalists();
    
    initReorderModal();
    document.getElementById('reorder_modal')
        ?.addEventListener('show.bs.modal', populateReorderModal);
});
