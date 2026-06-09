function addClearButtonToInputs() {
    const selector =
        'input[type="text"]:not(.no-clear-button):not([readonly]):not([disabled]),' +
        'input[type="search"]:not(.no-clear-button):not([readonly]):not([disabled]),' +
        'textarea:not(.no-clear-button):not([readonly]):not([disabled]),' +
        '.clear-button:not([readonly]):not([disabled])';

    let observer = null;
    let scheduled = false;

    function skip(element) {
        return element.type === 'hidden' ||
            element.style.display === 'none' ||
            element.classList.contains('no-clear-button') ||
            element.closest('.ts-wrapper') ||
            element.tabIndex === -1;
    }

    function createClearButton(element) {
        const clearButton = document.createElement('i');
        clearButton.className = 'fas fa-xmark clear-button-icon';
        clearButton.title = '[*clear input]';

        clearButton.style.cssText = `
            position: absolute;
            right: 10px;
            top: ${element.tagName.toLowerCase() === 'textarea' ? '10px' : '50%'};
            transform: ${element.tagName.toLowerCase() === 'textarea' ? 'none' : 'translateY(-50%)'};
            cursor: pointer;
            color: #667fc9;
            font-size: 1em;
            opacity: 0;
            transition: opacity 0.2s ease-in-out, color 0.2s ease-in-out;
            pointer-events: auto;
        `;
        return clearButton;
    }

    function initializeInput(element) {
        if (element.dataset.clearInitialized || skip(element)) return;
        element.dataset.clearInitialized = 'true';

        let wrapper = element.parentNode;
        if (!wrapper.classList.contains('clear-button-wrapper') &&
            !wrapper.classList.contains('input-wrapper')) {
            const isInline = element.style.width === 'auto' ||
                             element.classList.contains('form-control-inline');
            wrapper = document.createElement('div');
            wrapper.className = 'clear-button-wrapper';
            wrapper.style.cssText = 'position:relative;display:inline-block;width:' +
                                    (isInline ? 'auto' : '100%');
            element.parentNode.insertBefore(wrapper, element);
            wrapper.appendChild(element);
        }

        element.style.paddingRight = '24px';

        const clearButton = createClearButton(element);
        wrapper.appendChild(clearButton);

        const updateVisibility = () => {
            clearButton.style.opacity = element.value ? '1' : '0';
        };

        element.addEventListener('input', updateVisibility);
        element.addEventListener('change', updateVisibility);
        element.addEventListener('focus', updateVisibility);

        clearButton.addEventListener('mouseenter', () => clearButton.style.color = '#335599');
        clearButton.addEventListener('mouseleave', () => clearButton.style.color = '#667fc9');

        clearButton.addEventListener('click', (e) => {
            e.preventDefault();
            e.stopPropagation();

            element.value = '';
            updateVisibility();

            // Detach the datalist while focusing so the X does not open a
            // (potentially huge) native suggestion popup; rebind it on the
            // next genuine user interaction.
            const listId = element.getAttribute('list');
            if (listId) {
                element.removeAttribute('list');
                const rebind = () => element.setAttribute('list', listId);
                element.addEventListener('pointerdown', rebind, { once: true });
                element.addEventListener('keydown', rebind, { once: true });
            }
            element.focus();

            requestAnimationFrame(() => {
                element.dispatchEvent(new Event('input', { bubbles: true }));
                element.dispatchEvent(new Event('change', { bubbles: true }));
            });
        });

        updateVisibility();
    }

    function processInputs() {
        if (observer) observer.disconnect();
        document.querySelectorAll(selector).forEach(initializeInput);
        if (observer) {
            observer.observe(document.body, { childList: true, subtree: true });
        }
    }

    function scheduleProcess() {
        if (scheduled) return;
        scheduled = true;
        requestAnimationFrame(() => {
            scheduled = false;
            processInputs();
        });
    }

    observer = new MutationObserver((mutations) => {
        for (const mutation of mutations) {
            if (mutation.addedNodes.length) {
                scheduleProcess();
                break;
            }
        }
    });

    scheduleProcess();
}