function addClearButtonToInputs() {
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
        if (element.dataset.clearInitialized) {
            const existingButton = element.parentNode.querySelector('.clear-button-icon');
            if (existingButton) {
                existingButton.style.opacity = element.value ? '1' : '0';
            }
            return;
        }

        element.dataset.clearInitialized = 'true';

        let wrapper = element.parentNode;
        if (!wrapper.classList.contains('clear-button-wrapper')) {
            wrapper = document.createElement('div');
            wrapper.className = 'clear-button-wrapper';
            wrapper.style.cssText = 'position:relative;display:inline-block;width:100%';
            element.parentNode.insertBefore(wrapper, element);
            wrapper.appendChild(element);
        }

        element.style.paddingRight = '24px';

        const clearButton = createClearButton(element);
        wrapper.appendChild(clearButton);

        const updateVisibility = () => {
            requestAnimationFrame(() => {
                clearButton.style.opacity = element.value ? '1' : '0';
            });
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
            element.focus();
            updateVisibility();

            const inputEvent = new CustomEvent('clearButton', {
                bubbles: true,
                detail: { element }
            });
            element.dispatchEvent(inputEvent);
            element.dispatchEvent(new Event('input', { bubbles: true }));
            element.dispatchEvent(new Event('change', { bubbles: true }));
        });

        updateVisibility();
    }

    function processInputs() {
        const selector = `
            input[type="text"]:not(.no-clear-button):not([readonly]):not([disabled]),
            input[type="search"]:not(.no-clear-button):not([readonly]):not([disabled]),
            textarea:not(.no-clear-button):not([readonly]):not([disabled]),
            .clear-button:not([readonly]):not([disabled])
        `;

        requestAnimationFrame(() => {
            document.querySelectorAll(selector).forEach(initializeInput);
        });
    }

    processInputs();

    const observer = new MutationObserver((mutations) => {
        let shouldProcess = false;
        mutations.forEach(mutation => {
            if (mutation.addedNodes.length ||
                mutation.type === 'attributes' &&
                (mutation.attributeName === 'type' || mutation.attributeName === 'class')) {
                shouldProcess = true;
            }
        });
        if (shouldProcess) {
            processInputs();
        }
    });

    observer.observe(document.body, {
        childList: true,
        subtree: true,
        attributes: true,
        attributeFilter: ['type', 'class']
    });
}