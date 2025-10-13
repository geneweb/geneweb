# make sure workdir is the one where the Makefile is
PWD := $(shell pwd)
BOLD := \033[1m
YELLOW := \033[33m
END := \033[0m
# user parameter whether or not to use a virtual environment
NO_VENV ?= 0
ifeq ($(NO_VENV),0)
else
VIRTUAL_ENV := NO_VENV
endif

init: $(VIRTUAL_ENV)_installed
	pip install -r requirements.txt
# 	only install in editable mode if not already installed
# 	this allows to update the package without reinstalling it
	pip show geneweb &>/dev/null || pip install -e .

%_installed:
	@echo -ne "$(BOLD)"
	@echo "You are already in a virtual environment: ${VIRTUAL_ENV}"
	@echo "We will use that one."
	@echo -ne "$(END)"
	pip install --upgrade pip

_installed:
	@echo -ne "$(BOLD)${YELLOW}"
	@echo "You are not in a virtual environment."
	@echo -ne "Please create one first.$(END)$(BOLD)\n"
	@echo "Here is how to create a new virtual environment in the current directory:"
	@echo -ne "$(END)$(BOLD)" "  python3 -m venv .venv" "\n"
	@echo "Then activate it with:"
	@echo -ne "$(END)$(BOLD)" "  source .venv/bin/activate" "\n"
	@echo -ne "Then run $(END)$(BOLD)'make init' again to continue the installation.\n"
	@echo -ne "$(YELLOW)If you want to use an existing virtual environment, activate it first." "\n$(END)"
	@exit 1

tests:
	pytest

tests-cov:
	pytest --cov=. --cov-branch

gen_test_doc:
	-pytest --cov=. --cov-branch --cov-report=term-missing:skip-covered --junitxml=tests/pytest_report.log
	sed -i '/^---/,$$d' TESTS.md
	echo "---" >> TESTS.md
	echo "" >> TESTS.md
	./tests/gen_doc_all.sh >> TESTS.md

.PHONY: init tests tests-cov gen_test_doc
