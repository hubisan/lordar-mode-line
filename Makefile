# Makefile
# Switch to Eask
# Use this as base
# https://github.com/emacs-lsp/lsp-mode/blob/master/Makefile

EMACS ?= emacs
EASK ?= eask

.PHONY: help all package install compile test \
        lint lint-package lint-checkdoc lint-indent lint-relint \
        clean clean-elc clean-autoloads \
        emacs 

help:
	$(info )
	$(info - make                 # Show this help)
	$(info - make all             # Run package, install, compile, test and lint)
	$(info - make package         # Build package artifact)
	$(info - make install         # Install the package)
	$(info - make compile         # Compiles the files to check for errors/warnings)
	$(info - make test            # Run tests with buttercup)
	$(info - make lint            # Clean autoloads and run linters)
	$(info - make lint-package    # Clean autoloads and run package-lint)
	$(info - make lint-checkdoc   # Clean autoloads and run checkdoc)
	$(info - make lint-indent     # Clean autoloads and run indent-lint)
	$(info - make lint-relint     # Clean autoloads and run relint)
	$(info - make clean           # Clean everything)
	$(info - make clean-elc       # Remove byte compiled files generated)
	$(info - make clean-autoloads # Remove generated autoloads file)
	$(info - make emacs           # Run Emacs with package and dependencies installed)
	$(info )
	@echo > /dev/null

all: clean package install compile test lint

package:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> PACKAGING'
	@$(EASK) package

install:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> INSTALL'
	@$(EASK) install

compile:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	@$(EASK) compile

test:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	@$(EASK) test buttercup

lint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	@$(EASK) clean autoloads --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> package-lint'
	@$(EASK) lint package --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> checkdoc'
	@$(EASK) lint checkdoc --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n' '>>> indent-lint'
	@$(EASK) lint indent --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> relint'
	@$(EASK) lint regexps --verbose 0

lint-package:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT package-lint'
	@$(EASK) clean autoloads
	@$(EASK) lint package

lint-checkdoc:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT checkdoc'
	@$(EASK) clean autoloads
	@$(EASK) lint checkdoc

lint-indent:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT indent-lint'
	@$(EASK) clean autoloads
	@$(EASK) lint indent

lint-relint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT relint'
	@$(EASK) clean autoloads
	@$(EASK) lint regexps

clean:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN ALL'
	@$(EASK) clean all

clean-elc:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN ELC'
	@$(EASK) clean elc

clean-autoloads:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN AUTOLOADS'
	@$(EASK) clean autoloads

emacs:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> RUN EMACS'
	@$(EASK) package
	@$(EASK) install
	@$(EASK) emacs &
