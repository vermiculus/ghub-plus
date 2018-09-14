EMAKE_SHA1 = 1b23379eb5a9f82d3e2d227d0f217864e40f23e0
EMACS_ARGS = --eval "(setq checkdoc-arguments-in-order-flag nil)"
PACKAGE_BASENAME = ghub+

ifeq ($(MELPA_STABLE),true)
PACKAGE_ARCHIVES = gnu melpa-stable
else
PACKAGE_ARCHIVES = gnu melpa
endif
PACKAGE_TEST_DEPS = dash s
PACKAGE_TEST_ARCHIVES = gnu melpa

include emake.mk

.PHONY: clean

clean: ## Clean generated files
	rm -rf $(EMAKE_WORKDIR)
	rm *.elc

test-ert: EMACS_ARGS = -L ./test/
#test: test-ert
