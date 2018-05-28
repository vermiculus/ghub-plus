EENVS  = PACKAGE_FILE="ghub+.el"
EENVS += PACKAGE_LISP="ghub+.el"
EENVS += PACKAGE_TESTS="test/ert-tests.el"
EENVS += PACKAGE_ARCHIVES="gnu melpa"
EENVS += PACKAGE_TEST_DEPS="dash s"
EENVS += PACKAGE_TEST_ARCHIVES="melpa"
EMAKE := $(EENVS) emacs -batch -l emake.el \
	--eval "(setq checkdoc-arguments-in-order-flag nil)" \
	--eval "(emake (pop argv))"
.PHONY: clean setup install compile test

emake.el:
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'

emacs-travis.mk:
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

.elpa/:
	$(EMAKE) install

clean:
	rm -f *.elc		# delete compiled files
	rm -rf .elpa/		# delete dependencies
	rm -rf .elpa.test/
	rm -f emacs-travis.mk	# delete scripts
	rm -f emake.el
setup: emacs emake.el
install: .elpa/

compile:
	rm -f *.elc
	$(EMAKE) compile ~error-on-warn

test: test-ert test-checkdoc
test-ert: .elpa/
	$(EMAKE) test		# could also do $(EMAKE) test ert

test-checkdoc: .elpa/
	$(EMAKE) test checkdoc
ifeq ($(CI),true)
emacs: emacs-travis.mk		# This is CI.  Emacs may not be available, so install it.
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs
	emacs --version
else
emacs:				# This is not CI.  Emacs should already be available.
	which emacs && emacs --version
endif
