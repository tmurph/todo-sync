all: test

test:
	emacs -batch -L . \
-L org-mode \
-l test-ts-org-interaction.el \
-f ert-run-tests-batch-and-exit

install:
	cp ts-org-interaction.el ~/.emacs.d/site-lisp/

.PHONY: test install
