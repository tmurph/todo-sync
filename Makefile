all: test

test:
	emacs -batch -L ./org_asana/tests \
-l org-config.el \
-l test-org-interaction.el \
-f ert-run-tests-batch-and-exit
