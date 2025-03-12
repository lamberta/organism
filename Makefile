# Use environment variable EMACS if set, otherwise use emacs in PATH
EMACS ?= emacs

GRAPHAEL_PATH = ~/src/lamberta/graphael

LIB_FILES = -l organism-utils.el \
						-l organism-entry.el \
						-l organism-graph.el \
						-l organism-capture.el \
						-l organism-display.el \
						-l organism.el

TEST_FILES = -l organism-entry-test.el \
						 -l organism-capture-test.el \
						 -l organism-graph-test.el \
						 -l organism-utils-test.el

all:
	$(EMACS) --quick --batch \
		--eval "(add-to-list 'load-path \"$(GRAPHAEL_PATH)\")" \
		$(LIB_FILES) \
		-f batch-byte-compile *.el

test:
	$(EMACS) --quick --batch \
		-l ert \
		--eval "(add-to-list 'load-path \"$(GRAPHAEL_PATH)\")" \
		--eval "(setq organism-debug-enabled t)" \
		$(LIB_FILES) \
		$(TEST_FILES) \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

.PHONY: all test clean
