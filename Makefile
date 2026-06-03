EMACS ?= emacs
ELPA   = $(HOME)/.emacs.d/elpa

# Latest installed copy of each dep, by mtime.  The agent-shell glob
# excludes our own elpa install (agent-shell-dashboard) which would
# otherwise shadow it.
AGENT_SHELL = $(shell ls -td $(ELPA)/agent-shell-2* | head -1)
SHELL_MAKER = $(shell ls -td $(ELPA)/shell-maker-* | head -1)
ACP         = $(shell ls -td $(ELPA)/acp-*         | head -1)
DASH        = $(shell ls -td $(ELPA)/dash-*        | head -1)
S           = $(shell ls -td $(ELPA)/s-*           | head -1)
HT          = $(shell ls -td $(ELPA)/ht-*          | head -1)
TRANSIENT   = $(shell ls -td $(ELPA)/transient-*   | head -1)

BATCH = $(EMACS) --batch -Q \
	-L $(AGENT_SHELL) -L $(SHELL_MAKER) -L $(ACP) \
	-L $(DASH) -L $(S) -L $(HT) -L $(TRANSIENT) \
	-L .

.PHONY: test compile load clean

test:
	$(BATCH) -l test/agent-shell-dashboard-test.el \
	         -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile $(wildcard agent-shell-dashboard*.el)

load:
	$(BATCH) --eval "(require 'agent-shell-dashboard)" \
	         --eval "(message \"loaded: %s\" (fboundp 'agent-shell-dashboard))"

clean:
	rm -f *.elc test/*.elc
