## Compiling executable variables ####################################

-include env.mk

LOAD_PATH ?= -L .
EMACSBIN ?= emacs
BATCH = $(EMACSBIN) -Q --batch $(LOAD_PATH)

PKG = languagetool
RQD_PKG  =

all: lisp

## Source files ######################################################

ELS  = $(PKG)-core.el
ELS += $(PKG)-issue.el
ELS += $(PKG)-java.el
ELS += $(PKG)-correction.el
ELS += $(PKG)-console.el
ELS += $(PKG)-server.el
ELS += $(PKG).el
ELCS = $(ELS:.el=.elc)

## Build order #######################################################

$(PKG)-core.el:
$(PKG)-issue.el:
$(PKG)-java.el:
$(PKG)-correction.el: $(PKG)-core.el
$(PKG)-console.el: $(PKG)-core.el $(PKG)-issue.el $(PKG)-java.el
$(PKG)-server.el: $(PKG)-core.el $(PKG)-issue.el $(PKG)-java.el
$(PKG).el:  $(PKG)-correction.el $(PKG)-console.el $(PKG)-server.el

## Build #############################################################

lisp: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) \
	--eval "(when (file-exists-p \"$@\") (delete-file \"$@\"))" \
    --eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $<

clean:
	@printf "Cleaning byte compiled lisp...\n"
	@rm -f *.elc

## CI integration ####################################################

ci: lisp

require: $(RQD_PKG)
