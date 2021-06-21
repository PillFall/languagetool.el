EMACS = emacs

EMACS_FLAGS = -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f package-initialize -L . -f batch-byte-compile

SOURCES = $(filter-out %-autoloads.el %-pkg.el, $(wildcard *.el))

TARGETS = $(SOURCES:.el=.elc)

byte-compiled: $(TARGETS)

$(TARGETS): $(SOURCES)

%.elc: %.el
	$(EMACS) $(EMACS_FLAGS) $<

clean:
	rm *.elc
