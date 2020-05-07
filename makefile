EMACS = emacs

EMACS_FLAGS = -Q --batch -L . -f batch-byte-compile

SOURCES = $(filter-out %-autoloads.el %-pkg.el,$(wildcard *.el))

TARGETS = $(SOURCES:.el=.elc)

byte-compiled: $(TARGETS)

$(TARGETS): $(SOURCES)
	$(EMACS) $(EMACS_FLAGS) $?
