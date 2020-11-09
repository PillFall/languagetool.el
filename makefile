EMACS = emacs

EMACS_FLAGS = -Q --batch -L ~/.emacs.d/elpa/request-20201026.2324 -L . -f batch-byte-compile

SOURCES = $(filter-out %-autoloads.el %-pkg.el,$(wildcard *.el))

TARGETS = $(SOURCES:.el=.elc)

byte-compiled: $(TARGETS)

$(TARGETS): $(SOURCES)
	$(EMACS) $(EMACS_FLAGS) $?
