EMACS = emacs

languagetool.elc: languagetool.el
	$(EMACS) -q -batch -f batch-byte-compile \
		languagetool.el
