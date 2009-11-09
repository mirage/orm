.DEFAULT: all

.PHONY: all
all:
	$(MAKE) -f Makefile.syntax
	$(MAKE) -f Makefile.core

install: all
	$(SUDO) ocamlfind install weakid META pa_weakid.cm{i,a} p4_weakid.cm{i,o} hweak.{o,cm{i,o,x}}

uninstall:
	$(SUDO) ocamlfind remove weakid

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

clean:
	$(MAKE) -f Makefile.syntax clean
	$(MAKE) -f Makefile.core clean
