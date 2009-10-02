.PHONY: all
all:
	@cd lib && $(MAKE)
	@cd lib_test && $(MAKE)

.PHONY: install
install:
	@cd lib && $(MAKE) libinstall

.PHONY: uninstall
uninstall:
	@cd lib && $(MAKE) libuninstall

.PHONY: clean
clean:
	@cd lib && $(MAKE) clean
	@cd lib_test && $(MAKE) clean
	@cd lib_test && $(MAKE) -f Makefile.debug clean
