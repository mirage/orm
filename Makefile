.PHONY: all
all:
	@cd lib && $(MAKE)
	$(MAKE) reinstall
	@cd lib_test && $(MAKE)

.PHONY: test
test:
	@cd lib && $(MAKE)
	@cd lib_test && ($(MAKE) run || $(MAKE) slow)

.PHONY: install
install:
	@cd lib && $(MAKE) libinstall

.PHONY: uninstall
uninstall:
	@cd lib && $(MAKE) libuninstall

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: clean
clean:
	@cd lib && $(MAKE) clean
	@cd lib_test && $(MAKE) clean
	@cd lib_test && $(MAKE) -f Makefile.debug clean
