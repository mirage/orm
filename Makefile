.PHONY: all
all:
	$(MAKE) -C hash/
	$(MAKE) -C hash/ reinstall

	$(MAKE) -C weakid/
	$(MAKE) -C weakid/ reinstall

	$(MAKE) -C type-of/
	$(MAKE) -C type-of/ reinstall

	$(MAKE) -C lib/
	$(MAKE) -C lib/ reinstall

	$(MAKE) -C lib_test/

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
