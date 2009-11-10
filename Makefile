-include Makefile.config

.PHONY: all
all:
	$(MAKE) -C hash/
	$(MAKE) -C hash/ reinstall

	$(MAKE) -C weakid/
	$(MAKE) -C weakid/ reinstall

	$(MAKE) -C type-of/
	$(MAKE) -C type-of/ reinstall

	$(MAKE) -C value/
	$(MAKE) -C value/ reinstall

	$(MAKE) -C lib/
	$(MAKE) -C lib/ reinstall

	$(MAKE) -C lib_test/

.PHONY: test
test:
	$(MAKE) -C hash/test/
	$(MAKE) -C weakid/test/
	$(MAKE) -C type-of/test/
	$(MAKE) -C value/test/
	$(MAKE) -C lib_test/ run
	$(MAKE) -C lib_test/ slow

.PHONY: install
install: all

.PHONY: uninstall
uninstall:
	$(MAKE) -C hash/ uninstall
	$(MAKE) -C weakid/ uninstall
	$(MAKE) -C type-of/ uninstall
	$(MAKE) -C value/ uninstall
	$(MAKE) -C lib/ uninstall

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: clean
clean:
	$(MAKE) -C hash/ clean
	$(MAKE) -C weakid/ clean
	$(MAKE) -C type-of/ clean
	$(MAKE) -C value/ clean
	$(MAKE) -C lib/ clean
	$(MAKE) -C lib_test/ clean
	$(MAKE) -C lib_test/ -f Makefile.debug clean
