-include Makefile.config

.PHONY: all
all:
	$(MAKE) -C lib/

.PHONY: test
test: all
	$(MAKE) -C lib/
	$(MAKE) -C lib_test/ run

.PHONY: install
install: all
	$(MAKE) -C lib/ install

.PHONY: uninstall
uninstall:
	$(MAKE) -C lib/ uninstall

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: clean
clean:
	$(MAKE) -C lib/ clean
