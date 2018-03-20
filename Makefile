TARGET = mopsa

.PHONY: all analyzer utils c python tests install clean uninstall

all: utils c python analyzer

utils:
	$(MAKE) -C utils

c: utils
	$(MAKE) -C parsers/c

python: utils
	$(MAKE) -C parsers/python

analyzer: utils c python
	$(MAKE) -C analyzer
	@mkdir -p bin
	@cp analyzer/bin/$(TARGET).native bin/$(TARGET)

tests:
	$(MAKE) -C utils tests
	$(MAKE) -C analyzer tests

install:
	$(MAKE) -C parsers/c install
	$(MAKE) -C parsers/python install

clean:
	$(MAKE) -C utils clean
	$(MAKE) -C parsers/c clean
	$(MAKE) -C parsers/python clean
	$(MAKE) -C analyzer clean
	$(RM) bin/$(TARGET)

uninstall:
	$(MAKE) -C parsers/c uninstall
	$(MAKE) -C parsers/python uninstall
