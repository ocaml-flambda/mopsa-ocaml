TARGET = mopsa

.PHONY: all tests clean

all:
	$(MAKE) -C utils
	$(MAKE) -C parsers/c
	$(MAKE) -C parsers/stubs
	$(MAKE) -C parsers/python
	$(MAKE) -C parsers/universal
	$(MAKE) -C analyzer

tests:
	$(MAKE) -C analyzer tests

clean:
	$(MAKE) -C utils clean
	$(MAKE) -C parsers/c clean
	$(MAKE) -C parsers/stubs clean
	$(MAKE) -C parsers/python clean
	$(MAKE) -C parsers/universal clean
	$(MAKE) -C analyzer clean
