TARGET = mopsa

.PHONY: all tests analyzer-tests clean merlin

all:
	$(MAKE) -C utils
	$(MAKE) -C parsers/c
	$(MAKE) -C parsers/annotations/c
	$(MAKE) -C parsers/python
	$(MAKE) -C parsers/universal
	$(MAKE) -C analyzer

tests:
	$(MAKE) -C analyzer tests

clean:
	$(MAKE) -C utils clean
	$(MAKE) -C parsers/c clean
	$(MAKE) -C parsers/annotations/c clean
	$(MAKE) -C parsers/python clean
	$(MAKE) -C parsers/universal clean
	$(MAKE) -C analyzer clean
