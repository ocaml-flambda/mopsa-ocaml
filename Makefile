TARGET = mopsa

.PHONY: all tests analyzer-tests clean merlin

all:
	$(MAKE) -C utils
	$(MAKE) -C parsers/c
	$(MAKE) -C parsers/python
	$(MAKE) -C analyzer

tests:
	$(MAKE) -C utils tests
	$(MAKE) -C parsers/c tests
	$(MAKE) -C parsers/python tests
	$(MAKE) -C analyzer tests

analyzer-tests:
	$(MAKE) -C analyzer tests

merlin:
	$(MAKE) -C utils merlin
	$(MAKE) -C parsers/c merlin
	$(MAKE) -C parsers/python merlin
	$(MAKE) -C analyzer merlin

clean:
	$(MAKE) -C utils clean
	$(MAKE) -C parsers/c clean
	$(MAKE) -C parsers/python clean
	$(MAKE) -C analyzer clean
