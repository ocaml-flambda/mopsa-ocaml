#include "mopsa.h"

int incr(int x) {
  return x + 1;
}

int mult(int x, int y) {
  return x * y;
}

void test_call() {
  int i;
  i = 10;
  _mopsa_assert(incr(i) == 11);
}

void test_call_in_chain() {
  int i;
  i = 10;
  _mopsa_assert(mult(incr(i), 10) == 110);
}
