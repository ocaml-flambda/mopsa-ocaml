#include "mopsa.h"

int incr(int x) {
  return x + 1;
}

void test_call_1() {
  int i;
  i = 10;
  _mopsa_assert_true(incr(i) == 11);
}
