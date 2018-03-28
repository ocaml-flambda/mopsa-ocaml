#include "mopsa.h"

void test_address_of() {
  int i;
  int *p;
  p = &i;
  _mopsa_assert_true(p == &i);
}
