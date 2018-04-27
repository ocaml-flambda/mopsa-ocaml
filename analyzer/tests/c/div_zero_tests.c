#include "mopsa.h"
int test_0() {
  int a = 4 / 2;
  int b = 2 * 0;
  int c = a / b;
  _mopsa_assert_error(DIVISION_BY_ZERO);
}
