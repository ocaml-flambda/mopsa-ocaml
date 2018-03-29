#include "mopsa.h"

void test_array_assign() {
  int a[10];
  int i = 5;
  a[0] = i;
  _mopsa_assert_true(a[0] == 5);
}
