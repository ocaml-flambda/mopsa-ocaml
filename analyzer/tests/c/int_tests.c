#include "mopsa.h"

void test_add() {
  int i, j;
  i = 10;
  j = 20;
  _mopsa_assert_true(i + j == 30);
}
