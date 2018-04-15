#include "mopsa.h"


void test_int_condition_with_disjoint_cases() {
  int i = 2;
  int j;
  switch (i) {
  case 0: j = 0; break;
  case 1: j = 10; break;
  case 2: j = 20; break;
  case 3: j = 30; break;
  default: j = 40;
  }
  _mopsa_assert_true(j == 20);
}
