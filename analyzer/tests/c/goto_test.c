#include "mopsa.h"

int f0() {
  if (0 < 1) {
    goto l1;
  } else {
    goto l2;
  };
 l1: return 0;
 l2: return 1;
}

void goto_tests () {
  int a = 0;
  int b = 0;
  int c = 0;
 l3 : c = 0;
 l2 : a++;
  c = b;
 l1 : b++;
  if (b - c < 10) {
    goto l1;
  };
  if (a < 10) {
    goto l2;
  };
  _mopsa_assert_exists(a==10 && b == 100);
}

void test_0() {
  _mopsa_assert_true(f0() == 0);
}

void test_1() {
  goto_tests();
}
