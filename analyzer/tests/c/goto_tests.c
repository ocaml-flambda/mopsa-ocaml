#include "mopsa.h"

void test_backward_goto() {
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

int f0(int x) {
  if (x > 0) {
    goto l1;
  } else {
    goto l2;
  };
 l1: return 0;
 l2: return 1;
}


void test_goto_with_return_flows() {
  _mopsa_assert_true(f0(5) == 0);
}
