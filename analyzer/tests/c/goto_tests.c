#include "mopsa.h"

void test_backward_goto() {
  int a = 0;

 entry:
  if (a >= 10) goto exit;
  a++;
  goto entry;

 exit:
  _mopsa_assert_exists(a==10);
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
