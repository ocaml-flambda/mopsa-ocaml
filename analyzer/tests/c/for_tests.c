#include "mopsa.h"

void test_classic_form() {
  int i, j = 0;
  for(i = 0; i < 10; i++) {
    j++;
  }
  _mopsa_assert_true(i == 10);
}

void test_cond_as_break() {
  int i, j = 0;
  for(i = 0; ; i++) {
    if (i >= 10) {
      break;
    }
    j++;
  }
  _mopsa_assert_true(i == 10);
}
