#include "mopsa.h"

void test_deref_int() {
  int i = 10;
  int *p = &i;
  _mopsa_assert_true(*p == i);
}
