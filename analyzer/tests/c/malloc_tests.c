#include <stdlib.h>
#include "mopsa.h"

void test_int_allocation() {
  int *p, i = 10;
  p = (int*)malloc(sizeof(int));
  *p = 10;
  _mopsa_assert_true(*p == i);
}
