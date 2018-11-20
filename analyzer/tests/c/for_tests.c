#include "mopsa.h"

void test_classic_form() {
  int i;
  for(i = 0; i < 10; i++) {}
  _mopsa_assert(i == 10);
}
