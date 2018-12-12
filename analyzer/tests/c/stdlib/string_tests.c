#include <string.h>
#include "mopsa.h"

void test_memset_on_static_char_array() {
  char a[10];
  a[0] = 'a';
  memset(a, 0xAA, 10);
  _mopsa_assert(a[0] == 0xAA);
}
