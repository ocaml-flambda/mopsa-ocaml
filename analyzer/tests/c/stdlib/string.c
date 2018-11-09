#include <string.h>

void test_memset_on_static_char_array() {
  char a[10];
  memset(a, 0xAA, 10);
  _mopsa_assert(a[0] == 0xAA);
}
